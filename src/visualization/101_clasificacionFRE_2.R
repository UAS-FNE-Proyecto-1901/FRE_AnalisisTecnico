#' --- 
#' title: 'Clasificación de los FRE mediante análisis por clusterización' 
#' subtitle: 'Misión PRI 1901' 
#' date: '08-10--2021' 
#' author: 
#'        - name: Daniel S. Parra G. 
#'          email: dsparrag@minsalud.gov.co 
#'          institute: FNE 
#' institute: 
#'        - FNE: Misión PRI 1901 - Fondo Nacional de Estupefacientes 
#' abstract: |
#'       A continuación, se presenta un análisis mediante agrupación por clústers
#'       de los FRE de acuerdo a variables de desempeño y complejidad de los mismos.
#'       
#'       En este análisis se hace un preprocesamiento de los datos mediante 
#'       estandarización Z.
#'       
#'        
#' output:  
#'      - pdf_document: default 
#'      - html_document: default 
#' always_allow_html: true 
#' --- 

#+ setup1, warning = FALSE, message=FALSE
require(readxl)
require(skimr)
require(PerformanceAnalytics)
require(factoextra)
require(patchwork)
require(clValid)
require(plotly)
require(tidymodels)
require(tidyverse); theme_set(theme_bw())
source(file.path('src', 'models', '900_funcionesAlmacenamientoGrafico.R'), encoding = 'UTF-8')
fig_path <- file.path('figures', '011_clasificacion')

#+ lectura-datos
data <-
  read_excel(file.path(
    'data',
    'raw',
    'ClasificacionFRE',
    'variablesClasificacionFRE.xlsx'
  ), na = '-') %>% 
  mutate(Departamento...2 = str_to_title(Departamento...2))


#+ ggCorrelacion, fig.width=12, fig.asp=0.7, out.width="95%", fig.align='center',fig.pos="t",fig.cap="Correlación entre variables de PCA"
data %>%
  select(!contains('Departamento')) %>% 
  chart.Correlation(., histogram = TRUE, pch = 19)

if (knitr::is_html_output()) {
  skimr::skim(data)
}

#' # 1. Análisis por PCA
#' 
#' 

# 1. Análisis por PCA -----------------------------------------------------

pca1 <- data %>%
  drop_na() %>% 
  column_to_rownames('Departamento...2') %>% 
  select(!contains('Departamento')) %>% 
  prcomp(., scale = TRUE, center = TRUE)

dimVar <- pca1$sdev %>% {.^2*100/sum(.^2)}


#+ ggPCA0-var, fig.width=8, fig.asp=0.8, out.width="90%", fig.align='center',fig.pos="t",fig.cap="Distribución de las varianzas"
plot(pca1, main = 'Distribución de varianza')


gg1 <- fviz_pca_ind(pca1,
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
) + labs(title = 'PCA de individuos')

#+ ggPCA1-ind, fig.width=12, fig.asp=0.7, out.width="90%", fig.align='center',fig.pos="t",fig.cap="Individuos representados en PCA"
gg1
guardarGGplot(gg1, '001_fig_ind', 8, 6, fig_path)

gg2 <- fviz_pca_var(
  pca1,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE
) + labs(title = 'PCA - Explicación de variables')

#+ ggPCA1-var, fig.width=12, fig.asp=0.5, out.width="90%", fig.align='center',fig.pos="t",fig.cap="Explicación de variables en PCA"
gg2
guardarGGplot(gg2, '002_fig_var', 8, 6, fig_path)

#' # 2. Preprocesamiento
#' 
#' 

# 2. Preprocesamiento ---------------------------------------

norm_trans <- data %>% 
  recipe(~ Presupuesto_2020 + Presupuesto_Salud_2020 + Prop_productosCD_2021 + 
           `Cumplimiento_A1_2020-2021-06` + `Cumplimiento_A2_2020-2021-06` + 
           PropPortafolio + No_Inscritos + T_adq_Recet + Departamento...2) %>% 
  update_role(Departamento...2, new_role = 'ID') %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_naomit(all_numeric_predictors())

norm_obj <- norm_trans %>% 
  prep(training = data)

clean_data <- bake(norm_obj, data) %>% 
  column_to_rownames('Departamento...2')

#' # 3. Análisis de Clústers por Kmeans
#' 
#' 

# 3. Análisis de Clústers por Kmeans ---------------------------------------

kmValid <- clValid(clean_data, 2:30, clMethods = 'kmeans', 
                   validation = c('internal', 'stability'))


kmValid %>% optimalScores()

kmValidS <- as.data.frame(measures(kmValid)) %>% 
  {rownames_to_column(as_tibble(t(.)), var = 'k')} %>% 
  mutate(k = as.double(k) + 1,
         tot_withins_ss = map_dbl(k, ~ kmeans(clean_data, .x)$tot.withinss)) %>% 
  pivot_longer(cols = !matches('k'))  %>% 
  # Selección manual de óptimo de índices de validez 
  mutate(koptim = case_when(
    name == 'tot_withins_ss' ~ 9,
    name == 'AD' ~ 5,
    name == 'APN' ~ 2,
    name == 'Connectivity' ~ 2,
    name == 'FOM' ~ 5,
    name == 'Dunn' ~ 6,
    name == 'Silhouette' ~ 6,
    TRUE ~ 3
  ))

gg3 <- kmValidS  %>%  
  ggplot(aes(x = k, y = value)) + 
  geom_point() + geom_line() +
  geom_vline(aes(xintercept = koptim), col = 'blue4', lty = 'dashed') + 
  ggrepel::geom_label_repel(data = subset(kmValidS, k == koptim),
            aes(label = koptim)) +
  ylab('Valor') + 
  scale_color_manual(values = c('Sí' = 'red', 'No' = NA)) +
  facet_wrap(vars(name), scales = 'free_y')

#+ ggKmeans1, fig.width=8, fig.asp=0.6, out.width="80%", fig.align='center',fig.pos="t",fig.cap="Criterio de codo para clústers por Kmeans"
gg3
guardarGGplot(gg3, '003_elbowlKmeans', 8, 6, fig_path)

kmValidS %>% 
  distinct(name, koptim) %>% 
  arrange(koptim)

g1 <- kmeans(clean_data, 5) %>%
  fviz_cluster(., data = clean_data, stand = T, 
               show.clust.cent = F, repel = T, max.overlaps=Inf) +
  scale_fill_brewer(palette = 'Dark2') + 
  scale_colour_brewer(palette = 'Dark2') + 
  theme_bw() + labs(title = NULL)

#+ ggKmeans2, fig.width=8, fig.asp=0.6, out.width="90%", fig.align='center',fig.pos="t",fig.cap="Clústers Kmeans visualizados en primeros dos PC"
g1
guardarGGplot(g1, '004_clusterKmeans', 8, 5, fig_path)


g2 <- kmeans(clean_data, 5) %>%
  fviz_cluster(., data = clean_data, axes = c(1, 2)) +
  theme_bw() + labs(title = NULL)
g3 <- kmeans(clean_data, 5) %>%
  fviz_cluster(., data = clean_data, axes = c(1, 3)) +
  theme_bw() + labs(title = NULL)
g4 <- kmeans(clean_data, 5) %>%
  fviz_cluster(., data = clean_data, axes = c(2, 3)) +
  theme_bw() + labs(title = NULL)
g5 <- kmeans(clean_data, 5) %>%
  fviz_cluster(., data = clean_data, axes = c(1, 4)) +
  theme_bw() + labs(title = NULL)

ggt <- wrap_plots(g2, g3, g4, g5)

#+ ggKmeans3, fig.width=12, fig.asp=0.7, out.width="100%", fig.align='center',fig.pos="t",fig.cap="Clústers Kmeans visualizados en varios componentes"
ggt
guardarGGplot(ggt, '005_cluz_group', 12, 8, fig_path)

#' # 3. Clúster jerárquicos
#' 
funClusters_2 <- function(data, k) {
  t1 <- data %>% 
    dist(method = 'euclidean') %>% 
    hclust(method = 'complete')
  
  t2 <- cutree(t1, k)
  return(list(clust = t1, tree = t2))
}



#+ ggHclust1, fig.width=12, fig.asp=0.8, out.width="100%", fig.align='center',fig.pos="t",fig.cap="Dendrograma de análisis por clústers"
p1 <- plot(funClusters_2(clean_data, 3)$clust, cex = 0.6, hang = -1, ylab = 'Altura',
     main = 'Dendrograma de clúster', xlab = NULL)

pdf(file.path(fig_path, '010_dendrograma.pdf'), width = 12, height = 8)
plot(funClusters_2(clean_data, 3)$clust, cex = 0.6, hang = -1, ylab = 'Altura',
     main = 'Dendrograma de clúster', xlab = NULL)
dev.off()

saveRDS(p1, file.path(fig_path, '010_dendrograma.rds'))

gg1 <- funClusters_2(clean_data, 3)$clust$height %>%
  as.tibble() %>%
  add_column(groups = length(funClusters_2(clean_data, 3)$clust$height):1) %>%
  rename(height = value) %>% 
  ggplot(aes(x=groups, y = height)) +
  geom_point() + geom_line() +
  geom_vline(xintercept = 5, lty = 'dashed', col = 'blue3') +
  ylab('Altura') + 
  xlab('N.° de Clusters, k')

#+ ggHclust2, fig.width=8, fig.asp=0.6, out.width="80%", fig.align='center',fig.pos="t",fig.cap="Criterio de codo para clústers jerárquicos"
gg1
guardarGGplot(gg1, '012_elbowlWard', 6, 4, fig_path)

# Índices de validez de clúster para algoritmo jerárquico

#+ kmValid2b, include=FALSE, warning=FALSE, message=FALSE
kmValid <- clValid(clean_data, 2:30, clMethods = 'hierarchical', 
                   validation = c('internal', 'stability'), 
                   method = 'ward')

kmValid %>% optimalScores()

kmValidS <- as.data.frame(measures(kmValid)) %>% 
  {rownames_to_column(as_tibble(t(.)), var = 'k')} %>% 
  mutate(k = as.double(k) + 1) %>% 
  pivot_longer(cols = !matches('k'))  %>% 
  # Selección manual de óptimo de índices de validez 
  mutate(koptim = case_when(
    # name == 'AD' ~ 5,
    # name == 'FOM' ~ 5,
    name == 'ADM' ~ 7,
    name == 'APN' ~ 5,
    name == 'Silhouette' ~ 8,
    name == 'Connectivity' ~ 2,
    TRUE ~ 8
  ))

gg1b <- ggplot(kmValidS, aes(x = k, y = value)) + 
  geom_point() + geom_line() +
  geom_vline(aes(xintercept = koptim), col = 'blue4', lty = 'dashed') + 
  ggrepel::geom_label_repel(data = subset(kmValidS, k == koptim),
                            aes(label = koptim)) +
  ylab('Valor') + 
  scale_color_manual(values = c('Sí' = 'red', 'No' = NA)) +
  facet_wrap(vars(name), scales = 'free_y')

#+ ggKmeans1b, fig.width=8, fig.asp=0.6, out.width="80%", fig.align='center',fig.pos="t",fig.cap="Criterios de codo para clústers por Jerárquico"
gg1b
guardarGGplot(gg1b, '003b_elbowlKmeans', 8, 6, fig_path)


gg2 <- clean_data %>% 
  {fviz_cluster(list(data = ., cluster = funClusters_2(., 6)$tree), 
                repel = T, max.overlaps = Inf)} +
  theme_bw() + labs(title = NULL) +
  scale_color_brewer(palette = 'Dark2', name = 'Clúster') + 
  scale_fill_brewer(palette = 'Dark2', name = 'Clúster') +  
  scale_shape_discrete(name = 'Clúster') 

#+ ggHclust3, fig.width=8, fig.asp=0.6, out.width="90%", fig.align='center',fig.pos="t",fig.cap="Clústers Jerárquicos visualizados en primeros dos componentes"
gg2
guardarGGplot(gg2, '013_cluz_group', 8, 5, fig_path)


funClusters_3 <- function(axes) {
  clean_data %>% 
    {fviz_cluster(list(data = ., cluster = funClusters_2(., 6)$tree), 
                  axes = axes, ellipse = T)} +
    theme_bw() + labs(title = NULL) +
    scale_color_discrete(name = 'Clúster') + 
    scale_shape_discrete(name = 'Clúster') + 
    scale_fill_discrete(name = 'Clúster') 
}

g2 <- funClusters_3(c(1,2))
g3 <- funClusters_3(c(1,3))
g4 <- funClusters_3(c(2,3))
g5 <- funClusters_3(c(1,4))


ggt <- wrap_plots(g2, g3, g4, g5)

#+ ggHclust4, fig.width=12, fig.asp=0.7, out.width="100%", fig.align='center',fig.pos="t",fig.cap="Clústers jerarquizados visualizados en varios componentes"
ggt
guardarGGplot(ggt, '014_cluz_group2', 12, 10, fig_path)



colors <- c(`1` = '#a705b3', `2` = '#7081ff', `3` = '#ff0000',
            `4` = '#8cffb7', `5` = '#00ff5e')

# Creación de hover 
hov_data <- data %>%
  mutate(
    Prec2020 = round(Presupuesto_2020/1e9, 2),
    PrecSalud2020 = round(Presupuesto_Salud_2020/1e9, 2),
    Hover = glue::glue(
      "<b>{Departamento...2}</b> <br>",
      "<i>Complejidad</i> <br>",
      "Presupuesto (miles de millones): {Prec2020} <br>",
      "Presupuesto Salud (miles de millones): {PrecSalud2020} <br>",
      "No. de inscritos: {No_Inscritos} <br>",
      "Prop. uso portafolio: {round(PropPortafolio,2)} <br>",
      "<i>Eficiencia</i> <br>",
      "Prop. ventas por FRE vs CD: {round(Prop_productosCD_2021,2)} <br>",
      "Prop. cumplimiento A1: {round(`Cumplimiento_A1_2020-2021-06`,2)} <br>",
      "Prop. cumplimiento A2: {round(`Cumplimiento_A2_2020-2021-06`,2)} <br>",
      "Raz. tiempo de adq rec vs no. rec: {round(T_adq_Recet, 2)}"
    )
  ) %>% 
  select(Departamentos = Departamento...2, Hover)



trans_data <- as_tibble(pca1$x, rownames = 'Departamentos') %>% 
  left_join(funClusters_2(clean_data, 6)$tree %>% 
              as_tibble(rownames = 'Departamentos'), by = 'Departamentos') %>% 
  rename(Grupo_hclus = value) %>% 
  mutate(colores = colors[Grupo_hclus],
         Grupo_hclus = factor(Grupo_hclus))


fig <- trans_data %>% 
  left_join(hov_data, by = 'Departamentos') %>% 
  plot_ly(name = ~Grupo_hclus) %>% 
  add_trace(x = ~PC1, y = ~PC2, z = ~PC3,
            customdata = ~Hover, 
            hovertemplate = "%{customdata}", 
            mode = 'markers',
            type = 'scatter3d',
            color = ~Grupo_hclus,
            colors = "Dark2"
            # marker = list(color = ~colores, size=6)
            ) %>% 
  layout(scene = list( 
    xaxis = list(title = paste0('PC1 (', round(dimVar[1], 1), '%)'), range = c(-7,+7)),
    yaxis = list(title = paste0('PC2 (', round(dimVar[2], 1), '%)'), range = c(-2.5,+2.5)),
    zaxis = list(title = paste0('PC3 (', round(dimVar[3], 1), '%)'), range = c(-3,+3))
  )) 

#+ PC3D-1, fig.width=12, fig.asp=0.7, out.width="100%", fig.align='center',fig.pos="t",fig.cap="PCA3d"
if (knitr::is_html_output()) {
  fig
}

guardarPlotly(fig, '020_cluster_1', ruta = fig_path, libdir = 'plotly')

trans_data1 <- data %>% 
  left_join(funClusters_2(clean_data, 6)$tree %>% 
              as_tibble(rownames = 'Departamentos'), 
            by = c('Departamento...2' = 'Departamentos')) %>% 
  rename(Grupo_hclus = value, Departamentos = Departamento...2) %>% 
  mutate(Grupo_hclus = as.integer(Grupo_hclus))

tr_df1 <- trans_data1 %>% 
  group_by(Grupo_hclus) %>% 
  summarise(across(!matches('Departamento'), mean))

tr_df1

tr_df1 %>% 
  write_csv(file.path('references', 'clusterRead.csv'))



trans_data1 %>%
  mutate(across(!matches('Departamento|Grupo'), 
                function(x) x >= mean(x, na.rm = T))) %>% 
  group_by(Grupo_hclus) %>% 
  summarise(across(!matches('Departamento'), 
                   function(x) sum(x)/n())) %>% 
  drop_na(Grupo_hclus) %>% 
  column_to_rownames('Grupo_hclus') %>% 
  t()


aplicCuartiles <- function(rango) {
  
  quant <- quantile(rango, probs = seq(0, 1, by = 0.20), na.rm=TRUE) %>% 
    unique()
  
  quant_labels <- paste0('G', seq(1, length(quant)-1))
  
  # return(quant_labels)
  
  cut(rango,
      breaks= quant,
      labels = quant_labels,
      include.lowest=TRUE)
}

trans_data1 %>%
  filter(!is.na(Grupo_hclus)) %>% 
  mutate(across(!matches('Departamento|Grupo'), 
                ~aplicCuartiles(.x))) %>% 
  group_by(Grupo_hclus) %>% 
  summarise(across(!matches('Departamento'), 
                   function(x) paste0(unique(x), collapse = ','))) %>% 
  drop_na(Grupo_hclus) %>% 
  column_to_rownames('Grupo_hclus') %>% 
  t() %>% View()



trans_data1 %>%
  mutate(across(!matches('Departamento'), ~mean(.x, na.rm = T), 
                .names = "{.col}_mn")) %>% View()

# group_by(Grupo_hclus) %>%
#   summarise(across(!matches('Departamento'), function(x) {
#     sum(ifelse(x > mean(x), 1, 0)) / n()
#   }))

fig1 <- trans_data1 %>% 
  plot_ly(x = ~No_Inscritos, y = ~`Cumplimiento_A2_2020-2021-06`, 
          z = ~PropPortafolio, split = ~Grupo_hclus,
          colors = colors, name = ~Grupo_hclus, text = ~Departamentos, 
          hovertemplate = "%{text}<br>N.° inscritos: %{x}<br>Cumplimiento A2: %{y}<br>Prop. portafolio: %{z}")


#+ PC3D-2, fig.width=12, fig.asp=0.7, out.width="100%", fig.align='center',fig.pos="t",fig.cap="PCA3d-1"
if (knitr::is_html_output()) {
  fig1 %>% add_markers()
}



