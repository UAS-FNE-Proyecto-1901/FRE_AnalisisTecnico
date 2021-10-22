#' --- 
#' title: 'Análisis de preguntas relacionadas con recetarios oficiales' 
#' subtitle: 'Misión PRI 1901' 
#' date: '30-07-2021' 
#' author: 
#'        - name: Daniel S. Parra G. 
#'          email: dsparrag@minsalud.gov.co 
#'          institute: FNE 
#' institute: 
#'        - FNE: Misión PRI 1901 - Fondo Nacional de Estupefacientes 
#' abstract: |
#'        Análisis de preguntas relacionadas con recetarios oficiales. 
#'        Se realiza una revisión de la segunda parte de la herramienta de 
#'        recolección a Fondos Rotatorio de Medicamentos.
#'         
#' output:  
#'      - html_document: default 
#'      - pdf_document: default 
#' always_allow_html: true 
#' --- 

#+ setup1, warning=FALSE, message=FALSE
require(plotly)
require(tidyverse); theme_set(theme_bw())
require(lubridate)
require(ggrepel)
require(ggsflabel)
require(patchwork)

source(file.path('src', 'data', '901_funcionesMapa.R'), encoding = 'UTF-8')
source(file.path('src', 'models', '900_funcionesAlmacenamientoGrafico.R'), encoding = 'UTF-8')
source(file.path('src', 'visualization', '900_funcionExtraccionDummies.R'), encoding = 'UTF-8')

#'-------------------------------------------------------------------------------
# 1. Lectura de datos base ------------------
#'-------------------------------------------------------------------------------
# 
df <- read_csv(file.path('data', 'processed', '001_Herramienta_Procesada.csv')) %>% 
  mutate(
    Departamento = str_replace(Departamento, '(?<=San Andrés).+', ''),
    Departamento_1 = str_replace(Departamento_1, 'ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA', 'SAN ANDRÉS'))

df_DIVIPOLA <-
  read_csv(file.path('data', 'processed', '798_DANE_DEPARTAMENTO.csv'))

df_MUNICIPIO <-
  read_csv(file.path('data', 'processed', '799_DANE_DIVIPOLA.csv'), 
           locale = locale(encoding = 'latin1'))

df_total <- df %>% 
  right_join(colombiaGeoDF, by = c('CodigoDepartamento' = 'DPTO'))

df_total

#'-------------------------------------------------------------------------------
# 2. Número de existencias actuales en el FRE------------------
#'-------------------------------------------------------------------------------
labExistencias <- c('Sin existencias', '< 1000 u', '1000-5000 u', '> 5000 u')

df_total <- df_total %>% 
  mutate(ExistenciasRecetarios = case_when(
    `3.01 Existencias actuales de recetarios en el FRE` == 0 ~ labExistencias[1],
    `3.01 Existencias actuales de recetarios en el FRE` < 1000 ~ labExistencias[2],
    (`3.01 Existencias actuales de recetarios en el FRE` >= 1000) & 
      (`3.01 Existencias actuales de recetarios en el FRE` < 5000) ~ labExistencias[3],
    `3.01 Existencias actuales de recetarios en el FRE` >= 5000 ~ labExistencias[4]
  ))

escl_col <- c('red', 'orange', 'yellow', 'green')
names(escl_col) <- c(labExistencias)

gRecetarios1 <- df_total %>% 
  mutate(ExistenciasRecetarios = 
           factor(ExistenciasRecetarios, levels = labExistencias)) %>% 
  creacionCloroPletCol(geometry, ExistenciasRecetarios)

gRecetarios1[[1]] <- gRecetarios1[[1]] + 
  scale_fill_manual(name = 'Existencia \nRecetarios', values =escl_col) + 
  theme(legend.position = 'right',
        axis.text = element_blank()) + 
  labs(title = 'Existencias de recetarios en el FRE') + 
  guides(
    fill = guide_legend(title.position = 'top'))

gRecetarios1[[2]] <- gRecetarios1[[2]] + 
  scale_fill_manual(name = 'Existencia \nRecetarios', values =escl_col)
gRecetarios1[[3]] <- gRecetarios1[[3]] + 
  scale_fill_manual(name = 'Existencia \nRecetarios', values =escl_col)

gRecetarios1

guardarGGplot(gRecetarios1, '027_existenciaRecetarios_1', 8, 6)

#'-------------------------------------------------------------------------------
# 3. Existencias en circulación en el departamento------------------
#'-------------------------------------------------------------------------------
promedioCirculacionRecetarios <- function(string) {
  variable <- tryCatch({
    string1 <- as.double(string)
  }, warning = function(w) {
    string1 <- str_split(string, '\\-') %>% map_dbl(~mean(as.double(.x)))
  }, error = function(e) {
    string1 <- str_split(string, '\\-') %>% map_dbl(~mean(as.double(.x)))
  })
  
  variable
}


df_total <- df_total %>%
  mutate(
    ExistenciasCirculacion = map_dbl(
      `3.02 Existencias estimadas de recetarios en circulación en el departamento (promedio mensual)`,
      ~ promedioCirculacionRecetarios(.x)
    )
  )


gRecetarios2 <- df_total %>% 
  creacionCloroPletCol(geometry, ExistenciasCirculacion)

gRecetarios2[[1]] <- gRecetarios2[[1]] + 
  paletteer::scale_fill_paletteer_c("viridis::viridis", 
                                    name = 'Existencias en circulación') +  
  theme(legend.position = 'bottom',
        axis.text = element_blank()) + 
  labs(title = 'Circulación mensual de \nexistencias de recetarios') +
  guides(
    fill = guide_colourbar(barwidth = 20, title.position = 'top'))

gRecetarios2[[2]] <- gRecetarios2[[2]] + 
  paletteer::scale_fill_paletteer_c("viridis::viridis", 
                                    name = 'Existencias en circulación')
gRecetarios2[[3]] <- gRecetarios2[[3]] + 
  paletteer::scale_fill_paletteer_c("viridis::viridis", 
                                    name = 'Existencias en circulación')
gRecetarios2
guardarGGplot(gRecetarios2, '028_existenciaRecetarios_2', 7, 5)

#'-------------------------------------------------------------------------------
# 4. Duración estimada de las existencias ------------------
#'-------------------------------------------------------------------------------

col1 <- '3.03 Tiempo de duración proyectada de las actuales existencias de recetarios (semanas).'

gRecetarios3 <- df_total %>% 
  creacionCloroPletCol(geometry, !!sym(col1))

gRecetarios3[[1]] <- gRecetarios3[[1]] + 
    scale_fill_continuous(type = 'viridis', name = 'Duración recetarios (días)') + 
  theme(legend.position = 'bottom',
        axis.text = element_blank()) + 
  labs(title = 'Duración de existencias de recetarios \n(semanas)') +
  guides(
    fill = guide_colourbar(barwidth = 20, title.position = 'top'))


gRecetarios3[[2]] <- gRecetarios3[[2]] + 
  scale_fill_continuous(type = 'viridis', name = 'Duración recetarios (días)') 
gRecetarios3[[3]] <- gRecetarios3[[3]] + 
  scale_fill_continuous(type = 'viridis', name = 'Duración recetarios (días)') 

gRecetarios3
guardarGGplot(gRecetarios3, '029_existenciaRecetarios_3', 7, 5)

#'-------------------------------------------------------------------------------
# 5. Número de prescripciones por recetario ------------------
#'-------------------------------------------------------------------------------
col1 <- '3.05 N.º de prescripciones por recetario'

dftot1 <- df_total %>%
  select(col1 = col1) %>% table(dnn = 'conteo') %>% as_tibble

gRecetarios4 <- df_total %>% 
  select(col1 = col1) %>% 
  ungroup() %>% 
  drop_na() %>% 
  ggplot(aes(x = factor(col1))) + 
  geom_bar(stat = 'count', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = ..count..), stat = 'count', vjust = -0.5) + 
  coord_cartesian(ylim = c(0, max(dftot1$n)*1.2)) +
  ylab('Frecuencia') + xlab('N.° prescripciones por recetario') +
  labs(title = 'Número de prescripciones por recetario') + 
  theme(panel.grid = element_blank())
  
gRecetarios4

guardarGGplot(gRecetarios4, '030_existenciaRecetarios_4', 6, 4)

#'-------------------------------------------------------------------------------
# 6. Costo de adquisición por recetarios------------------
#'-------------------------------------------------------------------------------

col1 <- '3.06 Costo de adquisición del recetario (COP)'
col2 <- '3.07 Precio de venta del recetario (COP)'

gCostoRecetario <- df_total %>% 
  creacionCloroPletCol(geometry, !!sym(col1))

gCostoRecetario[[1]] <- gCostoRecetario[[1]] +
  scale_fill_continuous(label = scales::dollar, type = 'viridis', ) + 
  theme(legend.position = 'bottom',
        axis.text = element_blank()) + 
  labs(title = 'A') +
  guides(
    fill = guide_colourbar(barwidth = 15, title.position = 'top', title = ''))

gCostoRecetario[[2]] <- gCostoRecetario[[2]] +
  scale_fill_continuous(label = scales::dollar, type = 'viridis')
gCostoRecetario[[3]] <- gCostoRecetario[[3]] +
  scale_fill_continuous(label = scales::dollar, type = 'viridis')

gCostoRecetario



gPVTARecetario <- df_total %>% 
  creacionCloroPletCol(geometry, !!sym(col2))

gPVTARecetario[[1]] <- gPVTARecetario[[1]] +
  scale_fill_continuous(label = scales::dollar, type = 'viridis', ) + 
  theme(legend.position = 'bottom',
        axis.text = element_blank()) + 
  labs(title = 'B') +
  guides(
    fill = guide_colourbar(barwidth = 15, title.position = 'top', title = ''))

gPVTARecetario[[2]] <- gPVTARecetario[[2]] +
  scale_fill_continuous(label = scales::dollar, type = 'viridis')
gPVTARecetario[[3]] <- gPVTARecetario[[3]] +
  scale_fill_continuous(label = scales::dollar, type = 'viridis')

gPVTARecetario

gTotal <- wrap_plots(gCostoRecetario, gPVTARecetario)  + 
  plot_layout(ncol = 2, nrow=1, widths = c(0.5,0.5), tag_level = 'new') + 
  plot_annotation(tag_levels = 'A')

gTotal

guardarGGplot(gTotal, '031_costoRecetario', 10, 4)

df_total <- df_total %>% 
  mutate(MargenGanancia = map2_dbl(`3.06 Costo de adquisición del recetario (COP)`,
         `3.07 Precio de venta del recetario (COP)`, ~((.y - .x)/.x)))

meanMargenGanancia <- df_total$MargenGanancia %>% mean(na.rm= TRUE)

posicion1Lineas <- data.frame(
  label = c('200%', '300%', '400%', '500%'),
  xpos = rep(32000, 4),
  ypos = c(5e4, 9.2e4, 1.3e5, 1.6e5)
)

gComparativo1 <- df_total %>% 
  mutate(Departamento_1 = str_to_sentence(Departamento_1)) %>% 
  ggplot(aes(x = `3.06 Costo de adquisición del recetario (COP)`, 
             y = `3.07 Precio de venta del recetario (COP)`)) + 
  geom_point() + 
  stat_function(fun = function(x) 2*x, geom = 'line', linetype = 'dashed', col = 'blue') +
  stat_function(fun = function(x) 3*x, geom = 'line', linetype = 'dashed', col = 'blue') +
  stat_function(fun = function(x) 4*x, geom = 'line', linetype = 'dashed', col = 'blue') +
  stat_function(fun = function(x) 5*x, geom = 'line', linetype = 'dashed', col = 'blue') +
  stat_function(fun = function(x) meanMargenGanancia*x, geom = 'line', linetype = 'dotted', col = 'red') +
  geom_text(data = posicion1Lineas, aes(x = xpos, y = ypos, label = label), hjust = 0, col = 'blue') + 
  geom_label_repel(aes(label = Departamento_1), size = 2, max.overlaps = Inf) +
  coord_cartesian(xlim = c(0, 35000)) +
  scale_x_continuous(labels = scales::dollar) + 
  scale_y_continuous(labels = scales::dollar) + 
  xlab('Costo de adquisición (COP)') + 
  ylab('Precio de venta (COP)')

#+ comparativoMargen, fig.width = 8, fig.height=6, out.width="90%"
gComparativo1
guardarGGplot(gComparativo1, '032_comparativoDepartamentos', 8, 6)

#'-------------------------------------------------------------------------------
col1 <- '3.06 Costo de adquisición del recetario (COP)'
col2 <- '3.07 Precio de venta del recetario (COP)'

df_total <- df_total %>% 
  mutate(
    margen0 = .data[[col2]]*100/.data[[col1]],
    margen1 = cut(margen0, breaks = c(0, 100,200,300,400,500,600,1000))
  )

# df_total %>% 
#   select(Departamento_1, margen1) %>% View()

ggComparativo2 <- df_total %>%
  drop_na(any_of(c('margen1'))) %>% 
  ggplot(aes(x = margen1)) + 
  geom_bar(stat = 'count', 
           fill = '#6699ff', color = 'black', alpha = 0.6) +
  geom_label(stat = 'count', aes(label = ..count..), vjust=-0.4) +
  scale_y_continuous(expand = c(0, 0, 0.2, 0)) + 
  xlab('Márgen de ganancia para recetarios oficiales') + 
  ylab('Frecuencia')

#+ comparativoMargen2, fig.width = 8, fig.height=6, out.width="90%"
ggComparativo2
guardarGGplot(ggComparativo2, '032b_comparativoDepartamentos', 6, 4)

gmComparativo3 <- df_total %>% 
  # drop_na(any_of(c('margen1'))) %>% 
  creacionCloroPletCol(geometry, margen1)

gmComparativo3[[1]] <- gmComparativo3[[1]] + 
  geom_sf_label_repel(
    data = drop_na(df_total, any_of(c('margen1'))),
    aes(label = paste0(formatC(margen0, 0, format = "d"), ' %'),
        geometry = geometry), size = 3,
    max.overlaps = Inf) + 
  scale_fill_manual(name = 'Márgen (%)', 
                    values = c('purple', 'blue', 'green4', 'green1', 
                               'yellow', 'orange', 'red'),
                    breaks = levels(df_total$margen1)) +
  theme(legend.position = 'bottom',
        legend.title = element_blank(),
        axis.text = element_blank(), 
        axis.title = element_blank()) + 
  labs(title = 'Márgenes de venta de recetarios') 

#+ mapaMargen3, fig.width = 8, fig.height=6, out.width="90%"
gmComparativo3
guardarGGplot(gmComparativo3, '032c_comparativoDepartamentos', 8, 6)

# ¿El margen de ganancia de recetarios se corresponde con el número de personas en el FRE?
df_total <- df_total %>% rowwise() %>% 
  mutate(Profesiones = list(c_across(matches('Profesión\\sdel\\s(personal|funcionario)'))))

df_total$NoPersonas <- df_total$Profesiones %>% 
  map_dbl(function(x){sum(!is.na(x))})

df_total['NoPersonas1'] <- df_total$No.PersonasVinculadasDirectamente + 
  df_total$No.PersonasVinculadasAfiliacion

gComparativo2 <- df_total %>% 
  mutate(Departamento_1 = str_to_sentence(Departamento_1)) %>% 
  ggplot(aes(x = NoPersonas1, y = MargenGanancia)) + 
  geom_point() +
  stat_smooth(method = 'lm', formula = 'y ~ x') + 
  geom_label_repel(
    filter(df_total, str_detect(Departamento_1, 'LA\\s|NARIÑO')),
    mapping = aes(label = str_to_title(Departamento_1))) +
  scale_y_continuous(labels = scales::percent) + 
  ylab('Margen de ganancia por \n recetarios (%)') +
  xlab('N.° de personas')

gComparativo3 <- df_total %>% 
  ggplot(aes(x = factor(NoPersonas), y = MargenGanancia)) +
  geom_boxplot(fill = '#6699ff') + 
  xlab('Numero de personas') + 
  ylab('Margen de ganancia \n por recetarios (%)')

#+ figuraCompuesta1, fig.width = 9, fig.height=5, out.width="90%"

(gComparativo2 + gComparativo3)

(gComparativo2 + gComparativo3) %>% 
  guardarGGplot('033_comparativoDepartamentos1', 9, 5)

#'-------------------------------------------------------------------------------
# 7. Precio de venta por prescripción------------------
#'-------------------------------------------------------------------------------

col1 <- '3.08 Precio de venta por prescripción (COP)'

gPVTARecetario7 <- df_total %>% 
  creacionCloroPletCol(geometry, !!sym(col1))

gPVTARecetario7[[1]] <- gPVTARecetario7[[1]] +
  scale_fill_continuous(label = scales::dollar, type = 'viridis', name = NULL) + 
  theme(legend.position = 'bottom',
        axis.text = element_blank()) + 
  labs(title = 'Precio de venta de recetarios por prescripción') +
  guides(
    fill = guide_colourbar(barwidth = 15, title.position = 'top'))

gPVTARecetario7[[2]] <- gPVTARecetario7[[2]] +
  scale_fill_continuous(label = scales::dollar, type = 'viridis', name = NULL) 
gPVTARecetario7[[3]] <- gPVTARecetario7[[3]] +
  scale_fill_continuous(label = scales::dollar, type = 'viridis', name = NULL) 

gPVTARecetario7
guardarGGplot(gPVTARecetario7, '034_PVTA_recetarios_prescripcion', 7, 4)

#'-------------------------------------------------------------------------------
# 8. Modalidades de contratación para adquisición de Recetarios------------------
#'-------------------------------------------------------------------------------
col1 <- '3.13. ¿Qué modalidades de selección se utilizan en la contratación para adquisición de recetarios oficiales en el Departamento?'

tModalidadAdquisicion <- pull(df_total, col1) %>% 
  separarDummies(descartar = TRUE) %>% 
  apply(., 2, sum) %>% 
  data.frame(Tipo=names(.), Frec=., row.names=NULL)

gModalidadAdquisicion <- tModalidadAdquisicion %>% 
  ggplot(aes(y = Tipo, x = Frec)) + 
  geom_bar(stat = 'identity', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = Frec), position = position_dodge(width = 1), 
            hjust = -1) + 
  xlab('Frecuencia') + 
  scale_x_continuous(expand = c(0, 0, 0.2, 0)) + 
  coord_cartesian(xlim = c(0, NA)) +
  labs(title = 'Modalidad de Selección Contratación Recetarios') + 
  theme(axis.title.y = element_blank(),
        panel.grid = element_blank()) 
  
gModalidadAdquisicion

guardarGGplot(gModalidadAdquisicion, '035_modalidadAdquisicion', 7, 4)

#'-------------------------------------------------------------------------------
# 9. Tiempo de demora para adquisición de recetarios ------------------
#'-------------------------------------------------------------------------------

col1 <- '3.16. ¿Cuánto tiempo toma la adquisición de recetarios? (días)'

gDemoraRecetario <- df_total %>% 
  creacionCloroPletCol(geometry, !!sym(col1))

gDemoraRecetario[[1]] <- gDemoraRecetario[[1]]  +
  scale_fill_continuous(type = 'viridis') +
  theme(legend.position = 'bottom',
        axis.text = element_blank()) + 
  labs(title = 'Tiempo de demora para adquisición de recetarios') +
  guides(
    fill = guide_colourbar(barwidth = 20, title.position = 'top', 
                           title = 'Demora para adquisición (días)'))

gDemoraRecetario[[2]] <- gDemoraRecetario[[2]]  +
  scale_fill_continuous(type = 'viridis')
gDemoraRecetario[[3]] <- gDemoraRecetario[[3]]  +
  scale_fill_continuous(type = 'viridis')

gDemoraRecetario
guardarGGplot(gDemoraRecetario, '036_demoraRecetario', 7, 5)
