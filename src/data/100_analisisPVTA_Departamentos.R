#' --- 
#' title: 'Análisis de Precio de Venta por departamento' 
#' subtitle: 'Análisis de Precio de Venta por FRE' 
#' date: '26-08-2021' 
#' author: Daniel S. Parra G.
#' email: dsparrag@minsalud.gov.co
#' institute: Misión PRI 1901 - Fondo Nacional de Estupefacientes 
#' abstract: abstract 
#' output:  
#'      - html_document: default 
#'      - pdf_document: default 
#' always_allow_html: true
#' --- 
#' 
#'  
#'   

#+ Setup1, message=FALSE, warning=FALSE
require(tidyverse)
require(readxl)
require(sf)
require(patchwork)
require(plotly)
require(ggrepel)

#+ Setup2, message=FALSE, warning=FALSE
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
options(knitr.kable.NA = '')
cond1 <- knitr::is_html_output() | knitr::is_latex_output()

externalData <- file.path('data', 'external')  
source(file.path('src', 'models', '900_funcionesAlmacenamientoGrafico.R'), encoding = 'UTF-8')

#'-------------------------------------------------------------------------------
# 1. Introducción ------------------
#'-------------------------------------------------------------------------------

data <- read_excel(file.path(externalData, 'PRECIOS_VENTAS_MME_FRE.xlsx'), 
                   sheet = 'PVTA_DEPTO_1') %>% 
  select(-FRE)

# Datos de ventas a departamentos
data_1 <- data %>% 
  # Se completan los precios de ventas ausentes como NA
  complete(nesting(DEPTO, COD_DEPTO), nesting(UID, MEDICAMENTO, PRESENTACIÓN))

deptos_localizacion <-
  read_csv(file.path(externalData, '799_DANE_DIVIPOLA.csv'), 
           locale = locale(encoding = 'latin2') ) %>%
  group_by(CODIGO_DEPARTAMENTO) %>%
  slice(1) %>% ungroup()

# Lectura de datos de referencia de PVTA a FRE reportado por FNE
datos_ref <-
  read_excel(file.path(externalData, '010_CatalogoColombiaCompra.xls'),
             'CATALOGO', na = '-') %>% 
  rename(VALOR = `PRECIO FRE`) %>% 
  select(-c(MEDICAMENTO, PRESENTACIÓN, `PRECIO IPS`)) %>% 
  left_join(select(data_1, matches('MED|PRES|UID')) %>% 
              distinct(), by='UID') %>% 
  drop_na(MEDICAMENTO)

data_1 <- data_1 %>% 
  bind_rows(
    add_column(datos_ref, DEPTO = 'BOGOTÁ D.C.', COD_DEPTO = 11),
    add_column(datos_ref, DEPTO = 'CUNDINAMARCA', COD_DEPTO = 25)  
  )

# Adicionar datos de Vaupés
medic_vaupes <- c(
  'FENOBARBITAL 100 mg Tabletas',
  'MORFINA HCL 10 mg/Ml',
  'FENOBARBITAL SÓDICO 40 mg/mL',
  'MEPERIDINA HCL 100 mg/2 mL',
  'FENOBARBITAL SÓDICO 200 mg/mL',
  'FENOBARBITAL 0,4% Sln Oral'
)

medic_vaupes <- data_1 %>% 
  filter(DEPTO == 'CUNDINAMARCA') %>% 
  filter(MEDICAMENTO %in% medic_vaupes) %>% 
  mutate(
    VALOR = VALOR * 1.05,
    DEPTO = 'VAUPÉS', 
    COD_DEPTO = 97)

data_1 <- data_1 %>% 
  add_row(
    medic_vaupes
  )

# Localización GEO
geoCOL <- read_sf(file.path('data', 'external', 'colombia_geo.json'))

data1 <- data_1 %>%
  left_join(deptos_localizacion, by = c('DEPTO' = 'NOMBRE_DEPARTAMENTO'))  %>%
  mutate(COD_DEPTO = formatC(COD_DEPTO, width=2, flag=0)) %>% 
  left_join(geoCOL, by = c('COD_DEPTO' = 'DPTO'))

#'-------------------------------------------------------------------------------
# Análisis de Precios de Ventas

medicine_list <- list()

lst_medicamentos <- unique(data1$UID)
tbl_medicamentos <- data1 %>% distinct(UID, MEDICAMENTO, PRESENTACIÓN)

for (i in seq_along(lst_medicamentos)) {
  medicamento <- lst_medicamentos[i]
  med <-
    tbl_medicamentos[tbl_medicamentos$UID == medicamento, ][['MEDICAMENTO']]
  pres <-
    tbl_medicamentos[tbl_medicamentos$UID == medicamento, ][['PRESENTACIÓN']]
  
  medicine_list[[i]] <- data1 %>%
    filter(UID == medicamento) %>%
    ggplot(aes(fill = VALOR)) +
    geom_sf(aes(geometry = geometry)) + 
    labs(title = str_to_sentence(med),
         subtitle = str_to_sentence(pres)) + 
    theme_bw() + 
    scale_fill_viridis_c(name = 'Precio', labels=scales::dollar) + 
    scale_colour_continuous(labels = scales::dollar) + 
    theme(axis.title.y = element_blank(), 
          axis.title.x = element_blank(), 
          axis.text = element_blank())
}


medicine_sum <- medicine_list %>% purrr::reduce(`+`) 
medicine_sum1 <- (medicine_sum) + plot_layout(ncol = 6)

#+ fig.width = 20, fig.height = 15, out.width="100%"
medicine_sum1

guardarGGplot(medicine_sum1,
              '090_precioVentasDepartamentos',
              20,
              15,
              './figures/010_precios')

#'-------------------------------------------------------------------------------
# Variación de Análisis de Precios de Ventas

precioReferencia <- data1 %>% 
  filter(COD_DEPTO == '11') %>% 
  distinct(UID, MEDICAMENTO, PRESENTACIÓN, VALOR) %>% 
  rename(VALOR_REF = VALOR)

data1 <- data1 %>% 
  left_join(precioReferencia, by=c('MEDICAMENTO', 'PRESENTACIÓN', 'UID')) %>% 
  mutate(variacion = VALOR/VALOR_REF)

medicine_list <- list()

for (i in seq_along(lst_medicamentos)) {
  medicamento <- lst_medicamentos[i]
  med <-
    tbl_medicamentos[tbl_medicamentos$UID == medicamento, ][['MEDICAMENTO']]
  pres <-
    tbl_medicamentos[tbl_medicamentos$UID == medicamento, ][['PRESENTACIÓN']]
  
  
  medicine_list[[i]] <- data1 %>%
    filter(UID == medicamento) %>%
    ggplot(aes(fill = variacion)) +
    geom_sf(aes(geometry = geometry)) + 
    labs(title = str_to_sentence(med),
         subtitle = str_to_sentence(pres)) +
    scale_fill_viridis_c(name = 'Precio', labels = scales::percent_format()) + 
    theme_bw() + 
    theme(axis.title.y = element_blank(), 
          axis.title.x = element_blank(), 
          axis.text = element_blank())
}

medicine_sum <- medicine_list %>% purrr::reduce(`+`) 
medicine_sum1 <- (medicine_sum) + plot_layout(ncol = 6)

#+ fig.width = 20, fig.height = 15, out.width="100%"
medicine_sum1

guardarGGplot(medicine_sum1,
              '091_precioVentasDeptosVariacion',
              20,
              15,
              './figures/010_precios')

imputarTipo <- function(caracter) {
  caracter1 <- str_to_upper(caracter)
  case_when(
    str_detect(caracter1, '^FEN|PRI') ~ 'Anticonvulsivante',
    str_detect(caracter1, 'MOR|HIDROM|MEP|METAD') ~ 'Narcótico',
    str_detect(caracter1, 'METIL') ~ 'Sicoestimulante',
    str_detect(caracter1, 'CLOR') ~ 'Sedante',
    TRUE ~ NA_character_)
}
# imputarTipo('HIDRATO DE CLORAL')
# data1$MEDICAMENTO %>% unique() %>% imputarTipo()

outColor <- c('(0,1.12]' = '#93F062', '(1.12,1.2]' = '#F0E500', '(1.2,5]' = '#F01B00')

gFig1 <- data1 %>%
  mutate(
    tipo = imputarTipo(MEDICAMENTO),
    class = fct_reorder(str_to_sentence(MEDICAMENTO), (variacion), .fun='median')) %>% 
  group_by(class) %>% 
  mutate(out = cut(median(variacion, na.rm = T), c(0, 1.12, 1.20, 5))) %>%
  ungroup() %>%
  ggplot(aes(y = class, x = variacion, color = out, 
             fill = after_scale(alpha(color, 0.65)))) + 
  geom_vline(xintercept = 1, lty = 'dashed') + 
  geom_boxplot() + 
  geom_vline(xintercept = 1.12, lty = 'dashed', color = 'blue') + 
  theme_bw() + 
  xlab('Márgen de precio de venta \n sobre precio de compra') + 
  scale_x_continuous(labels = scales::percent_format()) + 
  scale_color_manual(values = outColor, name = 'Márgen') +
  scale_fill_manual(values = outColor, name = 'Márgen') +
  theme(axis.text.y = element_text(size = 8, angle=0, vjust = 0.5), 
        axis.title.y = element_blank(), 
        legend.position = c(0.85, .13), 
        legend.box.just = 'left',
        legend.direction = 'vertical')

#+ fig.width = 8, fig.height = 6
gFig1

guardarGGplot(gFig1,
              '092_boxplotMEDICAMENTO',
              8,
              6,
              './figures/010_precios')


dfData1 <- data1 %>% 
  mutate(
    tipo = imputarTipo(MEDICAMENTO),
    class = fct_reorder(str_to_sentence(MEDICAMENTO), (variacion), .fun='median')) %>% 
  group_by(MEDICAMENTO) %>% 
  summarise(
    promedio = mean(variacion, na.rm = T),
    minimo = min(variacion, na.rm = T),
    q1 = quantile(variacion, probs = 0.25, na.rm = T),
    mediana = median(variacion, na.rm = T),
    q3 = quantile(variacion, probs = 0.75, na.rm = T),
    maximo = max(variacion, na.rm = T)
    ) %>% 
  mutate(across(!contains("MEDICAMENTO"), ~paste0(round(.x*100, 1), ' %')))

dfData1 %>% knitr::kable()


gFig2 <- data1 %>%
  mutate(
    class = fct_reorder(str_to_title(DEPTO), (variacion), .fun='median')) %>%
  group_by(class) %>% 
  mutate(out = cut(median(variacion, na.rm = T), c(0, 1.12, 1.20, 5))) %>%
  ungroup() %>%
  ggplot(aes(y = class, x = variacion, color = out, 
             fill = after_scale(alpha(color, 0.65)))) + 
  geom_vline(xintercept = 1, lty = 'dashed') + 
  geom_boxplot() + 
  geom_vline(xintercept = 1.12, lty = 'dashed', color = 'blue') + 
  theme_bw() +
  xlab('Márgen de precio de venta \n sobre precio de compra') +
  scale_color_manual(values = outColor, name = 'Márgen') +
  scale_fill_manual(values = outColor, name = 'Márgen') +
  scale_x_continuous(
    labels = scales::percent_format(),
    # trans = 'log10'
    ) +
  theme(axis.text.y = element_text(size = 8, angle=0, vjust = 0.5), 
        axis.title.y = element_blank(),
        legend.position = c(0.85, .13), 
        legend.box.just = 'left', 
        legend.box.background = element_rect(fill = NULL),
        legend.direction = 'vertical')

#+ fig.width = 8, fig.height = 6
gFig2 + coord_cartesian(xlim = c(0.8, 1.75))

guardarGGplot(gFig2,
              '093_boxplotDEPTO',
              8,
              6,
              './figures/010_precios')


dfData2 <- data1 %>% 
  mutate(
    class = fct_reorder(str_to_sentence(DEPTO), (variacion), .fun='mean')) %>%  
  group_by(DEPTO) %>% 
  summarise(
    promedio = mean(variacion, na.rm = T),
    minimo = min(variacion, na.rm = T),
    q1 = quantile(variacion, probs = 0.25, na.rm = T),
    mediana = median(variacion, na.rm = T),
    q3 = quantile(variacion, probs = 0.75, na.rm = T),
    maximo = max(variacion, na.rm = T)
  ) %>% 
  mutate(across(!contains("DEPTO"), ~paste0(round(.x*100, 1), ' %')))

dfData2 %>% knitr::kable()

#'-------------------------------------------------------------------------------
# Lectura de datos de distancia en KM ------------------
#'-------------------------------------------------------------------------------
distancias <- read_excel(file.path(externalData, '798_distanciasKMBOG.xlsx'))

dfData2b <- dfData2 %>%
  mutate(across(!contains("DEPTO"), ~ as.numeric(str_replace(.x, ' %', '')))) %>%
  left_join(distancias %>% select(DEPTO, Distancia = Bogotá), by = c('DEPTO')) %>% 
  mutate(DEPTO = str_to_title(DEPTO))
  
outliers <- c('Casanare', 'Huila', 'Sucre', 'Cesar', 'Valle Del Cauca', 
              'Amazonas', 'Cauca', 'Córdoba')

lm1 <- lm(mediana ~ Distancia, dfData2b)

gFigRelacion <- dfData2b %>%
  ggplot(aes(x = Distancia, y = mediana)) +
  stat_smooth(method = 'lm', formula = 'y ~ x') +
  geom_hline(yintercept = 100, lty = 'dashed') +
  geom_point() +
  geom_errorbar(aes(ymin = q1, ymax = q3)) +
  geom_label_repel(
    dfData2b[cooks.distance(lm1) > 0.01, ],
    mapping = aes(label = DEPTO)) +
  xlab('Distancia a Bogotá (en KM)') +
  ylab('Mediana de márgenes de \nprecio de venta de MME (%)') +
  theme_bw()

#+ relacionMargenesCosto, fig.width=10, fig.height=8, out.width="100%"
gFigRelacion

guardarGGplot(gFigRelacion,
              '094_relacionMargenesCosto',
              8,
              6,
              './figures/010_precios')

data1 %>% 
  select(-c(geometry, HECTARES, PERIMETER, AREA, LATITUD, LONGITUD)) %>% 
  filter(!is.na(VALOR)) %>% 
  write_excel_csv(., file.path('data', 'processed', '020_reportesPVTAMMEanalizados.csv'))


#'-------------------------------------------------------------------------------
# Regresiones
#'-------------------------------------------------------------------------------
require(pdp)

dfEncuesta <- read_csv(
  file.path('data', 'processed', '001_Herramienta_Procesada.csv'), na = c('NR', 'NA')) %>% 
  mutate(
    Departamento = str_replace(Departamento, '(?<=San Andrés).+', ''),
    Departamento_1 = str_replace(
      Departamento_1, 
      'ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA', 
      'SAN ANDRÉS'
      ))

dfData2c <- dfData2b %>% 
  mutate(DEPTO = str_to_upper(DEPTO)) %>% 
  left_join(dfEncuesta, by = c('DEPTO' = 'Departamento_1'))

lm('mediana ~ Distancia', dfData2c) %>% summary()

x1 <- '4.53. Brinde una estimación del número de entidades que han realizado compras al FRE en el último año.'
x2 <- '3.12  Venta de medicamentos MME:'

lm('mediana ~ Distancia', dfData2c) %>% summary()

train <- dfData2c %>%
  rename(X1 = x1, X2 = x2) %>%
  mutate(
    X1 = ifelse(str_detect(X1, '\\D'), NA_real_, as.numeric(X1)),
    X2 = as.numeric(X2)
  ) %>%
  select(mediana, q1, q3, Distancia, X1, X2) %>% 
  drop_na()

plotData2c <- lm('mediana ~ Distancia + X1 + X2', data = train)

predictData2c <- predict(plotData2c)

pdp::partial(plotData2c, pred.var = 'Distancia') %>% 
  autoplot(smooth = TRUE)

quantileVec <- function(x, probs) {
  tibble(x = quantile(x, probs), probs = probs)
}

gPDP_relacion_1 <- pdp::partial(plotData2c, pred.var = 'X1', ice = T, center=F, plot=F) %>% 
  group_by(X1) %>% 
  summarise(quantileVec(yhat, c(0.025, 0.1, 0.50, 0.90, 0.975)), .groups = 'drop') %>% 
  pivot_wider(names_from = probs, names_glue = "P{probs*100}", values_from = 'x') %>% 
  ggplot(aes(x = X1)) +
  geom_hline(yintercept = 100, lty = 'dashed') +
  geom_ribbon(aes(ymin = P10, ymax = P90), alpha=0.1, fill='blue4') + 
  geom_ribbon(aes(ymin = P2.5, ymax = P97.5), alpha=0.1, fill='blue4') + 
  geom_line(aes(y = P50)) +
  geom_point(data = train, aes(x = X1, y = mediana)) +
  theme_bw() + 
  xlab('Estimación de N.° de entidades \n que han realizado compras al FRE') +
  ylab('Mediana de márgenes de \nprecio de venta de MME (%)')

gPDP_relacion_1


gPDP_relacion_2 <- pdp::partial(plotData2c, pred.var = 'X2', ice = T, center=F, plot=F) %>% 
  group_by(X2) %>% 
  summarise(quantileVec(yhat, c(0.025, 0.1, 0.50, 0.90, 0.975)), .groups = 'drop') %>% 
  pivot_wider(names_from = probs, names_glue = "P{probs*100}", values_from = 'x') %>% 
  ggplot(aes(x = X2)) +
  geom_hline(yintercept = 100, lty = 'dashed') +
  geom_ribbon(aes(ymin = P10, ymax = P90), alpha=0.1, fill='blue4') + 
  geom_ribbon(aes(ymin = P2.5, ymax = P97.5), alpha=0.1, fill='blue4') + 
  geom_line(aes(y = P50)) +
  geom_point(data = train, aes(x = X2, y = mediana)) +
  theme_bw() + 
  xlab('Proporción de ingresos correspondientes \na ventas de medicamentos') +
  ylab('Mediana de márgenes de \nprecio de venta de MME (%)')

gPDP_relacion_2


guardarGGplot(gPDP_relacion_1, '095_relacionPVTA_multireg_1', 8, 6, './figures/010_precios')
guardarGGplot(gPDP_relacion_2, '095_relacionPVTA_multireg_2', 8, 6, './figures/010_precios')

# plot(train$X1, train$mediana, 
#      xlab = 'N.° de entidades que realizaron compras \nal FRE en el último año', 
#      ylab = 'Mediana de márgen de \nprecio de medicamentos')
# lines(train$X1, predictData2c, lty = 2)
# 
# plot(train$X2, train$mediana, 
#      xlab = 'Proporción de ingresos por \nventa de medicamentos', 
#      ylab = 'Mediana de márgen de \nprecio de medicamentos')
# lines(train$X2, predictData2c, lty = 2)


plotData2c %>% summary()
