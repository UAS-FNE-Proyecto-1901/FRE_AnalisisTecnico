################################################################################-
#' --- 
#' title: 'Nombre' 
#' author: 'Daniel S. Parra G.' 
#' date: '01-01-2021' 
#' --- 
## Propósito del Script: 
## 
## 
## Copyright (c) Fondo Nacional de Estupefacientes, 2021 
## 
## Email: dsparra@minsalud.gov.co 
################################################################################-
require(plotly)
require(tidyverse); theme_set(theme_bw())
require(lubridate)
require(ggrepel)
require(patchwork)
# require(ggsflabel)
source(file.path('src', 'data', '901_funcionesMapa.R'), encoding = 'UTF-8')

#'-------------------------------------------------------------------------------
# 1. Lectura de datos base ------------------
#'-------------------------------------------------------------------------------
# 
df <- read_csv(file.path('data', 'processed', '001_Herramienta_Procesada.csv'))

df_DIVIPOLA <-
  read_csv(file.path('data', 'processed', '798_DANE_DEPARTAMENTO.csv'))

df_MUNICIPIO <-
  read_csv(file.path('data', 'processed', '799_DANE_DIVIPOLA.csv'), 
           locale = locale(encoding = 'latin1'))

df$`3.08 Precio de venta por prescripción (COP)`
df$CodigoDepartamento

df_total <- df %>% 
  right_join(colombiaGeoDF, by = c('CodigoDepartamento' = 'DPTO'))

df_total

#'-------------------------------------------------------------------------------
# 2. Número de existencias actuales en el FRE------------------
#'-------------------------------------------------------------------------------
df_total <- df_total %>% 
  mutate(ExistenciasRecetarios = case_when(
    `3.01 Existencias actuales de recetarios en el FRE` == 0 ~ 'Sin existencias',
    `3.01 Existencias actuales de recetarios en el FRE` < 1000 ~ '< 1000 u',
    (`3.01 Existencias actuales de recetarios en el FRE` >= 1000) & 
      (`3.01 Existencias actuales de recetarios en el FRE` < 5000) ~ '1000-5000 u',
    `3.01 Existencias actuales de recetarios en el FRE` >= 5000 ~ '> 5000 u'
  ))

gRecetarios1 <- df_total %>%
  ggplot() +
  geom_sf(
    aes(fill = ExistenciasRecetarios, geometry = geometry),
    colour = 'black',
    size = 0.1
  ) +
  # coord_sf(crs = st_crs(32618)) + 
  theme(legend.position = 'right',
        axis.text = element_blank()) + 
  labs(title = 'Existencias de recetarios en el FRE') +
  scale_fill_discrete(name = 'Existencia \nRecetarios') +
  guides(
    fill = guide_legend(title.position = 'top'))

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
  ggplot() +
  geom_sf(
    aes(fill = ExistenciasCirculacion, geometry = geometry),
    colour = 'black',
    size = 0.1
  ) +
  scale_fill_continuous(type = 'gradient', name = 'Existencia circulación') + 
  # coord_sf(crs = st_crs(32618)) + 
  theme(legend.position = 'bottom',
        axis.text = element_blank()) + 
  labs(title = 'Circulación mensual de \nexistencias de recetarios') +
  guides(
    fill = guide_colourbar(barwidth = 20, title.position = 'top'))

gRecetarios2

guardarGGplot(gRecetarios2, '028_existenciaRecetarios_2', 7, 5)

#'-------------------------------------------------------------------------------
# 4. Duración estimada de las existencias ------------------
#'-------------------------------------------------------------------------------

gRecetarios3 <- df_total %>% 
  ggplot() +
  geom_sf(
    aes(fill = `3.03 Tiempo de duración proyectada de las actuales existencias de recetarios (semanas).`, 
        geometry = geometry),
    colour = 'black',
    size = 0.1
  ) +
  scale_fill_continuous(type = 'viridis', name = 'Duración recetarios') + 
  # coord_sf(crs = st_crs(32618)) + 
  theme(legend.position = 'bottom',
        axis.text = element_blank()) + 
  labs(title = 'Duración de existencias de recetarios \n(semanas)') +
  guides(
    fill = guide_colourbar(barwidth = 20, title.position = 'top'))

gRecetarios3
guardarGGplot(gRecetarios3, '029_existenciaRecetarios_3', 7, 5)

#'-------------------------------------------------------------------------------
# 5. Número de prescripciones por recetario ------------------
#'-------------------------------------------------------------------------------
gRecetarios4 <- df_total %>% 
  pull(`3.05 N.º de prescripciones por recetario`) %>% 
  table() %>% as_tibble() %>% 
  ggplot(aes(x = ., y = n)) + 
  geom_bar(stat = 'identity', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  geom_label(aes(label = n), position = position_stack(1.1)) + 
  ylab('Frecuencia') + xlab('N.° prescripciones por recetario') +
  labs(title = 'Número de prescripciones por recetario') + 
  theme(panel.grid = element_blank())
  
gRecetarios4
guardarGGplot(gRecetarios4, '030_existenciaRecetarios_4', 6, 4)

#'-------------------------------------------------------------------------------
# 6. Costo de adquisición por recetarios------------------
#'-------------------------------------------------------------------------------
gCostoRecetario <- df_total %>% 
  ggplot() +
  geom_sf(
    aes(fill = `3.06 Costo de adquisición del recetario (COP)`, 
        geometry = geometry),
    colour = 'black',
    size = 0.1
  ) +
  scale_fill_continuous(label = scales::dollar, type = 'viridis', ) + 
  # geom_sf_label_repel(aes(label = `3.06 Costo de adquisición del recetario (COP)`)) + 
  # coord_sf(crs = st_crs(32618)) + 
  theme(legend.position = 'bottom',
        axis.text = element_blank()) + 
  labs(title = 'Costo de adquisición') +
  guides(
    fill = guide_colourbar(barwidth = 10, title.position = 'top', title = ''))

gCostoRecetario


gPVTARecetario <- df_total %>% 
  ggplot() +
  geom_sf(
    aes(fill = `3.07 Precio de venta del recetario (COP)`, 
        geometry = geometry),
    colour = 'black',
    size = 0.1
  ) +
  scale_fill_continuous(label = scales::dollar, type = 'viridis') + 
  # geom_sf_label_repel(aes(label = `3.06 Costo de adquisición del recetario (COP)`)) + 
  # coord_sf(crs = st_crs(32618)) + 
  theme(legend.position = 'bottom',
        axis.text = element_blank()) + 
  labs(title = 'Precio de venta') +
  guides(
    fill = guide_colourbar(barwidth = 10, title.position = 'top', title = ''))

(gCostoRecetario + gPVTARecetario) %>% 
guardarGGplot(., '031_costoRecetario', 6, 4)

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
  geom_label_repel(aes(label = Departamento_1), size = 3) +
  coord_cartesian(xlim = c(0, 35000)) +
  scale_x_continuous(labels = scales::dollar) + 
  scale_y_continuous(labels = scales::dollar) + 
  xlab('Costo de adquisición (COP)') + 
  ylab('Precio de venta (COP)')

gComparativo1

guardarGGplot(gComparativo1, '032_comparativoDepartamentos', 8, 6)

# ¿El margen de ganancia de recetarios se corresponde con el número de personas en el FRE?
df_total <- df_total %>% rowwise() %>% 
  mutate(Profesiones = list(c_across(matches('Profesión\\sdel\\s(personal|funcionario)'))))

df_total$NoPersonas <- df_total$Profesiones %>% 
  map_dbl(function(x){sum(!is.na(x))})

gComparativo2 <- df_total %>% 
  mutate(Departamento_1 = str_to_sentence(Departamento_1)) %>% 
  ggplot(aes(x = NoPersonas, y = MargenGanancia)) + 
  geom_point()+ 
  geom_label_repel(aes(label = Departamento_1)) +
  scale_y_continuous(labels = scales::percent) + 
  ylab('Margen de ganancia por recetarios (%)') +
  xlab('N.° de personas')

gComparativo3 <- df_total %>% 
  ggplot(aes(x = factor(NoPersonas), y = MargenGanancia)) +
  geom_boxplot(fill = '#6699ff') + 
  xlab('Numero de personas') + 
  ylab('Margen de ganancia \n por recetarios (%)')


(gComparativo2 + gComparativo3) %>% 
  guardarGGplot('033_comparativoDepartamentos1', 9, 5)

#'-------------------------------------------------------------------------------
# 7. Precio de venta por prescripción------------------
#'-------------------------------------------------------------------------------
gPVTARecetario7 <- df_total %>% 
  ggplot() + 
  geom_sf(
    aes(fill = `3.08 Precio de venta por prescripción (COP)`, 
        geometry = geometry),
    colour = 'black',
    size = 0.1
  ) +
  scale_fill_continuous(label = scales::dollar, type = 'viridis', name = NULL) + 
  # geom_sf_label_repel(aes(label = `3.06 Costo de adquisición del recetario (COP)`)) + 
  # coord_sf(crs = st_crs(32618)) + 
  theme(legend.position = 'bottom',
        axis.text = element_blank()) + 
  labs(title = 'Precio de venta de recetarios por prescripción') +
  guides(
    fill = guide_colourbar(barwidth = 15, title.position = 'top'))

gPVTARecetario7

guardarGGplot(gPVTARecetario7, '034_PVTA_recetarios_prescripcion', 7, 4)

#'-------------------------------------------------------------------------------
# 8. Modalidades de contratación para adquisición de Recetarios------------------
#'-------------------------------------------------------------------------------
gModalidadAdquisicion <- df_total %>% 
  pull(`3.13. ¿Qué modalidades de selección se utilizan en la contratación para adquisición de recetarios oficiales en el Departamento?`) %>% 
  table() %>% as_tibble() %>% 
  ggplot(aes(y = ., x = n)) + 
  geom_bar(stat = 'identity', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = n), position = position_dodge(width = 1), 
            hjust = -1) + 
  xlab('Frecuencia') + 
  coord_cartesian(xlim = c(0, 7)) +
  labs(title = 'Modalidad de Selección Contratación Recetarios') + 
  theme(axis.title.y = element_blank(),
        panel.grid = element_blank()) 
  
gModalidadAdquisicion

guardarGGplot(gModalidadAdquisicion, '035_modalidadAdquisicion', 7, 4)

#'-------------------------------------------------------------------------------
# 9. Tiempo de demora para adquisición de recetarios ------------------
#'-------------------------------------------------------------------------------

gDemoraRecetario <- df_total %>% 
  ggplot() + 
  geom_sf(
    aes(fill = `3.16. ¿Cuánto tiempo toma la adquisición de recetarios? (días)`, 
        geometry = geometry),
    colour = 'black',
    size = 0.1
  ) +
  scale_fill_continuous(type = 'viridis') + 
  # geom_sf_label_repel(aes(label = `3.06 Costo de adquisición del recetario (COP)`)) + 
  # coord_sf(crs = st_crs(32618)) + 
  theme(legend.position = 'bottom',
        axis.text = element_blank()) + 
  labs(title = 'Tiempo de demora para adquisición de recetarios') +
  guides(
    fill = guide_colourbar(barwidth = 20, title.position = 'top', title = 'Demora (días)'))


gDemoraRecetario

guardarGGplot(gDemoraRecetario, '036_demoraRecetario', 7, 5)

#'-------------------------------------------------------------------------------
# 10. ------------------
#'-------------------------------------------------------------------------------
