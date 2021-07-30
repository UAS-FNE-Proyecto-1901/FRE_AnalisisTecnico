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

df_total %>%
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
  guides(
    fill = guide_legend(title.position = 'top'))

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

df_total %>%
  ggplot() +
  geom_sf(
    aes(fill = ExistenciasCirculacion, geometry = geometry),
    colour = 'black',
    size = 0.1
  ) +
  scale_fill_continuous(type = 'gradient') + 
  # coord_sf(crs = st_crs(32618)) + 
  theme(legend.position = 'bottom',
        axis.text = element_blank()) + 
  labs(title = 'Circulación mensual de existencias de recetarios') +
  guides(
    fill = guide_colourbar(barwidth = 20, title.position = 'top'))

#'-------------------------------------------------------------------------------
# 4. Duración estimada de las existencias ------------------
#'-------------------------------------------------------------------------------

df_total %>% 
  ggplot() +
  geom_sf(
    aes(fill = `3.03 Tiempo de duración proyectada de las actuales existencias de recetarios (semanas).`, 
        geometry = geometry),
    colour = 'black',
    size = 0.1
  ) +
  scale_fill_continuous(type = 'viridis') + 
  # coord_sf(crs = st_crs(32618)) + 
  theme(legend.position = 'bottom',
        axis.text = element_blank()) + 
  labs(title = 'Duración de existencias de recetarios (semanas)') +
  guides(
    fill = guide_colourbar(barwidth = 20, title.position = 'top'))


#'-------------------------------------------------------------------------------
# 5. Número de prescripciones por recetario ------------------
#'-------------------------------------------------------------------------------
df_total %>% 
  pull(`3.05 N.º de prescripciones por recetario`) %>% 
  table() %>% as_tibble() %>% 
  ggplot(aes(x = ., y = n)) + 
  geom_bar(stat = 'identity', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  geom_label(aes(label = n), position = position_stack(1.1)) + 
  ylab('Frecuencia') + xlab('No personas en el FRE') +
  labs(title = 'Número de prescripciones por recetario')
  
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
  scale_fill_continuous(label = scales::dollar, type = 'viridis') + 
  # geom_sf_label_repel(aes(label = `3.06 Costo de adquisición del recetario (COP)`)) + 
  # coord_sf(crs = st_crs(32618)) + 
  theme(legend.position = 'bottom',
        axis.text = element_blank()) + 
  labs(title = 'Costo de Adquisición de Recetario') +
  guides(
    fill = guide_colourbar(barwidth = 20, title.position = 'top'))

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
  labs(title = 'Precio de venta del Recetario') +
  guides(
    fill = guide_colourbar(barwidth = 20, title.position = 'top'))

gCostoRecetario + gPVTARecetario

df_total <- df_total %>% 
  mutate(MargenGanancia = map2_dbl(`3.06 Costo de adquisición del recetario (COP)`,
         `3.07 Precio de venta del recetario (COP)`, ~((.y - .x)/.x)))

meanMargenGanancia <- df_total$MargenGanancia %>% mean(na.rm= TRUE)

df_total %>% 
  ggplot(aes(x = `3.06 Costo de adquisición del recetario (COP)`, 
             y = `3.07 Precio de venta del recetario (COP)`)) + 
  geom_point() + 
  stat_function(fun = function(x) 2*x, geom = 'line', linetype = 'dashed', col = 'blue') +
  stat_function(fun = function(x) 3*x, geom = 'line', linetype = 'dashed', col = 'blue') +
  stat_function(fun = function(x) 4*x, geom = 'line', linetype = 'dashed', col = 'blue') +
  stat_function(fun = function(x) 5*x, geom = 'line', linetype = 'dashed', col = 'blue') +
  stat_function(fun = function(x) meanMargenGanancia*x, geom = 'line', linetype = 'dotted', col = 'red') +
  geom_label_repel(aes(label = Departamento_1)) +
  scale_x_continuous(labels = scales::dollar) + 
  scale_y_continuous(labels = scales::dollar)

# ¿El margen de ganancia de recetarios se corresponde con el número de personas en el FRE?
df_total <- df_total %>% rowwise() %>% 
  mutate(Profesiones = list(c_across(matches('Profesión\\sdel\\s(personal|funcionario)'))))

df_total$NoPersonas <- df_total$Profesiones %>% 
  map_dbl(function(x){sum(!is.na(x))})

df_total %>% 
  ggplot(aes(x = NoPersonas, y = MargenGanancia)) + 
  geom_point()+ 
  geom_label_repel(aes(label = Departamento_1)) +
  scale_y_continuous(labels = scales::percent) + 
  ylab('Margen de ganancia por recetarios (%)')

df_total %>% 
  ggplot(aes(x = factor(NoPersonas), y = MargenGanancia)) +
  geom_boxplot(fill = '#6699ff') + 
  xlab('Numero de personas') + 
  ylab('Margen de ganancia \n por recetarios (%)')


#'-------------------------------------------------------------------------------
# 7. Precio de venta por prescripción------------------
#'-------------------------------------------------------------------------------
df_total %>% 
  ggplot() + 
  geom_sf(
    aes(fill = `3.08 Precio de venta por prescripción (COP)`, 
        geometry = geometry),
    colour = 'black',
    size = 0.1
  ) +
  scale_fill_continuous(label = scales::dollar, type = 'viridis') + 
  # geom_sf_label_repel(aes(label = `3.06 Costo de adquisición del recetario (COP)`)) + 
  # coord_sf(crs = st_crs(32618)) + 
  theme(legend.position = 'bottom',
        axis.text = element_blank()) + 
  labs(title = 'Precio de venta de recetarios por prescripción') +
  guides(
    fill = guide_colourbar(barwidth = 20, title.position = 'top'))

#'-------------------------------------------------------------------------------
# 8. Modalidades de contratación para adquisición de Recetarios------------------
#'-------------------------------------------------------------------------------
df_total %>% 
  pull(`3.13. ¿Qué modalidades de selección se utilizan en la contratación para adquisición de recetarios oficiales en el Departamento?`) %>% 
  table() %>% as_tibble() %>% 
  ggplot(aes(y = ., x = n)) + 
  geom_bar(stat = 'identity', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = n, x = n + 0.1)) + 
  xlab('Frecuencia') + 
  labs(title = 'Modalidad de Selección Contratación Recetarios') + 
  theme(axis.title.y = element_blank())

#'-------------------------------------------------------------------------------
# 9. Tiempo de demora para adquisición de recetarios ------------------
#'-------------------------------------------------------------------------------

df_total %>% 
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
    fill = guide_colourbar(barwidth = 20, title.position = 'top'))


#'-------------------------------------------------------------------------------
# 10. ------------------
#'-------------------------------------------------------------------------------
