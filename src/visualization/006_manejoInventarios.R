################################################################################-
#' --- 
#' title: 'Manejo de Inventarios' 
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
# require(ggsflabel)
source(file.path('src', 'data', '901_funcionesMapa.R'), encoding = 'UTF-8')


#'-------------------------------------------------------------------------------
# 1. Que herramienta utiliza el FRE para el control de inventarios------------------
#'-------------------------------------------------------------------------------
df <- read_csv(file.path('data', 'processed', '001_Herramienta_Procesada.csv'))

separarOtros <- function(string) {
  str_split(string, '\\,')
  
}

# separarOtros('Libro contable')

df$HerramientaInvetarios <-
  map2_chr(df$`3.24. ¿Qué herramienta utiliza el FRE para realizar el control de ventas de los recetarios?`,
           df$`Si la respuesta anterior fue otro, indique cual...64`,
           ~ifelse(.x != 'Otro', .x, .y))

df$HerramientaInvetarios %>% 
  table() %>% as_tibble() %>% 
  ggplot(aes(y = fct_reorder(., n), x = n)) +
  geom_bar(stat = 'identity', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = n, x = n + 0.1)) + 
  xlab('Frecuencia') + 
  labs(title = 'Herramientas de manejo inventarios') + 
  theme(axis.title.y = element_blank())


#'-------------------------------------------------------------------------------
# 1. ------------------
#'-------------------------------------------------------------------------------
df$`3.25. ¿Qué información (campos) se consigna en el instrumento aplicado en 3.24.?` %>% 
  separarDummies(.) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  summarise(conteo = sum(value)) %>% 
  ggplot(aes(y = fct_reorder(name, conteo), x = conteo)) +
  geom_bar(stat = 'identity', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = conteo, x = conteo + 1)) + 
  xlab('Frecuencia') + 
  labs(title = 'Frecuencia diligenciamiento base de datos de venta de recetarios') + 
  theme(axis.title.y = element_blank())
