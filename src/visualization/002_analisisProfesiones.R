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
require(patchwork)

source(file.path('src', 'data', '901_funcionesMapa.R'), encoding = 'UTF-8')

#'-------------------------------------------------------------------------------
# 1. Lectura de datos base ------------------
#'-------------------------------------------------------------------------------
# 
df <- read_csv(file.path('data', 'processed', '001_Herramienta_Procesada.csv'))

df <- df %>% 
  mutate(Profesion = ifelse(`Profesión del funcionario a cargo del FRE` != 'Otro', 
                            `Profesión del funcionario a cargo del FRE`, 
                            `Si la respuesta a la pregunta anterior fue "otro", indique cual:...17`)) 

df$`Profesión del personal de apoyo (1)` <- 
  map2_chr(df$`Profesión del personal de apoyo (1)`,
     df$`Si la respuesta a la pregunta anterior fue "otro", indique cual:...22`,
     function(x, y) {ifelse(x != 'Otro', x, y)})
df$`Profesión del personal de apoyo (2)` <- 
  map2_chr(df$`Profesión del personal de apoyo (2)`,
           df$`Si la respuesta a la pregunta anterior fue "otro", indique cual:...27`,
           function(x, y) {ifelse(x != 'Otro', x, y)})
df$`Profesión del personal de apoyo (3)` <- 
  map2_chr(df$`Profesión del personal de apoyo (3)`,
           df$`Si la respuesta a la pregunta anterior fue "otro", indique cual:...32`,
           function(x, y) {ifelse(x != 'Otro', x, y)})
df$`Profesión del personal de apoyo (4)` <- 
  map2_chr(df$`Profesión del personal de apoyo (4)`,
           df$`Si la respuesta a la pregunta anterior fue "otro", indique cual:...37`,
           function(x, y) {ifelse(x != 'Otro', x, y)})
df$`Profesión del personal de apoyo (5)` <- 
  map2_chr(df$`Profesión del personal de apoyo (5)`,
           df$`Si la respuesta a la pregunta anterior fue "otro", indique cual:...42`,
           function(x, y) {ifelse(x != 'Otro', x, y)})


df <- df %>% rowwise() %>% 
  mutate(Profesiones = list(c_across(matches('Profesión\\sdel\\s(personal|funcionario)'))))

#'-------------------------------------------------------------------------------
# 2. Perfiles profesionales en el FRE ------------------
#'-------------------------------------------------------------------------------


df$Profesion %>% 
  table() %>% as_tibble() %>% 
  rename(Profesion = '.') %>% 
  ggplot(aes(y = fct_reorder(Profesion, n), x = n)) + 
  geom_bar(stat = 'identity', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = n, x = n + 1)) + 
  xlab('Frecuencia') + 
  labs(title = 'Perfil profesional encargado del FRE') + 
  theme(axis.title.y = element_blank())

# No todo los FRE tienen un encargado con profesión de Químico Farmacéutico

df_Total <- df %>% 
  right_join(colombiaGeoDF, by = c('CodigoDepartamento' = 'DPTO'))

df_Total %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill = Profesion)) + 
  # coord_sf(crs = st_crs(32618)) + 
  labs(title = 'Profesión del encargado en el FRE') + 
  theme(axis.text = element_blank())

# Con apoyos
df$Profesiones %>% 
  unlist() %>% 
  table() %>% 
  as_tibble() %>% 
  ggplot(aes(y = fct_reorder(., n), x = n)) + 
  geom_bar(stat = 'identity', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = n, x = n + 1)) + 
  xlab('Frecuencia') + 
  labs(title = 'Perfiles profesionales en FRE (encargados y apoyo)') + 
  theme(axis.title.y = element_blank())

#'-------------------------------------------------------------------------------
# 3. ¿Cuantas personas trabajan en el FRE?------------------
#'-------------------------------------------------------------------------------

df_Total$NoPersonas <- df_Total$Profesiones %>% 
  map_dbl(function(x){sum(!is.na(x))})

df_Total %>% 
  select(NoPersonas) %>% table() %>% as_tibble() %>% 
  ggplot(aes(x = ., y = n)) + 
  geom_text(aes(label = n, y = n + 1)) + 
  geom_bar(stat = 'identity', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  ylab('Frecuencia') + xlab('No personas en el FRE') +
  labs(title = 'Conteo de personas que trabajan por FRE')

df_Total %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill = NoPersonas)) + 
  # coord_sf(crs = st_crs(32618)) + 
  scale_fill_continuous(type = 'viridis') + 
  labs(title = 'No personas en el FRE') + 
  theme(axis.text = element_blank())  
  
#'-------------------------------------------------------------------------------
# 4. ¿modalidad de contratación de personal?------------------
#'-------------------------------------------------------------------------------
df_Total <- df_Total %>% rowwise() %>% 
  mutate(ModalidadContratacion = list(c_across(matches('Tipo de vinculación'))))

# Todo el personal de todos los departamentos
df_Total$ModalidadContratacion %>%  unlist() %>% 
  str_replace(., 'De [P|p]lanta|No aplica', NA_character_) %>% 
  table() %>% as_tibble() %>% 
  ggplot(aes(x="", y=., fill=n)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  # theme_void() + 
  theme(legend.position="none") +
  # geom_text(aes(y = n, label = .), color = "white", size=6) +
  scale_fill_continuous()


















