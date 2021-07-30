################################################################################-
#' --- 
#' title: 'Preprocesamiento de Datos de FRE' 
#' author: 'Daniel S. Parra G.' 
#' date: '01-01-2021' 
#' --- 
## Propósito del Script: preprocesamiento de datos de FRE
## 
## 
## Copyright (c) Fondo Nacional de Estupefacientes, 2021 
## 
## Email: dsparra@minsalud.gov.co 
################################################################################-

require(tidyverse)
require(readxl)
require(lubridate)
require(reticulate)
require(fuzzywuzzyR)

#'-------------------------------------------------------------------------------
# 1. Lectura de datos base ------------------
#'-------------------------------------------------------------------------------
# 
data <- read_excel(file.path('data', 'intermediate', 
                             '001_Herramienta_Limpieza_Manual.xlsx'), 
                   na = c('', 'NA', 'N.A', 'No aplica'))

df_DIVIPOLA <-
  read_csv(file.path('data', 'processed', '798_DANE_DEPARTAMENTO.csv'))
df_MUNICIPIO <-
  read_csv(file.path('data', 'processed', '799_DANE_DIVIPOLA.csv'), 
           locale = locale(encoding = 'latin1'))

#'-------------------------------------------------------------------------------
# 2. Manipulación de fechas ------------------
#'-------------------------------------------------------------------------------

data$`Fecha creación FRE` %>% sapply(function(x) as_date(x))

#'-------------------------------------------------------------------------------
# 3. Cambios de carácter ------------------
#'-------------------------------------------------------------------------------
# Obtener valores cercanos
data$Departamento_1 <- data$Departamento %>%
  map_chr(~ str_to_upper(.x) %>%
            {
              fuzzywuzzyR::GetCloseMatches(., df_DIVIPOLA$NombreDepartamento,
                                           cutoff = 0.85)
            } %>%
            {
              `[`(1)
            })
# Unir códigos de departamento
data <- data %>% 
  left_join(df_DIVIPOLA, by = c('Departamento_1' = 'NombreDepartamento'))

# Unir códigos de municipio
data$Ciudad1 <- data$Ciudad %>%
  map_chr(
    ~ str_to_upper(.x) %>%
      fuzzywuzzyR::GetCloseMatches(., df_MUNICIPIO$NOMBRE_MUNICIPIO,
                                   cutoff = 0.6) %>%
      `[`(1)
  )

# Cambio en formato de costo de adquisición del Recetario
data$`3.06 Costo de adquisición del recetario (COP)` <- 
  data$`3.06 Costo de adquisición del recetario (COP)` %>% 
  str_replace_all(., '(\\$|\\.)', '') %>% 
  str_replace_all(., 'Por definir', '')

data %>% 
  select(matches('Precio de'))

data <- data %>% 
  mutate(across(matches('Precio de'), 
                ~.x %>% 
                  str_replace_all(., '(\\$)', '') %>% 
                  str_replace_all(., '(Por definir|No saben)', '')))



#'-------------------------------------------------------------------------------
# 2. ------------------
#'-------------------------------------------------------------------------------

data <- data %>% 
  mutate(across(CodigoDepartamento, ~formatC(.x, width = 2, flag = '0')))

data1 <- data %>% 
  mutate(across(matches('Nombre del'), ~str_to_title(.x)))


write_csv(data1, file.path('data', 'processed', '001_Herramienta_Procesada.csv'))
