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
                   na = c('', 'NA', 'N.A', 'No aplica'), 
                   sheet = 'Respuestas de formulario 1')

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
fwzy <- import('fuzzywuzzy')


# obtenerCercanos('Fra Bagg', c('Frodo Baggins', 'Tom Sawyer'), best=T)->x
obtenerCercanos <- function(string, listaPosibilidades, cutoff = 0.85, best = F) {
  str1 <- str_to_upper(string)
  
  if (best) {
    bests <- fwzy$process$extractOne(str1, listaPosibilidades, 
                                       score_cutoff = cutoff) 
  } else {
    bests <- fwzy$process$extractBests(str1, listaPosibilidades, 
                              score_cutoff = cutoff)
  }
  
  return(bests)
  
  # fuzzywuzzyR::GetCloseMatches(string = str_to_upper(string), 
  #                              sequence_strings = listaPosibilidades, 
  #                              cutoff = cutoff)
}


data$Departamento_1 <- data$Departamento %>%
  map_chr(~ obtenerCercanos(.x, df_DIVIPOLA$NombreDepartamento, best = T) %>% 
            {`[[`(., 1)})

# Unir códigos de departamento
data <- data %>% 
  left_join(df_DIVIPOLA, by = c('Departamento_1' = 'NombreDepartamento'))

# Unir códigos de municipio
data$Ciudad1 <- data$Ciudad %>%
  map_chr(~ obtenerCercanos(.x, df_MUNICIPIO$NOMBRE_MUNICIPIO, 
                        cutoff = 0.6, best = T)[[1]])

# Cambio en formato de costo de adquisición del Recetario
data$`3.06 Costo de adquisición del recetario (COP)` <- 
  data$`3.06 Costo de adquisición del recetario (COP)` %>% 
  str_replace_all(., '(\\$|\\.)', '') %>% 
  str_replace_all(., 'Por definir', '')

data %>% 
  select(matches('Precio de'))

data <- data %>% 
  mutate(across(matches('Precio de'), 
                ~str_replace_all(.x, '(\\$)', '') %>% 
                  {str_replace_all(., '(Por definir|No saben)', '')}))


#'-------------------------------------------------------------------------------
# 2. Modificación de columna de Códigos de Departamento ------------------
#'-------------------------------------------------------------------------------

data <- data %>% 
  mutate(across(matches('CodigoDepartamento'), 
                ~formatC(.x, width = 2, flag = '0')))

data1 <- data %>% 
  mutate(across(matches('Nombre del'), ~str_to_title(.x)))


#'-------------------------------------------------------------------------------
# 3. Acoplamiento de datos del DANE ------------------
#'-------------------------------------------------------------------------------
dataDANE2020 <- read_excel(
  file.path('data', 'external', '795_DANE_Censo_2018_2060.xlsx'),
  skip = 11,
  sheet = 'Departamental'
) %>%
  filter((AÑO == 2020) & `ÁREA GEOGRÁFICA` == 'Total') %>% 
  rename(poblac_total = Total) %>% 
  mutate(
    DPNOM = str_to_upper(DPNOM),
    DPNOM = str_replace(DPNOM, 'QUINDIO', 'QUINDÍO'),
    DPNOM = str_replace(DPNOM, 
                        'ARCHIPIÉLAGO DE SAN ANDRÉS', 
                        'ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA')
    )  

# Se unen los datos poblacionales de cada departamento
data1 <- data1 %>% 
  left_join(select(dataDANE2020, all_of(c('DPNOM', 'poblac_total'))), 
            by = c('Departamento_1' = 'DPNOM'))


#'-------------------------------------------------------------------------------
# 4. Acoplamiento de datos de REPS ------------------
#'-------------------------------------------------------------------------------
camasNombres <- c(
    "camas_adultos",
    "camas_intermedio_adulto",
    "camas_intensivo_adulto",
    "camas_agudo_mental",
    "camas_intermedio_mental",
    "camas_farmacodependencia",
    "camas_salud_mental",
    "ambulancias_medicada"
  )

# Lectura de datos de REPS

dataREPS <-
  read_csv(
    file.path('data', 'external', '793_MSPS_CONTEO_REPS.csv'),
    locale = locale(encoding = 'latin1')
  ) %>%
  mutate(depa_codigo = formatC(depa_codigo, width = 2, flag = '0'))  %>%
  select(depa_codigo, all_of(camasNombres))

# Se unen los datos de REPS de cada departamento

data1 <- data1 %>%
  left_join(dataREPS, by = c('CodigoDepartamento' = 'depa_codigo'))

#'-------------------------------------------------------------------------------
# 5. Escritura de archivo CSV ------------------
#'-------------------------------------------------------------------------------
write_csv(data1, file.path('data', 'processed', '001_Herramienta_Procesada.csv'))


