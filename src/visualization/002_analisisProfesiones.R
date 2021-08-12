#' --- 
#' title: 'Análisis de profesiones en encuestas a FRE' 
#' author: 'Daniel S. Parra G.' 
#' date: '30-07-2021' 
#' --- 
################################################################################-
## Propósito del Script: se muestran los resultados de un análisis de profesionales 
## en las encuestas realizadas a los FRE. 
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
source(file.path('src', 'visualization', '901_funcionesBarras.R'), encoding = 'UTF-8')
source(file.path('src', 'models', '900_funcionesAlmacenamientoGrafico.R'), encoding = 'UTF-8')

#'-------------------------------------------------------------------------------
# 1. Lectura de datos base ------------------
#'-------------------------------------------------------------------------------
# 
df <- read_csv(file.path('data', 'processed', '001_Herramienta_Procesada.csv'))

df <- df %>% 
  mutate(Profesion = ifelse(`Profesión del funcionario a cargo del FRE` != 'Otro', 
                            `Profesión del funcionario a cargo del FRE`, 
                            `Si la respuesta a la pregunta anterior fue "otro", indique cual:...17`)) 

for (i in 1:5) {
  
  # Nombre de columna con personal de Apoyo
  columna1 <- glue::glue('Profesión del personal de apoyo ({i})')
  
  # Creación nuevo nombre
  columna_fix <- str_replace_all(columna1, '\\s', '_') %>% 
    str_replace_all('\\(|\\)', '')
  columna_fix_q <- rlang::sym(columna_fix)
  
  # Localización de columna con personal de Apoyo
  loc <- which(colnames(df) == columna1)
  # Nombre de columna con otros
  columna2 <- df[,loc+1] %>% names()
  
  # Esta función toma la columna en la posición siguiente, si encuentra en la primera 
  # columna la palabra otro, lo reemplaza con el contenido en la segunda columna.
  vec_columna1 <- map2_chr(pull(df, columna1), 
                       pull(df, columna2), 
                       function(x, y) {ifelse(x != 'Otro', x, y)})
  
  df <- df %>% mutate(!!columna_fix_q := vec_columna1)
}


df <- df %>% rowwise() %>% 
  mutate(Profesiones = list(c_across(matches('Profesión\\_del\\_(personal|funcionario)'))))

#'-------------------------------------------------------------------------------
# 2. Perfiles profesionales en el FRE ------------------
#'-------------------------------------------------------------------------------


perfilProfesional1 <- df$Profesion %>% 
  table() %>% as_tibble() %>% 
  rename(Profesion = '.') %>% 
  barrasGraficoRev(Profesion, n,
                'Perfil profesional encargado del FRE', 
                'Frecuencia') +
  theme(panel.grid = element_blank())

perfilProfesional1

guardarGGplot(perfilProfesional1, '020_perfilProfesionalEncargado', 6, 4)

# No todo los FRE tienen un encargado con profesión de Químico Farmacéutico

df_Total <- df %>% 
  right_join(colombiaGeoDF, by = c('CodigoDepartamento' = 'DPTO'))

mapaProfesional1 <- df_Total %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill = Profesion)) + 
  # coord_sf(crs = st_crs(32618)) + 
  labs(title = 'Profesión del encargado en el FRE') + 
  theme(axis.text = element_blank(), panel.grid = element_blank())

mapaProfesional1

guardarGGplot(mapaProfesional1, '021_mapaProfesional', 8, 6)

# Con apoyos
perfilProfesional2 <- df$Profesiones %>% unlist() %>% 
  table() %>% as_tibble() %>% 
  rename(Profesiones = '.') %>% 
  barrasGraficoRev(Profesiones, n, 'Perfiles profesionales en FRE (encargados y apoyos)',
                xlab = 'Frecuencia') +
  theme(panel.grid = element_blank())

perfilProfesional2

guardarGGplot(perfilProfesional2, '022_perfilProfesional2', 8, 6)

#'-------------------------------------------------------------------------------
# 3. ¿Cuantas personas trabajan en el FRE?------------------
#'-------------------------------------------------------------------------------

df_Total$NoPersonas <- df_Total$Profesiones %>% 
  map_dbl(function(x){sum(!is.na(x))})

perfilProfesional3 <- df_Total %>% 
  select(NoPersonas) %>% table() %>% as_tibble() %>% 
  ggplot(aes(x = ., y = n)) + 
  geom_text(aes(label = n, y = n + 1)) + 
  geom_bar(stat = 'identity', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  ylab('Frecuencia') + xlab('No personas en el FRE') +
  theme_bw() +
  labs(title = 'Conteo de personas que trabajan por FRE')+
  theme(panel.grid = element_blank())

perfilProfesional3

guardarGGplot(perfilProfesional3, '023_perfilProfesional3', 6, 4)

# N. personas en el FRE

mapaProfesional2 <- df_Total %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry, fill = NoPersonas)) + 
  coord_sf(datum = NA) + 
  # coord_sf(crs = st_crs(32618)) + 
  scale_fill_continuous(type = 'viridis') + 
  labs(title = 'N.° personas en el FRE') + 
  theme(axis.text = element_blank()) +
  theme(panel.grid = element_blank())

mapaProfesional2

guardarGGplot(mapaProfesional2, '024_mapaProfesional2', 8, 6)
  
#'-------------------------------------------------------------------------------
# 4. ¿modalidad de contratación de personal?------------------
#'-------------------------------------------------------------------------------
df_Total <- df_Total %>% rowwise() %>% 
  mutate(ModalidadContratacion = list(c_across(matches('Tipo de vinculación'))))

# Todo el personal de todos los departamentos
dfContratacion <- df_Total$ModalidadContratacion %>%  unlist() %>% 
  str_replace(., 'De [P|p]lanta|No aplica', NA_character_) %>% 
  table() %>% as_tibble() 

pieProfesional1 <- dfContratacion %>%
  rename("Tipo" = ".") %>% 
  mutate(
    prop = round(n/sum(n), 3) * 100,
    label1 = paste0(Tipo, '\n', prop, "%")) %>% 
  pieChart(n, label1) +
  scale_fill_brewer(palette = "Set1") + 
  theme(legend.position = 'none')

pieProfesional1

guardarGGplot(pieProfesional1, '025_pieProfesional1', 5, 5)


pieProfesional2 <- dfContratacion %>% 
  plot_ly(labels = ~., values = ~n, type = 'pie', 
          textinfo = 'label+percent',
          marker = list(line = list(color = '#FFFFFF', width=1)), 
          showlegend = FALSE) 

pieProfesional2

guardarPlotly(pieProfesional2, '026_pieProfesional2', libdir = 'plotly')















