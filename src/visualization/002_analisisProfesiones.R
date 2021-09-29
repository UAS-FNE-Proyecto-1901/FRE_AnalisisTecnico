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
                            `Si la respuesta a la pregunta anterior fue "otro", indique cual:...17`)) %>% 
  mutate(
    Departamento = str_replace(Departamento, '(?<=San Andrés).+', ''),
    Departamento_1 = str_replace(Departamento_1, 'ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA', 'SAN ANDRÉS'))


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
  left_join(colombiaGeoDF, by = c('CodigoDepartamento' = 'DPTO'))

mapaProfesional1 <- df_Total %>% 
  creacionCloroPletCol(geometry, Profesion) + 
  plot_annotation(title = 'Profesión del encargado en el FRE') &
  theme(axis.text = element_blank(), 
        panel.grid = element_blank(),
        legend.title = element_blank()) 

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

#'-------------------------------------------------------------------------------
#'
df_Total['NoPersonas1'] <- df_Total$No.PersonasVinculadasDirectamente + 
  df_Total$No.PersonasVinculadasAfiliacion

perfilProfesional3b <- df_Total %>% 
  ggplot(aes(x = NoPersonas1)) + 
  geom_bar(stat = 'count', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  geom_label(stat = 'count', aes(y = ..count.., label = ..count..), 
             vjust = -0.5) + 
  scale_x_continuous(breaks = seq(0, 12, 1)) + 
  scale_y_continuous(expand = c(0.05, 0, 0.3, 0)) +
  ylab('Frecuencia absoluta') + xlab('No personas en el FRE') +
  theme_bw() +
  labs(title = 'Conteo de personas que trabajan por FRE')+
  theme(panel.grid = element_blank())

perfilProfesional3b
guardarGGplot(perfilProfesional3b, '023_perfilProfesional3b', 6, 4)

#'-------------------------------------------------------------------------------
#' N. personas en el FRE
#' 

mapaProfesional2 <- df_Total %>% 
  creacionCloroPletCol(geometry, NoPersonas1) +
  plot_annotation(title = 'N.° personas en el FRE') &
  scale_fill_continuous(type = 'viridis') & 
  theme(axis.text = element_blank(),
        panel.grid = element_blank())

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


col1 <- "Tipo_Contratacion_PersonasVinculadasDirectamente"
col2 <- "Tipo_Contratacion_PersonasVinculadasAfiliacion"

df <- df %>%
  mutate(
    col1_nom = as.numeric(str_extract(!!ensym(col1), "(?<=N)(\\d)")),
    col1_con = as.numeric(str_extract(!!ensym(col1), "(?<=C)(\\d)")),
    col2_nom = as.numeric(str_extract(!!ensym(col2), "(?<=N)(\\d)")),
    col2_con = as.numeric(str_extract(!!ensym(col2), "(?<=C)(\\d)"))
  )

df2 <- select(df, matches("^col")) %>%
  apply(2, sum) %>%
  {data.frame(nombre = names(.), frec = ., row.names = NULL)} %>%
  separate(nombre, c('columna', 'tipo'), '\\_') %>%
  mutate(
    tipo    = ifelse(tipo == 'nom', 'Nombramiento', 'Contrato'),
    columna = ifelse(columna == 'col1', 'Directamente', 'Afiliacion')
  )

prop <- df2 %>% group_by(tipo) %>% 
  summarise(frec = sum(frec)) %>% 
  add_column(columna = 'Total') %>% 
  add_row(df2) %>% 
  group_by(columna) %>% 
  mutate(frec_rel = frec / sum(frec),
         label1 = paste0(tipo, '\n', round(frec_rel, 3) * 100, "%"))


prop %>%
  filter(columna == 'Total') %>% 
  pieChart(frec_rel, label1) +
  scale_fill_brewer(palette = "Set1") + 
  theme(legend.position = 'none')

#'-------------------------------------------------------------------------------



pieProfesional1a <- prop %>%
  filter(columna == 'Directamente') %>% 
  pieChart(frec_rel, label1) +
  scale_fill_brewer(palette = "Set1") + 
  theme(legend.position = 'none') + 
  labs(title = 'Personal directo')

pieProfesional1b <- prop %>%
  filter(columna == 'Afiliacion') %>% 
  pieChart(frec_rel, label1) +
  scale_fill_brewer(palette = "Set1") + 
  theme(legend.position = 'none') + 
  labs(title = 'Personal vinculado')


pieProfesional1C <- (pieProfesional1a + pieProfesional1b) + 
  plot_annotation(tag_levels = 'A')


guardarGGplot(pieProfesional1C, '026_pieProfesional1b', 9, 5)


#'-------------------------------------------------------------------------------
#'
#'

pieProfesional2a <- filter(prop, columna == 'Directamente') %>% 
  plot_ly() %>% 
  add_pie(labels = ~tipo, values = ~frec, type = 'pie', 
          textinfo = 'label+percent',
          marker = list(line = list(color = '#FFFFFF', width=1)), 
          showlegend = FALSE, hole = 0.6,
          name = 'Vinculación directa',
          domain = list(x = c(0, 0.4), y = c(0.0, 1))) %>% 
  add_pie(data = filter(prop, columna == 'Afiliacion'),
          labels = ~tipo, values = ~frec, type = 'pie', 
          textinfo = 'label+percent',
          marker = list(line = list(color = '#FFFFFF', width=1)), 
          name = 'Vinculación por afiliación',
          showlegend = FALSE, hole = 0.4,
          domain = list(x = c(0.6, 1), y = c(0.0, 1)))


pieProfesional2a
guardarPlotly(pieProfesional2a, '026_pieProfesional2b', libdir = 'plotly')


#'-------------------------------------------------------------------------------

df_clas_reg <- read_csv(file.path('data', 'external', 'clasificacionRegiones.csv'), 
         locale = locale(encoding = 'latin1')) 

df %>% 
  left_join(df_clas_reg, by = c('Departamento_1' = 'Departamento_1')) %>% 
  group_by(Region_1) %>% 
  summarise(across(matches('^col'), sum)) %>% 
  pivot_longer(cols = matches('^col')) %>%
  separate(name, c('n1', 'n2'), '_') %>%
  pivot_wider(names_from = n1) %>% 
  mutate(value = col1 + col2) %>% 
  select(-col1, -col2) %>% 
  group_by(Region_1) %>% 
  mutate(prop = value/sum(value)) %>% 
  pivot_wider(names_from = n2, values_from = c(value, prop))

