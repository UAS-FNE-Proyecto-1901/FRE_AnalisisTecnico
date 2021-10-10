#' --- 
#' title: 'Ruta tecnológica - tercera parte' 
#' subtitle: 'Misión PRI 1901' 
#' date: '07-08-2021' 
#' author: 
#'        - name: Daniel S. Parra G. 
#'          email: dsparrag@minsalud.gov.co 
#'          institute: FNE 
#' institute: 
#'        - FNE: Misión PRI 1901 - Fondo Nacional de Estupefacientes 
#' abstract: |
#'      Informe final de asistencias técnicas a los FRE. En este script
#'      se muestran los análisis de ruta técnológica y diligenciamiento de 
#'      informes requeridos por resolución para el FNE.  
#'      
#' output:  
#'      - html_document: default 
#'      - pdf_document: default 
#' always_allow_html: true 
#' --- 

#+ setup1, warning = FALSE, echo=FALSE,

require(plotly)
require(tidyverse); theme_set(theme_bw())
require(lubridate)
require(ggrepel)
require(patchwork)
require(ggsflabel)
require(sf)
require(scatterpie)

#'-------------------------------------------------------------------------------
# 1. Introducción ------------------
#'-------------------------------------------------------------------------------
#+ source1
source(file.path('src', 'data', '901_funcionesMapa.R'), encoding = 'UTF-8')
source(file.path('src', 'visualization', '900_funcionExtraccionDummies.R'), encoding = 'UTF-8')
source(file.path('src', 'models', '900_funcionesAlmacenamientoGrafico.R'), encoding = 'UTF-8')
source(file.path('src', 'visualization', '901_funcionesBarras.R'), encoding = 'UTF-8')

df_DIVIPOLA <-
  read_csv(file.path('data', 'processed', '798_DANE_DEPARTAMENTO.csv'))

df_MUNICIPIO <-
  read_csv(file.path('data', 'processed', '799_DANE_DIVIPOLA.csv'), 
           locale = locale(encoding = 'latin1'))

df <- read_csv(file.path('data', 'processed', '001_Herramienta_Procesada.csv'), 
               na = c('N/A', 'No aplica', 'NA')) %>% 
  mutate(
    Departamento = str_replace(Departamento, '(?<=San Andrés).+', ''),
    Departamento_1 = str_replace(Departamento_1, 'ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA', 'SAN ANDRÉS'))

df

#'-------------------------------------------------------------------------------
# 4.62. ¿Cuáles controles realiza el FRE durante la venta de los MME? ------------------
#'-------------------------------------------------------------------------------
# En caso de venta directa a pacientes

col1 <- "4.62. ¿Cuáles controles realiza el FRE durante la venta de los MME?"
col2 <- "Si la respuesta anterior fue otra, indique cuales"

ggControlesVentasFRE <- pull(df, col2) %>%
  is.na() %>%
  ifelse(pull(df, col1),
         paste(pull(df, col1), pull(df, col2), sep = ',')) %>% 
  separarDummies(descartar = T) %>%
  pivot_longer(cols = everything()) %>% 
  mutate(
    name = str_wrap(name, 30),
    name = str_replace(name, 'almaceamiento', 'verificación'),
    name = str_replace(name, 'institciones', 'instituciones')) %>% 
  group_by(name) %>% 
  summarise(
    conteo = sum(value),
    propor = sum(value)/dim(df)[1],
    label1 = paste(conteo, '/', dim(df)[1])
  ) %>% 
  ggplot(aes(y = fct_reorder(name, propor), x = propor)) +
  geom_bar(stat = 'identity', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = label1), hjust = -0.3, size = 3) + 
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(labels = scales::percent_format()) +
  xlab('Proporción (%)') + 
  labs(title = 'Controles realizados en ventas directas a pacientes') + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

#+ ggControlesVentasFRE, fig.width = 8, fig.height = 6, out.width = "60%"
ggControlesVentasFRE
guardarGGplot(ggControlesVentasFRE, '120_ControlesVentasFRE', 8, 6)

#'-------------------------------------------------------------------------------
# 4.73. ¿Qué tiempo toma la consolidación del Anexo 1 de la ------------------
# R. 1479/2006? (en días) 
#'-------------------------------------------------------------------------------

nivelesConsolidacion <- c("Menos de una hora",
  "De una a dos horas",
  "De 2 a 4 horas",
  "De 4 a 6 horas",
  "De 6 a 24 horas",
  "Entre 24 horas y una semana",
  "Más de una semana")


col1 <- "4.73. ¿Qué tiempo toma la consolidación del Anexo 1 de la R. 1479/2006? (en días)"

ggConsolidacionA1 <- select(df, all_of(c(col1 = col1))) %>% 
  drop_na() %>% 
  mutate(col1 = factor(col1, rev(nivelesConsolidacion))) %>% 
  ggplot(aes(y = col1)) +
  geom_bar(stat = 'count', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = ..count..), stat = 'count', hjust = -0.5) + 
  xlab('Frecuencia') + 
  scale_x_continuous(expand = c(0,0,0.2,0)) +
  scale_y_discrete(drop = F) +
  labs(title = "Tiempo en consolidación de A1 de la R1479/2006") + 
  coord_cartesian(xlim = c(0, NA)) + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

#+ ggConsolidacionA1, fig.width = 8, fig.height = 6, out.width = "60%"
ggConsolidacionA1
guardarGGplot(ggConsolidacionA1, '121_TiemposConsolidacionA1', 8, 6)

col2 <- '4.53. Brinde una estimación del número de entidades que han realizado compras al FRE en el último año.'

ggConsolidacionA1_2 <-
  select(df, all_of(c('Departamento_1', col2, col1 = col1))) %>% 
  drop_na() %>% 
  mutate(
    col1 = factor(col1, rev(nivelesConsolidacion)),
    col2 = ifelse(str_detect(!!ensym(col2), '\\D'), NA_real_, as.numeric(!!ensym(col2)))
    ) %>% 
  ggplot(aes(x = col2, y = col1)) +
  geom_violin(fill = 'blue4', alpha = 0.2) + 
  geom_point(shape = 1) + 
  # geom_dotplot(binaxis='y', stackdir='center', dotsize=1) + 
  xlab('N.° de instituciones que han \n realizado compras el último año') + 
  ylab('Tiempo de diligenciamiento de Anexo 1') +
  theme(panel.grid = element_blank())

#+ ggConsolidacionA2, fig.width = 8, fig.height = 6, out.width = "60%"
ggConsolidacionA1_2
guardarGGplot(ggConsolidacionA1_2, '121_TiemposConsolidacionA1_2', 8, 6)

#'-------------------------------------------------------------------------------
# 4.81. ¿Cómo se recibe el FRE el anexo 13 de la Resolución ------------------ 
# 1478 de 2006 
#'-------------------------------------------------------------------------------
col1 <- "4.81. ¿Cómo se recibe el FRE el anexo 13 de la Resolución 1478 de 2006"

ggRecepcionA13 <- pull(df, col1) %>% 
  separarDummies(descartar = T) %>%
  pivot_longer(cols = everything()) %>% 
  mutate(
    name = str_wrap(name, 30),
    name = str_replace(name, 'almaceamiento', 'verificación'),
    name = str_replace(name, 'institciones', 'instituciones')) %>% 
  group_by(name) %>% 
  summarise(
    conteo = sum(value),
    propor = sum(value)/dim(df)[1],
    label1 = paste(conteo, '/', dim(df)[1])
  ) %>% 
  ggplot(aes(y = fct_reorder(name, propor), x = propor)) +
  geom_bar(stat = 'identity', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = label1), hjust = -0.3, size = 3) + 
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(labels = scales::percent_format()) +
  xlab('Proporción (%)') + 
  labs(title = 'Recepción del A13 de la R. 1478/2006') + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

#+ ggRecepcionA13, fig.width = 8, fig.height = 6, out.width = "60%"
ggRecepcionA13
guardarGGplot(ggRecepcionA13, '122_RecepcionA13', 8, 6)

#'-------------------------------------------------------------------------------
# 4.83. ¿Cuánto tiempo se archivan en el FRE los ------------------ 
# informes recibidos? 
#'-------------------------------------------------------------------------------
col1 <- "4.83. ¿Cuánto tiempo se archivan en el FRE los informes recibidos?" 

nivelesTiempo <- c(
  "0 a 6 meses",
  "6 a 12 meses",
  "1 a 2 años",
  "2 a 5 años",
  "> 5 años",
  "Los informes de FRE no se archivan"
) %>% 
  str_wrap(25)

ttArchivoInformes <- select(df, col1 = col1) %>% 
  drop_na() %>% 
  mutate(
    col1 = str_wrap(col1, 25),
    col1 = factor(col1, levels = rev(nivelesTiempo)))

ttArchivoInformes_max <- ttArchivoInformes %>% table(dnn = 'Conteo') %>% as_tibble() %>% 
  pull(n) %>% max()

ggArchivoInformes <- ttArchivoInformes %>%
  ggplot(aes(y = col1)) +
  geom_bar(stat = 'count', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = ..count..), stat = 'count', hjust = -0.5) + 
  xlab('Frecuencia') + 
  scale_y_discrete(drop = F) +
  labs(title = "Tiempo de archivo de informes A13 de R.1478/2006") + 
  coord_cartesian(xlim = c(0, NA)) + 
  scale_x_continuous(expand = c(0,0,0.2,0)) + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

#+ ggArchivoInformes, fig.width = 8, fig.height = 6, out.width = "60%"
ggArchivoInformes
guardarGGplot(ggArchivoInformes, '123_ArchivoInformesFRE', 6, 4)

#'-------------------------------------------------------------------------------
# 4.92. Tiempos de diligenciamiento del Anexo 2 de la resolución------------------
#'-------------------------------------------------------------------------------
# La columna 4.92_1 está en horas

col1 <- '4.53. Brinde una estimación del número de entidades que han realizado compras al FRE en el último año.'

df2 <- df %>%
  mutate(
    col0 = str_extract(`4.92_1`, '(?<=\\|)(.+)') %>%
      as.numeric(),
    col2 = ifelse(str_detect(!!ensym(col1), '\\D'), NA_real_, as.numeric(!!ensym(col1)))
  )

ggAnexo2dilig_1 <- df %>% 
  mutate(Variable = str_extract(`4.92_1`, '(.)(?=\\|)')) %>% 
  ggplot(aes(x = Variable)) + 
  geom_bar(color = 'blue', fill = 'blue', alpha = 0.4) + 
  geom_text(aes(label = ..count..), stat = 'count', vjust = -0.5) + 
  scale_y_continuous(expand = c(0.1,0,0.1,0)) + 
  xlab('N.° de personas involucradas') + 
  ylab('Conteo') 

#+ ggAnexo2dilig_1, fig.width = 8, fig.height = 6, out.width = "60%"
ggAnexo2dilig_1
guardarGGplot(ggAnexo2dilig_1, '137_evaluacionAnexo1', 6, 4)





lm1 <- lm(col0/8 ~ log(col2), df2)

breaks     <- 10^(0:3)
min_breaks <- rep(1:9, 3)*(10^rep(0:3, each=9))


ggAnexo2dilig_2 <- df2 %>%
  ggplot(aes(x = col2, y = col0/8)) +
  geom_point() + 
  stat_smooth(method = 'lm', formula = 'y ~ log(x)') + 
  scale_x_continuous(trans = 'pseudo_log', 
                     breaks = breaks, minor_breaks = min_breaks) + 
  geom_text_repel(
    data = df2[!is.na(df2$col0), ][cooks.distance(lm1) > 0.015, ], 
            aes(label = str_to_title(Departamento_1))) +
  xlab('N.° de instituciones que han \n realizado compras el último año') + 
  ylab('Tiempo de diligenciamiento \nde Anexo 2 (días laborales)') 


#+ ggAnexo2dilig_2, fig.width = 8, fig.height = 6, out.width = "60%"
ggAnexo2dilig_2
guardarGGplot(ggAnexo2dilig_2, '137_evaluacionAnexo2', 6, 4)

#'-------------------------------------------------------------------------------
# 4.93. ¿Cuánto tiempo se archivan en el FRE los informes ------------------
# recibidos? 
#'-------------------------------------------------------------------------------
col1 <- "4.93. ¿Cuánto tiempo se archivan en el FRE los informes recibidos?" 

nivelesTiempo <- c("0 a 6 meses",
                   "6 a 12 meses",
                   "1 a 2 años",
                   "2 a 5 años",
                   "> 5 años",
                   "Los informes del FRE no\nse archivan")

ggArchivoInformes1 <- select(df, col1 = col1) %>% 
  drop_na() %>% 
  mutate(
    col1 = str_wrap(col1, 25),
    col1 = factor(col1, levels = rev(nivelesTiempo))) %>%
  ggplot(aes(y = col1)) +
  geom_bar(stat = 'count', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = ..count..), stat = 'count', hjust = -0.5) + 
  xlab('Frecuencia') + 
  labs(title = "Tiempo de archivo de informes A13 de R.1478/2006") + 
  scale_x_continuous(expand = c(0,0,0.2,0)) + 
  coord_cartesian(xlim = c(0, NA)) + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

#+ ggArchivoInformes1, fig.width = 8, fig.height = 6, out.width = "60%"
ggArchivoInformes1
guardarGGplot(ggArchivoInformes1, '124_ArchivoInformesFRE1', 6, 4)

#'-------------------------------------------------------------------------------
# 4.10.1. ¿De qué manera el FRE recuerda o hace seguimiento al ------------------
# envío de los informes de consumo a las instituciones autorizadas 
# para el manejo de MME? (llamada, correo, visita) 
#'-------------------------------------------------------------------------------
col1 <- "4.10.1. ¿De qué manera el FRE recuerda o hace seguimiento al envío de los informes de consumo a las instituciones autorizadas para el manejo de MME? (llamada, correo, visita)"

nivelsRecuerdo <- c("Llamada",
  "Correo",
  "Visita personal",
  "SMS",
  "Boletín",
  "No lo hace")


ggRecuerdoEnvioInforme <- pull(df, col1) %>% 
  separarDummies(descartar = T) %>%
  pivot_longer(cols = everything()) %>% 
  mutate(
    name = str_wrap(name, 30),
    name = str_replace(name, 'almaceamiento', 'verificación'),
    name = str_replace(name, 'institciones', 'instituciones')) %>% 
  group_by(name) %>% 
  summarise(
    conteo = sum(value),
    propor = sum(value)/dim(df)[1],
    label1 = paste(conteo, '/', dim(df)[1])
  ) %>% 
  ggplot(aes(y = fct_reorder(name, propor), x = propor)) +
  geom_bar(stat = 'identity', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = label1), hjust = -0.3, size = 3) + 
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(labels = scales::percent_format()) +
  xlab('Proporción (%)') + 
  labs(title = 'Mecanismo de seguimiento de instituciones') + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

#+ ggRecuerdoEnvioInforme, fig.width = 8, fig.height = 6, out.width = "60%"
ggRecuerdoEnvioInforme
guardarGGplot(ggRecuerdoEnvioInforme, '125_SeguimientoEnvioInformes', 6, 4)

#'-------------------------------------------------------------------------------
# 4.10.3. ¿Qué medidas realiza el FRE ante el incumplimiento del ----------------
# envío de los informes? 
#'-------------------------------------------------------------------------------

col1 <- "4.10.3. ¿Qué medidas realiza el FRE ante el incumplimiento del envío de los informes?"

incumplimientoEnvioInformes <- c("Llamado de atención",
  "Amonestación",
  "Multa",
  "Circular",
  "Denuncia")

ggIncumplimientoEnvio <- pull(df, col1) %>% 
  separarDummies(descartar = T) %>%
  select(-1) %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(
    name = str_wrap(name, 30),
    name = factor(name, incumplimientoEnvioInformes)
    ) %>% 
  group_by(name) %>% 
  summarise(
    conteo = sum(value, na.rm = TRUE),
    propor = conteo/dim(df)[1],
    label1 = paste(conteo, '/', dim(df)[1])
  ) %>% 
  ggplot(aes(y = fct_reorder(name, propor), x = propor)) +
  geom_bar(stat = 'identity', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = label1), hjust = -0.3, size = 3) + 
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_discrete(drop = F) +
  xlab('Proporción (%)') + 
  labs(title = 'Medidas por incumplimiento de envío de informes') + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

#+ ggIncumplimientoEnvio, fig.width = 8, fig.height = 6, out.width = "60%"
ggIncumplimientoEnvio
guardarGGplot(ggIncumplimientoEnvio, '126_IncumplimientoEnvioInformes', 6, 4)

#'-------------------------------------------------------------------------------
# 4.10.4. ¿Cómo se garantiza la seguridad de la información? ------------------
#'-------------------------------------------------------------------------------
col1 <- "4.10.4. ¿Cómo se garantiza la seguridad de la información?"
col2 <- "Si la respuesta a anterior fue otro, indique cuales"

ggGarantiasSeguridadInformacion <- pull(df, col1) %>% 
  {ifelse(str_detect(., 'Otro'), 
          paste(pull(df, col1), pull(df, col2), sep = ', '), 
          pull(df, col1))} %>% 
  separarDummies(descartar = T) %>%
  pivot_longer(cols = everything()) %>% 
  mutate(
    name = str_wrap(name, 20),
    name = str_replace(name, 'atenci', 'atención')) %>% 
  group_by(name) %>% 
  summarise(
    conteo = sum(value, na.rm = TRUE),
    propor = conteo/dim(df)[1],
    label1 = paste(conteo, '/', dim(df)[1])
  ) %>% 
  filter(name != 'Otro') %>% 
  ggplot(aes(y = fct_reorder(name, propor), x = propor)) +
  geom_bar(stat = 'identity', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = label1), hjust = -0.3, size = 3) + 
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(labels = scales::percent_format()) +
  xlab('Proporción (%)') + 
  labs(title = 'Garantía en la seguridad de información') + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

#+ ggGarantiasSeguridadInformacion, fig.width = 8, fig.height = 6, out.width = "60%"
ggGarantiasSeguridadInformacion
guardarGGplot(ggGarantiasSeguridadInformacion, '127_GarantiaInformacion', 6, 4)

#'-------------------------------------------------------------------------------
# 4.10.5. ¿El FRE cuenta con alguna base de datos donde exista ------------------
# información consolidad sobre las instituciones inscritas?, 
#'-------------------------------------------------------------------------------

col1 <- "4.10.5. ¿El FRE cuenta con alguna base de datos donde exista información consolidad sobre las instituciones inscritas?, p.ej. número de camas, ventas mensuales, complejidad, etc."

ggInformacionInscritos <- select(df, frec = col1) %>% 
  table() %>% as_tibble() %>% 
  rename(label = '.') %>% 
  mutate(
    prop = n/sum(n),
    label1 = paste0(label, "\n", round(prop,3)*100, '%')) %>%
  pieChart(n, label1) +
  theme(legend.position = 'none') + 
  scale_fill_brewer(palette = 'Set1') +
  labs(title = "BD de información de instituciones inscritas")

#+ ggInformacionInscritos, fig.width = 8, fig.height = 6, out.width = "60%"
ggInformacionInscritos
guardarGGplot(ggInformacionInscritos, '128_InformInscritos', 6, 4)


#'-------------------------------------------------------------------------------
# 4.10.6. ¿El FRE cuenta con información consolidada sobre los ------------------
# pacientes que han adquirido medicamentos?
#'-------------------------------------------------------------------------------
col1 <- "4.10.6. ¿El FRE cuenta con información consolidada sobre los pacientes que han adquirido medicamentos?, p.ej. cuanto se ha comprado, con qué indicaciones, etc."

ggInformacionPacientes <- select(df, frec = col1) %>% 
  table() %>% as_tibble() %>% 
  rename(label = '.') %>% 
  mutate(
    prop = n/sum(n),
    label1 = paste0(label, "\n", round(prop,3)*100, '%')) %>%
  pieChart(n, label1) +
  theme(legend.position = 'none') + 
  scale_fill_brewer(palette = 'Set1') +
  labs(title = "BD de información de pacientes que han adquirido MME")

#+ ggInformacionPacientes, fig.width = 8, fig.height = 6, out.width = "60%"
ggInformacionPacientes
guardarGGplot(ggInformacionPacientes, '129_InformPacientes', 6, 4)

#'-------------------------------------------------------------------------------
# 4.50. ¿Con qué frecuencia (número de ventas por año) el FRE realiza -----------
# compra de MME al FNE? 
#'-------------------------------------------------------------------------------
col1 <- "4.50. ¿Con qué frecuencia (número de ventas por año) el FRE realiza compra de MME al FNE?"

ggFrecVentasFNE <- rename(df, col1 = col1) %>%
  drop_na(col1) %>% 
  mutate(
    Departamento_1 = str_to_title(Departamento_1),
    Departamento_1 = fct_reorder(Departamento_1, col1)) %>% 
  ggplot(aes(x = col1, 
             y = Departamento_1)) +
  geom_segment(aes(yend = Departamento_1, xend = 0), col = "#1a41bd") +
  geom_point(aes(x = col1), col = "#1a41bd") +
  geom_text(aes(x = col1, label = col1), size = 3, hjust=-0.5) + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank()) + 
  xlab('Cantidad de pedidos de medicamentos \nal FNE por año')

#+ ggFrecVentasFNE, fig.width = 6, fig.height = 4, out.width = "60%"
ggFrecVentasFNE
guardarGGplot(ggFrecVentasFNE, '130_FrecComprasFNR', 6, 4)

ggFrecVentasFNE1 <- rename(df, col1 = col1) %>%
  drop_na(col1) %>% 
  mutate(
    Departamento_1 = str_to_title(Departamento_1),
    Departamento_1 = fct_reorder(Departamento_1, col1)
    ) %>% 
  ggplot(aes(x = col1)) + 
  geom_bar(fill = alpha('#1a41bd', 0.5), color = 'black') + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  xlab('N.° de pedidos por año') + 
  ylab('Frecuencia') + 
  scale_x_continuous(breaks = 1:12) + 
  scale_y_continuous(expand = c(0,0,0.2,0)) 

ggFrecVentasFNE1
guardarGGplot(ggFrecVentasFNE1, '130b_FrecComprasFNR', 6, 4)


ggFrecVentasFNE2 <- rename(df, col1 = col1) %>%
  drop_na(col1) %>% 
  mutate(
    Departamento_1 = str_to_title(Departamento_1),
    Departamento_1 = fct_reorder(Departamento_1, col1),
    T_EOQ = 12/col1
  ) %>% 
  ggplot(aes(x = T_EOQ)) + 
  geom_bar(fill = alpha('#1a41bd', 0.5), color = 'black') + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  xlab('Tiempo entre pedidos') + 
  ylab('Frecuencia') + 
  scale_x_continuous(breaks = 1:12) + 
  scale_y_continuous(expand = c(0,0,0.2,0)) 

ggFrecVentasFNE2
guardarGGplot(ggFrecVentasFNE2, '130c_FrecComprasFNR', 6, 4)

#'-------------------------------------------------------------------------------
# 4.66¿En el Departamento existe algún establecimiento - diferente------------------
# al FRE - que realice venta a pacientes de Medicamentos monopolio el Estado? 
#'-------------------------------------------------------------------------------

col1 <- "4.66     ¿En el Departamento existe algún establecimiento - diferente al FRE - que realice venta a pacientes de Medicamentos monopolio el Estado?"

ggInstitucionesAdicionales <- select(df, frec = col1) %>% 
  table() %>% as_tibble() %>% 
  rename(label = '.') %>% 
  mutate(
    prop = n/sum(n),
    label1 = paste0(label, "\n", round(prop,3)*100, '%')) %>%
  pieChart(n, label1) +
  theme(legend.position = 'none') + 
  scale_fill_brewer(palette = 'Set1') +
  labs(title = "Existen instituciones en el departamento que también realicen ventas a pacientes")

#+ ggInstitucionesAdicionales, fig.width = 8, fig.height = 6, out.width = "60%"
ggInstitucionesAdicionales
guardarGGplot(ggInstitucionesAdicionales, '131_InstitucionesAdicionales', 6, 4)

#'-------------------------------------------------------------------------------
# ¿Cuánto tiempo toma la venta de los recetarios para los clientes? (días) ------------------
#'-------------------------------------------------------------------------------
col1 <- "¿Cuánto tiempo toma la venta de los recetarios para los clientes? (días)"

colombiaGeoDF <- read_sf(file.path('data', 'external', 'colombia_geo.json'))

ggTiempoVentaRec <- df %>% 
  rename(col1 = col1) %>% 
  left_join(colombiaGeoDF, by = c('CodigoDepartamento' = 'DPTO')) %>% 
  mutate(
    Departamento_1 = str_to_title(Departamento_1),
    label1 = ifelse(
    !is.na(col1), paste0(Departamento_1, '\n', col1, ' días'), NA_character_
  )) %>% 
  ggplot(aes(fill = col1)) + 
  geom_sf(aes(geometry = geometry))+ 
  geom_sf_label_repel(aes(label = label1), size = 2, fill='white') +
  labs(title = 'Tiempo para la venta de recetarios a clientes') +
  scale_fill_gradientn(colours = colorspace::heat_hcl(7), name = 'Tiempo') +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

#+ ggTiempoVentaRec, fig.width = 8, fig.height = 6, out.width = "90%"
ggTiempoVentaRec
guardarGGplot(ggTiempoVentaRec, '132_TiempoVentaInstituciones', 8,6)

#'-------------------------------------------------------------------------------
# ¿Con que frecuencia el FRE vende MME a una institución? ------------------
#'-------------------------------------------------------------------------------

col1 <- "¿Con que frecuencia el FRE vende MME a una institución?"

frec_vec <- c("Diaria",
              "Cada dos días",
              "Cada semana",
              "Cada quince días",
              "Cada mes",
              "Rara vez")

ggFrecVentaInstitu <- select(df, all_of(c(col1 = col1))) %>% 
  drop_na() %>% 
  mutate(col1 = factor(col1, rev(frec_vec))) %>% 
  ggplot(aes(y = col1)) + 
  geom_bar(stat = 'count', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  scale_x_continuous(expand = c(0,0,0.2,0)) + 
  scale_y_discrete(drop = F) +
  xlab('Frecuencia') + 
  geom_text(aes(label = ..count..), stat = 'count', hjust = -0.8) +
  coord_cartesian(xlim = c(0, NA)) +
  labs(title = 'Frecuencia de ventas del FRE a instituciones') + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

#+ ggFrecVentaInstitu, fig.width = 6, fig.height = 4, out.width = "60%"
ggFrecVentaInstitu
guardarGGplot(ggFrecVentaInstitu, '133_FrecVentaInstit', 6,4)

#'-------------------------------------------------------------------------------
# 4.37. ¿Qué proporción de los medicamentos que ocupa el almacén ------------------
# corresponde a MME? 
#'-------------------------------------------------------------------------------
col1 <- "4.37. ¿Qué proporción de los medicamentos que ocupa el almacén corresponde a MME?"

ocupacion_vec <- c('0-20%', "20-40%", "40-60%", "60-80%", "80-100%", NA_character_)

ggPropMedicamento <- select(df, col1 = col1) %>% 
  mutate(col1 = factor(col1, rev(ocupacion_vec))) %>% 
  ggplot(aes(y = col1)) + 
  geom_bar(stat = 'count', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  xlab('Frecuencia') + 
  geom_text(aes(label = ..count..), stat = 'count', hjust = -0.8) +
  scale_y_discrete(drop = F) +
  scale_x_continuous(expand = c(0,0,0.5,0)) + 
  coord_cartesian(xlim = c(0, 10)) +
  labs(title = 'Proporción que ocupan los MME dentro del almacén') + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

#+ ggPropMedicamento, fig.width = 6, fig.height = 4, out.width = "60%"
ggPropMedicamento
guardarGGplot(ggPropMedicamento, '134_PropOcupacionAlmacen', 6,4)

#'-------------------------------------------------------------------------------
# Ingresos de los FRE ------------------
#'-------------------------------------------------------------------------------
df2 <- read_csv(file.path('data', 'processed', '001_Herramienta_Procesada.csv'), 
                na = c('N/A', 'No aplica', 'NA', 'NR')) %>% 
  mutate(
    Departamento = str_replace(Departamento, '(?<=San Andrés).+', ''),
    Departamento_1 = str_replace(Departamento_1, 
                                 'ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA', 'SAN ANDRÉS'))

df2_total <- select(df2, matches("^3\\.12"), Departamento_1, CodigoDepartamento) %>% 
  select(-1) %>% 
  pivot_longer(cols = matches("^3\\.12")) %>% 
  mutate(
    name = str_replace(name, "3.12\\s{1,4}", ""),
    Departamento_1 = str_to_title(Departamento_1)) %>% 
  left_join(colombiaGeoDF, by = c('CodigoDepartamento' = 'DPTO'))
  
df2_total <- df2_total %>% 
  mutate(
    name = str_replace(name, "\\:", "") %>% str_wrap(35),
    Departamento_1 = str_replace(Departamento_1, 
                                 'Archipiélago De San Andrés, Providencia Y Santa Catalina', 
                                 'SAN ANDRÉS')
  )


escala_color <- c('red', 'green', 'orange', 'yellow', 'purple', 'blue')
names(escala_color) <- df2_total$name %>% unique()

ggPropIngresoDepartamentos <- df2_total %>% 
  ggplot(aes(x = value, y = reorder(Departamento_1, desc(Departamento_1)), fill = name)) + 
  geom_bar(stat = 'identity', position = 'stack') + 
  scale_fill_manual(values = escala_color, name = NULL) + 
  xlab('Proporción') + 
  theme(axis.title.y = element_blank())

#+ ggPropIngresoDepartamentos, fig.width = 8, fig.height = 6, out.width = "60%"
ggPropIngresoDepartamentos
guardarGGplot(ggPropIngresoDepartamentos, '135_IngresosFRE1', 8,6)

#'-------------------------------------------------------------------------------

group_by(df_MUNICIPIO, CODIGO_DEPARTAMENTO) %>% 
  slice(1) -> df_MUNICIPIO_1

df3 <- select(df2, matches("^3\\.12"), Departamento_1, CodigoDepartamento) %>% 
  select(-1) %>%
  rename_with(~str_replace(.x, "3.12\\s{1,4}", "")) %>% 
  rename_with(~str_replace(.x, "\\:", "")) %>% 
  rename_with(~str_wrap(.x, 35)) %>% 
  left_join(df_MUNICIPIO_1, by = c('CodigoDepartamento' = 'CODIGO_DEPARTAMENTO'))  %>% 
  left_join(colombiaGeoDF, by = c('CodigoDepartamento' = 'DPTO'))
  

ggPropIngresoDepartamentos1 <- df3 %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry)) + 
  geom_scatterpie(aes(x=LONGITUD, y=LATITUD, group=NOMBRE_DEPARTAMENTO),
                  data = df3,
                  cols=colnames(df3)[1:6], pie_scale = 1.5) +
  theme(axis.title = element_blank(),
        axis.text = element_blank()) +
  scale_fill_manual(values = escala_color, name = NULL)

#+ ggPropIngresoDepartamentos1, fig.width = 8, fig.height = 6, out.width = "60%"
ggPropIngresoDepartamentos1
guardarGGplot(ggPropIngresoDepartamentos1, '136_IngresosFRE2', 8, 6)
