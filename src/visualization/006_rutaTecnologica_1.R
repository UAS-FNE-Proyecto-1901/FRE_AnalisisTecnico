#' --- 
#' title: 'Evaluación de Sección de Ruta Tecnológica'
#' subtitle: 'Misión PRI 1901' 
#' date: '30-06-2021' 
#' author: 
#'        - name: Daniel S. Parra G. 
#'          email: dsparrag@minsalud.gov.co 
#'          institute: FNE 
#' institute: 
#'        - FNE: Misión PRI 1901 - Fondo Nacional de Estupefacientes 
#' abstract: |
#'       Evaluación de preguntas en la sección de ruta tecnológica. Se tratan 
#'       algunas preguntas de inventario y otras preguntas relacionadas 
#'       con el manejo de los recetarios oficiales. 
#'       
#' output:  
#'      - html_document: default 
#'      - pdf_document: default 
#' always_allow_html: true 
#' --- 

#+ setup1, warning=FALSE, message=FALSE
require(plotly)
require(tidyverse); theme_set(theme_bw())
require(lubridate)
require(ggrepel)
require(patchwork)

#+ source1, warning = FALSE, message=FALSE
source(file.path('src', 'data', '901_funcionesMapa.R'), encoding = 'UTF-8')
source(file.path('src', 'visualization', '900_funcionExtraccionDummies.R'), encoding = 'UTF-8')
source(file.path('src', 'models', '900_funcionesAlmacenamientoGrafico.R'), encoding = 'UTF-8')
source(file.path('src', 'visualization', '901_funcionesBarras.R'), encoding = 'UTF-8')

#'-------------------------------------------------------------------------------
# 1. Que herramienta utiliza el FRE para el control de inventarios------------------
#'-------------------------------------------------------------------------------
df <- read_csv(file.path('data', 'processed', '001_Herramienta_Procesada.csv'), 
               na = c('N/A', 'No aplica', 'NA'))

separarOtros <- function(string) {
  str_split(string, '\\,')
  
}

# separarOtros('Libro contable')

df$HerramientaInvetarios <-
  map2_chr(df$`3.24. ¿Qué herramienta utiliza el FRE para realizar el control de ventas de los recetarios?`,
           df$`Si la respuesta anterior fue otro, indique cual...64`,
           ~ifelse(.x != 'Otro', .x, .y))

gHerramientaInventario <- df$HerramientaInvetarios %>% 
  table() %>% as_tibble() %>% 
  ggplot(aes(y = fct_reorder(., n), x = n)) +
  geom_bar(stat = 'identity', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = n), hjust = -0.3) + 
  xlab('Frecuencia') + 
  labs(title = 'Herramientas de manejo inventarios') + 
  coord_cartesian(xlim = c(0, NA)) + 
  theme(
    axis.title.y = element_blank(), 
    panel.grid = element_blank())

gHerramientaInventario

guardarGGplot(gHerramientaInventario, '040_herramInventario', 7, 4)

#'-------------------------------------------------------------------------------
# 2. ¿Qué información se utiliza para diligencia la BD? ------------------
#'-------------------------------------------------------------------------------

gCDilBDRecet <- df$`3.25. ¿Qué información (campos) se consigna en el instrumento aplicado en 3.24.?` %>% 
  separarDummies(.) %>% 
  pivot_longer(cols = everything()) %>% 
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
  xlab('Proporción') + 
  labs(title = 'Proporción de FRE que diligencian estos campos en BD de ventas de recetarios') + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

gCDilBDRecet

guardarGGplot(gCDilBDRecet, '041_BD_diligBDRecet', 7, 4)

#'-------------------------------------------------------------------------------
# 3. Planes de contigencia en caso de no disponib de recet------------------
#'-------------------------------------------------------------------------------
col1 = "3.26. ¿El FRE cuenta con un plan de contingencia (verbal o escrito) en caso de no disponibilidad de recetarios oficiales?. Sí lo tiene, descríbalo."

df %>% 
  select(Departamento_1, col1 = col1) %>% 
  mutate(Departamento_1 = str_to_title(Departamento_1)) %>% 
  gt::gt() %>% 
  gt::tab_options(table.font.size = 9) %>% 
  gt::cols_label(
    col1 = 'Planes de Contingencia',
    Departamento_1  = 'Departamento') %>% 
  gt::opt_row_striping(row_striping = TRUE)

#'-------------------------------------------------------------------------------
# 3.27. ¿Qué tan de acuerdo está el FRE con la implementación del --------------- 
# Recetario Oficial Electrónico (ROE)? 
#'-------------------------------------------------------------------------------
col1 <- "3.27. ¿Qué tan de acuerdo está el FRE con la implementación del Recetario Oficial Electrónico (ROE)?"

escLikert <- c('Muy en desacuerdo', 'Algo en desacuerdo', 'Ni de acuerdo ni en\ndesacuerdo', 
               'Algo de acuerdo', 'Muy de acuerdo')

ttAcuerdoImplROE <- pull(df, col1) %>% 
  {as_tibble(table(., dnn = 'Tipo'))}

ggAcuerdoImplROE <- ttAcuerdoImplROE %>%
  mutate(prop = round(n/sum(n), 2),
         label = paste0(n, ' (', prop, ')')) %>% 
  mutate(Tipo = str_wrap(Tipo, 20),
         Tipo = factor(Tipo, levels = escLikert)) %>% 
  ggplot(aes(y = Tipo, x = n)) + 
  geom_bar(stat = 'identity', fill = '#527ACC', color = 'black', alpha = 0.6) +
  geom_text(aes(label = label), hjust = -0.3, size = 3.5) + 
  coord_cartesian(xlim = c(0, max(ttAcuerdoImplROE$n)*1.25)) +
  xlab("Frecuencia (%)") + 
  labs(title = '¿Qué tan de acuerdo está con la implementación del ROE?') + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

ggAcuerdoImplROE

guardarGGplot(ggAcuerdoImplROE, '042_FRE_implementacionROE', 6, 4)

#+ Razones para estar en acuerdo/desacuerdo con la implementación del ROE

df %>% 
  select(
    Dept = Departamento_1,
    'EL' = `3.27. ¿Qué tan de acuerdo está el FRE con la implementación del Recetario Oficial Electrónico (ROE)?`,
    'Justificacion' = `3.28. ¿Qué argumentos utilizó para la elección de la respuesta en la pregunta 3.27.?`) %>% 
  mutate(
    EL = factor(EL, levels = escLikert),
    Dept = str_to_title(Dept)) %>% 
  arrange(EL) %>% 
  gt::gt(groupname_col = 'EL') %>% 
  gt::tab_options(table.font.size = 10) %>% 
  gt::cols_label(Dept = 'Departamento', 
                 Justificacion = 'Justificación') %>% 
  gt::opt_row_striping(row_striping = TRUE)

#'-------------------------------------------------------------------------------
# 3.31. ¿Qué actividades realiza el FRE para realizar un seguimiento ------------
# al uso de los recetarios oficiales?
#'-------------------------------------------------------------------------------

col1 <- '3.31. ¿Qué actividades realiza el FRE para realizar un seguimiento al uso de los recetarios oficiales?'
col2 <- 'Si la respuesta anterior fue otro, indique cual...71'


df[,'ActividadesFRE'] <- pull(df, col1) %>% 
  str_detect('Otro') %>%
  ifelse(paste(pull(df, col1), pull(df, col2), sep = ','),
         pull(df, col1))

ggActividadesSeguimROE <- df$ActividadesFRE %>% 
  separarDummies(., descartar = T) %>% 
  select(!Otro) %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(
    name = str_wrap(name, 40),
    name = str_replace(name, 'verficacion', 'verificación'),
    name = str_replace(name, 'institciones', 'instituciones')) %>% 
  group_by(name) %>% 
  summarise(
    conteo = sum(value, na.rm = T),
    propor = conteo/dim(df)[1],
    label1 = paste(conteo, '/', dim(df)[1])
  ) %>% 
  ggplot(aes(y = fct_reorder(name, propor), x = propor)) +
  geom_bar(stat = 'identity', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = label1), hjust = -0.3, size = 3) + 
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(labels = scales::percent_format()) +
  xlab('Proporción (%)') + 
  labs(title = 'Actividades del FRE para realizar seguimiento al uso de recetarios') + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

ggActividadesSeguimROE

guardarGGplot(ggActividadesSeguimROE, '043_SeguimientoUsoRecetarios', 8, 5)

#'-------------------------------------------------------------------------------
# 3.33. ¿Con qué frecuencia realiza el FRE el control de existencias ------------
# o saldos de los recetarios oficiales? 
#'-------------------------------------------------------------------------------

col1 <- '3.33. ¿Con qué frecuencia realiza el FRE el control de existencias o saldos de los recetarios oficiales?'
col2 <- 'Si la respuesta anterior fue otro, indique cual...74'

frecRevision <- c('Diario', 'Semanal', 'Quincenal', 'Mensual', 'Ocasionalmente', 'No aplica')

ggFrecControlExistRecetario <- df %>% 
  {map2_chr(pull(., col1), pull(., col2), ~ifelse(.x != 'Otro', .x, .y))} %>% 
  table(useNA = 'always') %>% as_tibble() %>%
  rename(Frec = '.') %>% 
  mutate(
    Frec = ifelse(is.na(Frec), 'No aplica', Frec),
    Frec = factor(Frec, rev(frecRevision)),
    ) %>% 
  ggplot(aes(y = Frec, x = n)) +
  geom_bar(stat = 'identity', fill = '#6699ff', color = 'black', alpha = 0.6) +
  geom_text(aes(label = n), hjust = -0.6) +
  # coord_cartesian(xlim = c(0, 1)) +
  # scale_x_continuous(labels = scales::percent_format()) +
  xlab('Proporción (%)') + 
  labs(title = 'Frecuencia de revisión existencias de recetarios') + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

ggFrecControlExistRecetario

guardarGGplot(ggFrecControlExistRecetario, '044_FrecExistenciaRecetarios', 6, 5)

#'-------------------------------------------------------------------------------
# 3.36. ¿Con cuales medidas de seguridad internas/externas cuenta ---------------
# el recetario oficial? 
#'-------------------------------------------------------------------------------

col1 <- '3.36. ¿Con cuales medidas de seguridad internas/externas cuenta el recetario oficial?'
col2 <- 'Si la respuesta anterior fue otro, indique cual...78'

ggMedidasSeguridadRec <- pull(df, col1) %>% str_detect('Otro') %>%
  ifelse(paste(pull(df, col1), pull(df, col2), sep = ','),
         pull(df, col1)) %>% 
  separarDummies(.) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  summarise(
    conteo = sum(value),
    propor = sum(value)/dim(df)[1],
    label1 = paste(conteo, '/', dim(df)[1])
  ) %>%
  ungroup() %>% 
  mutate(name = str_wrap(name, 30)) %>%
  filter(name != 'Otro') %>% 
  ggplot(aes(y = fct_reorder(name, propor), x = propor)) +
  geom_bar(stat = 'identity', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = label1), hjust = -0.3, size = 3) + 
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(labels = scales::percent_format()) +
  xlab('Proporción') + 
  labs(title = 'Medidas de seguridad de recetarios') + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

ggMedidasSeguridadRec

guardarGGplot(ggMedidasSeguridadRec, '045_MedidasSeguridadRec', 7, 5)

#'-------------------------------------------------------------------------------
# Comparativo entre N.° de medidas y N.° de características de recetarios -------
#'-------------------------------------------------------------------------------

Nmedidas <- pull(df, col1) %>% str_detect('Otro') %>%
  ifelse(paste(pull(df, col1), pull(df, col2), sep = ','),
         pull(df, col1)) %>% 
  separarDummies() %>% 
  select(!Otro) %>% 
  rowwise() %>% 
  mutate(N_medidas = sum(c_across(everything()))) %>% 
  pull(N_medidas)

df[, 'Nmedidas'] <- Nmedidas

#+ warning = FALSE
ggPVTARec1 <- ggplot(data = df,
       aes(x = Nmedidas,
           y = `3.06 Costo de adquisición del recetario (COP)`)) +
  stat_smooth(method = 'lm', se = F, lty = 'dashed') + 
  geom_point() +
  geom_label_repel(aes(label = str_to_sentence(Departamento_1) %>% str_wrap(10)), 
                   size = 3) + 
  scale_x_continuous(breaks = seq(0,10,2)) +
  scale_y_continuous(labels = scales::dollar_format()) + 
  ylab('Costo adquisición (COP)') + 
  xlab('N.° de características de seguridad')

#+ warning = FALSE
ggPVTARec2 <- ggplot(data = df, aes(x = Nmedidas, 
                      y = `3.07 Precio de venta del recetario (COP)`)) +
  stat_smooth(method = 'lm', se = F, lty = 'dashed') + 
  geom_point()+ 
  geom_label_repel(aes(label = str_to_sentence(Departamento_1) %>% str_wrap(10)), 
                   size = 3) + 
  scale_x_continuous(breaks = seq(0,10,2)) +
  scale_y_continuous(labels = scales::dollar_format()) + 
  ylab('Precio de venta de recetario (COP)') + 
  xlab('N.° de características de seguridad')

ggPVTARecTotal <- ggPVTARec1 + ggPVTARec2

#+ graficoCompuesto1, fig.width=10, fig.height=5, out.width="90%", warning=FALSE, message=FALSE
ggPVTARecTotal

guardarGGplot(ggPVTARecTotal, '046_ComparativoCostosRecetarios', 10, 5)

#'-------------------------------------------------------------------------------
# 3.41. ¿Se reciben los recetarios oficiales de las instituciones ---------------
# inscritas para el manejo de MCE? 
#'-------------------------------------------------------------------------------

col1 <- "3.41. ¿Se reciben los recetarios oficiales de las instituciones inscritas para el manejo de MCE?"

ggReciboRecetarios <- pull(df, col1) %>%
  table(.) %>% as_tibble() %>%
  mutate(
    prop = n / sum(n),
    ncumsum = cumsum(prop) - 0.5 * prop,
    label1 = paste(., scales::percent(prop, accuracy = 3), sep = '\n')
  ) %>%
  pieChart(prop, label1) + 
  scale_fill_brewer(palette = 'Set1', name = 'Recibo recetarios') +
  theme(legend.position="bottom") + 
  labs(title = 'Existe recibo de recetarios oficiales de instituciones inscritas')
  
  
#+ pieChart1, fig.width=8, fig.height=5, out.width="90%", warning=FALSE, 
#+ message=FALSE
ggReciboRecetarios

guardarGGplot(ggReciboRecetarios, '047_ReciboRecetarios', 6, 5)


#'-------------------------------------------------------------------------------
col1 <- '3.42.¿Con qué frecuencia se reciben los recetarios oficiales de las IPS? Si aplica 3.41.'
col2 <- 'Si la respuesta anterior fue otro, indique cual...81'
col3 <- '3.43. ¿En qué fechas se reciben los recetarios oficiales de la IPS? Si aplica 3.4.1'


map2_chr(pull(df, col1), pull(df, col2), ~ ifelse(.x != 'Otro', .x, .y)) %>%
  table() %>% as_tibble()

tFrecRecetOficialesIPS <- pull(df, col3) %>% 
  table(dnn = 'F_recibo') %>% as_tibble() %>% 
  mutate(F_recibo = str_wrap(F_recibo, 30L))

ggFrecRecetOficialesIPS <- tFrecRecetOficialesIPS %>% 
  mutate(label1 = n) %>% 
  ggplot(aes(y = fct_reorder(F_recibo, n), x = n)) +
  geom_bar(stat = 'identity', fill = '#6699ff', color = 'black', alpha = 0.6) +
  geom_text(aes(label = label1), hjust = -0.8) + 
  xlab('Frecuencia') + 
  coord_cartesian(xlim = c(0, max(tFrecRecetOficialesIPS$n)*1.2)) +
  labs(title = 'Tiempo de recepción recetarios oficiales desde IPS') + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

#+ ggFrecRecetariosOficialesIPS, fig.width=7, fig.height=5, out.width="90%", warning=FALSE, 
#+ message=FALSE
ggFrecRecetOficialesIPS

guardarGGplot(ggFrecRecetOficialesIPS, '048_ReciboRecetariosIPS', 7, 5)

#'-------------------------------------------------------------------------------
# 3.61. ¿Cuánto tiempo se archivan en el FRE los recetarios oficiales? ----------
#'-------------------------------------------------------------------------------

col1 <- "3.61. ¿Cuánto tiempo se archivan en el FRE los recetarios oficiales?"

lvlDuracion <- c("0 a 6 meses", "1 a 2 años", "2 a 5 años", "> 5 años", "NA")

ttDuracionFRE <- pull(df, col1) %>% 
  {as_tibble(table(., dnn = 'Tiempo'))} 

ggDuracionFRE <- select(df, Duracion = col1) %>% 
  drop_na() %>%
  mutate(Duracion = factor(Duracion, rev(lvlDuracion))) %>%
  ggplot(aes(y = Duracion)) + 
  geom_bar(stat = 'count', fill = '#6699ff', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = ..count..), stat = 'count', hjust = -0.5) + 
  coord_cartesian(xlim = c(0, max(ttDuracionFRE$n)*1.2)) + 
  xlab('Frecuencia') +
  labs(title = 'Tiempo de archivo de los recetarios oficiales en los FRE') +
  theme(axis.title.y = element_blank())


#+ ggDuracionFRE, fig.width=7, fig.height=5, out.width="90%", warning=FALSE, 
#+ message=FALSE
ggDuracionFRE

guardarGGplot(ggDuracionFRE, '049_DuracionRecetFRE', 7, 5)


#'-------------------------------------------------------------------------------
# 3.62. ¿Con cuales medidas de seguridad se cuentan para el ------------------ 
# almacenamiento de los recetarios oficiales? 
#'-------------------------------------------------------------------------------

col1 <- "3.62. ¿Con cuales medidas de seguridad se cuentan para el almacenamiento de los recetarios oficiales?"

ggMedidasSeguridadAlmac <- pull(df, col1) %>% 
  na.omit() %>% separarDummies(.) %>%
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
  labs(title = 'Medidas de seguridad en almacenamiento de recetarios') + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

#+ ggMedidasSeguridadAlmac, fig.width=7, fig.height=5, out.width="90%", warning=FALSE, 
#+ message=FALSE
ggMedidasSeguridadAlmac

guardarGGplot(ggMedidasSeguridadAlmac, '050_MedidasSeguridadAlmac', 7, 5)

#'-------------------------------------------------------------------------------
# 3.66. ¿Qué información (campos) se consigna en la herramienta -----------------
# aplicado en 3.65.? 
#'-------------------------------------------------------------------------------
col1 <- "3.66. ¿Qué información (campos) se consigna en la herramienta aplicado en 3.65.?"

ggInformacionHerramienta <- pull(df, col1) %>% 
  separarDummies() %>% 
  select(-1) %>% 
  pivot_longer(cols = everything()) %>% 
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
  xlab('Proporción (%)') + 
  labs(title = 'Proporción de FRE que diligencian estos campos en BD de ventas de recetarios') + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

#+ ggInformacionHerramienta, fig.width=7, fig.height=5, out.width="90%", warning=FALSE, 
#+ message=FALSE
ggInformacionHerramienta

guardarGGplot(ggInformacionHerramienta, '051_InformacionHerramienta', 7, 5)



