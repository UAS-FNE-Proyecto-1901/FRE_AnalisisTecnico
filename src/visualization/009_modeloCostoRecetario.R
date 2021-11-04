#' --- 
#' title: 'Evaluación de Sección de Ruta Tecnológica'
#' subtitle: 'Misión PRI 1901' 
#' date: '30-06-2021' 
#' author: Daniel S. Parra G. 
#' email: dsparrag@minsalud.gov.co 
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
require(tidymodels)
require(pdp)

#+ source1, warning = FALSE, message=FALSE
source(file.path('src', 'data', '901_funcionesMapa.R'), encoding = 'UTF-8')
source(file.path('src', 'visualization', '900_funcionExtraccionDummies.R'), 
       encoding = 'UTF-8')
source(file.path('src', 'models', '900_funcionesAlmacenamientoGrafico.R'), 
       encoding = 'UTF-8')
source(file.path('src', 'visualization', '901_funcionesBarras.R'), encoding = 'UTF-8')

#'-------------------------------------------------------------------------------
# 1. Que herramienta utiliza el FRE para el control de inventarios------------------
#'-------------------------------------------------------------------------------
df <- read_csv(file.path('data', 'processed', '001_Herramienta_Procesada.csv'), 
               na = c('N/A', 'No aplica', 'NA'), show_col_types = FALSE) %>% 
  mutate(
    Departamento = str_replace(Departamento, '(?<=San Andrés).+', ''),
    Departamento_1 = str_replace(
      Departamento_1, 
      'ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA', 
      'SAN ANDRÉS')
    )

#'-------------------------------------------------------------------------------
# 1. Lectura de datos------------------
#'-------------------------------------------------------------------------------
#'
#' ## Lectura de datos
#' 
# Crear característica de N. de medidas
col1 <- '3.36. ¿Con cuales medidas de seguridad internas/externas cuenta el recetario oficial?'
col2 <- 'Si la respuesta anterior fue otro, indique cual...78'


df[, 'Nmedidas'] <- pull(df, col1) %>% str_detect('Otro') %>%
  ifelse(paste(pull(df, col1), pull(df, col2), sep = ','),
         pull(df, col1)) %>% 
  separarDummies(.) %>% 
  select(!Otro) %>% 
  rowwise() %>% 
  mutate(N_medidas = sum(c_across(everything()))) %>% 
  pull(N_medidas)

#'-------------------------------------------------------------------------------
# 2. Preparación de datos ------------------
#'-------------------------------------------------------------------------------
#'
#' ## Preparación de datos
#' 


col1 <- '3.06 Costo de adquisición del recetario (COP)'
col2 <- '3.05 N.º de prescripciones por recetario'
col3 <- '3.13. ¿Qué modalidades de selección se utilizan en la contratación para adquisición de recetarios oficiales en el Departamento?'
col4 <- 'Act_Impr'
col5 <- 'Act_Impr_2'

X_matriz <- df %>% 
  rename(
    Costo = .data[[col1]],
    NoPrescripciones = .data[[col2]],
    Modalidades = .data[[col3]],
    Act_Impr_1 = .data[[col4]],
    Act_Impr_2 = .data[[col5]]
  ) %>% 
  select(all_of(c('Costo', 'NoPrescripciones', 'Nmedidas', 'Modalidades', 
                  'Act_Impr_1', 'Act_Impr_2'))) 

X_matriz['Act_Impr'] <- X_matriz['Act_Impr_1'] + X_matriz['Act_Impr_2']

X_matriz_1 <- recipe(Costo ~ NoPrescripciones + Modalidades + Nmedidas, data = X_matriz) %>% 
  step_dummy(Modalidades) %>% 
  prep(training = X_matriz) %>% 
  bake(new_data = NULL)

# colnames(X_matriz) <- colnames(X_matriz) %>% 
#   str_replace('\\s', '\\_')

#'-------------------------------------------------------------------------------
# 3. Modelo de regresión ------------------
#'-------------------------------------------------------------------------------
#'
#'## Modelo de Regresión
#'
lm1 <- lm(Costo ~ NoPrescripciones + Nmedidas + 
            Modalidades_Licitación.pública + Modalidades_Mínima.Cuantía + 
            Modalidades_Selección.abreviada, 
          data = X_matriz_1)

lm1 %>% summary()


vec_modalidad <- c(
  'Modalidades_Licitación.pública',
  'Modalidades_Mínima.Cuantía',
  'Modalidades_Selección.abreviada'
)

parContrataDir <- vector('list', 3L)

for (i in seq_along(vec_modalidad)) {
  parContrataDir[[i]] <-
    pdp::partial(lm1,  pred.var = vec_modalidad[[i]],
                 chull = TRUE, ice= TRUE)  
}

#' ## Gráficos de dependencia parcial

funcionBoxplots <- function(data, variable) {
  xlab1 <- rlang::quo_name(rlang::enquo(variable)) %>% 
    str_replace('\\_', ' ')
  
  as_tibble(data) %>% 
    ggplot(aes(x = {{variable}}, y = yhat, group = {{variable}})) + 
    geom_boxplot(outlier.size = 0, outlier.alpha = 0) + 
    geom_dotplot(binaxis='y', stackdir='center',
                 shape = 16, color = 'blue1', fill = 'blue1') +
    scale_x_continuous(breaks = c(0, 1),
                     labels = c('No', 'Sí')) +
    scale_y_continuous(labels = scales::dollar_format()) + 
    xlab(xlab1) + 
    ylab(bquote(hat(C)[recetario]))
}

ggdepend1 <- pdp::partial(lm1, pred.var = 'NoPrescripciones', 
                          chull = TRUE, ice = TRUE) %>% 
  autoplot() + 
  geom_point(data = X_matriz, aes(x=NoPrescripciones, y=Costo), 
             shape = 16, color = 'blue1') +
  scale_y_continuous(labels = scales::dollar_format()) + 
  xlab('N.° de prescripciones') +
  ylab(bquote(hat(C)[recetario]))

ggdepend2 <- pdp::partial(lm1, pred.var = 'Nmedidas', 
                          chull = TRUE, ice = TRUE) %>% 
  autoplot() + 
  geom_point(data = X_matriz, aes(x=Nmedidas, y=Costo), 
             shape = 16, color = 'blue1') +
  scale_y_continuous(labels = scales::dollar_format()) + 
  xlab('N.° de medidas de seguridad') +
  ylab(bquote(hat(C)[recetario]))

# ggdepend6 <- pdp::partial(lm1, pred.var = 'Act_Impr', 
#              chull = TRUE, ice = TRUE) %>% 
#   autoplot() + 
#   scale_y_continuous(labels = scales::dollar_format()) + 
#   xlab('N.° de oferentes en el departamento') +
#   ylab(bquote(hat(C)[recetario]))


ggdepend3 <- funcionBoxplots(parContrataDir[[1]], Modalidades_Licitación.pública) + 
  xlab('Modalidad: \n Licitación Pública')
ggdepend4 <- funcionBoxplots(parContrataDir[[2]], Modalidades_Mínima.Cuantía) + 
  xlab('Modalidad: \n Mínima cuantía')
ggdepend5 <- funcionBoxplots(parContrataDir[[3]], Modalidades_Selección.abreviada) + 
  xlab('Modalidad: \n Selección abreviada')

ggdependT <-
  wrap_plots(ggdepend1, ggdepend2, ggdepend3, ggdepend4, ggdepend5)

#+ ggdependT, warning=FALSE, message=FALSE, fig.cap="Gráficos de dependencia parcial", fig.align="center", fig.pos="t"
ggdependT + plot_annotation(title = 'Gráficos de dependencia parcial')

guardarGGplot(ggdependT, '046c_GraficasDependenParcial', 10, 6)


#+ X_matriz, warning=FALSE, message=FALSE, fig.cap="Costo vs No. de prescripciones", fig.align="center", fig.pos="t"
X_matriz %>% 
  drop_na(Costo) %>% 
  ggplot(aes(x = NoPrescripciones, y = Costo))+ 
  geom_point() + 
  stat_smooth(method = 'lm', formula = "y~x")
