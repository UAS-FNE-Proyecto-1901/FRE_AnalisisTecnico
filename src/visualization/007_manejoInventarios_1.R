#' --- 
#' title: 'Gráficos de Manejo de Inventarios' 
#' subtitle: 'Misión PRI 1901' 
#' date: '07-08-2021' 
#' author: 
#'        - name: Daniel S. Parra G. 
#'          email: dsparrag@minsalud.gov.co 
#'          institute: FNE 
#' institute: 
#'        - FNE: Misión PRI 1901 - Fondo Nacional de Estupefacientes 
#' abstract: |
#'       Informe final de asistencias técnicas a los FRE. En este script se mencionan las respuestas a 
#'       preguntas relacionadas con el manejo del inventario por parte de los FRE.
#'        
#' output:  
#'      - html_document: default 
#'      - pdf_document: default 
#' always_allow_html: true 
#' --- 

#+ setup1, warning = FALSE, message=FALSE
require(plotly)
require(tidyverse); theme_set(theme_bw())
require(lubridate)
require(ggrepel)
require(patchwork)
require(ggsflabel)

#'-------------------------------------------------------------------------------
# 1. Introducción ------------------
#'-------------------------------------------------------------------------------
#+ source1
source(file.path('src', 'data', '901_funcionesMapa.R'), encoding = 'UTF-8')
source(file.path('src', 'models', '900_funcionesAlmacenamientoGrafico.R'), encoding = 'UTF-8')
source(file.path('src', 'visualization', '900_funcionExtraccionDummies.R'), encoding = 'UTF-8')
source(file.path('src', 'visualization', '901_funcionesBarras.R'), encoding = 'UTF-8')

df_DIVIPOLA <-
  read_csv(file.path('.', 'data', 'processed', '798_DANE_DEPARTAMENTO.csv'))

df_MUNICIPIO <-
  read_csv(file.path('data', 'processed', '799_DANE_DIVIPOLA.csv'), 
           locale = locale(encoding = 'latin1'))

df <- read_csv(file.path('data', 'processed', '001_Herramienta_Procesada.csv'), 
               na = c('N/A', 'No aplica', 'NA', '<NA>', 'N.R', '-')) %>% 
  mutate(
    Departamento = str_replace(Departamento, '(?<=San Andrés).+', ''),
    Departamento_1 = str_replace(Departamento_1, 'ARCHIPIÉLAGO DE SAN ANDRÉS, PROVIDENCIA Y SANTA CATALINA', 'SAN ANDRÉS'))

#'-------------------------------------------------------------------------------
# 4.01. ¿Con cuales herramientas cuenta el FRE para el manejo de inventarios?-------
#'-------------------------------------------------------------------------------

col1 <- '4.01. ¿Con cuales herramientas cuenta el FRE para el manejo de inventarios?'
col2 <- 'Si seleccionó paquete ofimático o software, especifique cuál...94'

ggHerramientas1 <- pull(df, col1) %>% 
  separarDummies(., descartar = T) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  summarise(
    conteo = sum(value, na.rm = TRUE),
    propor = conteo/dim(df)[1],
    label1 = paste(conteo, '/', dim(df)[1])
  ) %>% 
  ggplot(aes(y = name, x = propor)) + 
  geom_bar(stat = 'identity', fill = '#3366CC', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = label1), hjust = -0.8, size = 4) + 
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(labels = scales::percent_format()) +
  xlab('Proporción') + 
  labs(title = 'Herramientas para el manejo de inventarios') + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

ggHerramientas1

guardarGGplot(ggHerramientas1, '071_Herramienta', 7, 5)

ggHerramientas2 <- pull(df, col2) %>% 
  {ifelse(is.na(.), pull(df, col1), pull(df, col2))} %>% 
  separarDummies(T) %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(name = str_replace(name, 'Paquete ofimático', 'Excel')) %>% 
  group_by(name) %>% 
  summarise(
    conteo = sum(value, na.rm = TRUE),
    propor = conteo/dim(df)[1],
    label1 = paste(conteo, '/', dim(df)[1])
  ) %>% 
  ggplot(aes(y = fct_reorder(name, propor), x = propor)) + 
  geom_bar(stat = 'identity', fill = '#3366CC', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = label1), hjust = -0.8, size = 4) + 
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(labels = scales::percent_format()) +
  xlab('Proporción') + 
  labs(title = 'Herramientas para el manejo de inventarios') + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

ggHerramientas2

guardarGGplot(ggHerramientas2, '072_Herramienta', 7, 5)
#'-------------------------------------------------------------------------------
# 4.02. ¿Qué herramienta usa el FRE para la consolidación de anexos  -----------
# de la Resolución 1479 de 2006?
#'-------------------------------------------------------------------------------

col1 <- '4.02. ¿Qué herramienta usa el FRE para la consolidación de anexos de la Resolución 1479 de 2006?'
col2 <- 'Si seleccionó paquete ofimático o software, especifique cuál...96'
  
#### REVISION

ggHerrConsolidacion <- 
  # pull(df, col2) %>% 
  # {ifelse(is.na(.), pull(df, col1), pull(df, col2))} %>% 
  pull(df, col1) %>%
  separarDummies(descartar = T) %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(name = str_replace(name, 'Paquete ofimático', 'Excel')) %>% 
  group_by(name) %>% 
  summarise(
    conteo = sum(value, na.rm = TRUE),
    propor = conteo/dim(df)[1],
    label1 = paste(conteo, '/', dim(df)[1])
  ) %>% 
  mutate(name = str_wrap(name, 25)) %>% 
  ggplot(aes(y = fct_reorder(name, propor), x = propor)) + 
  geom_bar(stat = 'identity', fill = '#3366CC', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = label1), hjust = -0.8, size = 4) + 
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(labels = scales::percent_format()) +
  xlab('Proporción') + 
  labs(title = 'Herramientas diligenciamiento Res 1479/2006') + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

ggHerrConsolidacion

guardarGGplot(ggHerrConsolidacion, '073_HerramientaConsol', 7, 5)

#'-------------------------------------------------------------------------------
# 4.03. ¿Qué medios utiliza de manera frecuente para la ------------------
# comunicación con sus clientes? 
#'-------------------------------------------------------------------------------

col1 <- "4.03. ¿Qué medios utiliza de manera frecuente para la comunicación con sus clientes?"

ggMediosComunicacion <- pull(df, col1) %>% 
  separarDummies(., descartar = T) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  summarise(
    conteo = sum(value, na.rm = TRUE),
    propor = conteo/dim(df)[1],
    label1 = paste(conteo, '/', dim(df)[1])
  ) %>% 
  ggplot(aes(y = fct_reorder(name, propor), x = propor)) + 
  geom_bar(stat = 'identity', fill = '#3366CC', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = label1), hjust = -0.2, size = 3) + 
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(labels = scales::percent_format()) +
  xlab('Proporción') + 
  labs(title = 'Canales de comunicación FRE') + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

ggMediosComunicacion

guardarGGplot(ggMediosComunicacion, '074_MediosComunicacion', 7, 5)

#'-------------------------------------------------------------------------------
# 4.04. ¿Cómo puntuaría la velocidad de conexión de su internet? ------------------
#'-------------------------------------------------------------------------------

col1 <- "4.04. ¿Cómo puntuaría la velocidad de conexión de su internet?"

escala1 <- c('Excelente', 'Buena', 'Aceptable', 'Mala', 'Muy mala')

ggConexionInternet <- pull(df, col1) %>% 
  table() %>% as_tibble() %>%
  mutate(Fct = factor(., escala1)) %>% 
  ggplot(aes(y = Fct, x = n)) + 
  geom_bar(stat = 'identity', fill = '#3366CC', color = 'black', alpha = 0.6) +
  geom_text(aes(label = n), hjust = -0.5, size = 4) + 
  coord_cartesian(xlim = c(0, NA)) +
  scale_x_continuous(expand = c(0,0,0,1.2)) +
  xlab('N.° de FRE, frecuencia') + 
  labs(title = 'Velocidad de conexión de Internet') + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

ggConexionInternet

guardarGGplot(ggConexionInternet, '075_ConexionInternet', 7, 5)

col1 <- "4.05. ¿Cuántos equipos de cómputo tiene el FRE para el desarrollo de sus actividades?"

ggEquiposComputo <- pull(df, col1) %>%
  table() %>% as_tibble() %>%
  mutate(Equipos = as.double(.)) %>% 
  ggplot(aes(x = Equipos, y = n)) +
  geom_segment(aes(xend = Equipos, y = 0, yend = n), color="grey30") + 
  geom_point(size = 4, color = "#990f3f") + 
  geom_label(aes(label = n), vjust = -0.9, label.padding = unit(0.4, "lines")) +
  xlab('N.° de equipos') + 
  ylab('N.° de FRE, frecuencia') + 
  coord_cartesian(ylim = c(0, NA)) +
  scale_x_continuous(breaks = 0:6) +
  scale_y_continuous(expand = c(0, 0.0, 0.50, 0)) + 
  labs(title = 'N.° de equipos de computo en el FRE') +
  theme(panel.grid = element_blank())

#+ ggEquiposComputo, fig.width=6, fig.height=4, out.width="90%"
ggEquiposComputo
# 
guardarGGplot(ggEquiposComputo, '076_EquiposComputo', 6, 4)

#' Relación entre el número de personas (disponibilidad de recurso humano) y
#' equipos de computo presente en las instalaciones del FRE

df_total <- df %>% rowwise() %>% 
  mutate(Profesiones = list(c_across(matches('Profesión\\sdel\\s(personal|funcionario)'))))

df_total$NoPersonas <- df_total$Profesiones %>% 
  map_dbl(function(x){sum(!is.na(x))})

# Se realiza una copia de reserva
df_total1 <- df_total

df_total_2 <- df_total %>%
  rename(NoEquipos = col1) %>%
  mutate(Departamento_1 = str_to_title(Departamento_1)) %>% 
  drop_na(NoEquipos)

df_total_2['NoPersonas1'] <- df$No.PersonasVinculadasDirectamente

reg1 <- lm(NoEquipos ~ NoPersonas1, df_total_2)

ggCorrelacionEquipos <-  df_total_2 %>% 
  ggplot(aes(y = NoEquipos, x = NoPersonas1)) + 
  stat_smooth(se = T, method = 'lm', lty = 'dashed', col = 'black') +
  geom_point() +
  geom_label_repel(
    data = df_total_2[abs(reg1$residuals) > 0.8, ],
    mapping = aes(label = Departamento_1), size=3) +
  xlab('N.° de personas vinculadas \n directamente al FRE') +
  ylab('N.° de equipos') + 
  labs(title = 'Correlación entre personas y equipos') +
  theme(panel.grid = element_blank())  

#+ ggCorrelacionEquipos, fig.width=6, fig.height=4, out.width="90%"
ggCorrelacionEquipos

guardarGGplot(ggCorrelacionEquipos, '077_CorrEquiposComputo', 6, 4)

#'-------------------------------------------------------------------------------
# 4.06. Los equipos de cómputo disponibles son: ------------------
#'-------------------------------------------------------------------------------
col1 <- '4.06. Los equipos de cómputo disponibles son:'

ttOpinionEquipos <- pull(df, col1) %>% 
  separarDummies(descartar = TRUE) %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(name = str_replace(name, 'Paquete ofimático', 'Excel')) %>% 
  group_by(name) %>% 
  summarise(
    conteo = sum(value, na.rm = TRUE),
    propor = conteo/dim(df)[1],
    label1 = paste(conteo, '/', dim(df)[1])
  ) %>% 
  mutate(name = str_to_sentence(name) %>% str_wrap(30))

ggOpinionEquipos <- ttOpinionEquipos %>% 
  ggplot(aes(y = fct_reorder(name, conteo), x = conteo)) +
  geom_bar(stat = 'identity', fill = '#3366CC', color = 'black', alpha = 0.6) +
  geom_text(aes(label = label1), hjust = -0.4, size = 4) + 
  xlab('Proporción (%)') + 
  labs(title = 'Opinión sobre los equipos de cómputo') + 
  coord_cartesian(xlim = c(0, max(ttOpinionEquipos$conteo) * 1.5)) +
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

#+ ggOpinionEquipos, fig.width=6, fig.height=4, out.width="90%"
ggOpinionEquipos

guardarGGplot(ggOpinionEquipos, '078_OpinionEquipos', 6, 4)


col1 <- "4.12. ¿Qué herramienta utiliza el FRE para realizar la estimación de compra de MME?"

ggHerramientasCompras <- pull(df, col1) %>%
  separarDummies(T) %>%
  pivot_longer(cols = everything()) %>% 
  mutate(name = str_replace(name, 'Paquete ofimático', 'Excel')) %>% 
  group_by(name) %>% 
  summarise(
    conteo = sum(value, na.rm = TRUE),
    propor = conteo/dim(df)[1],
    label1 = paste(conteo, '/', dim(df)[1])
  ) %>% 
  mutate(name = str_to_sentence(name) %>% str_wrap(20)) %>%
  pieChart(conteo, name, repel = TRUE) +
  scale_fill_brewer(palette = 'Set1') +
  theme(legend.position = 'none')

# ggHerramientasCompras <- pull(df, col1) %>%
#   table() %>% as_tibble() %>%
#   rename(label = '.') %>%
#   mutate(prop = n / sum(n),
#          label1 = paste0(label, '\n', n, ' - ', round(prop, 3)*100, '%')) %>%
#   pieChart(n, label1) +
#   scale_fill_brewer(palette = 'Set1') +
#   theme(legend.position = 'none')

ggHerramientasCompras
guardarGGplot(ggHerramientasCompras, '079_HerramientasCompras', 8, 6)

#'-------------------------------------------------------------------------------
# Evaluación de tiempos de etapas de compra ------------------
#'-------------------------------------------------------------------------------

col1 <- "4.13. ¿Cuánto tiempo toma la etapa de estimación de la necesidad de compra de MME (en días)?"
col2 <- "4.14. ¿Cuánto tiempo toma la etapa precontractual para la compra de MME? (en semanas)"
col3 <- "4.15. ¿Cuánto tiempo toma la etapa contractual para la compra de MME? (en semanas)"
col4 <- "4.16. ¿Cuánto tiempo toma la solicitud en plataforma tecnológica? (en días)"
col5 <- "4.17. ¿Cuánto tiempo toma el despacho de los MME? (en días)"
col6 <- "4.18. ¿Cuánto tiempo transcurre en el caso de traslados desde otros departamentos desde la solicitud hasta el despacho? (en días)"


tiempos_df <- data.frame(
  T1 = pull(df, col1),
  T2 = pull(df, col2),
  T3 = pull(df, col3),
  T4 = pull(df, col4),
  T5 = pull(df, col5)
) %>%
  mutate(T2 = T2 * 7,
         T3 = T3 * 7)

df_total <- df %>% cbind(tiempos_df) %>% 
  as_tibble() %>% 
  pivot_longer(cols = matches('T[1-5]')) %>%  
  group_by(Departamento_1) %>%  
  mutate(T_total = sum(value, na.rm = T),
         Departamento_1 = str_to_sentence(Departamento_1), 
         name = factor(name, rev(paste0('T', 1:5))))

nombresCorrectos <- c('Estimación', 'Precontractual', 
                      'Contractual', 'Solicitud Plataforma', 'Despacho')


ggProcesosAdquisicion <- df_total %>% 
  ggplot(aes(x = value, y = fct_reorder(Departamento_1, T_total), group = name)) +
  geom_bar(stat = 'identity', position = 'stack', aes(fill = name)) + 
  geom_text(aes(label = value), size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(
    labels = rev(nombresCorrectos), 
    palette = 'Set3',
    name = 'Etapa') + 
  xlab('Tiempo (días)') + 
  theme(axis.title.y = element_blank(), 
        legend.position = 'bottom') + 
  labs(title = 'Seguimiento de Procesos de adquisición')
  
#+ ggProcesosAdquisicion, fig.width = 12, fig.height = 7, out.width = "60%"
ggProcesosAdquisicion

guardarGGplot(ggProcesosAdquisicion, '080_ProcesosAdquisición', 12, 7)

plotlyProcesosAdquisicion <- df_total %>%
  mutate(Etapa1 = as.numeric(str_sub(name, -1)) %>% map_chr(~nombresCorrectos[.x])) %>% 
  plot_ly(
    type = 'bar',
    x = ~value,
    y = ~fct_reorder(Departamento_1, T_total),
    color = ~Etapa1
  ) %>% 
  layout(yaxis = list(title = NA),
         xaxis = list(title = 'Tiempo (días)'),
         barmode = 'stack') %>% 
  config(displaylogo = FALSE, displayModeBar=F)

#+ plotlyProcesosAdquisicion, out.width = "80%"
plotlyProcesosAdquisicion

guardarPlotly(plotlyProcesosAdquisicion, '081_ProcesosAdquisición', 12, 7, 
              libdir = 'plotly')



graficoBarrasTiempos <- function(tiempo, 
                                 titulo,
                                 data = df_total,
                                 xpos = 0.95,
                                 breaks = 1:15, fill = 'blue1') {
  
  filt_data <- data %>% filter(name == tiempo)
  
  mn_val <- median(filt_data$value, na.rm = T)
  q1_val <- round(quantile(filt_data$value, prob = 0.25, na.rm = T), 1)
  q3_val <- round(quantile(filt_data$value, prob = 0.75, na.rm = T), 1)
  
  mi_val <- min(filt_data$value, na.rm = T)
  ma_val <- max(filt_data$value, na.rm = T)
  

  NVal = (ma_val - mi_val) * xpos + mi_val
  
  filt_data %>%
    ggplot(aes(x = value)) +
    geom_histogram(bins = 18, 
                   fill='blue1', alpha=.4, 
                   color='gray2') +
    # geom_density(stat = 'count', color = fill) + 
    geom_vline(xintercept = mn_val, lty = 'dashed') + 
    geom_vline(xintercept = q1_val, lty = 'dashed', col = 'gray50') + 
    geom_vline(xintercept = q3_val, lty = 'dashed', col = 'gray50') + 
    annotate(geom = 'label', x = NVal, y = Inf, 
             label = glue::glue("Med. (IQR): {mn_val} ({q3_val-q1_val})"), 
             vjust = 1.2, size = 3.2, hjust = 1) + 
    xlab('Tiempo (días)') +
    ylab('N.° de FRE, frecuencia') +
    labs(title = titulo) + 
    theme(panel.grid = element_blank())  
}

dfTiempos1 <- paste0('T', 1:5)

ggProcesoAdqu <- 
  wrap_plots(pmap(list(dfTiempos1, nombresCorrectos),
                                 graficoBarrasTiempos))

#+ ggProcesoAdqu, fig.width = 10, fig.height = 6, out.width = "60%"
ggProcesoAdqu
guardarGGplot(ggProcesoAdqu, '082b_ProcesosAdquisición', 10, 6)

ggProcesosAdquisicion1 <- df_total %>%
  # mutate(map(name, function(x) x))
  ggplot(aes(x = value, y = fct_reorder(Departamento_1, T_total), group = name)) +
  geom_bar(stat = 'identity', aes(fill = name)) + 
  geom_text(aes(label = value), size = 3, hjust = -0.5) +
  scale_fill_discrete(
    labels = rev(c('Estimación', 'Precontractual', 'Contractual', 'Solicitud Plataforma', 'Despacho')),
    name = 'Etapa') + 
  xlab('Tiempo (días)') + 
  facet_wrap(vars(name)) +
  theme(axis.title.y = element_blank(), 
        legend.position = 'bottom') + 
  labs(title = 'Seguimiento de Procesos de adquisición')

#+ ggProcesosAdquisicion1, fig.width = 8, fig.height = 6, out.width = "60%"
ggProcesosAdquisicion1

guardarGGplot(ggProcesosAdquisicion1, '082_ProcesosAdquisición', 8, 6)

df_total %>%
  group_by(name) %>%
  summarise(
    mn = mean(value, na.rm = T),
    md = median(value, na.rm = T),
    li = quantile(value, 0.05 / 2, T),
    ls = quantile(value, 1 - (0.05 / 2), T)
  ) %>% 
  arrange(desc(name))


df_total %>% 
  select(Departamento_1, T_total) %>% 
  dplyr::distinct() %>% 
  arrange(desc(T_total)) %>% pull(T_total) %>% mean()

#'-------------------------------------------------------------------------------
# Proceso de translados interdepartamentales ------------------
#'-------------------------------------------------------------------------------

ggTiemposTraslados <- df %>%
  select(Tiempo = all_of(col6), Departamento_1) %>% 
  drop_na() %>% 
  mutate(Departamento_1 = str_to_title(Departamento_1)) %>% 
  ggplot(aes(x = Tiempo, y = fct_reorder(Departamento_1, Tiempo))) + 
  geom_segment(
    aes(x = 0, xend = Tiempo, yend = fct_reorder(Departamento_1, Tiempo)),
    color = '#3366CC') +
  geom_point(aes(x = Tiempo), color = 'blue4') + 
  geom_text(aes(label = Tiempo), hjust = -0.5) + 
  coord_cartesian(xlim = c(0, NA)) +
  scale_x_continuous(expand = c(0,0,0.2,0)) +
  xlab('Tiempo (días)') +
  labs(title = 'Tiempos en translados interdepartamentales') +
  theme(axis.title.y = element_blank(), panel.grid = element_blank())


#+ ggTiemposTraslados, fig.width = 6, fig.height = 4, out.width = "60%"
ggTiemposTraslados
guardarGGplot(ggTiemposTraslados, '083_TiemposTranslados', 6, 4)


ggTiemposTraslados1 <- df %>%
  rename(Tiempo = col6) %>%
  left_join(colombiaGeoDF, by = c('CodigoDepartamento' = 'DPTO')) %>%
  mutate(
    Departamento_1 = str_to_title(Departamento_1),
    label1 = ifelse(
      !is.na(Tiempo),
      paste0(Departamento_1, '\n', Tiempo, ' días'),
      NA_character_
    )
  ) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = Tiempo)) +
  geom_sf_label_repel(aes(label = label1), size = 3) +
  labs(title = 'Tiempos para traslados interdepartamentales') +
  scale_fill_viridis_c() + 
  # scale_fill_gradientn(colours = colorspace::heat_hcl(7)) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

#+ ggTiemposTraslados1, fig.width = 8, fig.height = 6, out.width = "60%"
ggTiemposTraslados1
guardarGGplot(ggTiemposTraslados1, '084_TiemposTranslados', 8,6)

rm(list = ls(pattern = '^col'))


col1 <- "4.19. ¿Qué tan conforme se encuentra el FRE con la plataforma Colombia Compra Eficiente?"
col2 <- "4.20. Justifique la respuesta a la pregunta 4.19"

escala1 <- c('Muy inconforme', 'Algo inconforme', 'Ni conforme ni inconforme',
             'Algo conforme', 'Muy conforme')

ggCompraEficiente <- select(df, Escala = col1) %>% 
  mutate(Escala = factor(Escala, level = escala1)) %>% 
  ggplot(aes(y = Escala)) + 
  geom_bar(stat = 'count', fill = '#3366CC', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = ..count..), stat = 'count', hjust = -0.8) + 
  scale_x_continuous(expand = c(0.05, 0.05, 0.15, 0.05)) +
  coord_cartesian(xlim = c(0, NA)) +
  xlab('N.° de FRE, frecuencia') + 
  theme(axis.title.y = element_blank(), 
        legend.position = 'bottom') + 
  labs(title = 'Conformidad con plataforma Colombia Compra Eficiente')

#+ ggCompraEficiente, fig.width = 8, fig.height = 6, out.width = "60%"
ggCompraEficiente
guardarGGplot(ggCompraEficiente, '085_ConformidadColombiaCompra', 8, 6)

df %>%
  select(Deptmn = Departamento_1,
         Escala = all_of(col1),
         Justif = all_of(col2)) %>% 
  mutate(
    Escala = factor(Escala, levels = escala1),
    Deptmn = str_to_title(Deptmn)) %>% 
  arrange(Escala) %>% 
  gt::gt(groupname_col = 'Escala') %>% 
  gt::tab_options(table.font.size = 9)

#'-------------------------------------------------------------------------------
# 4.22. ¿Cuánto tiempo se emplea para la recepción y almacenamiento --------------
# de los medicamentos? (en días) 
#'-------------------------------------------------------------------------------
colombiaGeoDF <- read_sf(file.path('data', 'external', 'colombia_geo.json'))

col1 <- "4.22. ¿Cuánto tiempo se emplea para la recepción y almacenamiento de los medicamentos? (en días)"

ggRecepcion <- df %>%
  rename(tiempoRecepcion = col1) %>%
  drop_na(tiempoRecepcion) %>%
  mutate(Departamento_1 = str_to_title(Departamento_1)) %>% 
  ggplot(aes(x = tiempoRecepcion, 
             y = fct_reorder(Departamento_1, tiempoRecepcion))) +
  geom_point(color = 'blue4') + 
  geom_segment(aes(x = 0, 
                   xend = tiempoRecepcion, 
                   yend = fct_reorder(Departamento_1, tiempoRecepcion))) + 
  # geom_bar(stat = 'identity',fill = '#3366CC', color = 'black', alpha = 0.6) +
  geom_text(aes(label = tiempoRecepcion), hjust = -0.8) +
  xlab('Tiempo de recepción (días)') + 
  coord_cartesian(xlim = c(0, NA)) +
  scale_x_continuous(expand = c(0,0,0.5,0)) + 
  theme(axis.title.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = 'bottom') + 
  labs(title = 'Tiempos en recepción y almacenamiento de medicamentos')

#+ ggRecepcion, fig.width = 8, fig.height = 6, out.width = "60%"
ggRecepcion
guardarGGplot(ggRecepcion, '086_RecepcionMedicamento', 8, 6)


ggRecepcionb <- df %>%
  rename(tiempoRecepcion = col1) %>%
  drop_na(tiempoRecepcion) %>%
  mutate(Departamento_1 = str_to_title(Departamento_1)) %>% 
  ggplot(aes(x = tiempoRecepcion)) +
  geom_bar(fill = alpha('blue4', 0.4), col ='black') + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.3) +
  xlab('Tiempo de recepción (días)') +
  ylab('N.° de FRE, frecuencia') +
  scale_y_continuous(expand = c(0,0,0.2,0)) +
  theme(panel.grid = element_blank(),
        legend.position = 'bottom') + 
  labs(title = 'Tiempos en recepción y almacenamiento de medicamentos')

ggRecepcionb
guardarGGplot(ggRecepcionb, '086b_RecepcionMedicamento', 8, 6)



ggTiempoRecepcionMedicamentos <- df %>% 
  right_join(colombiaGeoDF, by = c('CodigoDepartamento' = 'DPTO')) %>% 
  rename(tiempoRecepcion = col1) %>% 
  creacionCloroPletCol(geometry, tiempoRecepcion)

ggTiempoRecepcionMedicamentos[[1]] <- ggTiempoRecepcionMedicamentos[[1]] +
  scale_fill_viridis_c() + 
  # scale_fill_gradientn(colours = 'viridis') +
  theme(axis.title.y = element_blank(), 
        legend.position = 'bottom',
        axis.text = element_blank()) + 
  labs(title = 'Tiempo de recepción de medicamentos') +
  guides(
    fill = guide_colourbar(barwidth = 20, title.position = 'top', title = 'Demora (días)'))

ggTiempoRecepcionMedicamentos[[2]] <- ggTiempoRecepcionMedicamentos[[2]] +
  scale_fill_viridis_c()
ggTiempoRecepcionMedicamentos[[3]] <- ggTiempoRecepcionMedicamentos[[3]] +
  scale_fill_viridis_c()

#+ ggTiempoRecepcionMedicamentos, fig.width = 8, fig.height = 6, out.width = "60%"
ggTiempoRecepcionMedicamentos
guardarGGplot(ggTiempoRecepcionMedicamentos, '087_RecepcionMedicamento', 8, 6)

#'-------------------------------------------------------------------------------
# 4.31. ¿Qué controles se tienen para limitar el acceso de medicamentos al personal? ------------------
#'-------------------------------------------------------------------------------
col1 <- "4.31. ¿Qué controles se tienen para limitar el acceso de medicamentos al personal?"
col2 <- "Si la respuesta anterior fue otro, indique cual...116"

textoLargo <- "Procedimientos para el manejo de personal huésped, visitantes, mantenimiento o no empleados del FRE"

xDF <- pull(df, col2) %>%
  is.na() %>%
  ifelse(pull(df, col1),
         paste(pull(df, col1), pull(df, col2), sep = ',')) %>% 
  separarDummies(., descartar = T)

# Existe una columna que quedó separa pese a que proviene del mismo factor

ggSeguridadMedicamentos <- xDF %>% 
  select(!colnames(xDF)[10:12]) %>% 
  mutate({{textoLargo}} := apply(xDF[,colnames(xDF)[10:12]], 1, sum) %>% as.logical()) %>% 
  # select(!Otro) %>% 
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
  geom_bar(stat = 'identity', fill = '#3366CC', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = label1), hjust = -0.3, size = 3) + 
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(labels = scales::percent_format()) +
  xlab('Proporción (%)') + 
  labs(title = 'Medidas de seguridad en almacenamiento de MME') + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

#+ ggSeguridadMedicamentos, fig.width = 8, fig.height = 6, out.width = "60%"
ggSeguridadMedicamentos
guardarGGplot(ggSeguridadMedicamentos, '088_SeguridadMedicamentos', 8, 6)

#' Entre los FRE con mayor cantidad de medidas de seguridad para los medicamentos se tiene
#'  

data.frame(Depto = df$Departamento, Suma = apply(xDF, 1, sum), Camaras=xDF$Cámaras, 
           Disponib = xDF$`Disponibilidad de personal de seguridad privada`) %>% 
  arrange(desc(Suma))



#'-------------------------------------------------------------------------------
# 4.32. ¿Con que frecuencia se revisan las condiciones ambientales ------------- 
# en el lugar de almacenamiento de los medicamentos?
#'-------------------------------------------------------------------------------
col1 <- "4.32. ¿Con que frecuencia se revisan las condiciones ambientales en el lugar de almacenamiento de los medicamentos?"


etiquetasFrecuencia <- c('Dos veces al día', 'Diaria', 'Dos veces a la semana',
                         'Tres veces a la semana', 'Una vez a la semana',
                         'Cada mes',
                         'No se revisan condiciones\nambientales') 

ggFrecCondiciones <- pull(df, col1) %>% 
  table() %>% as_tibble() %>% 
  rename(Frec = '.') %>% 
  add_row(Frec = etiquetasFrecuencia[3], n = 0) %>% 
  add_row(Frec = etiquetasFrecuencia[4], n = 0) %>% 
  add_row(Frec = etiquetasFrecuencia[5], n = 0) %>% 
  add_row(Frec = etiquetasFrecuencia[6], n = 0) %>% 
  mutate(Frec = str_wrap(Frec, 30),
         Frec = factor(Frec, levels = rev(etiquetasFrecuencia))) %>%
  ggplot(aes(y = Frec, x =  n)) + 
  geom_bar(stat = 'identity', fill = '#3366CC', color = 'black', alpha = 0.6) + 
  labs(title = 'Frec. de revisión de condiciones ambientales') + 
  geom_text(aes(label = n), hjust = -0.6, size = 4) +
  scale_x_continuous(expand = c(0.05, 0.05, 0.15, 0.05)) +
  coord_cartesian(xlim = c(0, NA)) + 
  xlab("N.° de FRE, frecuencia") +
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

#+ ggFrecCondiciones, fig.width = 6, fig.height = 4, out.width = "60%"
ggFrecCondiciones
guardarGGplot(ggFrecCondiciones, '089_FrecuenciaRevisionCondiciones', 6, 4)

#'-------------------------------------------------------------------------------
# 4.33. ¿Qué equipo o tecnología se utiliza para el control y ------------------
# seguimiento de condiciones ambientales? 
#'-------------------------------------------------------------------------------

col1 <- "4.33. ¿Qué equipo o tecnología se utiliza para el control y seguimiento de condiciones ambientales?"

ggMetodosControlAmb <- pull(df, col1) %>% 
  separarDummies(., descartar = T) %>% 
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
  geom_bar(stat = 'identity', fill = '#3366CC', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = label1), hjust = -0.3, size = 3) + 
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(labels = scales::percent_format()) +
  xlab('Proporción (%)') + 
  labs(title = 'Tecnologías de control y seguimiento de condiciones ambientales') + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

#+ ggMetodosControlAmb, fig.width = 6, fig.height = 4, out.width = "60%"
ggMetodosControlAmb
guardarGGplot(ggMetodosControlAmb, '090_MetodoSeguimControlAmb', 6, 4)

#'-------------------------------------------------------------------------------
# 4.34. ¿Cada cuánto se hace la calibración y mantenimiento de los equipos? ------------------
#'-------------------------------------------------------------------------------
col1 <- "4.34. ¿Cada cuánto se hace la calibración y mantenimiento de los equipos?"

calibMant <- c("Entre 1 a 3 meses",
  "Entre 3 a 6 meses",
  "Entre 6 a 9 meses",
  "Entre 9 a 12 meses",
  "En periodos mayores a 12 meses",
  "No se realiza")

ggCalibMant <- select(df, col1 = col1) %>%
  mutate(col1 = factor(col1, calibMant)) %>% 
  ggplot(aes(y = col1)) + 
  geom_bar(stat = 'count', fill = '#3366CC', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = ..count..), stat='count', hjust = -0.8, size = 4) + 
  xlab('N.° de FRE, frecuencia') + 
  scale_y_discrete(drop = FALSE) +
  scale_x_continuous(expand = c(0.05, 0.05, 0.15, 0.05)) + 
  coord_cartesian(xlim = c(0, NA)) +
  labs(title = "Frecuencia de calibración/mantenimiento equipos") + 
  theme(axis.title.y = element_blank())

#+ ggCalibMant, fig.width = 6, fig.height = 4, out.width = "60%"
ggCalibMant
guardarGGplot(ggCalibMant, '091_FrecSeguimientoCalibracion', 6, 4)

#'-------------------------------------------------------------------------------
# 4.36. ¿Con que otros medicamentos o productos se comparten los ----------------
# MME en el almacén?
#'-------------------------------------------------------------------------------

col1 <- "4.36. ¿Con que otros medicamentos o productos se comparten los MME en el almacén?"

ggOtrosProductos <- pull(df, col1) %>% 
  separarDummies(descartar = T) %>% 
  select(-2) %>% 
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
  geom_bar(stat = 'identity', fill = '#3366CC', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = label1), hjust = -0.3, size = 3) + 
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(labels = scales::percent_format()) +
  xlab('Proporción (%)') + 
  labs(title = 'Tecnologías de control y seguimiento de condiciones ambientales') + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

#+ ggOtrosProductos, fig.width = 8, fig.height = 6, out.width = "60%"
ggOtrosProductos
guardarGGplot(ggOtrosProductos, '092_ProductosCompartidos', 8, 6)

#'-------------------------------------------------------------------------------
# 4.38. ¿Qué tan conforme se encuentra el FRE el transporte de ------------------
# los MME desde el FNE?
#'-------------------------------------------------------------------------------

col1 <- "4.38. ¿Qué tan conforme se encuentra el FRE el transporte de los MME desde el FNE?"
col2 <- "4.39. Justifique la respuesta a la pregunta 4.38."

escalaLikert <- c("Muy inconforme", "Algo inconforme",
                  "Ni conforme ni inconforme",
                  "Algo conforme", "Muy conforme")

ggTransporte <- select(df, likert = col1) %>% 
  mutate(likert = factor(likert, levels = escalaLikert)) %>% 
  ggplot(aes(y = likert)) + 
  geom_bar(stat = 'count', fill = '#3366CC', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = ..count..), stat = 'count', hjust = -0.5) +
  xlab('N.° de FRE, frecuencia') + 
  coord_cartesian(xlim = c(0, NA)) + 
  scale_x_continuous(expand = c(0.05, 0.05, 0.15, 0.05)) +
  labs(title = 'Conformidad con el servicio de transporte desde el FNE') + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

#+ ggTransporte, fig.width = 6, fig.height = 4, out.width = "60%"
ggTransporte
guardarGGplot(ggTransporte, '093_TransporteProductos', 6, 4)


df %>%
  select(Deptmn = Departamento_1,
         Escala = col1,
         Justif = col2) %>% 
  mutate(
    Escala = factor(Escala, levels = escalaLikert),
    Deptmn = str_to_title(Deptmn)) %>% 
  arrange(Escala) %>% 
  gt::gt(groupname_col = 'Escala') %>% 
  gt::tab_options(table.font.size = 9)

#'-------------------------------------------------------------------------------
# 4.41. ¿Cuál es la frecuencia definida para el control de ------------------
# existencias de los medicamentos? 
#'-------------------------------------------------------------------------------
col1 <- "4.41. ¿Cuál es la frecuencia definida para el control de existencias de los medicamentos?"
col2 <- "4.42. Describa el proceso completo que hace el FRE para el control de existencias y fechas de vencimiento."

frecRevisionMed <- c("Varias veces al día",
  "Diaria",
  "Cada dos días",
  "Cada semana",
  "Cada quince días",
  "Cada mes")

ggFrecControlExistencias <-
  drop_na(df, any_of(col1)) %>% 
  select(frec = col1) %>% 
  mutate(frec = factor(frec, rev(frecRevisionMed))) %>% 
  ggplot(aes(y = frec)) + 
  geom_bar(stat = 'count', fill = '#3366CC', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = ..count..), stat = 'count', hjust = -0.5) +
  xlab('N.° de FRE, frecuencia') + 
  scale_y_discrete(drop = FALSE) +
  coord_cartesian(xlim = c(0, NA)) + 
  scale_x_continuous(expand = c(0.05, 0.05, 0.10, 0.05)) + 
  labs(title = 'Frec. de control de existencia de medicamentos MME') + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

#+ ggFrecControlExistencias, fig.width = 6, fig.height = 4, out.width = "60%"
ggFrecControlExistencias
guardarGGplot(ggFrecControlExistencias, '094_FrecControlExistencias', 6, 4)




gmFrecControlExistencias <- df_total %>% 
  rename(frec = col1) %>%
  left_join(colombiaGeoDF, by = c('CodigoDepartamento' = 'DPTO')) %>% 
  creacionCloroPletCol(geometry, frec)

gmFrecControlExistencias[[1]] <- gmFrecControlExistencias[[1]] + 
  scale_fill_brewer(palette = 'Set1', name = NULL) +
  theme(axis.title.y = element_blank(), 
        legend.position = 'right',
        axis.text = element_blank()) + 
  labs(title = 'Frecuencia de control de existencias')

gmFrecControlExistencias[[2]] <- gmFrecControlExistencias[[2]] + 
  scale_fill_brewer(palette = 'Set1', name = NULL) 
gmFrecControlExistencias[[3]] <- gmFrecControlExistencias[[3]] + 
  scale_fill_brewer(palette = 'Set1', name = NULL) 

#+ gmFrecControlExistencias, fig.width = 6, fig.height = 4, out.width = "60%"
gmFrecControlExistencias
guardarGGplot(gmFrecControlExistencias, '094_FrecControlExistenciasMapa', 6, 4)


df %>%
  select(Deptmn = Departamento_1,
         Frecuencia_Definida = col1,
         Proceso = col2) %>% 
  mutate(
    Deptmn = str_to_title(Deptmn)) %>% 
  arrange(Frecuencia_Definida) %>% 
  gt::gt(groupname_col = 'Frecuencia_Definida') %>% 
  gt::tab_options(table.font.size = 9)

#'-------------------------------------------------------------------------------
# 4.43. ¿Se utilizan técnicas de semaforización en el inventario? ---------------
#'-------------------------------------------------------------------------------
col1 <- "4.43. ¿Se utilizan técnicas de semaforización en el inventario?"

ggTecnicasSemaforizacion <- select(df, frec = col1) %>% 
  table() %>% as_tibble() %>% 
  rename(label = '.') %>%
  mutate(
    prop = n/sum(n),
    label1 = paste0(label, "\n", round(prop,3)*100, '%')) %>% 
  pieChart(prop, label1) +
  theme(legend.position = 'none') + 
  scale_fill_brewer(palette = 'Set1') +
  labs(title = "Utilización de técnicas de semaforización de inventarios")

#+ ggTecnicasSemaforizacion, fig.width = 6, fig.height = 4, out.width = "60%"
ggTecnicasSemaforizacion
guardarGGplot(ggTecnicasSemaforizacion, '095_UsoTecnicasSemaforizacion', 6, 4)

#'-------------------------------------------------------------------------------
# 4.45. ¿El FRE tiene niveles de seguridad en el inventario definidos? ------------------
#'-------------------------------------------------------------------------------
col1 <- "4.45. ¿El FRE tiene niveles de seguridad en el inventario definidos?"

ggNivelSeguridad <- select(df, frec = col1) %>% 
  table() %>% as_tibble() %>% 
  rename(label = '.') %>% 
  mutate(
    prop = n/sum(n),
    label1 = paste0(label, "\n", round(prop,3)*100, '%')) %>%
  pieChart(n, label1) +
  theme(legend.position = 'none') + 
  scale_fill_brewer(palette = 'Set1') +
  labs(title = "Utilización de nivel de seguridad en inventario")

#+ ggNivelSeguridad, fig.width = 6, fig.height = 4, out.width = "60%"
ggNivelSeguridad
guardarGGplot(ggNivelSeguridad, '096_UsoNivelSeguridad', 6, 4)

#'-------------------------------------------------------------------------------
# 4.49. ¿Se han presentado casos de vencimiento de medicamentos en --------------
# el almacén del FRE en los últimos 4 años? 
#'-------------------------------------------------------------------------------
col1 <- "4.49. ¿Se han presentado casos de vencimiento de medicamentos en el almacén del FRE en los últimos 4 años?"

ggVencimientoMed <- select(df, frec = col1) %>% 
  table() %>% as_tibble() %>% 
  rename(label = '.') %>% 
  mutate(
    prop = n/sum(n),
    label1 = paste0(label, "\n", round(prop,3)*100, '%')) %>%
  pieChart(n, label1) + 
  theme(legend.position = 'none') + 
  scale_fill_brewer(palette = 'Set1') +
  labs(title = "Presentación de casos de vencimiento de medicamentos en 4 años")

#+ ggVencimientoMed, fig.width = 6, fig.height = 4, out.width = "60%"
ggVencimientoMed
guardarGGplot(ggVencimientoMed, '097_CasosVencimiento', 6, 4)


col2 <- "Si la respuesta anterior fue Si, indique cuales...137"

ggMedicVencidos <- pull(df, col2) %>%  
  na.omit() %>% 
  {separarDummies(., descartar = T)} %>%
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
  geom_bar(stat = 'identity', fill = '#3366CC', color = 'black', alpha = 0.6) + 
  geom_text(aes(label = label1), hjust = -0.3, size = 3) + 
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(labels = scales::percent_format()) +
  xlab('Proporción (%)') + 
  labs(title = 'Medicamentos que se han vencido en los últimos años') + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank())

#+ ggMedicVencidos, fig.width = 6, fig.height = 4, out.width = "60%"
ggMedicVencidos
guardarGGplot(ggMedicVencidos, '098_CasosVencimiento', 6, 4)

#'-------------------------------------------------------------------------------
# 4.53. Brinde una estimación del número de entidades que han ------------------
# realizado compras al FRE en el último año.
#'-------------------------------------------------------------------------------

col1 <- "4.53. Brinde una estimación del número de entidades que han realizado compras al FRE en el último año."

ggEntidadesCompradoras <- df %>% select(Departamento_1, col1 = col1) %>% 
  filter(str_detect(col1, '\\d')) %>% 
  mutate(Depto1 = str_to_title(Departamento_1),
         col1 = as.numeric(col1),
         Depto1 = fct_reorder(Depto1, col1)) %>% 
  ggplot(aes(y = Depto1, yend = Depto1)) + 
  geom_segment(aes(x = 0, xend = col1), col = "#1a41bd") +
  geom_point(aes(x = col1), col = '#3d1ab0') + 
  geom_text(aes(x = col1, label = col1), size = 3, hjust = -0.6) + 
  scale_x_continuous(expand = c(0.05, 0.05, 0.20, 0.05)) + 
  theme(axis.title.y = element_blank(), panel.grid = element_blank()) + 
  xlab('N.° de inscritos en el departamento')


#+ ggEntidadesCompradoras, fig.width = 6, fig.height = 4, out.width = "60%"
ggEntidadesCompradoras
guardarGGplot(ggEntidadesCompradoras, '099_NEntidadesCompradoras', 6, 4)

df_total1['NoPersonas1'] <-
  df_total1$No.PersonasVinculadasDirectamente + df_total1$No.PersonasVinculadasAfiliacion

# Sumar todas los tipos de camas desde el REPS

df_total1['camasREPS'] <- df_total1['camas_adultos'] +
  df_total1['camas_intensivo_adulto'] + df_total1['camas_intermedio_adulto'] + 
  df_total1['camas_intermedio_mental'] + df_total1['camas_agudo_mental'] + 
  df_total1['camas_farmacodependencia'] + 
  df_total1['camas_salud_mental'] + 
  df_total1['ambulancias_medicada']

deptosMostrar <- c("Córdoba", "Antioquia", "Bolívar", 
                   "Norte De Santander", "Valle Del Cauca", "Cesar", 
                   'Boyacá', 'Meta', 'Atlántico')

df_total_2 <- df_total1 %>%
  filter(str_detect(col1, '\\d')) %>%
  select(Departamento_1, col1 = col1, 
         NoPersonas = NoPersonas1, 
         PoblTotal = poblac_total,
         camasREPS) %>%
  ungroup() %>% 
  mutate(Departamento_1 = str_to_title(Departamento_1),
         col1 = as.numeric(col1))

ggRecursosFRE <- df_total_2 %>%
  ggplot(aes(x = col1, y = NoPersonas)) +
  stat_smooth(method = 'lm', lty = 'dashed',
              fill = 'blue1', alpha = 0.1) +
  geom_point() + 
  geom_text_repel(aes(label = Departamento_1), size = 3) + 
  xlab('N.° de instituciones inscritas en el departamento \n con compras el último año') + 
  ylab('N.° de personas que prestan servicios \n al FRE (directamente o por afiliación)') + 
  theme(panel.grid = element_blank())

#+ ggRecursosFRE, fig.width = 6, fig.height = 4, out.width = "60%"
ggRecursosFRE
guardarGGplot(ggRecursosFRE, '100_RelacionRecursosFRE', 6, 4)


lm1 <- lm(col1 ~ PoblTotal, df_total_2)

ggRecursosFRE2a <- df_total_2 %>%
  ggplot(aes(x = PoblTotal, y = col1)) +
  stat_smooth(method = 'lm', lty = 'dashed',
              fill = 'blue1', alpha = 0.1) +
  geom_point() + 
  geom_text_repel(
    data = df_total_2[df_total_2$Departamento_1 %in% deptosMostrar,],
    mapping = aes(label = Departamento_1), size = 3) +
  ylab('N.° de instituciones inscritas en el departamento \n con compras el último año') +
  xlab('Población departamental') +
  theme(panel.grid = element_blank())

#+ ggRecursosFRE2a, fig.width = 6, fig.height = 4, out.width = "60%"
ggRecursosFRE2a
guardarGGplot(ggRecursosFRE2a, '100a_RelacionRecursosFRE', 6, 4)


lm2 <- lm(col1 ~ camasREPS, df_total_2)

ggRecursosFRE2b <- df_total_2 %>%
  ggplot(aes(x = camasREPS, y = col1)) +
  stat_smooth(method = 'lm', lty = 'dashed',
              fill = 'blue1', alpha = 0.1) +
  geom_point() + 
  geom_text_repel(
    data = df_total_2[df_total_2$Departamento_1 %in% deptosMostrar,], 
    mapping = aes(label = Departamento_1), size = 3) +
  xlab('N.° de camas en REPS (tipos seleccionados)') +
  ylab('N.° de instituciones inscritas en \n el departamento con compras el último año') +
  theme(panel.grid = element_blank())


#+ ggRecursosFRE2b, fig.width = 6, fig.height = 4, out.width = "60%"
ggRecursosFRE2b
guardarGGplot(ggRecursosFRE2b, '100b_RelacionRecursosFRE', 6, 4)











