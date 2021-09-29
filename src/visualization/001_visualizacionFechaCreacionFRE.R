#' --- 
#' title: 'Serie de tiempo - Fecha de creación de FRE' 
#' author: 'Daniel S. Parra G.' 
#' date: '30-06-2021' 
#' --- 
################################################################################-
## Propósito del Script: Fecha de creación de FRE
## 
## 
## Copyright (c) Fondo Nacional de Estupefacientes, 2021 
## 
## Email: dsparra@minsalud.gov.co 
################################################################################-

require(vistime)
require(plotly)
require(tidyverse)
require(lubridate)
require(ggrepel)

# 
source(file.path('src', 'models', '900_funcionesAlmacenamientoGrafico.R'), 
       encoding = 'UTF-8')

#'-------------------------------------------------------------------------------
# 1. Introducción ------------------
#'-------------------------------------------------------------------------------

#' Introducción
df <- read_csv(file.path('data', 'processed', '001_Herramienta_Procesada.csv')) %>% 
  mutate(Departamento_1 = 
           ifelse(str_detect(Departamento_1, 'ANDRÉS'), 'SAN ANDRÉS', Departamento_1))

#' Definición de escala de tiempo
year_df <- tibble(start = seq(1960, 2015, 5),
                  end = seq(1965, 2020, 5)) %>%
  mutate(start = as.character(start))

#' Definición de escala de tiempo

year_df <- year_df %>% 
  add_column(color = rep(c('#258a40', '#59ff86'), dim(year_df)[1] / 2),
             event = paste0('ID', formatC(1:dim(year_df)[1], width = 2, flag = '0')),
             group = 'Serie Tiempo') %>% 
  mutate(across(c(start, end), list(date = function(x) ymd(paste(x, '01', '01', sep = '-')))))

#'-------------------------------------------------------------------------------
# 2. Visualización GGplotly ------------------
#'-------------------------------------------------------------------------------

#' Definición de escala de tiempo

paleta <- RColorBrewer::brewer.pal(12, 'Set3')

dfPersonalizado1 <- df %>%
  select(Departamento_1, `Fecha creación FRE`) %>%
  drop_na() %>%
  mutate(
    start_date = as.Date(`Fecha creación FRE`),
    end_date   = as.Date(`Fecha creación FRE`),
    start = Departamento_1,
    group = 'Fecha de Creación',
    color = map_chr(Departamento_1, ~ paleta[runif(1, 1, 12)])
  ) %>%
  bind_rows(year_df) 

# Gráfico VISTime
p <- dfPersonalizado1 %>%
  vistime(col.event = 'start',
          col.start = 'start_date',
          col.end = 'end_date')

# Personalización gráfico plotly
pp <- plotly::plotly_build(p)

marker_idx <- which(purrr::map_chr(pp$x$data, "mode") == "markers")

for(i in marker_idx) pp$x$data[[i]]$marker$size <- 12
text_idx <- which(purrr::map_chr(pp$x$data, "mode") == "text")

for(i in text_idx) {
  pp$x$data[[i]]$textfont$size <- 10
  pp$x$data[[i]]$text <- str_to_sentence(pp$x$data[[i]]$text)
}


text_idx_1 <-
  which((map(pp$x$data, "text") %>% str_detect('Tolima')) &
          (map_chr(pp$x$data, "mode") == "text"))

for(i in text_idx_1) {
  pp$x$data[[i]]$y <-
    pp$x$data[[i]]$y %>% {. + rnorm(length(.), 0, 0.2)}
}

# Cambiar hover Text de los marcadores
pp$x$data[[marker_idx]]$text <- pp$x$data[[marker_idx]]$text %>% 
  str_to_title() %>% 
  str_replace("(?<=\\d{4}\\-\\d{2}\\-\\d{2})\\s.*(?=\\<)", '')

pp$x$config$displaylogo <- FALSE

pp

# Almacenamiento de gráfico

guardarPlotly(pp, '001_serieTiempoCreación', libdir = 'visJS')

#'-------------------------------------------------------------------------------
# 3. Visualización PDF ------------------
#'-------------------------------------------------------------------------------
dfPersonalizado2 <- dfPersonalizado1 %>%
  mutate(start = str_to_title(start)) %>% 
  add_row(group = c('NF')) %>% 
  add_row(group = c('NI'), .before = 1L)

gp <- dfPersonalizado2 %>%
  gg_vistime(
    col.event = 'start',
    col.start = 'start_date',
    col.end = 'end_date',
    optimize_y = TRUE,
    show_labels = T
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    text = element_text(size = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

gp
gp$layers[[3]]$aes_params$shape <- 21
gp$layers[[3]]$aes_params$size <- 3.5
gp$layers[[5]]$geom_params$arrow <- arrow(length = unit(0.01, 'npc'))
gp$layers[[5]]$aes_params$segment.alpha <- 0.5
gp$layers[[5]]$geom_params$segment.colour <- rgb(0.9, 1, 1)
gp$layers[[5]]$geom_params$max.overlaps <- Inf

gp

guardarGGplot(gp, '002_serieTiempoCreación', 10, 4)
