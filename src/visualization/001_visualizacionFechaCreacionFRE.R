################################################################################-
#' --- 
#' title: 'Serie de tiempo - Fecha de creación de FRE' 
#' author: 'Daniel S. Parra G.' 
#' date: '01-01-2021' 
#' --- 
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


df <- read_csv(file.path('data', 'processed', '001_Herramienta_Procesada.csv'))


year_df <- tibble(start = seq(1960, 2015, 5),
                  end = seq(1965, 2020, 5)) %>%
  mutate(start = as.character(start))

year_df <- year_df %>% 
  add_column(color = rep(c('#258a40', '#59ff86'), dim(year_df)[1] / 2),
             event = paste0('ID', formatC(1:dim(year_df)[1], width = 2, flag = '0')),
             group = 'Serie Tiempo') %>% 
  mutate(across(c(start, end), list(date = function(x) ymd(paste(x, '01', '01', sep = '-')))))

vistime(year_df, 
        col.event = 'start', 
        col.start = 'start_date', 
        col.end = 'end_date')

paleta <- RColorBrewer::brewer.pal(12, 'Set3')


p <- df %>%
  select(Departamento_1, `Fecha creación FRE`) %>%
  drop_na() %>%
  mutate(
    start_date = as.Date(`Fecha creación FRE`),
    end_date   = as.Date(`Fecha creación FRE`),
    start = Departamento_1,
    group = 'Fecha de Creación', 
    color = map_chr(Departamento_1, ~paleta[runif(1, 1, 12)])
  ) %>% 
  bind_rows(year_df) %>% 
  vistime(col.event = 'start', 
          col.start = 'start_date', 
          col.end = 'end_date')


pp <- plotly::plotly_build(p)

marker_idx <- which(purrr::map_chr(pp$x$data, "mode") == "markers")
for(i in marker_idx) pp$x$data[[i]]$marker$size <- 12
text_idx <- which(purrr::map_chr(pp$x$data, "mode") == "text")

for(i in text_idx){
  pp$x$data[[i]]$textfont$size <- 10
  pp$x$data[[i]]$text <- str_to_sentence(pp$x$data[[i]]$text)
  }

# pp$x$config$displaylogo

text_idx_1 <-
  which((map(pp$x$data, "text") %>% str_detect('Tolima')) &
          (map_chr(pp$x$data, "mode") == "text"))

for(i in text_idx_1) pp$x$data[[i]]$y <- pp$x$data[[i]]$y %>% {. + rnorm(length(.), 0, 0.2)}

pp$x$config$displaylogo <- FALSE

pp

