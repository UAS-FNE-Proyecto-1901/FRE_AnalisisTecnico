#' Almacenamiento de PDF y objeto R
#'
#' @param objeto objeto gráfico de clase GGPLOT
#' @param nombre nombre en la carpeta
#' @param amplitud amplitud de imágen
#' @param altura altura de imágen
#' @param ruta ruta de almacenamiento gráfico
#' @param incluirPNG bool (default = FALSE), 
#'
#' @return 
#' Nada, sólo almacena en disco
#' @export
#'
#' @examples
#' g1 <- mtcars %>% {qplot(mpg, wt, data = .)}
#' guardarGGplot(g1, 'ejemplo')
#' 
guardarGGplot <- function(objeto, nombre, amplitud = 8, altura = 6, 
                          ruta = 'figures', incluirPNG = FALSE) {
  
  saveRDS(objeto, file.path(ruta, paste0(nombre, '.rds')))
  # 
  if ('ggplot' %in% class(objeto) | 'patchwork' %in% class(objeto)) {
    ggsave(filename = paste0(nombre, '.pdf'), plot = objeto, 
           device = 'pdf', path = ruta, 
           1, width = amplitud, height = altura)
    #
    if (incluirPNG) {
      ggsave(paste0(nombre, '.png'), objeto, 'png', ruta, 
             1, width = amplitud, height = altura)
    } 
  } 
}

#'-------------------------------------------------------------------------------
#' Almacenamiento de Plotly y objeto R
#'
#' @param objeto objeto gráfico de clase GGPLOT
#' @param nombre nombre en la carpeta
#' @param amplitud amplitud de imágen
#' @param altura altura de imágen
#' @param ruta ruta de almacenamiento gráfico
#' @param incluirPNG bool (default = FALSE), 
#'
#' @return 
#' Nada, sólo almacena en disco
#' @export
#'
#' @examples
#' g1 <- mtcars %>% {qplot(mpg, wt, data = .)}
#' guardarGGplot(g1, 'ejemplo')
#' 
guardarPlotly <- function(objeto, nombre, amplitud = 8, altura = 6, 
                          selfcontained = TRUE, libdir = NULL, 
                          title = nombre, knitrOptions = list(), 
                          ruta = 'figures') {
  
  saveRDS(objeto, file.path(ruta, paste0(nombre, '.rds')))
  # 
  if ('plotly' %in% class(objeto) | 'htmlwidget' %in% class(objeto)) {
    htmlwidgets::saveWidget(
      widget = objeto, 
      file = normalizePath(file.path(ruta, paste0(nombre, '.html'))), 
      selfcontained = selfcontained, 
      libdir = libdir, 
      title = title, 
      knitrOptions = knitrOptions
    )
  } 
}
