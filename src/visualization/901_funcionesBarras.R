#' Gráfico ordenado
#'
#' @param data 
#' @param fct1 factor a graficar
#' @param fct1_rev factor de etiquetas y órden
#' @param title 
#' @param xlab 
#' @param ylab 
#' @param col 
#'
#' @return
#' @export
#'
#' @examples
#' 
barrasGraficoRev <- function(data, fct1, fct1_rev, title = NULL, xlab=NULL, ylab=NULL, 
                          col = '#6699ff') {
  data %>% 
    ggplot(aes(y = fct_reorder({{fct1}}, {{fct1_rev}}), x = {{fct1_rev}})) + 
    geom_bar(stat = 'identity', fill = '#6699ff', color = 'black', alpha = 0.6) + 
    geom_text(aes(label = {{fct1_rev}}, x = {{fct1_rev}} + 1)) + 
    xlab(xlab) + ylab(ylab) + 
    labs(title = title) + 
    theme(axis.title.y = element_blank())  
}
