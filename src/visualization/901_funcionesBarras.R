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


#'-------------------------------------------------------------------------------
#' Gráfico de torta
#'
#' @param data_frame 
#' @param nvar 
#' @param textvar 
#'
#' @return
#' @export
#'
#' @examples
#' 
pieChart <- function(data_frame, nvar, textvar, repel = FALSE){
  gPie <- data_frame %>% 
    arrange(desc({{textvar}})) %>% 
    mutate(prop = {{nvar}} / sum({{nvar}}),
           ncumsum = cumsum(prop) - 0.5 * prop) %>%
    ggplot(aes(x = "", y = prop,  fill = {{textvar}})) +
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) + 
    theme_void() +
    theme(legend.position="bottom")
  
  if (repel) {
    gPie1 <- gPie +
      ggrepel::geom_text_repel(aes(y = ncumsum, label = {{textvar}}), color = "white", size=4)
  } else {
    gPie1 <- gPie +
      geom_text(aes(y = ncumsum, label = {{textvar}}), color = "white", size=4)
  }
  return(gPie1)
}