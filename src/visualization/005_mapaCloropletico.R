#' Mapa simple cloroplético
#'
#' @param df_sf (data.frame) 
#' @param variable nombre de la variable con regeno
#' @param geometry 
#' @param type 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
mapaSimpleColombia <- function(df_sf, variable, geometry = 'geometry', type = 'viridis') {
  geometry_q <- rlang::ensym(geometry)
  variable_q <- rlang::ensym(variable)
  
  df_sf %>%
    ggplot() +
    geom_sf(
      aes(fill = !!variable, geometry = !!geometry_q),
      colour = 'black',
      size = 0.1
    ) +
    scale_fill_continuous(labels = scales::dollar, type = type) + 
    coord_sf(crs = st_crs(32618)) 
}