require(sf)
require(ggplot2)
require(patchwork)

colombiaGeoDF <- read_sf(file.path('data', 'external', 'colombia_geo.json'))

creacionCloroPletCol <- function(data, geom_var, fill_var) {
  
  mapf_func <- function(gg, dir1, dir2) {
    gg + xlim(dir1[1], dir1[2]) + ylim(dir2[1], dir2[2]) +
      theme_void() + 
      theme(legend.position = 'none', 
            panel.background = element_rect(fill='white'),
            plot.margin = unit(c(0,0,0,0), "cm"))
  }
  
  g1 <- ggplot(data = data) + 
    geom_sf(mapping = aes(geometry = {{geom_var}}, fill = {{fill_var}}))
  
  gsub1 <- mapf_func(g1, c(-81.75, -81.68), c(12.47, 12.62))
  gsub2 <- mapf_func(g1, c(-81.40, -81.34), c(13.31, 13.40))
  
  gt <- g1 + 
    inset_element(gsub1, 0.00, 0.8, 0.20, 0.97, clip = T, ignore_tag = T) + 
    inset_element(gsub2, 0.20, 0.9, 0.30, 0.97, clip = T, ignore_tag = T)
  
  return((gt))
}
