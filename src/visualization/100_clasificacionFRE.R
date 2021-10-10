

require(tidyverse)

require(readxl)
require(skimr)
require(PerformanceAnalytics)
require(factoextra)


data <-
  read_excel(file.path(
    'data',
    'raw',
    'ClasificacionFRE',
    'variablesClasificacionFRE.xlsx'
  ), na = '-')


data %>%
  select(!contains('Departamento')) %>% 
  chart.Correlation(., histogram = TRUE, pch = 19)


skimr::skim(data)



pca1 <- data %>%
  drop_na() %>% 
  column_to_rownames('Departamento...2') %>% 
  select(!contains('Departamento')) %>% 
  prcomp(., scale = TRUE, center = TRUE)



pca1 %>% plot()


fviz_pca_ind(pca1,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


fviz_pca_var(pca1,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


clean_data <- data %>%
  drop_na() %>% 
  column_to_rownames('Departamento...2') %>% 
  select(!contains('Departamento'))

# kmeans1 <- 
funClusters <- function(data, k) {
  data %>%
    kmeans(k)
}
  

k.values <- data.frame(k = 1:30)

k.values['TWITH'] <- map_dbl(k.values$k, ~funClusters(clean_data, .x)$tot.withinss)

k.values %>% 
  ggplot(aes(x = k, y = TWITH)) + 
  geom_point() + 
  geom_line()


funClusters_2 <- function(data, k) {
  t1 <- data %>% 
    dist(method = 'euclidean') %>% 
    hclust(method = 'complete')
  
  t2 <- cutree(t1, k)
  return(list(clust = t1, tree = t2))
}



plot(funClusters_2(clean_data, 3)$clust, cex = 0.6, hang = -1)

clean_data %>% 
  {fviz_cluster(list(data = ., cluster = funClusters_2(., 5)$tree))}

# stats::cutree(:, k = 4) %>% 
  # as.data.frame() %>% 
  # dplyr::rename(.,cluster_group = .) %>%
  # tibble::rownames_to_column("type_col")
  # 

