separarDummies <- function(vector) {
  # Separar los factores únicos
  dimFactor <- lapply(vector, function(x) str_split(x, '\\,')) %>% 
    unlist() %>% 
    sapply(., function(x){str_trim(x)}) %>% 
    unique()
  
  # Lista vacía
  ls_factorEscogencia <- list()
  
  # Llenar las listas vacías con verdadero o falso si tiene la palabra
  for (i in 1:length(dimFactor)) {
    ls_factorEscogencia[[i]] <- vector %>% 
      sapply(., function(x){str_detect(x, dimFactor[i])})
  }
  
  # Convertir lista a dataframe
  dataframeElementos <- as_tibble(do.call(cbind, ls_factorEscogencia))
  colnames(dataframeElementos) <- dimFactor
  
  
  return(dataframeElementos)
}


graficoVariables <- function(var, fill_color = 'green', alpha_fill = 0.5, contour_color = 'green4') {
  dimVar <- dim(var)
  
  lista <- apply(var, 2, function(x) {
    sum(x)
  })
  
  df <- tibble(nombre = names(lista),
               Frec = lista, 
               Frec_rel = lista)
  
  g <- df %>% 
    ggplot(aes(x = Frec, y = fct_reorder(nombre, Frec))) + 
    geom_bar(stat = 'identity', fill = alpha(fill_color, alpha_fill), color = contour_color) + 
    theme(axis.title.y = element_blank()) + 
    xlab('Frecuencia')
  
  return(list(df = df, g = g))
}


graficoBarrasAnidado <- function(df, xvar, xlab, yvar = 'perc', ylab = 'Porcentaje (%)', title = '', option = 'D') {
  xvar_quo <- rlang::ensym(xvar)
  yvar_quo <- rlang::ensym(yvar)
  
  df1 <- df %>% filter(!!yvar_quo > 0.1)
  
  ggplot(df, aes(x = !!xvar_quo, 
                 color = name, 
                 y = !!yvar_quo * 100, 
                 fill = name)) +
    geom_bar(stat = 'identity') + 
    geom_text(aes(label = scales::percent(!!yvar_quo, 1), 
                  y = !!yvar_quo * 100), color = 'white', 
              fill = 'white', position = position_stack(vjust = 0.5), size = 3) + 
    scale_fill_viridis_d(option = option) + 
    scale_color_viridis_d(option = option) +
    xlab(xlab) + ylab(ylab) + 
    labs(title = title) + 
    theme(legend.title = element_blank())
}
