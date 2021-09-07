ECHO OFF

RScript -e "bookdown::render_book(c('index.Rmd', 'L001-introduccion.Rmd', 'L002-objetivos.Rmd', 'L003-justificacion.Rmd', 'L101-EstructuraFRE.Rmd', 'L102-RecetariosOficiales.Rmd', 'L103-RutaTecnologica.Rmd', 'L104-GestionInformes.Rmd', 'L301-Regional.Rmd', 'L500-Glosario.Rmd', 'L501-Creditos.Rmd'), bookdown::word_document2() )"

PAUSE