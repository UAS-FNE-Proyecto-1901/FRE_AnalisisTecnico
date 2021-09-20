ECHO OFF

RScript -e "bookdown::render_book(c('index.Rmd', 'L001_Introduccion.Rmd', 'L002_Objetivos.Rmd', 'L003_Justificacion.Rmd', 'L101_EstructuraFRE.Rmd', 'L102_RecetariosOficiales.Rmd', 'L103_Medicamentos.Rmd', 'L104_GestionInformes.Rmd', 'L301_Regional.Rmd', 'L500_Glosario.Rmd', 'L501_Creditos.Rmd'), bookdown::pdf_book(keep_tex = TRUE, includes = rmarkdown::includes(in_header = c('estructura.tex') ) ) )"

PAUSE