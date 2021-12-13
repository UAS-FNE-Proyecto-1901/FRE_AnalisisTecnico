ECHO OFF

RScript -e "bookdown::render_book(input = c('index.Rmd'), bookdown::pdf_book(keep_tex = TRUE, includes = rmarkdown::includes(in_header = c('estructura.tex'), before_body = c('introduccion.tex') ), toc_depth=3, template='default_template.tex', lof=TRUE, lot=TRUE, pandoc_args = c('-V', 'classoption=oneside') ), config_file = '_pdfbook.yml', params=c(rmd_files=c('index.Rmd'))) "

PAUSE