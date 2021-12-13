ECHO ON

RScript -e "bookdown::render_book(c('index.Rmd'), bookdown::word_document2(), config_file='_wordbook.yml')"

PAUSE