if(F){
  install.packages('roxygen2', 'testthat', 'devtools')
}

requireNamespace('devtools')

if(F){
  devtools::use_data(bearing, overwrite=T)
}

devtools::build()
devtools::check(args='--no-examples')
devtools::document(roclets=c('rd', 'collate', 'namespace', 'vignette'))
devtools::install()
