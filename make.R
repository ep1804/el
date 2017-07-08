if(F){
  install.packages('roxygen2')
  install.packages('testthat')
  install.packages('devtools')
}

requireNamespace('devtools')

if(F){
  devtools::use_data(bearing, overwrite=T)
}

devtools::build()
devtools::check(args = c('--no-examples'))
devtools::document(roclets=c('rd', 'collate', 'namespace', 'vignette'))
devtools::install()
