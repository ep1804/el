if(F){
  install.packages('roxygen2')
  install.packages('testthat')
  install.packages('devtools')

  devtools::use_data(bearing, overwrite=T)

  devtools::check(args = c('--no-examples'))
}

requireNamespace('devtools')

devtools::build()

devtools::document(roclets=c('rd', 'collate', 'namespace', 'vignette'))

devtools::install()
