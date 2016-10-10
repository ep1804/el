if(F){
  install.packages('roxygen2', 'testthat', 'devtools')
}

requireNamespace('devtools')

if(F){
  devtools::use_data(bearing, overwrite=T)
}

devtools::build()
devtools::check(args='--no-examples')
devtools::document()
devtools::install()