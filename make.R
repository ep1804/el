requireNamespace('devtools')

#devtools::use_data(bearing, overwrite=T)
devtools::build()
devtools::check(args='--no-examples')
devtools::document()
devtools::install()