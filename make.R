requireNamespace('devtools')

#devtools::use_data(bearing, overwrite=T)
devtools::build()
devtools::check()
devtools::document()
devtools::install()