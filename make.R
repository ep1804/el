requireNamespace('devtools')

devtools::build()
devtools::check(document = F)
devtools::install()