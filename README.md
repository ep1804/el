## my routin`E L`ibrary

This library is written for

 - Making my routine data science tasks easier.
 - Making a consistent interface to some R packages that have heterogeneous coding styles.

## Install

```r
install.packages('devtools') # Optional if you don't have this
library(devtools)
install_github('ep1804/el')
```

## Run

```r
library(el)
```

## Dependencies

You may need following packages:

```r
install.packages('caret')
install.packages('caretEnsemble')
install.packages('corrplot')
install.packages('diptest')
install.packages('e1071')
install.packages('fpc')
install.packages('futile.logger')
install.packages('GeneCycle')
install.packages('glmnet')
install.packages('MASS')
install.packages('matrixcalc')
install.packages('randomForest')
install.packages('ranger')
install.packages('rgl')
install.packages('rpart')
install.packages('forecast')
```

## Development

For manualy building and installing

```r
source('make.R')
```

## Note

### Coding Style

 - I try to follow Google's style because I don't want to routinely type underscore(_).
