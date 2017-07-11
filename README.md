## my routin`E L`ibrary

This library is written for

 - Making my routine data science tasks easier.
 - Making a consistent interface to some R packages that have heterogeneous coding styles.

## Install

```r
install.packages('https://github.com/ep1804/el/releases/download/v0.1.5/el_0.1.5.tar.gz', repos = NULL)
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
install.packages('deepnet')
install.packages('diptest')
install.packages('e1071')
install.packages('fpc')
install.packages('forecast')
install.packages('futile.logger')
install.packages('GeneCycle')
install.packages('ggplot2')
install.packages('glmnet')
install.packages('kernlab')
install.packages('lattice')
install.packages('MASS')
install.packages('matrixcalc')
install.packages('randomForest')
install.packages('ranger')
install.packages('reshape2')
install.packages('rgl')
install.packages('rpart')
install.packages('rpart.plot')
install.packages('xgboost')
```

## Development

For manualy building and installing

```r
source('make.R')
```

## Note

### Coding Style

 - I try to follow Google's style because I don't want to routinely type underscore(_).
