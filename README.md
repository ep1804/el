## my routinE Library

This library is written...

 - To make my routine data science tasks easy.
 - To make a consistent interface to some R packages that have heterogeneous coding styles.

## Install

Download release version (zip) and install it to your R.

```
install.packages('corrplot')
install.packages('diptest')
install.packages('e1071')
install.packages('futile.logger')
install.packages('MASS')
install.packages('matrixcalc')
install.packages('GeneCycle')
install.packages('randomForest')
install.packages('rgl')
install.packages('rpart')
install.packages(path_to_zip_file, repos = NULL, dependencies=c("Depends", "Imports"))
```

## Import

```
library(el)
```

## Build, test

```
source('make.R')
```

## Note

### Coding Style

 - I try to follow Google's style because I don't want to routinely type underscore(_).
