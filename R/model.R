requireNamespace('caret')
requireNamespace('caretEnsemble')
requireNamespace('deepnet')
requireNamespace('ggplot2')
requireNamespace('glmnet')
requireNamespace('kernlab')
requireNamespace('lattice')
requireNamespace('ranger')
requireNamespace('reshape2')
requireNamespace('rpart')
requireNamespace('rpart.plot')
requireNamespace('stats')
requireNamespace('xgboost')

#' Fit a classification or regression model with CV-based parameter tuning, and show
#' in-sample performance
#'
#' @param x        numeric or factor data.frame.
#' @param y        numeric vector or factor. If factor, classification model is built.
#'                 If numeric vector, regression model is built.
#' @param cvFolds  Cross-validation fold number
#' @param size.lim numeric. If data size is larger than this, it is sampled.
#' @param plot     logical. Plot or not
#'
#' @return list of final models
#' @export
#'
#' @examples
#' x <- iris[1:4]
#' y <- iris$Species
#' fits <- el.model(x, y)
#' el.model.compare(fits)
#' el.model.show(fits$RF)
#' el.model.varImp(fits$RF)
#'
el.model <- function(x, y, cvFolds = 7, size.lim = 10000, plot = TRUE) {

  if (is.vector(y) & is.numeric(y)) {
    isClassification <- FALSE
  } else if(is.factor(y)) {
    isClassification <- TRUE
    levels(y) <- sapply(levels(y), function(s){ gsub(" ", "_", s) })
  } else {
    logger.error("y should be a numeric vector or a factor.")
    return()
  }

  if (!is.data.frame(x)) {
    logger.error("x should be a data.frame.")
    return()
  }

  x <- as.data.frame(x) # for the some data.frame subclasses that violates LSP
  colnames(x) <- sapply(colnames(x), function(s){ gsub(" ", "_", s) })

  if (nrow(x) > size.lim) {
    wh <- sort(sample(nrow(x), size.lim))
    y <- y[wh]
    x <- x[wh,]
  }

  folds <- caret::createFolds(y, k = cvFolds)

  if(isClassification){
    trControl <- caret::trainControl(
      method = 'cv',
      number = length(folds),
      classProbs = TRUE,
      verboseIter = TRUE,
      savePredictions = TRUE,
      index = folds
    )

    metric = 'Kappa'
  } else {
    trControl <- caret::trainControl(
      method = 'cv',
      number = length(folds),
      verboseIter = TRUE,
      savePredictions = TRUE,
      index = folds
    )

    metric = 'RMSE'
  }

  # For linear regression, expanding factors to set of numeric varibles
  xn <- if (all(sapply(x, is.numeric))) {
    x
  } else {
    logger.info("For some models, factor variables are expanded to numerical variables.")
    el.numerize(x)
  }

  # Fit regularized linear regression model with CV-based parameter tuning
  fit.lm <- caret::train(
    x = xn,
    y = y,
    method = 'glmnet',
    metric = metric,
    trControl = trControl
  )

  # Fit random forest model with CV-based parameter tuning
  fit.rf <- caret::train(
    x = x,
    y = y,
    method = 'ranger',
    metric = metric,
    importance = 'impurity',
    trControl = trControl
  )

  # Fit gradient boosted reg. tree model with CV-based parameter tuning
  fit.gbrt <- caret::train(
    x = xn,
    y = y,
    method = 'xgbTree',
    metric = metric,
    trControl = trControl
  )

  # Recursive partitioning model with CV-based parameter tuning
  grid <- expand.grid(cp=seq(0, 0.05, 0.01))
  fit.rp <- caret::train(
    x = x,
    y = y,
    method = 'rpart',
    metric = metric,
    tuneGrid = grid,
    trControl = trControl
  )

  # Fit SVM model with CV-based parameter tuning
  fit.svm <- caret::train(
    x = xn,
    y = y,
    method = 'svmRadial',
    metric = metric,
    preProcess = c("center", "scale"),
    trControl = trControl
  )

  # Fit neural network model with CV-based parameter tuning
  fit.nn <- caret::train(
    x = xn,
    y = y,
    method = if(isClassification) 'nnet' else 'brnn',
    metric = metric,
    preProcess = c("center", "scale"),
    trControl = trControl
  )

  if (plot) {
    el.model.show(fit.lm, 'LM')
    el.model.show(fit.rf, 'RF')
    el.model.show(fit.gbrt, 'GBRT')
    el.model.show(fit.rp, 'RP')
    el.model.show(fit.svm, 'SVM')
    el.model.show(fit.nn, 'NN')

    rpart.plot::prp(fit.rp$finalModel, type=4, extra=1, main = 'Decision Tree')
  }

  # Compare models
  fits <- list(LM = fit.lm, RF = fit.rf, GBRT = fit.gbrt, RP = fit.rp, SVM = fit.svm, NN = fit.nn)

  el.model.compare(fits, plot)

  fits
}
