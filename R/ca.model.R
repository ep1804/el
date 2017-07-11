requireNamespace('caret')
requireNamespace('caretEnsemble')
requireNamespace('glmnet')
requireNamespace('ranger')
requireNamespace('xgboost')
requireNamespace('kernlab')
requireNamespace('deepnet')
requireNamespace('lattice')
requireNamespace('stats')
requireNamespace('ggplot2')
requireNamespace('reshape2')

#' Fit a classification model with CV-based parameter tuning, and show in-sample performance
#'
#' @param x       numeric or factor data.frame.
#' @param y       numeric or factor vector.
#' @param cvFolds Cross-validation fold number
#' @param size.lim numeric. If data size is larger than this, it is sampled.
#' @param plot    logical. Plot or not
#'
#' @return list of final models
#' @export
#'
#' @examples
#' dia <- ggplot2::diamonds
#' dia <- dia[sample(1:nrow(dia), 800),]
#' x <- dia
#' x$price <- NULL
#' y <- dia$price
#'
#' fits <- el.re.model(x, y)
#'
el.ca.two.model <- function(x, y, cvFolds = 7, size.lim = 10000, plot = TRUE) {

  if (!is.vector(y) & !is.factor(y)) {
    logger.error("y should be a factor or logical.")
    return()
  }

  if(is.logical(y)){ y <- as.factor(y) }

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

  trControl <- caret::trainControl(
    method = 'cv',
    number = length(folds),
    summaryFunction = caret::twoClassSummary,
    classProbs = TRUE,
    verboseIter = TRUE,
    savePredictions = TRUE,
    index = folds
  )

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
    metric = 'ROC',
    trControl = trControl
  )

  # Fit random forest model with CV-based parameter tuning
  fit.rf <- caret::train(
    x = x,
    y = y,
    method = 'ranger',
    metric = 'ROC',
    importance = 'impurity',
    trControl = trControl
  )

  # Fit gradient boosted reg. tree model with CV-based parameter tuning
  fit.gbrt <- caret::train(
    x = xn,
    y = y,
    method = 'xgbTree',
    metric = 'ROC',
    trControl = trControl
  )

  # Recursive partitioning model with CV-based parameter tuning
  grid <- expand.grid(cp=seq(0, 0.05, 0.01))
  fit.rp <- caret::train(
    x = x,
    y = y,
    method = 'rpart',
    metric = 'ROC',
    tuneGrid = grid,
    trControl = trControl
  )

  # Fit SVM model with CV-based parameter tuning
  fit.svm <- caret::train(
    x = xn,
    y = y,
    method = 'svmRadial',
    metric = 'ROC',
    preProcess = c("center", "scale"),
    trControl = trControl
  )

  # Fit neural network model with CV-based parameter tuning
  fit.nn <- caret::train(
    x = xn,
    y = y,
    method = 'dnn',
    metric = 'ROC',
    preProcess = c("center", "scale"),
    trControl = trControl
  )

  if (plot) {

    plotPred <- function(y, pred, main) {
      df <- data.frame(y, prediction = pred)
      df <- data.frame(index = 1:length(y), df[order(y),])
      df <- reshape2::melt(df, id.vars = 'index')
      g <-
        ggplot2::ggplot(data = df, aes(x = index, y = value, col = variable)) +
        ggplot2::geom_jitter(alpha = 0.5) +
        ggplot2::ggtitle(main)
      print(g)
    }

    # Tuning on training parameters and predictions on training data
    print(plot(fit.lm, main = 'Linear model tuning'))
    plotPred(y, predict(fit.lm), 'LM prediction on train data')
    print(plot(caret::varImp(fit.lm), main = 'Variable importance by LM'))

    print(plot(fit.rf, main = 'RF tuning'))
    plotPred(y, predict(fit.rf), 'RF model prediction on train data')
    print(plot(caret::varImp(fit.rf), main = 'Variable importance by RF'))

    print(plot(fit.gbrt, main = 'GBRT tuning'))
    plotPred(y, predict(fit.gbrt), 'GBRT model prediction on train data')
    print(plot(caret::varImp(fit.gbrt), main = 'Variable importance by GBRT'))

    print(plot(fit.rp, main = 'RP tuning'))
    plotPred(y, predict(fit.rp), 'RP model prediction on train data')
    print(plot(caret::varImp(fit.rp), main = 'Variable importance by RP'))

    print(plot(fit.svm, main = 'SVM tuning'))
    plotPred(y, predict(fit.svm), 'SVM model prediction on train data')
    print(plot(caret::varImp(fit.svm), main = 'Variable importance by SVM'))

    print(plot(fit.nn, main = 'NN tuning'))
    plotPred(y, predict(fit.nn), 'NN model prediction on train data')
    print(plot(caret::varImp(fit.nn), main = 'Variable importance by NN'))
  }

  # Compare models
  fits <- list(LM = fit.lm, RF = fit.rf, GBRT = fit.gbrt, RP = fit.rp, SVM = fit.svm, NN = fit.nn)
  fits.comp <- caret::resamples(fits)
  logger.info("Model comparison summary:", summary(fits.comp), capture = T)

  if (plot) {
    print(lattice::bwplot(fits.comp, metric = 'ROC', main = 'Model comparison by RMSE'))
    print(rpart.plot::prp(fit.rp$finalModel, type=4, extra=1, main = 'Decision Tree'))
  }

  fits
}
