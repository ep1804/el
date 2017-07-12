requireNamespace('caret')
requireNamespace('lattice')
requireNamespace('ggplot2')
requireNamespace('reshape2')

#' Plot information on trained model tuning and performance
#'
#' @param fit  caret train result object
#' @param name model name
#'
#' @export
#'
el.model.show <- function(fit, name = 'model'){

  logger.info(paste('Model', name, 'summary'))
  print(fit)

  plotPred <- function(y, pred, main) {
    df <- data.frame(y, prediction = pred)
    df <- data.frame(index = 1:length(y), df[order(y),])
    df <- reshape2::melt(df, id.vars = 'index')
    g <- ggplot2::ggplot(data = df, aes(x = index, y = value, col = variable)) +
      ggplot2::geom_jitter(alpha = 0.5) +
      ggplot2::ggtitle(main)
    print(g)
  }

  print(plot(fit, main = paste('Parameter tuning on', name)))

  y <- fit$trainingData$.outcome
  pred <- predict(fit)
  plotPred(y, pred, paste('Prediction on train data by', name))

  if(fit$modelType == 'Classification'){
    print(caret::confusionMatrix(fit))
  }

  el.model.varImp(fit, name, plot = T)

  return()
}

#' Plot variable importance by trained model tuning and performance
#'
#' @param fit  caret train result object
#' @param name model name
#'
#' @export
#'
el.model.varImp <- function(fit, name = 'model', plot = TRUE){
  vi <- caret::varImp(fit)

  vi$importance <- as.data.frame(vi$importance) # nnet bug

  print(plot(vi, main = paste('Variable importance by', name)))

  vi$importance
}

#' Compare models
#'
#' @param fits list. Named list of caret train result objects
#' @param plot logical. Plot or not
#'
#' @return list. compare statistics
#' @export
#'
el.model.compare <- function(fits, plot = TRUE){

  fits.comp <- caret::resamples(fits)
  sumy <- summary(fits.comp)
  logger.info("Model comparison summary:", sumy, capture = T)

  if (plot) {
    if(fits[[1]]$modelType  == 'Classification'){
      print(lattice::bwplot(fits.comp, metric = 'Kappa', main = 'Model comparison by Kappa'))
      print(lattice::bwplot(fits.comp, metric = 'Accuracy', main = 'Model comparison by Accuracy'))
    } else {
      print(lattice::bwplot(fits.comp, metric = 'RMSE', main = 'Model comparison by RMSE'))
      print(lattice::bwplot(fits.comp, metric = 'Rsquared', main = 'Model comparison by R-squared'))
    }
  }

  sumy$statistics
}
