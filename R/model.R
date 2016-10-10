requireNamespace('caret')
requireNamespace('caretEnsemble')
requireNamespace('lattice')
requireNamespace('glmnet')
requireNamespace('ranger')
requireNamespace('stats')

#' Fit a model with CV-based parameter tuning, and show in-sample performance
#'
#' @param y    numeric or factor vector.
#' @param x    numeric or factor data.frame.
#' @param plot logical. Plot or not
#'
#' @return list(LM = fit.lm, RF = fit.rf)
#' @export
#'
el.model <- function(y, x, plot=TRUE){
  
  if(!is.vector(y) | !is.data.frame(x)) return()
  
  if(is.factor(y)){
    
    return() # TODO - classification
    
  } else {
    
    # CV folds to be used in models
    folds <- caret::createFolds(y, k = 10)
    
    # trainControl object to be used in models
    trControl <- caret::trainControl(
      method = 'cv',
      number = 10,
      verboseIter = TRUE,
      savePredictions = TRUE,
      index = folds
    )
    
    # For linear regression, expanding factors to set of numeric varibles
    xn <- if(all(sapply(x, is.numeric))){
      x
    } else {
      fm <- paste('~', paste(colnames(x), collapse = ' + '))
      stats::model.matrix(stats::as.formula(fm), x)
    }
      
    # Fit regularized linear regression model with CV-based parameter tuning
    fit.lm <- caret::train(
      x = xn,
      y = y,
      method = 'glmnet',
      trControl = trControl
    )
    
    # Fit regularized random forest model with CV-based parameter tuning
    fit.rf <- caret::train(
      x = x,
      y = y,
      method = 'ranger',
      trControl = trControl
    )
    
    if(plot){
      print(plot(fit.lm, main = 'Linear model performance'))
      
      print(plot(fit.rf, main = 'Random forest performance'))
    }
    
    # Compare models
    fits <- list(LM = fit.lm, RF = fit.rf)
    fits.comp <- caret::resamples(fits)
    logger.info("Model comparison summary:", summary(fits.comp), capture = T)
    
    if(plot){
      print(lattice::bwplot(fits.comp, metric = 'RMSE', 
                   main = 'Model comparison by RMSE (1/2)'))
      print(lattice::xyplot(fits.comp, metric = 'RMSE', 
                            main = 'Model comparison by RMSE (2/2)'))
      print(lattice::xyplot(fits.comp, metric = 'Rsquared'))
    }
    
    fits
  }
}