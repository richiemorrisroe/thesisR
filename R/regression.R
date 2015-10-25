##' .. Essentially a wrapper around a particular sequence of glmnet functions
##'
##' .. Thats just it. 
##' @title penalised_regression
##' @param x a dataframe of all numeric column variables
##' @param y the response variable
##' @param testdata the new dataset to calculate coefficients on
##' @param newy the test response variable
##' @param alpha parameter to control lasso vs ridge
##' @param nfolds number of splits for cross-validation
##' @param type type of output from predict, current options are coefficients (of a new model fit on test data) or response - the actual predicted test values
##' @param family as per glm
##' @return either a vector of predictions or the coefficients of the new model (as a sparse matrix)
##' @author Richie Morrisroe

penalised_regression <- function(x, y,
                                 testdata,
                                 newy,
                                 alpha,
                                 nfolds=10,
                                 type=c("coefficients", "response"),
                                 family="gaussian") {
  x.mat <- as.matrix(x)
  testdata.mat <- as.matrix(testdata)
  ## browser()
  cvres <- glmnet::cv.glmnet(x=x.mat, y=y, nfolds=nfolds, family=family)
  mod <- glmnet::glmnet(x=x.mat, y=y, alpha=alpha, family=family)
  pred.coef <- glmnet::predict(mod, testdata.mat, s=cvres$lambda.min, type=type)
  if(type == "response") {
  pred <- data.frame(pred=as.vector(pred.coef), obs=newy)
  return(pred)
}
  else{
    return(pred.coef)
  }
  }
##' {Something that would have been rocking, had I been able to make it work}
##' {See above}
##' @title tune_loess
##' @param formula a formula describing the model
##' @param data the dataset to fit the model to
##' @param newdata the dataset to predict over
##' @param tune_length length of solutions to tune over
##' @param ... arguments passed through to loess
##' @return not much, currently
##' @author Richie Morrisroe
tune_loess <- function(formula, data, newdata, tune_length, ...) {
  formula <- as.formula(formula)
  seq <- seq(0, 1, by=tune_length)
  part <- list(length=length(seq))
  part.index <- caret::createDataPartition(
      formula[2], p=0.8, times=length(seq))
  for (i in seq_along(part.index)) {
    part[[i]] <- data[part.index,]
  }
  fitlist <- vector(mode="list", length=length(seq))
  for (j in seq_along(tune_length)) {
    fitlist[[j]] <- loess(formula, data=part[[j]], span=seq[i])
          }
    fitlist
}
##refactor this to make it more general
placglm <- function (data, indices) {
    d <- data[indices,]
    mod <- glm(PlacResp~TCQIATMean*LOTR+Age+OptIATMean+Rei,
               data=d, family=binomial(link="logit"))
    return(
        summary(mod)$coefficients[,4])
}
