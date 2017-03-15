#' A parametric g formula prediction function
#'
#' This function is used by the pgf function to predict
#' values from model objects.
#' @param mod A model object to predict from.
#' @param ndat The new data to predict from.
#' @keywords g-formula
#' @export
#' @examples
#' pFunc()
pFunc<-function(mod,ndat){as.numeric(predict(mod,newdata=ndat,onlySL=T)$pred>runif(1))}
