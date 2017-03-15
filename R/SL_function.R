#' A SuperLearner Function
#'
#' This function allows you to fit the SuperLearner.
#' @param dependent A numeric dependent variable, binomial or gaussian.
#' @param independent A matrix with all independent variables.
#' @param id A numeric individual identifier.
#' @param library A list of SL libraries.
#' @keywords SuperLearner
#' @export
#' @examples
#' SL.model()
SL.model <- function(dependent,independent,id,library,cores) {
  output <- SuperLearner::SuperLearner(Y=dependent, X=independent,id=id,
                                       SL.library=SL.library,
                                       method="method.NNLS",family=binomial())
  return(output)
}
