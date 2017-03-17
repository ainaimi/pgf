#' A SuperLearner Function
#'
#' This function allows you to fit the SuperLearner.
#' @param multicore A binary integer (0,1) variable to choose parallel
#' or sequential processing. If "0", standard SuperLearner is
#' used. If "1" mcSuperLearner is used. If "2" snowSuperLearner is used,
#' and other arguments are required. Defaults to "0".
#' @param dependent A numeric dependent variable, binomial or gaussian.
#' @param independent A matrix with all independent variables.
#' @param id A numeric individual identifier.
#' @param library A list of SL libraries.
#' @keywords SuperLearner
#' @export
#' @examples
#' SL.model()
SL.model <- function(multicore=0,dependent,independent,id,library,cores=4) {
  if(multicore==0){
    output <- SuperLearner::SuperLearner(Y=dependent, X=independent,id=id,
                                         SL.library=SL.library,
                                         method="method.NNLS",family=binomial())
  } else if(multicore==1){
    output <- SuperLearner::mcSuperLearner(Y=dependent, X=independent,id=id,
                                             SL.library=SL.library,
                                             method="method.NNLS",family=binomial())
  } else if(multicore==2){
    cl<-makeCluster(cores, type="FORK")
    clusterSetRNGStream(cl, iseed=123)
    output <- SuperLearner::snowSuperLearner(cluster=cl,Y=dependent, X=independent,id=id,
                                             SL.library=SL.library,
                                             method="method.NNLS",family=binomial())
    stopCluster(cl)
  }
  return(output)
}
