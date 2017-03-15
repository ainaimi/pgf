#' A package loading function
#'
#' This function automatically loads and
#' installs listed packages.
#' @param pkg A combined package list.
#' @keywords g-formula
#' @export
#' @examples
#' pkg<-c("SuperLearner","data.table","parallels")
#' ipak(pkg)
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
