#' Aspirin compliance on pregnancy outcomes
#'
#' Data from a randomized trial on low dose aspirin in
#' women trying to conceive (EAGeR Trial). Data on 1228 women
#' with a total of 10,929 person-months. Data are simulated from
#' the original trial data.
#'
#' @docType data
#'
#' @usage data(aspirin)
#'
#' @format A data frame with several variables.
#'
#' @keywords datasets
#'
#' @references Schisterman et al. (2014) Lancet 384(9937):29-36
#' (\href{https://www.ncbi.nlm.nih.gov/pubmed/?term=24702835}{PubMed})
#'
#'
#' @examples
#' data(aspirin)
#' require(cmprsk)
#' a<-subset(aspirin,aspirin$y>0)
#' cuminc<-cuminc(a$month,a$y,cencode=4)
#' \donttest{plot(cuminc)}
"aspirin"
