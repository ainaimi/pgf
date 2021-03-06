% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ipak.R
\name{ipak}
\alias{ipak}
\title{A package loading function}
\usage{
ipak(pkg)
}
\arguments{
\item{pkg}{A combined package list.}
}
\description{
This function automatically loads and
installs listed packages.
}
\examples{
pkg<-c("SuperLearner","data.table","parallels")
ipak(pkg)
}
\keyword{g-formula}
