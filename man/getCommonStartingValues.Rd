% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HelperFunctions.R
\name{getCommonStartingValues}
\alias{getCommonStartingValues}
\title{Yields the union of required starting values for two solow variants}
\usage{
getCommonStartingValues(ModelCode1, ModelCode2)
}
\arguments{
\item{ModelCode1}{Model abbreviation for some Solow variant.}

\item{ModelCode2}{Model abbreviation for some Solow variant.}
}
\description{
Yields the union of required starting values for two solow variants
}
\examples{
getunifiedstartingvalues("BS", "ESSOE")
}
