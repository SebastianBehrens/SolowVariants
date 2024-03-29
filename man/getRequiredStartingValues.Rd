% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HelperFunctions.R
\name{getRequiredStartingValues}
\alias{getRequiredStartingValues}
\title{Get the variables that need starting values for t = 0 of the Solow variant}
\usage{
getRequiredStartingValues(ModelCode)
}
\arguments{
\item{ModelCode}{Model abbreviation, such as "BS", "GS", "ESHC", ESSRO". (See the vignette to this package for an exhaustive listing of possible model codes.)}
}
\description{
Each simulation function requires certain key endogenous variables to be
initialised at \eqn{t = 0}. This function yields precisely those variables
(resp. their abbreviations).
}
\examples{
getRequiredStartingValues("ESSOE")
}
