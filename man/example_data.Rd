% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/example_data.R
\docType{data}
\name{example_data}
\alias{example_data}
\title{Example Dataset}
\format{
A named list:
\describe{
\item{data.long}{a simulation data in long format. Each subject may contribute multiple records
(id: Subject id,
Xt: Values of longitudinal biomarker,
vtime: Biomarker measurement time)}
\item{data.short}{a simulation data contains subject-level information.
Each subject only has one record.
(id: Subject id,
Y: Observed event time,
delta: Event indicator. 1 = event, 0 = censored,
Z: a binary covariate,
Zcont: a continuous covariate}
}
}
\usage{
example_data
}
\description{
A list of two example data frames used for demonstrating package functionality.
}
\examples{
data(example_data)
str(example_data)
}
\keyword{datasets}
