% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getCourseCorrection_nls.R
\name{getCourseCorrection_nls}
\alias{getCourseCorrection_nls}
\title{Get Course Correction Parameter using Nonlinear Regression of Time on Expected Values}
\usage{
getCourseCorrection_nls(race, results, referenceRunners, guess,
  alpha = 4.4, beta = 2355, baseID = "mGeneseo15")
}
\arguments{
\item{race}{The numeric ID of the race you want to estimate the correction for}
}
\value{
gammaFit The best fit estimate for the course correction parameter, gamma.
}
\description{
This function estimates and returns the course correction parameter, gamma, for courses that are not the base course.
}
\examples{
getCourseCorrection(race = "mWilliams15", results, referenceRunners, guess, baseID = "mGeneseo15")
}
\keyword{country,}
\keyword{cross}
\keyword{handicapping}
\keyword{rating,}
\keyword{speed}
