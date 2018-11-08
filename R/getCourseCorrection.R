#' Get Course Correction Parameter
#' 
#' This function estimates and returns the course correction parameter, gamma, for courses that are not the base course.
#' @param results The results for the race you are speed rating
#' @param alpha The number of seconds one speed rating point is equal to
#' @param beta The number of seconds marking the zero point of the scale
#' @param lower_thresh The quantile results must be greater than to contribute to calculating the course correction
#' @param upper_thresh The quantile results must be less than to contribute to calculating the course correction
#' @param baseID The race ID for the base race
#' @param baseIntercept The regression intercept for the base race
#' @return gammaFit The best fit estimate for the course correction parameter, gamma.
#' @keywords speed rating, cross country, handicapping
#' @export
#' @examples 
#' getCourseCorrection(results = geneseo16, alpha = 4.4, beta = 2355, lower_thresh = 0.1, upper_thresh = 0.95, baseID = "mGeneseo15", baseIntercept = 1527.197)

getCourseCorrection <- function(results,
                                alpha = 4.4, 
                                beta = 2355,
                                lower_thresh = 0.1,
                                upper_thresh = 1,
                                baseID = "mGeneseo15",
                                baseIntercept = 1527.197) {
  require(readr)
  require(dplyr)
  if (results$raceID[1] == baseID) {
    message("Race is same as the base race.")
    courseCorrection <- 0
  } else {
    m0 <- lm(seconds ~ place, data = results)
    intercept <- m0$coefficients[1]
    courseCorrection <- intercept - baseIntercept
  }
  return(courseCorrection)
}
