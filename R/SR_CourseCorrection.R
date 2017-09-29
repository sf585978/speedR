#' Course Corrected Speed Rating Estimation
#' 
#' This function estimates speed ratings for a course with a course correction.
#' @param x Time in seconds for runners as a numeric vector
#' @param alpha The number of seconds equal to one speed point
#' @param beta The centering parameter
#' @param gamma The course correction parameter indicating estimated average difference between a course and the baseline
#' @keywords speed rating, cross crountry, handicapping
#' @export
#' @examples
#' SR_CourseCorrection(1595.2, 4.4, 2355, -46)

SR_CourseCorrection <- function(x, alpha, beta, gamma) {
  (beta - x - gamma) / alpha
}