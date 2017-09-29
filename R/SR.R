#' Baseline Speed Rating Estimation
#' 
#' This function estimates speed ratings for a baseline course.
#' @param x Time in seconds for runners as a numeric vector
#' @param alpha The number of seconds equal to one speed point
#' @param beta The centering parameter
#' @keywords speed rating, cross crountry, handicapping
#' @export
#' @examples
#' SR(1573.6, 4.4, 2355)

SR <- function(x, alpha, beta) {
  (beta - x) / alpha
}