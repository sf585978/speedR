#' Linear Model for Predicting Course Corrections
#'
#' Model trained on 74 races with human coded course corrections. Model predicts
#' course correction from the difference between the baseline race and the new
#' race at the 10th, 20th, 30th, 40th, and 50th percentiles, as well as the new
#' race's skewness, kurtosis, standard deviation, number of runners, and
#' mean time.
#'
#' @docType data
#'
#' @usage data(lm_course_correction)
#'
#' @format An object of class \code{"lm"}.
#'
#' @keywords models
#'
"m_lm"
