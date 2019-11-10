#' Generalied Additive Model for Predicting Course Corrections
#'
#' Model trained on 74 races with human coded course corrections. Model predicts
#' course correction from the difference between the baseline race and the new
#' race at the 10th, 20th, 30th, 40th, and 50th percentiles, as well as the new
#' race's skewness, kurtosis, and standard deviation.
#'
#' @docType data
#'
#' @usage data(gam_course_correction)
#'
#' @format An object of class \code{"gam", "glm", "lm"}.
#'
#' @keywords model
#'
"m_gam"
