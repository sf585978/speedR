#' Get Course Correction Parameter
#' 
#' This function estimates and returns the course correction parameter, gamma, for courses that are not the base course.
#' @param race The numeric ID of the race you want to estimate the correction for
#' @param guess Your best guess of the course correction
#' @keywords speed rating, cross country, handicapping
#' @export
#' @examples 
#' getCourseCorrection(9198, 8)

getCourseCorrection <- function(race, guess) {
  require(readr)
  require(dplyr)
  results <- suppressWarnings(suppressMessages(read_csv("Data/Results/results.csv")))
  load("Data/referenceRunners.RData")
  results <- suppressWarnings(results %>%
    filter(raceID == race) %>%
    inner_join(referenceRunners, by = "name"))
  alpha <- 4.4
  beta <- 2355
  x <- results$seconds
  y <- results$genny15SR
  gammaFit <- nls(y ~ SR_CourseCorrection(x, 
                                          alpha,
                                          beta,
                                          gamma), 
                  start = list(gamma = guess))
  plot(x, y)
  curve(SR_CourseCorrection(x, alpha, beta, gammaFit$m$getPars()), add = TRUE,
        col = "red")
  print(gammaFit)
  return(gammaFit$m$getPars())
}
