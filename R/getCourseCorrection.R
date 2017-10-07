#' Get Course Correction Parameter
#' 
#' This function estimates and returns the course correction parameter, gamma, for courses that are not the base course.
#' @param race The numeric ID of the race you want to estimate the correction for
#' @return gammaFit The best fit estimate for the course correction parameter, gamma.
#' @keywords speed rating, cross country, handicapping
#' @export
#' @examples 
#' getCourseCorrection(9198, 8)

getCourseCorrection <- function(race,
                                results,
                                referenceRunners,
                                alpha = 4.4, 
                                beta = 2355, 
                                baseID = "8306") {
  courseCorrections <- numeric(length(race))
  require(readr)
  require(dplyr)
  for(i in 1:length(race)) {
    if (race[i] == baseID) {
      message("Race is same as base race.")
      courseCorrections[i] <- 0
    } else {
      results2 <- suppressWarnings(results %>%
                                     filter(raceID == race[i]) %>%
                                     filter(seconds > quantile(seconds, 0.03)) %>%
                                     filter(seconds < quantile(seconds, 0.95)) %>%
                                     inner_join(referenceRunners,
                                               by = c("name", "school")))
      guess <- mean(results$seconds - results2$gennyTime)
      x <- results2$seconds
      y <- results2$gennySR
      gammaFit <- nls(y ~ SR_CourseCorrection(x, 
                                              alpha,
                                              beta,
                                              gamma), 
                      start = list(gamma = guess))
      plot(x, y)
      curve(SR_CourseCorrection(x, alpha, beta, gammaFit$m$getPars()), 
            add = TRUE,
            col = "red")
      courseCorrections[i] <- gammaFit$m$getPars()
    }
  }
  courseCorrections <- data.frame(raceID = race, gamma = courseCorrections)
  return(courseCorrections)
}
""