#' Get Course Correction Parameter
#' 
#' This function estimates and returns the course correction parameter, gamma, for courses that are not the base course.
#' @param race The numeric ID of the race you want to estimate the correction for
#' @return gammaFit The best fit estimate for the course correction parameter, gamma.
#' @keywords speed rating, cross country, handicapping
#' @export
#' @examples 
#' getCourseCorrection(race = "mWilliams15", results, referenceRunners, guess, baseID = "mGeneseo15")

getCourseCorrection <- function(race,
                                results,
                                referenceRunners,
                                guess,
                                alpha = 4.4, 
                                beta = 2355, 
                                baseID = "mGeneseo15") {
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
                                     filter(seconds > quantile(seconds, 
                                                               0.03)) %>%
                                     filter(seconds < quantile(seconds,
                                                               0.95)) %>%
                                     inner_join(referenceRunners,
                                                by = c("name", "school")))
      if(nrow(results2) == 0) {
        message(paste("No reference runners found for ", race[i], ".", sep =""))
        courseCorrections[i] <- NA
        next()
      }
      x <- results2$seconds
      y <- results2$refSR
      weights <- 1 - (results2$place/400)
      tt <- try(
        nls(y ~ SR_CourseCorrection(x, 
                                    alpha,
                                    beta,
                                    gamma),
            weights = weights,
            start = list(gamma = guess), 
            control = (maxiter = 500))
      )
      if (is(tt, "try-error")) {
        message(paste("There was a problem with estimating the course correction for ",
                      race[i], ".", sep = ""))
        courseCorrections[i] <- NA
        next()
      } else {
        gammaFit <- nls(y ~ SR_CourseCorrection(x, 
                                                alpha,
                                                beta,
                                                gamma), 
                        weights = weights,
                        start = list(gamma = guess), 
                        control = (maxiter = 500))
      }
      plot(x, y, main = race[i])
      curve(SR_CourseCorrection(x, alpha, beta, gammaFit$m$getPars()), 
            add = TRUE,
            col = "red")
      courseCorrections[i] <- gammaFit$m$getPars()
    }
  }
  courseCorrections <- data.frame(raceID = race, gamma = courseCorrections)
  courseCorrections$raceID <- as.character(courseCorrections$raceID)
  return(courseCorrections)
}
