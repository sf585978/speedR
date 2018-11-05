#' Get Course Correction Parameter
#' 
#' This function estimates and returns the course correction parameter, gamma, for courses that are not the base course.
#' @param race The numeric ID of the race you want to estimate the correction for
#' @return gammaFit The best fit estimate for the course correction parameter, gamma.
#' @keywords speed rating, cross country, handicapping
#' @export
#' @examples 
#' getCourseCorrection(race = "mWilliams15", results, referenceRunners, guess, baseID = "mGeneseo15")

getCourseCorrection_avgDiff <- function(results,
                                        referenceRunners,
                                        alpha = 4.4, 
                                        beta = 2355, 
                                        lower_thresh = 0.05,
                                        upper_thresh = 0.95,
                                        baseID = "mGeneseo15") {
  require(readr)
  require(dplyr)
  if (results$raceID[1] == baseID) {
    message("Race is same as base race.")
    courseCorrections <- 0
  } else {
    results2 <- suppressWarnings(results %>%
                                   # filter(seconds > quantile(seconds, 
                                   # lower_thresh),
                                   # seconds < quantile(seconds,
                                   # upper_thresh)) %>%
                                   inner_join(referenceRunners,
                                              by = c("name", "school")))
    lower_thresh <- quantile(results$seconds, lower_thresh)
    upper_thresh <- quantile(results$seconds, upper_thresh)
    results2 <- results2[which(results2$seconds > lower_thresh), ]
    results2 <- results2[which(results2$seconds < upper_thresh), ]
    if(nrow(results2) == 0) {
      message(paste("No reference runners found for ", results$raceID[1], ".", 
                    sep =""))
      courseCorrections <- NA
      next()
    }
    x <- results2$seconds
    y <- results2$refSR
    unCorrected <- SR_CourseCorrection(x, alpha, beta, gamma = 0)
    diffs <- y - unCorrected
    avg.dff <- mean(diffs)
    
    plot(x, y, main = results$raceID[1])
    curve(SR_CourseCorrection(x, alpha, beta, avg.dff), 
          add = TRUE,
          col = "red")
    courseCorrections <- avg.dff
  }
  
  courseCorrections <- data.frame(raceID = results$raceID[1], 
                                  gamma = courseCorrections)
  courseCorrections$raceID <- as.character(courseCorrections$raceID)
  return(courseCorrections)
}
