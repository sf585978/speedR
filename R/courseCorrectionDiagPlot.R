#' Visualize Quality of Estimates
#' 
#' This function estimates and returns the course correction parameter, gamma, for courses that are not the base course.
#' @param results The results for the race you are speed rating
#' @param reference The data frame of expected performances
#' @param alpha The number of seconds one speed rating point is equal to
#' @param beta The number of seconds marking the zero point of the scale
#' @param gamma The course correction parameter
#' @return A plot of expected speed rating against time with a line visualizing estimated speed ratings
#' @keywords speed rating, cross country, handicapping
#' @export
#' @examples 
#' getCourseCorrection(results = geneseo16, alpha = 4.4, beta = 2355, lower_thresh = 0.1, upper_thresh = 0.95, baseID = "mGeneseo15", baseIntercept = 1527.197)

courseCorrectionDiagPlot <- function(results,
                                     references,
                                     alpha = 4.4, 
                                     beta = 2355,
                                     gamma) {
  require(readr)
  require(dplyr)
  require(ggplot2)
  require(ggrepel)
  results2 <- suppressWarnings(results %>%
                                 inner_join(references,
                                            by = c("name", "school")))
  if(nrow(results2) == 0) {
    message(paste("No reference runners found for ", race[i], ".", sep =""))
    return()
  } else {
    results2 <- results2 %>%
      mutate(z = SR_CourseCorrection(x, alpha, beta, gamma),
             cols = ifelse(y > z, "red", "green"),
             diffs = z - y,
             label = ifelse(abs(diffs) > quantile(abs(diffs), 0.95), name, NA))
      ggplot(results2, aes(x = seconds, y = refSR, color = cols)) +
        geom_point() +
        scale_color_manual("Vs. Expectation", 
                           values = c("forestgreen", "red"),
                           labels = c("Overperformed", "Underperformed")) +
        geom_line(aes(x = x, y = z), color = "blue") +
        geom_label_repel(data = subset(results2, diffs > 0),
                         aes(label = label),
                         force = 30,
                         nudge_y = -10) +
        geom_label_repel(data = subset(results2, diffs < 0),
                         aes(label = label),
                         force = 30,
                         nudge_y = 10) +
        theme_bw() +
        xlab("Seconds") +
        ylab("Expected Speed Rating") +
        theme(legend.position = c(0.075, 0.15),
              legend.background = element_rect(fill = "gray90", 
                                               size = .5, 
                                               linetype = "dotted")) +
        ggtitle("Performance Against Expected") +
        labs(caption = "Blue line indicates estimated speed rating. Labels are included for runners who were in the top 5% of difference from their expected performance.")
  }
}
  