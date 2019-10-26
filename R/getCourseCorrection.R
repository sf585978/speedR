#' Get Course Correction Parameter
#' 
#' This function estimates and returns the course correction parameter, gamma, for courses that are not the base course.
#' @param results The results for the race you are speed rating
#' @param alpha The number of seconds one speed rating point is equal to
#' @param beta The number of seconds marking the zero point of the scale
#' @param lower_thresh The quantile results must be greater than to contribute to calculating the course correction
#' @param upper_thresh The quantile results must be less than to contribute to calculating the course correction
#' @param race_dist The distance of the race being analyzed
#' @param baseID The race ID for the base race
#' @param baseIntercept The regression intercept for the base race
#' @return gammaFit The best fit estimate for the course correction parameter, gamma.
#' @keywords speed rating, cross country, handicapping
#' @export
#' @examples 
#' getCourseCorrection(results = geneseo16, alpha = 4.4, beta = 2355, lower_thresh = 0.1, upper_thresh = 0.95, race_dist = "8k", baseID = "mGeneseo15", baseIntercept = 1527.197)

getCourseCorrection <- function(results,
                                alpha = 4.4, 
                                beta = 2355,
                                lower_thresh = 0.5,
                                upper_thresh = 1,
                                race_dist = "8k",
                                baseID = "mGeneseo15",
                                baseIntercept = 1531.96) {
  require(readr)
  require(dplyr)
  require(ggplot2)
  require(ggrepel)
  
  if (race_dist == "8k") {
    window_l = 24
  } else if (race_dist == "6k") {
    window_l = 18
  } else if (race_dist == "5k") {
    window_l = 15
  } else if (race_dist == "4k") {
    window_l = 12
  }
  
  if (results$raceID[1] == baseID) {
    message("Race is same as the base race.")
    courseCorrection <- 0
  } else {
    results <- results %>%
      mutate(cond = ifelse(place < quantile(place, (1 - lower_thresh)) &
                             place > quantile(place, (1 - upper_thresh)),
                           1, 0))
    results$weight <- NA
    for (i in 1:nrow(results)) {
      results$weight[i] <- 
        (nrow(
          results[which(
            round(results$seconds, 1) %in% seq(from = round(results$seconds[i], 
                                                  digits = 1) - window_l,
                                     to = round(results$seconds[i], 
                                                digits = 1) + window_l,
                                     by = 0.1)
          ), ]
        ) - 1) * results$cond[i]
    }
    m0 <- lm(seconds ~ place, data = results, weights = results$weight)
    intercept <- m0$coefficients[1]
    courseCorrection <- baseIntercept - intercept
    output <- data.frame(label = c(baseID, results$raceID[1]),
                         x = c(0, 0),
                         y = c(baseIntercept, intercept),
                         color = c("blue", "red"))
    print(ggplot(results, aes(x = place, y = seconds)) +
            geom_point() +
            geom_point(aes(x = 0, y = intercept), size = 3, color = "red") +
            geom_point(aes(x = 0, y = baseIntercept), 
                       size = 3, 
                       color = "blue") +
            geom_label_repel(data = output, 
                             aes(x = x, y = y, label = label, color = color),
                             force = 10,
                             min.segment.length = unit(1, 'lines'),
                             nudge_y = 10) +
            scale_color_manual(guide = FALSE, values = c("blue", "red")) +
            # geom_label_repel(aes(x = 0, y = intercept), 
            #                  label = results$raceID[1],
            #                  color = "red") +
            geom_smooth(method = "lm", se = FALSE, color = "red",
                        mapping = aes(weight = weight)) +
      theme_bw() +
      xlab("Place") +
      ylab("Seconds") +
      ggtitle(paste("Seconds against Place - ", 
                    results$raceID[1],
                    sep = ""),
      subtitle = paste("Intercept = ", 
                       intercept,
                       ", Course Correction = ",
                       courseCorrection, sep = "")))
  }
  return(courseCorrection)
}

