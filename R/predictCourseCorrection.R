#' Get Course Correction Parameter
#' 
#' This function estimates and returns the course correction parameter, gamma, for courses that are not the base course.
#' @param results The results for the race you are speed rating
#' @param race_dist The distance of the race being analyzed
#' @param model_type Identifies whether to use linear or additive model for prediction
#' @return gamma The predicted course correction
#' @keywords speed rating, cross country, handicapping
#' @import ggplot2 
#' @import ggrepel
#' @import dplyr
#' @import ggpointdensity
#' @importFrom e1071 skewness kurtosis
#' @export
#' @examples 
#' predictCourseCorrection(results = geneseo16, race_dist = "8k", model_type = "lm")

predictCourseCorrection <- function(results,
                                    race_dist = "8k",
                                    model_type = "lm",
                                    references = updatedReference,
                                    alpha = 4.4, 
                                    beta = 2355) {
  
  data("geneseo15_metrics")
  
  results_df <- data.frame(race = unique(results$raceID),
                           pctile_10 = quantile(results$seconds, 0.1),
                           pctile_20 = quantile(results$seconds, 0.2),
                           pctile_30 = quantile(results$seconds, 0.3),
                           pctile_40 = quantile(results$seconds, 0.4),
                           pctile_50 = quantile(results$seconds, 0.5),
                           skew = skewness(results$seconds),
                           kurt = kurtosis(results$seconds),
                           std_dev = sd(results$seconds),
                           mean_time = mean(results$seconds),
                           n_runners = length(results$seconds))
  
  results_df <-
    results_df %>%
    mutate(pctile_10_diff = pctile_10 - geneseo15_metrics$pctile_10,
           pctile_20_diff = pctile_20 - geneseo15_metrics$pctile_20,
           pctile_30_diff = pctile_30 - geneseo15_metrics$pctile_30,
           pctile_40_diff = pctile_40 - geneseo15_metrics$pctile_40,
           pctile_50_diff = pctile_50 - geneseo15_metrics$pctile_50)

  if(model_type == "lm") {
    data("lm_course_correction")
    gamma <- predict(m_lm, newdata = results_df)
  } else {
    data("gam_course_correction")
    gamma <- predict.gam(m_gam, newdata = results_df)
  }
  
  results2 <- suppressWarnings(results %>%
                                 inner_join(references,
                                            by = c("name", "school")))
  if(nrow(results2) <= 20) {
    message(paste("Not enough reference runners found for ", results$raceID[1], ".", sep =""))
    return(gamma)
  } else {
    x <- results2$seconds
    results2 <- results2 %>%
      mutate(z = SR_CourseCorrection(x, alpha, beta, gamma),
             cols = ifelse(refSR > z, "red", "green"),
             diffs = z - refSR,
             label = ifelse(abs(diffs) > quantile(abs(diffs), 0.95), name, NA),
             residual = z - refSR)
    
    results3 <- results2 %>%
      filter(seconds <= quantile(seconds, 0.5))
    
    average_residual <- median(results2$residual)
    
    gamma <- gamma + average_residual
  }
  return(gamma)
}
