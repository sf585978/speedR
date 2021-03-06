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

n_neighbors <- function(x, y, bandwidth_x = 5, bandwidth_y = 5) {
  require(dplyr)
  dataset <- as.data.frame(cbind(x, y))
  n_neighbors <- numeric(nrow(dataset))
  for (i in 1:nrow(dataset)) {
    x_i <- dataset$x[i]
    y_i <- dataset$y[i]
    k <- 
      tryCatch({
        dataset %>%
          filter(x <= x_i + bandwidth_x,
                 x >= x_i - bandwidth_x,
                 y <= y_i + bandwidth_y,
                 y >= y_i - bandwidth_y)
      }, error = function(e) {
        NULL
      })
    
    if(is.null(k)) {
      n_neighbors[i] <- 0
    } else{
      n_neighbors[i] <- nrow(k) - 1
    }
  }
  return(n_neighbors)
}

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
    preds <- predict(m_lm, newdata = results_df, interval = "prediction")
    gamma <- preds[1]
    gamma_low = preds[2]
    gamma_high = preds[3]
    
    results2 <- suppressWarnings(results %>%
                                   inner_join(references,
                                              by = c("name", "school")))
    if(nrow(results2) <= 20) {
      message(paste("Not enough reference runners found for ", 
                    results$raceID[1], ".", sep =""))
      return(gamma)
    } else {
      x <- results2$seconds
      gamma_space <- seq(round(gamma_low), round(gamma_high), by = 1)
      gamma_x <- gamma_space[1]
      results2 <- results2 %>%
        mutate(z = SR_CourseCorrection(x, alpha, beta, gamma_x),
               cols = ifelse(refSR > z, "red", "green"),
               diffs = z - refSR,
               label = ifelse(abs(diffs) > quantile(abs(diffs), 0.95), name,
                              NA),
               residual = z - refSR,
               n_neighbors = n_neighbors(seconds, refSR))
      average_resid <- sqrt(weighted.mean(results2$residual ^ 2, 
                                          w = results2$n_neighbors))
      for (i in 2:length(gamma_space)) {
        gamma_y <- gamma_space[i]
        results2 <- results2 %>%
          mutate(z = SR_CourseCorrection(x, alpha, beta, gamma_y),
                 cols = ifelse(refSR > z, "red", "green"),
                 diffs = z - refSR,
                 label = ifelse(abs(diffs) > quantile(abs(diffs), 0.95), name,
                                NA),
                 residual = z - refSR,
                 n_neighbors = n_neighbors(seconds, refSR))
        average_resid_y <- sqrt(weighted.mean(results2$residual ^ 2, 
                                              w = results2$n_neighbors + 0.001))
        if (average_resid_y > average_resid) {
          gamma <- gamma_x
          return(gamma)
        } else {
          gamma_x <- gamma_y
          average_resid <- average_resid_y
        }
      }
      # results3 <- results2 %>%
      #   filter(seconds <= quantile(seconds, 0.5))
      
      # average_residual <- 2 * weighted.mean(results2$residual,
      #                                       w = results2$n_neighbors)
      # 
      # gamma <- gamma + average_residual
    }
  } else {
    data("gam_course_correction")
    gamma <- predict.gam(m_gam, newdata = results_df)
  }
    return(gamma)
}
  