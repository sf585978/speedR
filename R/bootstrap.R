#' Bootstrap race results to generate reference estimate
#'
#' This function inputs a master file of speed ratings and references and returns a list of updated reference speed ratings for the runners contained in the master list.
#' @param allSpeedRatings The master speed rating data frame
#' @param year The year of the current race
#' @return The data frame of updated reference speed ratings with an uncertainty measure taking into account weighting scheme
#' @keywords speed rating, cross country, handicapping
#' @export
#' @examples
#' bootstrap(allSpeedRatings, 2018)

bootstrap <- function(allSpeedRatings, year) {
  require(dplyr)
  require(progress)
  allSpeedRatings <-
    allSpeedRatings %>%
    mutate(week_2 = stringr::str_remove(Week, "Week "),
           week_2 = as.numeric(week_2))
  uniques <- unique(allSpeedRatings[c("Name", "School")])
  counts <- numeric(length(uniques$Name))
  out <- numeric(length(uniques$Name))
  out2 <- numeric(length(uniques$Name))
  years <-numeric(length(uniques$Name))
  pb <- progress_bar$new(total = length(uniques$Name))
  for (i in 1:length(uniques$Name)) {
    individualResults <- allSpeedRatings %>%
      filter(Name == uniques$Name[i]) %>%
      filter(School == uniques$School[i])
    nResults <- length(individualResults$`Speed Rating`)
    if (nResults == 1) {
      out[i] <- individualResults$`Speed Rating`
      out2[i] <- 2.5
      counts[i] <- 1
      years[i] <- max(individualResults$Year)
      pb$tick()
    } else {
      w <- rep(1, nResults)
      for (j in 1 : length(individualResults$Name)) {
        if (individualResults$Year[j] != year) {
          w[j] <- 0.1/(year - individualResults$Year[j])
        } else {
          w[j] <- (3 / max(individualResults$week_2 ^ 3)) * 
            (individualResults$week_2[j] ^ 3)
        }
        if ((individualResults$`Speed Rating`[j] /
             mean(individualResults$`Speed Rating`)) < 0.975) {
          w[j] <- 0.1
        }
      }
      w[which.max(individualResults$`Speed Rating`)] <- 
        w[which.max(individualResults$`Speed Rating`)] + 1
      # w[nResults] <- w[nResults] + 0.5
      B = 1000
      n = 1
      boot.samples = matrix(sample(individualResults$`Speed Rating`, 
                                   size = B * n, 
                                   replace = TRUE,
                                   prob = w),
                            B, 
                            n)
      boot.statistics <- apply(boot.samples, 1, mean)
      boot.mean <- mean(boot.statistics)
      boot.se <- sd(boot.statistics)
      # samples <- sample(individualResults$`Speed Rating`,
      #                   size = 1000,
      #                   replace = TRUE,
      #                   prob = w)
      out[i] <- boot.mean
      out2[i] <- boot.se
      counts[i] <- nResults
      years[i] <- max(individualResults$Year)
      pb$tick()
    }
  }
  returned <- as.data.frame(cbind(uniques[, 1:2], out, out2, counts, years))
  colnames(returned) <- c("name", "school", "refSR", "se", "Number of Races",
                          "Most Recent Year")
  return(returned)
}
