#' Get Updating Reference Speed Rating
#'
#' This function inputs a master file of speed ratings and references and returns a list of updated reference speed ratings for the runners contained in the master list.
#' @param allSpeedRatings The master speed rating data frame
#' @param updatedReference The master updated reference data frame
#' @param race The raceID for the newest race
#' @param year The year of the current race
#' @return updatedReferences The data frame of updated reference speed ratings
#' @keywords speed rating, cross country, handicapping
#' @export
#' @examples
#' getUpdatedRefSR(allSpeedRatings, c("mOswego17", "mWilliams17", "mGeneseo17"))

getUpdatedRefSR <- function(allSpeedRatings, updatedReference, race, year) {
  require(dplyr)
  require(progress)
  uniques <- allSpeedRatings %>%
    filter(Race == race)
  uniques <- unique(uniques[c("Name", "School", "Year")])
  counts <- numeric(length(uniques$Name))
  out <- numeric(length(uniques$Name))
  years <-numeric(length(uniques$Name))
  pb <- progress_bar$new(total = length(uniques$Name))
  for (i in 1:length(uniques$Name)) {
    individualResults <- allSpeedRatings %>%
      filter(Name == uniques$Name[i]) %>%
      filter(School == uniques$School[i])
    nResults <- length(individualResults$`Speed Rating`)
    w <- rep(1, nResults)
    # w[which(individualResults$`Speed Rating` ==
    #           max(individualResults$`Speed Rating`))] <- nResults
    for (j in 1 : length(individualResults$Name)) {
      if (individualResults$Year != year) {
        w[j] <- 0.25
      } else {
        if (individualResults$Week[i] %in% c("Week 1", "Week 2", "Week 3",
                                             "Preseason")) {
          w[j] <- 0.5
        }
        if (individualResults$Week[i] %in% c("Week 9", "Week 10", "Week 11",
                                             "Week 12")) {
          w[j] <- 1.5
        }
        if ((individualResults$`Speed Rating`[j] /
             mean(individualResults$`Speed Rating`)) > 1.1 |
            (individualResults$`Speed Rating`[j] /
             mean(individualResults$`Speed Rating`)) < 0.9) {
          w[j] <- w[j] - 0.25
        }
      }
    }
    w[nResults] <- w[nResults] + 0.5
    out[i] <- weighted.mean(individualResults$`Speed Rating`, w = w)
    counts[i] <- nResults
    years[i] <- max(individualResults$Year)
    pb$tick()
  }
  updatedReferences <- as.data.frame(cbind(uniques, out, counts, years))
  colnames(updatedReferences) <- c("name", "school", "refSR", "Number of Races",
                                   "Most Recent Year")

  notUpdatedReferences <- updatedReference %>%
    anti_join(updatedReferences, by = c("name" = "name", "school" = "school"))

  updatedReferences <- as.data.frame(rbind(updatedReferences,
                                           notUpdatedReferences))

  return(updatedReferences)
}
