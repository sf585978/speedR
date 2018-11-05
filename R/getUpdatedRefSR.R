#' Get Updating Reference Speed Rating
#'
#' This function inputs a master file of speed ratings for a year and a list of races and returns a list of updated reference speed ratings for the runners contained in the master list.
#' @param allSpeedRatings The master speed rating data frame
#' @param race The raceID for the newest race
#' @return updatedReferences The data frame of updated reference speed ratings
#' @keywords speed rating, cross country, handicapping
#' @export
#' @examples
#' getUpdatedRefSR(allSpeedRatings, c("mOswego17", "mWilliams17", "mGeneseo17"))

getUpdatedRefSR <- function(allSpeedRatings, race) {
  require(dplyr)
  latestResults <- allSpeedRatings %>%
    filter(Race == race)
  uniques <- unique(latestResults[c("Name", "School")])
  counts <- numeric(length(uniques$Name))
  out <- numeric(length(uniques$Name))
  for (i in 1:length(uniques$Name)) {
    individualResults <- allSpeedRatings %>%
      filter(Name == uniques$Name[i],
             School == uniques$School[i])
    nResults <- length(individualResults$`Speed Rating`)
    w <- rep(1, nResults)
    # w[which(individualResults$`Speed Rating` ==
    #           max(individualResults$`Speed Rating`))] <- nResults
    for (j in 1 : length(individualResults$Name)) {
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
    w[nResults] <- w[nResults] + 0.5
    out[i] <- weighted.mean(individualResults$`Speed Rating`, w = w)
    counts[i] <- nResults
  }
  updatedReferences <- as.data.frame(cbind(uniques, out, counts))
  colnames(updatedReferences) <- c("name", "school", "refSR", "Number of Races")
  return(updatedReferences)
}
