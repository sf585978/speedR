#' Get Updating Reference Speed Rating
#'
#' This function inputs a master file of speed ratings for a year and a list of races and returns a list of updated reference speed ratings for the runners contained in the master list.
#' @param allSpeedRatings The master speed rating data frame
#' @param races The list of races in chronological order
#' @return updatedReferences The data frame of updated reference speed ratings
#' @keywords speed rating, cross country, handicapping
#' @export
#' @examples
#' getUpdatedRefSR(allSpeedRatings, c("mOswego17", "mWilliams17", "mGeneseo17"))

getUpdatedRefSR <- function(allSpeedRatings, races, updatedReference) {
  require(dplyr)
  runners2beUpdated <- allSpeedRatings %>%
    filter(Race == races[length(races)])
  allSpeedRatingsFilter <- allSpeedRatings %>%
    filter(Name %in% runners2beUpdated$Name)
  nameSchool <- unique(allSpeedRatingsFilter[c("Name", "School")])
  out <- numeric(nrow(unique(allSpeedRatingsFilter[c("Name", "School")])))
  for (i in 1:nrow(unique(allSpeedRatingsFilter[c("Name", "School")]))) {
    individualResults <- allSpeedRatingsFilter %>%
      filter(Name == nameSchool$Name[i]) %>%
      filter(School == nameSchool$School[i])
    nResults <- length(individualResults$`Speed Rating`)
    if (nResults > 2) {
      numerator <- sum(individualResults$`Speed Rating`) +
        max(individualResults$`Speed Rating`) +
        0.5 * individualResults$`Speed Rating`[which(individualResults$Race ==
                                                       races[max(which(races %in%
                                                                         individualResults$Race))])]
      denominator <- nResults + 1.5
      out[i] <- numerator / denominator
    } else {
      out[i] <- mean(individualResults$`Speed Rating`)
    }
  }
    updatedReferences <- as.data.frame(cbind(nameSchool, out))
    colnames(updatedReferences) <- c("name", "school", "refSR")
    updatedReference <- updatedReference %>%
      anti_join(updatedReferences, by = c("name", "school"))
    updatedReference <- rbind(updatedReference, updatedReferences)
    return(updatedReferences)
}
