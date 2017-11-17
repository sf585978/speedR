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

getUpdatedRefSR <- function(allSpeedRatings, races) {
  uniques <- unique(allSpeedRatings[c("Name", "School")])
  out <- numeric(length(uniques$Name))
  for (i in 1:length(uniques$Name)) {
    individualResults <- allSpeedRatings %>%
      filter(Name == uniques$Name[i]) %>%
      filter(School == uniques$School[i])
    nResults <- length(individualResults$`Speed Rating`)
    w <- rep(1, nResults)
    w[which(individualResults$`Speed Rating` ==
              max(individualResults$`Speed Rating`))] <- nResults
    w[nResults] <- w[nResults] + ((1 / 3) * nResults)
    out[i] <- weighted.mean(individualResults$`Speed Rating`, w = w)
  }
  updatedReferences <- as.data.frame(cbind(uniques, out))
  colnames(updatedReferences) <- c("name", "school", "refSR")
  return(updatedReferences)
}
