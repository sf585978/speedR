#' Save Speed Ratings
#' 
#' This function saves speed rating output to a designated data folder.
#' @param speedRankings A list of speed rating data frames
#' @param location The folder destination you want to save your output
#' @keywords speed rating, cross crountry, handicapping
#' @export
#' @examples
#' addSpeedRatings(speedRankings)

addSpeedRatings <- function(speedRankings, location = "Data/Speed Ratings/") {
  filenames<-paste0(location,names(speedRankings), ".csv")
  Map(write.csv,speedRankings,filenames)
}
