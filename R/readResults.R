#' Read Race Results
#' 
#' Load and clean race results data in .csv format.
#' @param file The location of individual race results in .csv format
#' @return A tibble of race results with incomplete rows and extra columns removed.
#' @export
#' @examples
#' readResults("Data/Results/mRowanV15.csv")

readResults <- function(file) {
  require(readr)
  results <- suppressWarnings(suppressMessages(read_csv(file)))
  results <- Filter(function(x)!all(is.na(x)), results)
  results <- results[complete.cases(results[,1]), ]
  results <- results[, !(names(results) %in% c("time"))]
  return(results)
}
