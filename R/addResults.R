#' Add Results to Master Data
#' 
#' This function adds race results to the master results table.
#' @param file The name of the individual race results .csv file
#' @keywords results, cross country
#' @export
#' @examples
#' addResults("Data/Results/mRegionals15.csv")

addResults <- function(file) {
  library(readr)
  newResults <- read_csv()
  write.table(newResults, file = "Data/Results/results.csv", sep = ",", 
              col.names = FALSE, append = TRUE)
}
