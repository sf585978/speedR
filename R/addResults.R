#' Add Results to Master Data
#' 
#' This function adds race results to the master results table.
#' @param file The name of the individual race results .csv file
#' @keywords results, cross country
#' @export
#' @examples
#' addResults("Data/Results/mRegionals15.csv")

addResults <- function(file) {
  for (i in 1:length(file)) {
    newResults <- readResults(file[i])
    raceID <- unique(newResults$raceID)
    if(file.exists("Data/Results/results.csv")) {
      results <- suppressWarnings(suppressMessages(
        read_csv("Data/Results/results.csv")))
      if(raceID %in% unique(results$raceID)) {
        message("The race ID is already included in master results.")
      } else {
        write.table(newResults, file = "Data/Results/results.csv", sep = ",", 
                    col.names = FALSE, append = TRUE) 
        message(paste("Race ", raceID, " has been added to master results.", 
                      sep = ""))
      }
    } else {
      write.table(newResults, file = "Data/Results/results.csv", sep = ",", 
                  col.names = NA)
      message("No master results were found. A new master results file has been created.")
    }
  }
}
