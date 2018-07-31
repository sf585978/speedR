#' Estimate Speed Ratings
#'
#' This function estimates speed ratings for a course with a course correction
#' and returns an easy to interpret data frame of results with the estimated
#' speed ratings.
#' @param race The race you would like to calculate speed ratings for
#' @param results The master results data frame
#' @param courseCorrections The master course corrections data frame
#' @param alpha The number of seconds equal to one speed point
#' @param beta The centering parameter
#' @param week The week of the race
#' @keywords speed rating, cross crountry, handicapping
#' @export
#' @examples
#' estimateSpeedRankings("9349", results, courseCorrections)


estimateSpeedRankings <- function(race,
                                  results,
                                  courseCorrections,
                                  alpha = 4.4,
                                  beta = 2355, conversion = 1,
                                  week, ref = ur) {
  require(dplyr)
  # gamma <- courseCorrections$gamma[which(courseCorrections$raceID == race)]
  # x <- results$seconds[which(results$raceID == race)]
  # runners <- results$name[which(results$raceID == race)]
  # schools  <- results$school[which(results$raceID == race)]
  gamma <- courseCorrections %>%
    filter(raceID == race) %>%
    select(gamma)
  x <- results %>%
    filter(raceID == race) %>%
    select(seconds) %>%
    as.vector()
  x <- x * conversion
  runners <- results %>%
    filter(raceID == race) %>%
    select(name) %>%
    as.vector()
  schools <- results %>%
    filter(raceID == race) %>%
    select(school) %>%
    as.vector()
  raceID <- rep(race, length(x))
  weekID <- rep(week, length(x))
  obj <- as.data.frame(cbind(runners, schools, x, raceID,
                             SR_CourseCorrection(x,
                                                 alpha, beta,
                                                 gamma), weekID))
  colnames(obj) <- c("Name", "School", "Seconds", "Race", "Speed Rating",
                     "Week")
  if (missing(ref)) {
    return(obj)
  } else {
    obj <- obj %>%
      left_join(ref, by = c("Name" = "name")) %>%
      mutate(Difference = `Speed Rating` - refSR) %>%
      select(Name, School, Seconds, Race, `Speed Rating`, Difference)
    return(obj)
  }
}
