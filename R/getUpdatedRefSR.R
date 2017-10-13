getUpdatedRefSR <- function(allSpeedRatings, races) {
  nameSchool <- unique(allSpeedRatings[c("Name", "School")])
  out <- numeric(nrow(unique(allSpeedRatings[c("Name", "School")])))
  for (i in 1:nrow(unique(allSpeedRatings[c("Name", "School")]))) {
    individualResults <- allSpeedRatings %>%
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
    out.df <- as.data.frame(cbind(nameSchool, out))
    colnames(out.df) <- c("name", "school", "refSR")
    return(out.df)
}
