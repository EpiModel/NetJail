#CELL-LEVEL

# Degree distribution at each time point
for (i in seq_len(nrow(dates))){
  currentNW <- as.network(network.extract(cDynNW, at = dates$DayNum[i]))
  dist <- degreedist(currentNW, print = FALSE)
  dist <- data.frame(deg = as.integer(substr(names(dist), 7, 8)),
                     num = as.integer(dist))
  names(dist)[names(dist) == "num"] <- toString(dates$Date[i])
  if (i == 1) {
    cDegDists <- dist
  } else {
    cDegDists <- merge(cDegDists, dist, by = "deg", all = TRUE)
  }
}
remove(dist, i, currentNW)

#Histograms:
day <- 1
barplot(cDegDists[, (day + 1)], names.arg = cDegDists$deg, xlab = "Degree",
        ylab = "Frequency",
        main = paste("Degree Distribution on", dates$Date[day], sep=" "))

#Mean degree at each time point
cMeanDeg <- lapply(cDegDists[ , -1], function(x)
  sum(cDegDists$deg * x, na.rm = TRUE)/sum(x, na.rm = TRUE))
cMeanDeg <- do.call(rbind, cMeanDeg)
cMeanDeg <- data.frame(dates$Date, cMeanDeg)
plot(cMeanDeg, xlab = "Date", ylab = "Mean Degree",
     main = "Mean degree over time")

#Mean degree across all data
cOverallMeanDeg <- mean(cMeanDeg$cMeanDeg)

#Mean degree across the daily subset of data
cSubsetMeanDeg <- mean(cMeanDeg$cMeanDeg[12:22])

#Averaged degree distribution
cDegDists$Avg <- rowSums(cDegDists[,-1], na.rm = TRUE) / ncol(cDegDists[,-1])
barplot(cDegDists$Avg, names.arg = cDegDists$deg, xlab = "Degree",
        ylab = "Frequency",
        main = paste("Averaged Degree Distribution (all data)"))

#Averaged degree distribution for daily subset only
cDegDists$Avg_s <- rowSums(cDegDists[,12:22], na.rm = TRUE) /
  ncol(cDegDists[,12:22])
barplot(cDegDists$Avg_s, names.arg = cDegDists$deg, xlab = "Degree",
        ylab = "Frequency",
        main = paste("Averaged Degree Distribution (daily subset)"))












floors <- cDailyLoc
floors$loc <- substr(floors$loc, 0, 1)


currentNW <- as.network(network.extract(cDynNW, at = dates$DayNum[1]))
nodes <- data.frame(id = get.vertex.attribute(currentNW, "vertex.names"),
                    race = get.vertex.attribute(currentNW, "Race"),
                    age = get.vertex.attribute(currentNW, "Age"),
                    floor = floors[floors$Date == dates$Date[1], ]$loc,
                    degree = get_degree(currentNW))




#Degree distribution by floor at each time point
#Mean degree by floor at each time point
#Mean degree by floor across all data
#Mean degree by floor across the daily subset of data
#Averaged degree distribution for each floor
#Averaged degree distribution for each floor for daily subset only



# Turnover rates within cells using daily data
# Turnover rates within cells using all data
# Turnover rates within cells by floor using daily data
# Turnover rates within cells by floor using all data



# Turnover rate into and out of jail (using the whole data)
# Turnover rate into and out of jail (using daily data)
# Turnover rate into and out of jail by floor (using the whole data)
# Turnover rate into and out of jail by floor (using daily data)

#Breakdown by race and by age!




#BLOCK-LEVEL
