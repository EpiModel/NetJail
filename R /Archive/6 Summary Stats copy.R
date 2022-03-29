#CELL-LEVEL

# degDists <- list()
# for (i in seq_len(nrow(dates))){
#   currentNW <- as.network(network.extract(cDynNW, at = dates$DayNum[i]))
#   #edgeCount <- table(unlist(as.data.frame(currentNW)[, c("tail", "head")]))
#   #edgeCount <- data.frame(id = names(edgeCount), edges = as.integer(edgeCount))
#   dist <- degreedist(currentNW, print = FALSE)
#   #meanDeg <- data.frame(deg = substr(names(dist), 7, 8), num = as.integer(dist))
#
#   name <- paste('DegDist:',dates$Date[i],sep=' ')
#   degDists[[name]] <- dist
# }
# remove(dist, i, name, currentNW)

# Degree distribution at each time point
for (i in seq_len(nrow(dates))){
  currentNW <- as.network(network.extract(cDynNW, at = dates$DayNum[i]))
  dist <- degreedist(currentNW, print = FALSE)
  dist <- data.frame(deg = as.integer(substr(names(dist), 7, 8)),
                     num = as.integer(dist))
  names(dist)[names(dist) == "num"] <- toString(dates$Date[i])
  if (i == 1) {
    degDistsdf <- dist
  } else {
    degDistsdf <- merge(degDistsdf, dist, by = "deg", all = TRUE)
  }
}
remove(dist, i, currentNW)

ll <- lapply(degDistsdf[ , -1], function(x) weighted.mean(x, w = degDistsdf$deg, na.rm = TRUE))

#Histograms!

#Mean degree at each time point
#Mean degree across all data
#Mean degree across the daily subset of data

#Averaged degree distribution: degree0 at time 1 + ... + degree0 at time 25 / 25 days
#Averaged degree distribution for daily subset only


#Using either the initial floor or splitting contacts with different floors...
#Degree distribution by floor at each time point
#Mean degree by floor at each time point
#Mean degree by floor across all data
#Mean degree by floor across the daily subset of data
#Averaged degree distribution for each floor: degree0 at time 1 + ... + degree0 at time 25 / 25 days
#Averaged degree distribution for each floor for daily subset only



# Turnover rates within cells using daily data
# Turnover rates within cells using all data
# Turnover rates within cells by floor using daily data
# Turnover rates within cells by floor using all data



# Turnover rate into and out of jail (using the whole data)
# Turnover rate into and out of jail (using daily data)
# Turnover rate into and out of jail by floor (using the whole data)
# Turnover rate into and out of jail by floor (using daily data)

#By race and by age!

#BLOCK-LEVEL
