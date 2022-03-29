library(EpiModel)

#CELL-LEVEL

#Create a df that lists out known spells in each location for each id
#People in the same cell in consecutive rosters are assumed to have stayed in
#the same cell between those two rosters
cLocs <- merge(cDailyLoc, dates, by = "Date")
cLocs <- cLocs[with(cLocs, order(id, DayIndex)), ]
ind <- which(cLocs$id==lag(cLocs$id) & cLocs$loc==lag(cLocs$loc) &
               cLocs$DayIndex==(lag(cLocs$DayIndex) + 1))
cLocs$newLoc <- "True"
cLocs[ind, ]$newLoc <- "False"
cLocs$locCounter <- cumsum(cLocs$newLoc == "True")
cLocs$newLoc <- NULL
remove(ind)
cLocs <- cLocs %>% group_by(locCounter) %>%
  summarise(id = first(id), loc = first(loc), onset = min(DayNum),
            terminus = max(DayNum) + 1)
cLocs$locCounter <- NULL
cLocs$onset[cLocs$onset == min(cLocs$onset) ] <- -Inf
cLocs$terminus[cLocs$terminus == max(cLocs$terminus) ] <- Inf
cLocs <- transform(cLocs, id = as.numeric(id))

#Create a df of spells of presence in jail for each id
#People who appear in consecutive rosters are assumed to have been in the jail
#between those two rosters (even if their cell changed)
activeDays <- merge(cDailyLoc, dates, by = "Date")
activeDays <- activeDays[with(activeDays, order(id, DayIndex)), ]
ind <- which(activeDays$id==lag(activeDays$id) &
            activeDays$DayIndex == (lag(activeDays$DayIndex) + 1))
activeDays$newStay <- "True"
activeDays[ind, ]$newStay <- "False"
activeDays$stayCounter <- cumsum(activeDays$newStay == "True")
activeDays$newStay <- NULL
remove(ind)
activeDays <- activeDays %>% group_by(stayCounter) %>%
  summarise(id = first(id), onset = min(DayNum), terminus = max(DayNum) + 1)
activeDays$stayCounter <- NULL
activeDays$onset[activeDays$onset == min(activeDays$onset) ] <- -Inf
activeDays$terminus[activeDays$terminus == max(activeDays$terminus) ] <- Inf

#Adjust df of edges
cEdges <- transform(cEdges, onset = as.numeric(firstTime))
cEdges <- within(cEdges, onset[startCensored == 'Y'] <- -Inf)
cEdges <- within(cEdges, lastTime[endCensored == 'Y'] <- Inf)
cEdges$terminus <- cEdges$lastTime + 1
cEdges$firstTime <- NULL
cEdges$lastTime <- NULL

#Create a dynamic network
cNW <- network(cEdges[cEdges$onset == -Inf, c("head", "tail")],
               directed = FALSE, vertices = data.frame(name = ids$id))
cDynNW <- networkDynamic(base.net = cNW,
                         vertex.spells =
                            cLocs[, c("onset", "terminus", "id", "loc")],
                          edge.spells =
                            cEdges[ , c("onset", "terminus", "tail", "head")],
                          create.TEAs = TRUE,
                          vertex.TEA.names = c("floor"))

#Set vertex attributes
ids$InitialAge <- as.integer((ids$FirstDate - ids$DOB)/365.25)
cDynNW <- set.vertex.attribute(cDynNW, "Race", ids$Race)
cDynNW <- set.vertex.attribute(cDynNW, "Age", ids$InitialAge)
cDynNW <- set.vertex.attribute(cDynNW, "Gender", ids$Gender)

#Activate vertices
cDynNW <- activate.vertices(x = cDynNW, onset = activeDays$onset,
                            terminus = activeDays$terminus,
                            v = activeDays$id)

#Overall degree distribution at each time point
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
day <- 1 #Day index
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

#Degree distribution by attribute: floor, age, race
for (i in seq_len(nrow(dates))){
  currNW <- as.network(network.extract(cDynNW, at = dates$DayNum[i]))
  currNodes <- data.frame(id = get.vertex.attribute(currNW, "vertex.names"),
                          race = get.vertex.attribute(currNW, "Race"),
                          age = get.vertex.attribute(currNW, "Age"),
                          floor = get.vertex.attribute.active(currNW, "floor",
                                                        at = dates$DayNum[i]),
                          degree = get_degree(currNW))
  currNodes$floor <- substr(currNodes$floor, 0, 1)

  dist_f <- currNodes %>% group_by(floor, degree) %>% summarise(count = n())
  dist_r <- currNodes %>% group_by(race, degree) %>% summarise(count = n())
  dist_a <- currNodes %>% group_by(age, degree) %>% summarise(count = n())

  names(dist_f)[names(dist_f) == "count"] <- toString(dates$Date[i])
  names(dist_r)[names(dist_r) == "count"] <- toString(dates$Date[i])
  names(dist_a)[names(dist_a) == "count"] <- toString(dates$Date[i])

  if (i == 1){
    cDegDists_floor <- dist_f
    cDegDists_race <- dist_r
    cDegDists_age <- dist_a
  } else {
    cDegDists_floor <- merge(cDegDists_floor, dist_f, by = c("floor", "degree"),
                             all = TRUE)
    cDegDists_race <- merge(cDegDists_race, dist_r, by = c("race", "degree"),
                            all = TRUE)
    cDegDists_age <- merge(cDegDists_age, dist_a, by = c("age", "degree"),
                           all = TRUE)
  }
}
remove(dist_f, dist_r, dist_a, currNW, currNodes)

#Histograms of degree distribution by floor:
day <- 2 #day index (not day number)
floor <- 3
barplot(cDegDists_floor[cDegDists_floor$floor == floor, ][, (day + 2)],
        names.arg = cDegDists_floor[cDegDists_floor$floor == floor, ]$degree,
        xlab = "Degree", ylab = "Frequency",
        main = paste("Degree Distribution on", dates$Date[day], "on Floor",
                     floor, sep=" "))

#Histograms of degree distribution by race:
day <- 3 #day index (not day number)
race <- "Asian"
barplot(cDegDists_race[cDegDists_race$race == race, ][, (day + 2)],
        names.arg = cDegDists_race[cDegDists_race$race == race, ]$degree,
        xlab = "Degree", ylab = "Frequency",
        main = paste("Degree Distribution on", dates$Date[day], "for",
                     race, "Residents", sep=" "))

#Histograms of degree distribution by age:
day <- 2 #day index (not day number)
age <- 17
barplot(cDegDists_age[cDegDists_age$age == age, ][, (day + 2)],
        names.arg = cDegDists_age[cDegDists_age$age == age, ]$degree,
        xlab = "Degree", ylab = "Frequency",
        main = paste("Degree Distribution on", dates$Date[day],
                     "for Residents aged", age, sep=" "))

#Mean degree by attribute at each time point
#Mean degree by attribute across all data
#Mean degree by attribute across the daily subset of data

#Turnover rates within cells using daily data
#Turnover rates within cells using all data
#Turnover rates within cells by attribute using daily data
#Turnover rates within cells by attribute using all data

#Turnover rate into and out of jail (using the whole data)
#Turnover rate into and out of jail (using daily data)
#Turnover rate into and out of jail by attribute (using the whole data)
#Turnover rate into and out of jail by attribute (using daily data)

#Validate all of the above

#Create write-up
