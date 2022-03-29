library(EpiModel)

#CELL-LEVEL
#Create a static network
cNW <- network(cEdges[cEdges$firstTime == 1, c("head", "tail")],
               directed = FALSE, vertices = data.frame(name = ids$id))

#Set age, race, and gender as attributes
ids$InitialAge <- as.integer((ids$FirstDate - ids$DOB)/365.25)
cNW <- set.vertex.attribute(cNW, "Race", ids$Race)
cNW <- set.vertex.attribute(cNW, "Age", ids$InitialAge)
cNW <- set.vertex.attribute(cNW, "Gender", ids$Gender)

#Create a dynamic network
cEdges_adj <- transform(cEdges, startTime = as.numeric(firstTime))
cEdges_adj <- within(cEdges_adj, startTime[startCensored == 'Y'] <- -Inf)
cEdges_adj <- within(cEdges_adj, lastTime[endCensored == 'Y'] <- Inf)
cEdges_adj$endTime <- cEdges_adj$lastTime + 1
cEdges_adj$firstTime <- NULL
cEdges_adj$lastTime <- NULL
cDynNW <- networkDynamic(cNW, edge.spells = cEdges_adj[ , c("startTime",
                                                            "endTime",
                                                            "tail", "head")])
remove(cEdges_adj)

#Activate nodes for days they are present in data
activeDays <- m_stdLoc[ , c("SoNum", "Date")]
activeDays <- merge(activeDays, ids[ , c("SoNum", "id")], by = "SoNum")
activeDays <- merge(activeDays, dates, by = "Date")
activeDays <- activeDays[with(activeDays, order(id, DayIndex)), ]
ind <- which(activeDays$id==lag(activeDays$id) &
               activeDays$DayIndex == (lag(activeDays$DayIndex) + 1))
activeDays$newStay <- "True"
activeDays[ind, ]$newStay <- "False"
activeDays$stayCounter <- cumsum(activeDays$newStay == "True")
activeDays$newStay <- NULL
remove(ind)
activeDays <- activeDays %>% group_by(stayCounter) %>%
  summarise(id = first(id), firstDay = min(DayNum), lastDay = max(DayNum))
activeDays$stayCounter <- NULL
activeDays$firstDay[activeDays$firstDay == min(activeDays$firstDay) ] <- -Inf
activeDays$lastDay[activeDays$lastDay == max(activeDays$lastDay) ] <- Inf
cDynNW <- activate.vertices(x = cDynNW, onset = activeDays$firstDay,
                                        terminus = (activeDays$lastDay + 1),
                                        v = activeDays$id)



#BLOCK-LEVEL
#Create a static network
bNW <- network(bEdges[bEdges$firstTime == 1, c("head", "tail")],
               directed = FALSE, vertices = data.frame(name = ids$id))

#Set age, race, and gender as attributes
bNW <- set.vertex.attribute(bNW, "Race", ids$Race)
bNW <- set.vertex.attribute(bNW, "Age", ids$InitialAge)
bNW <- set.vertex.attribute(bNW, "Gender", ids$Gender)

# #Create a dynamic network
# bEdges_adj <- transform(bEdges, startTime = as.numeric(firstTime))
# bEdges_adj <- within(bEdges_adj, startTime[startCensored == 'Y'] <- -Inf)
# bEdges_adj <- within(bEdges_adj, lastTime[endCensored == 'Y'] <- Inf)
# bEdges_adj$endTime <- bEdges_adj$lastTime + 1
# bEdges_adj$firstTime <- NULL
# bEdges_adj$lastTime <- NULL
# bDynNW <- networkDynamic(bNW,
#                          edge.spells = bEdges_adj[ , c("startTime", "endTime",
#                                                        "tail", "head")])
# remove(bEdges_adj)
#
# #Activate nodes for days they are present in data
# bDynNW <- activate.vertices(x = bDynNW, onset = activeDays$firstDay,
#                                         terminus = (activeDays$lastDay + 1),
#                                         v = activeDays$id)
