library(EpiModel)

#CELL-LEVEL
#Create a static network
initEdges <- cEdges_s[cEdges_s$firstTime == 1, ]
ids_s <- m_stdLoc_s %>% group_by(subsetID) %>%
  summarise (DOB = last(DOB), Gender = first(Gender), Race = last(Race),
             firstDate = min(Date))
ids_s$InitialAge <- as.integer((ids_s$firstDate - ids_s$DOB)/365.25)
ids_s$firstDate <- NULL
cNW_s <- network(initEdges[ , c("head", "tail")], directed = FALSE,
                 vertices = data.frame(name = ids_s$subsetID), multiple = TRUE)
remove(initEdges)

#Set age, race, and gender as attributes
cNW_s <- set.vertex.attribute(cNW_s, "Race", ids_s$Race)
cNW_s <- set.vertex.attribute(cNW_s, "Age", ids_s$InitialAge)
cNW_s <- set.vertex.attribute(cNW_s, "Gender", ids_s$Gender)

#Create a dynamic network
cEdges_s_adj <- transform(cEdges_s, startTime = as.numeric(firstTime))
cEdges_s_adj <- within(cEdges_s_adj, startTime[startCensored == 'Y'] <- -Inf)
cEdges_s_adj <- within(cEdges_s_adj, lastTime[endCensored == 'Y'] <- Inf)
cEdges_s_adj$endTime <- cEdges_s_adj$lastTime + 1
cEdges_s_adj$firstTime <- NULL
cEdges_s_adj$lastTime <- NULL
cDynNW_s <- networkDynamic(cNW_s, edge.spells = cEdges_s_adj[ ,c("startTime",
                                                          "endTime", "tail",
                                                          "head")])
remove(cEdges_s_adj)

#Activate nodes for days they are present in data
m_stdLoc_s <- subset(m_stdLoc, (Date >= as.Date("2022-01-11") &
                                  Date <= as.Date("2022-01-22")))
m_stdLoc_s <- m_stdLoc_s[with(m_stdLoc_s, order(SoNum)), ]
m_stdLoc_s <- m_stdLoc_s %>% group_by(SoNum) %>%
  mutate(subsetID = cur_group_id()) %>% ungroup()
activeDays_s <- m_stdLoc_s[ , c("SoNum", "Date", "subsetID")]
activeDays_s <- activeDays_s %>% group_by(Date) %>%
  mutate(Day = cur_group_id()) %>% ungroup()
activeDays_s <- activeDays_s[with(activeDays_s, order(subsetID, Day)), ]
ind <- which(activeDays_s$subsetID==lag(activeDays_s$subsetID) &
               activeDays_s$Day == (lag(activeDays_s$Day) + 1))
activeDays_s$newStay <- "True"
activeDays_s[ind, ]$newStay <- "False"
activeDays_s$stayCounter <- cumsum(activeDays_s$newStay == "True")
activeDays_s$newStay <- NULL
remove(ind)
activeDays_s <- activeDays_s %>% group_by(stayCounter) %>%
  summarise(subsetID = first(subsetID), firstDay = min(Day),
            lastDay = max(Day))
activeDays_s$stayCounter <- NULL

activeDays_s$firstDay[activeDays_s$firstDay == min(activeDays_s$firstDay) ] <-
  -Inf
activeDays_s$lastDay[activeDays_s$lastDay == max(activeDays_s$lastDay) ] <- Inf
cDynNW_s <- activate.vertices(x = cDynNW_s, onset = activeDays_s$firstDay,
                              terminus = (activeDays_s$lastDay + 1),
                              v = activeDays_s$subsetID)




#BLOCK-LEVEL
#Create a static network
initEdges <- bEdges_s[bEdges_s$firstTime == 1, ]
bNW_s <- network(initEdges[ , c("head", "tail")], directed = FALSE,
                 vertices = data.frame(name = ids_s$subsetID), multiple = TRUE)
remove(initEdges)

#Set age, race, and gender as attributes
bNW_s <- set.vertex.attribute(bNW_s, "Race", ids_s$Race)
bNW_s <-set.vertex.attribute(bNW_s, "Age", ids_s$InitialAge)
bNW_s <- set.vertex.attribute(bNW_s, "Gender", ids_s$Gender)

# #Create a dynamic network
# bEdges_s_adj <- transform(bEdges_s, startTime = as.numeric(firstTime))
# bEdges_s_adj <- within(bEdges_s_adj, startTime[startCensored == 'Y'] <- -Inf)
# bEdges_s_adj <- within(bEdges_s_adj, lastTime[endCensored == 'Y'] <- Inf)
# bEdges_s_adj$endTime <- bEdges_s_adj$lastTime + 1
# bEdges_s_adj$firstTime <- NULL
# bEdges_s_adj$lastTime <- NULL
# bDynNW_s <- networkDynamic(bNW_s, edge.spells = bEdges_s_adj[ ,c("startTime",
#                                                                  "endTime",
#                                                                  "tail",
#                                                                  "head")])
# remove(bEdges_s_adj)
#
# #Activate nodes for days they are present in data
# bDynNW_s <- activate.vertices(x = bDynNW_s, onset = activeDays_subset$firstDay,
#                               terminus = (activeDays_subset$lastDay + 1),
#                               v = activeDays_subset$subsetID)
