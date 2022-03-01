library(EpiModel)

#CELL-LEVEL
#Create a static network
starting_edges <- edges_cell[edges_cell$startTime == 1, ]
cellNetwork <- network(starting_edges[ , c("head", "tail")],
                       directed = FALSE,
                       vertices = data.frame(name = seq_len(nrow(counts))),
                       multiple = TRUE)

#Set age, race, and gender as attributes
counts$InitialAge <-
  as.integer((counts$FirstDate - counts$DOB)/365.25)
cellNetwork <- set.vertex.attribute(cellNetwork, "Race", counts$Race)
cellNetwork <-
  set.vertex.attribute(cellNetwork, "Age", counts$InitialAge)
cellNetwork <- set.vertex.attribute(cellNetwork, "Gender", counts$Gender)

#Create a dynamic network
#Note: Need time of dissolution (not last timestep the edge is active)
edges_cell_adj <- transform(edges_cell, startTime = as.numeric(startTime))
edges_cell_adj <- within(edges_cell_adj, startTime[startCensored == 'Y']
                            <- -Inf)
edges_cell_adj <- within(edges_cell_adj, endTime[endCensored == 'Y']
                            <- Inf)
edges_cell_adj$endTime <- edges_cell_adj$endTime + 1
cellDynamicNetwork <- networkDynamic(cellNetwork,
                                  edge.spells = edges_cell_adj[ ,
                                    c("startTime", "endTime", "tail", "head")])

#Activate cells between the first and last day they are present in data
#Gaps are currently not considered
activeDays <- counts[, c("id", "FirstDate", "LastDate")]
activeDays <- merge(activeDays, dates, by.x = "FirstDate", by.y = "Date")
names(activeDays)[names(activeDays) == 'Day'] <- 'FirstDay'
activeDays <- merge(activeDays, dates, by.x = "LastDate", by.y = "Date")
names(activeDays)[names(activeDays) == 'Day'] <- 'LastDay'
activeDays <- activeDays[, c("id", "FirstDay", "LastDay")]
activeDays <- activeDays[with(activeDays, order(id)), ]
activeDays$FirstDay[activeDays$FirstDay ==
                           min(activeDays$FirstDay) ] <- -Inf
activeDays$LastDay[activeDays$LastDay ==
                      max(activeDays$LastDay) ] <- Inf

cellDynamicNetwork <- activate.vertices(x = cellDynamicNetwork,
                                        onset = activeDays$FirstDay,
                                        terminus = (activeDays$LastDay + 1),
                                        v = activeDays$id)
