#% of Edges within same age group...

table2 <- data.frame(category = c("Overall", "Under 20", "20 - 29", "30 - 39", "40 - 49", "50 - 59", "60+", "Black", "Other", "White", "1", "2", "3", "4", "5", "6", "7"),
                     cPropSameAge.Jan = NA, cPropSameAge.Apr = NA, cPropSameAge.All = NA, bPropSameAge.Jan = NA, bPropSameAge.Apr = NA, bPropSameAge.All = NA,
                     cPropSameAge.Jan2 = NA, cPropSameAge.Apr2 = NA, cPropSameAge.All2 = NA, bPropSameAge.Jan2 = NA, bPropSameAge.Apr2 = NA, bPropSameAge.All2 = NA)


# Approach 1 --------------------------------------------------------------

mixingSetup <- function(Edges){

  temp1 <- Edges[ ,c("head", "tail", "onset", "terminus")]
  temp2 <- Edges[ ,c("head", "tail", "onset", "terminus")]
  temp2[1:2] <- temp2[2:1]

  doubledEdges <- rbind(temp1, temp2)
  remove(temp1, temp2)
  doubledEdges <- merge(doubledEdges, ids[, c("id", "InitialAge")],
                        by.x = "head", by.y = "id")
  doubledEdges$head.age <- cut(doubledEdges$InitialAge,
                               breaks=c(0, 20, 30, 40, 50, 60, 100),
                               right = FALSE, labels = c("Under 20", "20 - 29",
                                                         "30 - 39", "40 - 49",
                                                         "50 - 59", "60+"))
  doubledEdges$InitialAge <- NULL
  doubledEdges <- merge(doubledEdges, ids[, c("id", "InitialAge")],
                        by.x = "tail", by.y = "id")
  doubledEdges$tail.age <- cut(doubledEdges$InitialAge,
                               breaks=c(0, 20, 30, 40, 50, 60, 100),
                               right = FALSE, labels = c("Under 20", "20 - 29",
                                                         "30 - 39", "40 - 49",
                                                         "50 - 59", "60+"))
  doubledEdges$InitialAge <- NULL

  doubledEdges <- merge(doubledEdges, ids[, c("id", "Race")], by.x = "head", by.y = "id")
  doubledEdges[doubledEdges$Race != "White" & doubledEdges$Race != "Black", ]$Race <- "Other"
  names(doubledEdges)[names(doubledEdges) == 'Race'] <- 'head.race'

  locs <- merge(m_stdLoc[, c("SoNum", "Date", "Floor")], ids[, c("SoNum", "id")])
  locs <- merge(locs, dates[, c("Date", "DayNum")])
  locs[locs$DayNum == 1, "DayNum"] <- -Inf

  doubledEdges <- merge(doubledEdges, locs, by.x = c("head", "onset"), by.y = c("id", "DayNum"))

  return(doubledEdges)
}

mixingTable2 <- function(column, doubledEdges) {
  overall <- vector(mode="numeric", length=nrow(dates))
  for (i in seq_len(nrow(dates))) {
    currTime <- doubledEdges[doubledEdges$onset <= dates$DayNum[i] &
                               doubledEdges$terminus >= (dates$DayNum[i] + 1), ]
    currTime <- currTime %>% group_by(head.age, tail.age, .drop = FALSE) %>%
      summarise(numEdges = n(), .groups = "drop_last")
    overall[i] <- sum(currTime[currTime$head.age == currTime$tail.age, "numEdges"]) / sum(currTime$numEdges)
  }
  table2[table2$category == "Overall", column] <- round(mean(overall), 4) * 100

  ages <- table2$category[c(2:7)]
  for (j in seq_along(ages)){
    temp <- vector(mode="numeric", length=nrow(dates))
    for (i in seq_len(nrow(dates))) {
      currTime <- doubledEdges[doubledEdges$onset <= dates$DayNum[i] &
                                 doubledEdges$terminus >= (dates$DayNum[i] + 1), ]
      currTime <- currTime %>% group_by(head.age, tail.age, .drop = FALSE) %>%
        summarise(numEdges = n(), .groups = "drop_last")
      currTime$head.age <- as.character(currTime$head.age)
      temp[i] <- sum(currTime[currTime$head.age == currTime$tail.age & currTime$head.age == ages[j], "numEdges"]) / sum(currTime[currTime$head.age == ages[j], "numEdges"])
    }
    table2[table2$category == ages[j], column] <- round(mean(temp), 4) * 100
  }

  races <- table2$category[c(8:10)]
  for (j in seq_along(races)){
    temp <- vector(mode="numeric", length=nrow(dates))
    for (i in seq_len(nrow(dates))) {
      currTime <- doubledEdges[doubledEdges$onset <= dates$DayNum[i] &
                                 doubledEdges$terminus >= (dates$DayNum[i] + 1), ]
      currTime <- currTime %>% group_by(head.race, head.age, tail.age, .drop = FALSE) %>%
        summarise(numEdges = n(), .groups = "drop_last")
      temp[i] <- sum(currTime[currTime$head.age == currTime$tail.age & currTime$head.race == races[j], "numEdges"]) / sum(currTime[currTime$head.race == races[j], "numEdges"])
    }
    table2[table2$category == races[j], column] <- round(mean(temp), 4) * 100
  }

  floors <- table2$category[c(11:17)]
  for (j in seq_along(floors)){
    temp <- vector(mode="numeric", length=nrow(dates))
    for (i in seq_len(nrow(dates))) {
      currTime <- doubledEdges[doubledEdges$onset <= dates$DayNum[i] &
                                 doubledEdges$terminus >= (dates$DayNum[i] + 1), ]
      currTime <- currTime %>% group_by(Floor, head.age, tail.age, .drop = FALSE) %>%
        summarise(numEdges = n(), .groups = "drop_last")
      temp[i] <- sum(currTime[currTime$head.age == currTime$tail.age & currTime$Floor == floors[j], "numEdges"]) / sum(currTime[currTime$Floor == floors[j], "numEdges"])
    }
    table2[table2$category == floors[j], column] <- round(mean(temp), 4) * 100
  }

  return(table2)
}

#JANUARY
filenames <- list.files("data/output/edges/Jan", pattern="*.RData",
                        full.names=TRUE)
lapply(filenames,load,.GlobalEnv)

cDoubledEdges <- mixingSetup(cEdges)
table2 <- mixingTable2("cPropSameAge.Jan", cDoubledEdges)

bDoubledEdges <- mixingSetup(bEdges)
table2 <- mixingTable2("bPropSameAge.Jan", bDoubledEdges)

rm(list=setdiff(ls(), c("table1", "table2", lsf.str())))

#APRIL
filenames <- list.files("data/output/edges/Apr", pattern="*.RData",
                        full.names=TRUE)
lapply(filenames,load,.GlobalEnv)

cDoubledEdges <- mixingSetup(cEdges)
table2 <- mixingTable2("cPropSameAge.Apr", cDoubledEdges)

bDoubledEdges <- mixingSetup(bEdges)
table2 <- mixingTable2("bPropSameAge.Apr", bDoubledEdges)

rm(list=setdiff(ls(), c("table1", "table2", lsf.str())))

#ALL
filenames <- list.files("data/output/edges/All", pattern="*.RData",
                        full.names=TRUE)
lapply(filenames,load,.GlobalEnv)

cDoubledEdges <- mixingSetup(cEdges)
table2 <- mixingTable2("cPropSameAge.All", cDoubledEdges)

bDoubledEdges <- mixingSetup(bEdges)
table2 <- mixingTable2("bPropSameAge.All", bDoubledEdges)

rm(list=setdiff(ls(), c("table1", "table2")))



# Approach 2 --------------------------------------------------------------

mixingSetup2 <- function(edges){

  edges <- merge(edges, ids[, c("id", "InitialAge")], by.x = "head", by.y = "id")
  edges$head.age <- cut(edges$InitialAge, breaks=c(0, 20, 30, 40, 50, 60, 100),
                               right = FALSE, labels = c("Under 20", "20 - 29", "30 - 39", "40 - 49", "50 - 59", "60+"))
  edges$InitialAge <- NULL
  edges <- merge(edges, ids[, c("id", "InitialAge")], by.x = "tail", by.y = "id")
  edges$tail.age <- cut(edges$InitialAge, breaks=c(0, 20, 30, 40, 50, 60, 100),
                               right = FALSE, labels = c("Under 20", "20 - 29", "30 - 39", "40 - 49", "50 - 59", "60+"))
  edges$InitialAge <- NULL

  edges <- merge(edges, ids[, c("id", "Race")], by.x = "head", by.y = "id")
  edges[edges$Race != "White" & edges$Race != "Black", ]$Race <- "Other"
  names(edges)[names(edges) == 'Race'] <- 'head.race'

  edges <- merge(edges, ids[, c("id", "Race")], by.x = "tail", by.y = "id")
  edges[edges$Race != "White" & edges$Race != "Black", ]$Race <- "Other"
  names(edges)[names(edges) == 'Race'] <- 'tail.race'

  locs <- merge(m_stdLoc[, c("SoNum", "Date", "Floor")], ids[, c("SoNum", "id")])
  locs <- merge(locs, dates[, c("Date", "DayNum")])
  locs[locs$DayNum == 1, "DayNum"] <- -Inf

  edges <- merge(edges, locs, by.x = c("head", "onset"), by.y = c("id", "DayNum"))

  return(edges)
}

mixingTable2v2 <- function(column, edges) {
  overall <- vector(mode="numeric", length=nrow(dates))
  for (i in seq_len(nrow(dates))) {
    currTime <- edges[edges$onset <= dates$DayNum[i] & edges$terminus >= (dates$DayNum[i] + 1), ]
    currTime <- currTime %>% group_by(head.age, tail.age, .drop = FALSE) %>% summarise(numEdges = n(), .groups = "drop_last")
    overall[i] <- sum(currTime[currTime$head.age == currTime$tail.age, "numEdges"]) / sum(currTime$numEdges)
  }
  table2[table2$category == "Overall", column] <- round(mean(overall), 4) * 100

  ages <- table2$category[c(2:7)]
  for (j in seq_along(ages)){
    temp <- vector(mode="numeric", length=nrow(dates))
    for (i in seq_len(nrow(dates))) {
      currTime <- edges[edges$onset <= dates$DayNum[i] & edges$terminus >= (dates$DayNum[i] + 1), ]
      currTime <- currTime[currTime$head.age == ages[j] | currTime$tail.age == ages[j], ]
      currTime <- currTime %>% group_by(head.age, tail.age, .drop = FALSE) %>% summarise(numEdges = n(), .groups = "drop_last")
      currTime$head.age <- as.character(currTime$head.age)
      temp[i] <- sum(currTime[currTime$head.age == currTime$tail.age, "numEdges"]) / sum(currTime$numEdges)
    }
    table2[table2$category == ages[j], column] <- round(mean(temp), 4) * 100
  }

  races <- table2$category[c(8:10)]
  for (j in seq_along(races)){
    temp <- vector(mode="numeric", length=nrow(dates))
    for (i in seq_len(nrow(dates))) {
      currTime <- edges[edges$onset <= dates$DayNum[i] & edges$terminus >= (dates$DayNum[i] + 1), ]
      currTime <- currTime[currTime$head.race == races[j] | currTime$tail.race == races[j], ]
      currTime <- currTime %>% group_by(head.age, tail.age, .drop = FALSE) %>% summarise(numEdges = n(), .groups = "drop_last")
      temp[i] <- sum(currTime[currTime$head.age == currTime$tail.age, "numEdges"]) / sum(currTime$numEdges)
    }
    table2[table2$category == races[j], column] <- round(mean(temp), 4) * 100
  }

  floors <- table2$category[c(11:17)]
  for (j in seq_along(floors)){
    temp <- vector(mode="numeric", length=nrow(dates))
    for (i in seq_len(nrow(dates))) {
      currTime <- edges[edges$onset <= dates$DayNum[i] & edges$terminus >= (dates$DayNum[i] + 1), ]
      currTime <- currTime %>% group_by(Floor, head.age, tail.age, .drop = FALSE) %>% summarise(numEdges = n(), .groups = "drop_last")
      temp[i] <- sum(currTime[currTime$head.age == currTime$tail.age & currTime$Floor == floors[j], "numEdges"]) / sum(currTime[currTime$Floor == floors[j], "numEdges"])
    }
    table2[table2$category == floors[j], column] <- round(mean(temp), 4) * 100
  }

  return(table2)
}

#JANUARY
filenames <- list.files("data/output/edges/Jan", pattern="*.RData", full.names=TRUE)
lapply(filenames,load,.GlobalEnv)

edges <- mixingSetup2(cEdges)
table2 <- mixingTable2v2("cPropSameAge.Jan2", edges)

edges <- mixingSetup2(bEdges)
table2 <- mixingTable2v2("bPropSameAge.Jan2", edges)

rm(list=setdiff(ls(), c("table1", "table2", lsf.str())))

#APRIL
filenames <- list.files("data/output/edges/Apr", pattern="*.RData",
                        full.names=TRUE)
lapply(filenames,load,.GlobalEnv)

edges <- mixingSetup2(cEdges)
table2 <- mixingTable2v2("cPropSameAge.Apr2", edges)

edges <- mixingSetup2(bEdges)
table2 <- mixingTable2v2("bPropSameAge.Apr2", edges)

rm(list=setdiff(ls(), c("table1", "table2", lsf.str())))

#ALL
filenames <- list.files("data/output/edges/All", pattern="*.RData",
                        full.names=TRUE)
lapply(filenames,load,.GlobalEnv)

edges <- mixingSetup2(cEdges)
table2 <- mixingTable2v2("cPropSameAge.All2", edges)

edges <- mixingSetup2(bEdges)
table2 <- mixingTable2v2("bPropSameAge.All2", edges)

rm(list=setdiff(ls(), c("table1", "table2")))

