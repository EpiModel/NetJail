
# Basic descriptive stats of jail and residents ---------------------------

formatTable1 <- function() {
  ids$InitialAge <- cut(ids$InitialAge, breaks=c(0, 20, 30, 40, 50, 60, 100),
                        right = FALSE, labels = c("Under 20", "20 - 29",
                                                  "30 - 39", "40 - 49",
                                                  "50 - 59", "60+"))
  ids[ids$Race != "White" & ids$Race != "Black", ]$Race <- "Other"
  ids <- rename(ids,c('Age'='InitialAge'))

  m_stdLoc2 <- merge(m_stdLoc, ids[, c("SoNum", "id")], by = "SoNum")

  #Sex
  table1a <- as.data.frame(table(ids$Gender))
  table1a$Var1 <- "Male"
  table1a$Freq <- paste0(table1a$Freq, " (",
                         round(table1a$Freq/nrow(ids), 3) * 100, "%)")
  table1a <- rbind(c("Sex", NA), table1a)

  #Age
  table1b <- as.data.frame(table(ids$Age))
  table1b$Freq <- paste0(table1b$Freq, " (",
                         round(table1b$Freq/nrow(ids), 3) * 100, "%)")
  table1b$Var1 <- as.character(table1b$Var1)
  table1b <- rbind(c("Age", NA), table1b)

  #Race
  table1c <- as.data.frame(table(ids$Race))
  table1c$Freq <- paste0(table1c$Freq, " (",
                         round(table1c$Freq/nrow(ids), 3) * 100, "%)")
  table1c$Var1 <- as.character(table1c$Var1)
  table1c <- rbind(c("Race", NA), table1c)

  #Floor
  table1d <- m_stdLoc2 %>% group_by(Floor) %>% summarise(Freq = n_distinct(id))
  names(table1d)[names(table1d) == 'Floor'] <- 'Var1'
  table1d$Var1 <- paste0("Ever on Floor ", table1d$Var1)
  table1d$Freq <- paste0(table1d$Freq, " (",
                         round(table1d$Freq/nrow(ids), 3) * 100, "%)")
  table1d <- rbind(c("Floor", NA), table1d)

  table1 <- rbind(table1a, table1b, table1c, table1d)

  #long-term vs. short-term
  if(exists("types1")){
    table1e <- types1[1:2, c(1,3)]
    names(table1e)[names(table1e) == 'type'] <- 'Var1'
    names(table1e)[names(table1e) == 'numPeople'] <- 'Freq'
    table1e$Freq <- paste0(table1e$Freq, " (",
                           round(table1e$Freq/nrow(ids), 3) * 100, "%)")
    table1e <- rbind(c("Duration", NA), table1e)
    table1 <- rbind(table1, table1e)
  } else {
    table1 <- rbind(table1, c("Duration", NA), c("Long-Term", "-"),
                    c("Short-Term", "-"))
  }

  return(table1)
}

firstDay <- as.Date("2021-10-27")
lastDay <- as.Date("2022-05-27")
#Run Step 0 + Step 16 + 1st part of 17 (8 - 36)
table1.All <- formatTable1()
rm(list=setdiff(ls(), c("table1.All", lsf.str())))


firstDay <- as.Date("2022-01-11")
lastDay <- as.Date("2022-01-21")
#Run Step 0
table1.Jan <- formatTable1()
rm(list=setdiff(ls(), c("table1.All", "table1.Jan", lsf.str())))

firstDay <- as.Date("2022-04-01")
lastDay <- as.Date("2022-04-11")
#Run Step 0
table1.Apr <- formatTable1()
rm(list=setdiff(ls(), c("table1.All", "table1.Jan", "table1.Apr", lsf.str())))

table1 <- cbind(table1.Jan, table1.Apr, table1.All)
table1 <- table1[, c(1, 2, 4, 6)]
rm(list=setdiff(ls(), c("table1")))

write.csv(table1, "table1.csv")

# Summary stats of mean degree, age mixing --------

formatTable2 <- function(){

  #Standardize Column Names
  bMeanDeg$category <- "Overall"
  names(bMeanDeg)[names(bMeanDeg) == 'dates.Date'] <- 'Date'
  names(bMeanDeg_age)[names(bMeanDeg_age) == 'age'] <- 'category'
  names(bMeanDeg_race)[names(bMeanDeg_race) == 'race'] <- 'category'
  names(bMeanDeg_floor)[names(bMeanDeg_floor) == 'floor'] <- 'category'

  #Combine block-level statistics into one df
  bMeanDeg <- rbind(bMeanDeg, bMeanDeg_age, bMeanDeg_race, bMeanDeg_floor)

  #Standardize column names
  cMeanDeg$category <- "Overall"
  names(cMeanDeg)[names(cMeanDeg) == "dates.Date"] <- "Date"
  names(cMeanDeg_age)[names(cMeanDeg_age) == 'age'] <- 'category'
  names(cMeanDeg_race)[names(cMeanDeg_race) == 'race'] <- 'category'
  names(cMeanDeg_floor)[names(cMeanDeg_floor) == 'floor'] <- 'category'

  #Combine cell-level statistics into one df
  cMeanDeg <- rbind(cMeanDeg, cMeanDeg_age, cMeanDeg_race, cMeanDeg_floor)

  #Average across timeframe
  cMeanDeg.avg <- cMeanDeg %>% group_by(category) %>%
    summarise(cMeanDeg.avg = mean(cMeanDeg), cSD.avg = mean(sd))
  bMeanDeg.avg <- bMeanDeg %>% group_by(category) %>%
    summarise(bMeanDeg.avg = mean(bMeanDeg), bSD.avg = mean(sd))

  #Re-order rows
  cMeanDeg.avg <- cMeanDeg.avg[c(15, 16, 3, 5, 7, 9, 11, 13, 14, 17, 1, 2, 4, 6,
                                 8, 10, 12), ]
  bMeanDeg.avg <- bMeanDeg.avg[c(15, 16, 3, 5, 7, 9, 11, 13, 14, 17, 1, 2, 4, 6,
                                 8, 10, 12), ]

  #Combine block-level and cell-level stats into one df
  MeanDeg.avg <- cbind(cMeanDeg.avg, bMeanDeg.avg)

  #Combine means and sds into one column
  MeanDeg.avg$cMeanDeg.avg <- paste0(round(MeanDeg.avg$cMeanDeg.avg, 2), " (",
                                     round(MeanDeg.avg$cSD.avg, 2), ")")
  MeanDeg.avg$bMeanDeg.avg <- paste0(round(MeanDeg.avg$bMeanDeg.avg, 2), " (",
                                     round(MeanDeg.avg$bSD.avg, 2), ")")

  #Remove unneeded columns
  MeanDeg.avg <- MeanDeg.avg[, c("category", "cMeanDeg.avg", "bMeanDeg.avg")]

  return(MeanDeg.avg)
}

#JANUARY
filenames <- list.files("data/output/meanDeg/Jan", pattern="*.RData",
                        full.names=TRUE)
lapply(filenames,load,.GlobalEnv)

MeanDeg.Jan.avg <- formatTable2()

rm(list=setdiff(ls(), c("table1", "MeanDeg.Jan.avg", lsf.str())))


#APRIL
filenames <- list.files("data/output/meanDeg/Apr", pattern="*.RData",
                        full.names=TRUE)
lapply(filenames,load,.GlobalEnv)

MeanDeg.Apr.avg <- formatTable2()

rm(list=setdiff(ls(), c("table1", "MeanDeg.Jan.avg", "MeanDeg.Apr.avg",
                        lsf.str())))


#FULL TIMEFRAME
filenames <- list.files("data/output/meanDeg/All", pattern="*.RData",
                        full.names=TRUE)
lapply(filenames,load,.GlobalEnv)

MeanDeg.All.avg <- formatTable2()

rm(list=setdiff(ls(), c("table1", "MeanDeg.Jan.avg", "MeanDeg.Apr.avg",
                        "MeanDeg.All.avg")))

table2 <- cbind(MeanDeg.Jan.avg[, c(1,2)], MeanDeg.Apr.avg[, 2],
                MeanDeg.All.avg[, 2], MeanDeg.Jan.avg[, 3],
                MeanDeg.Apr.avg[, 3], MeanDeg.All.avg[, 3])

r2 <- c("By Age", NA, NA, NA, NA, NA, NA)
table2 <- rbind(table2[1, ], r2, table2[2:nrow(table2), ])

r9 <- c("By Race", NA, NA, NA, NA, NA, NA)
table2 <- rbind(table2[1:8, ], r9, table2[9:nrow(table2), ])

r13 <- c("By Floor", NA, NA, NA, NA, NA, NA)
table2 <- rbind(table2[1:12, ], r13, table2[13:nrow(table2), ])

rm(list=setdiff(ls(), c("table1", "table2")))

#% of Edges within same age group...

table2$cPropSameAge.Jan <- NA
table2$cPropSameAge.Apr <- NA
table2$cPropSameAge.All <- NA
table2$bPropSameAge.Jan <- NA
table2$bPropSameAge.Apr <- NA
table2$bPropSameAge.All <- NA

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

  ages <- table2$category[c(3:8)]
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

  races <- table2$category[c(10:12)]
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

  floors <- table2$category[c(14:20)]
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

# Dissolution/duration stats ----------------------------------------------

#JANUARY
filenames <- list.files("data/output/turnover/Jan", pattern="*.RData",
                        full.names=TRUE)
lapply(filenames,load,.GlobalEnv)

changes.Jan <- cbind(cell_changes[, c(1, 4)], block_changes[, c(1, 4)], turnover_rates[, c(1, 6)])
changes.Jan[, c(3, 5)] <- NULL

rm(list=setdiff(ls(), c("table1", "table2", "changes.Jan")))

#APRIL
filenames <- list.files("data/output/turnover/Apr", pattern="*.RData",
                        full.names=TRUE)
lapply(filenames,load,.GlobalEnv)

changes.Apr <- cbind(cell_changes[, c(1, 4)], block_changes[, c(1, 4)], turnover_rates[, c(1, 6)])
changes.Apr[, c(3, 5)] <- NULL

rm(list=setdiff(ls(), c("table1", "table2", "changes.Jan", "changes.Apr")))

table3 <- data.frame(category = changes.Jan$Category,
                     cellChangeRate.Jan = changes.Jan$CellChangeRate,
                     cellChangeRate.Apr = changes.Apr$CellChangeRate,
                     blockChangeRate.Jan = changes.Jan$BlockChangeRate,
                     blockChangeRate.Apr = changes.Apr$BlockChangeRate,
                     releases.Jan = changes.Jan$TurnoverOut,
                     releases.Apr = changes.Apr$TurnoverOut)
table3 <- table3 %>% mutate_if(is.numeric, round, digits = 3)
table3 <- table3[c(1, 12:17, 9:11, 2:8), ]

rm(list=setdiff(ls(), c("table1", "table2", "table3")))

#ALL
filenames <- list.files("data/output/turnover/All", pattern="*.RData",
                        full.names=TRUE)
lapply(filenames,load,.GlobalEnv)

formatDurations <- function(df){
  df$percentSpells <- round(df$percentSpells, 1)

  df$censoring <- NA
  df[df$startCensored == "N" & df$endCensored == "N", "censoring"] <- "Uncensored"
  df[df$startCensored == "Y" & df$endCensored == "N", "censoring"] <- "Left-Censored"
  df[df$startCensored == "N" & df$endCensored == "Y", "censoring"] <- "Right-Censored"
  df[df$startCensored == "Y" & df$endCensored == "Y", "censoring"] <- "Both-Censored"
  df[,c("startCensored", "endCensored", "numSpells", "meanNumLocs")] <- NULL

  df <- df %>% pivot_wider(names_from = censoring, values_from = c(meanDur, percentSpells))
  df <- df[c(14, 6, 1:5, 15:17, 7:13), ]
  df$Uncensored <- paste0(round(df$meanDur_Uncensored, 2), " (", df$percentSpells_Uncensored, "%)")
  df$LeftCensored <- paste0(round(df$`meanDur_Left-Censored`, 2), " (", df$`percentSpells_Left-Censored`, "%)")
  df$RightCensored <- paste0(round(df$`meanDur_Right-Censored`, 2), " (", df$`percentSpells_Right-Censored`, "%)")
  df$BothCensored <- paste0(round(df$`meanDur_Both-Censored`, 2), " (", df$`percentSpells_Both-Censored`, "%)")

  df <- df[,c(1, 10, 11, 12, 13)]
}

temp1 <- formatDurations(cLocDurations)
temp2 <- formatDurations(bLocDurations)
temp3 <- formatDurations(durationsSummary)

table3$UncensoredCell <- temp1$Uncensored
table3$LeftCensoredCell <- temp1$LeftCensored
table3$RightCensoredCell <- temp1$RightCensored
table3$BothCensoredCell <- temp1$BothCensored

table3$UncensoredBlock <- temp2$Uncensored
table3$LeftCensoredBlock <- temp2$LeftCensored
table3$RightCensoredBlock <- temp2$RightCensored
table3$BothCensoredBlock <- temp2$BothCensored

table3$UncensoredJail <- temp3$Uncensored
table3$LeftCensoredJail <- temp3$LeftCensored
table3$RightCensoredJail <- temp3$RightCensored
table3$BothCensoredJail <- temp3$BothCensored

r2 <- c("By Age", rep(NA,18))
table3 <- rbind(table3[1, ], r2, table3[2:nrow(table3), ])

r9 <- c("By Race", rep(NA,18))
table3 <- rbind(table3[1:8, ], r9, table3[9:nrow(table3), ])

r13 <- c("By Floor", rep(NA,18))
table3 <- rbind(table3[1:12, ], r13, table3[13:nrow(table3), ])

rm(list=setdiff(ls(), c("table1", "table2", "table3")))
