library(dplyr)


# Approach 1 --------------------------------------------------------------
# number and percent of edges that B/B vs B/W vs. B/O vs. W/W vs. W/O vs. O/O

calcRaceMix <- function(edges){
  edges <- merge(edges, ids[, c("id", "Race")], by.x = "head", by.y = "id" )
  edges <- rename(edges, head.race = Race)
  edges[edges$head.race != "White" & edges$head.race != "Black", ]$head.race <- "Other"

  edges <- merge(edges, ids[, c("id", "Race")], by.x = "tail", by.y = "id" )
  edges <- rename(edges, tail.race = Race)
  edges[edges$tail.race != "White" & edges$tail.race != "Black", ]$tail.race <- "Other"

  #How many edges are B/B vs B/W vs. B/O vs. W/W vs. W/O vs. O/O? (on avg. across timeframe)
  mixList <- list()
  for (i in seq_len(nrow(dates))) {
    currTime <- edges[edges$onset <= dates$DayNum[i] & edges$terminus >= (dates$DayNum[i] + 1), ]
    currTime <- currTime %>% group_by(race1 = pmin(head.race, tail.race), race2 = pmax(head.race, tail.race), .drop = FALSE) %>%
      summarise(numEdges = n(), .groups = "drop_last")
    #currTime$percentEdges <- round(currTime$numEdges / sum(currTime$numEdges), 3) * 100

    mixList[[length(mixList) + 1]] <- currTime
  }

  mixDF <- suppressWarnings(Reduce(function(x, y) merge(x, y, by=c("race1", "race2"), all = TRUE), mixList))
  mixDF[is.na(mixDF)] <- 0
  #mixDF <- do.call(cbind, mixList)
  mixDF$avgNumEdges <- rowSums(mixDF[sapply(mixDF, is.numeric)]) / length(mixList)

  mixDF <- mixDF[, c(1, 2, ncol(mixDF))]
  mixDF$percentEdges <- round(mixDF$avgNumEdges / sum(mixDF$avgNumEdges), 3) * 100
  mixDF$avgNumEdges <- round(mixDF$avgNumEdges, 2)

  return(mixDF)
}

#JANUARY
filenames <- list.files("data/output/edges/Jan", pattern="*.RData", full.names=TRUE)
lapply(filenames,load,.GlobalEnv)
cRaceMix.Jan <- calcRaceMix(cEdges)
bRaceMix.Jan <- calcRaceMix(bEdges)
rm(list=setdiff(ls(), c("cRaceMix.Jan", "bRaceMix.Jan", lsf.str())))

#APRIL
filenames <- list.files("data/output/edges/Apr", pattern="*.RData", full.names=TRUE)
lapply(filenames,load,.GlobalEnv)
cRaceMix.Apr <- calcRaceMix(cEdges)
bRaceMix.Apr <- calcRaceMix(bEdges)
rm(list=setdiff(ls(), c("cRaceMix.Jan", "bRaceMix.Jan", "cRaceMix.Apr", "bRaceMix.Apr", lsf.str())))

#ALL
filenames <- list.files("data/output/edges/All", pattern="*.RData", full.names=TRUE)
lapply(filenames,load,.GlobalEnv)
cRaceMix.All <- calcRaceMix(cEdges)
bRaceMix.All <- calcRaceMix(bEdges)
rm(list=setdiff(ls(), c("cRaceMix.Jan", "bRaceMix.Jan", "cRaceMix.Apr", "bRaceMix.Apr", "cRaceMix.All", "bRaceMix.All")))


# Approach 2 --------------------------------------------------------------
# Of edges with a Black node, how many have the other node Black vs. White vs. Other? Same for White, Other

calcRaceMix2 <- function(edges){
  edges <- merge(edges, ids[, c("id", "Race")], by.x = "head", by.y = "id" )
  edges <- rename(edges, head.race = Race)
  edges[edges$head.race != "White" & edges$head.race != "Black", ]$head.race <- "Other"

  edges <- merge(edges, ids[, c("id", "Race")], by.x = "tail", by.y = "id" )
  edges <- rename(edges, tail.race = Race)
  edges[edges$tail.race != "White" & edges$tail.race != "Black", ]$tail.race <- "Other"

  #How many edges are B/B vs B/W vs. B/O vs. W/W vs. W/O vs. O/O? (on avg. across timeframe)
  races <- c("Black", "White", "Other")
  raceMixList <- list()
  for (j in seq_along(races)){
    raceEdges <- edges[edges$head.race == races[j] | edges$tail.race == races[j], ]
    mixList <- list()
    for (i in seq_len(nrow(dates))) {
      currTime <- raceEdges[raceEdges$onset <= dates$DayNum[i] & raceEdges$terminus >= (dates$DayNum[i] + 1), ]
      currTime <- currTime %>% group_by(race1 = pmin(head.race, tail.race), race2 = pmax(head.race, tail.race), .drop = FALSE) %>%
        summarise(numEdges = n(), .groups = "drop_last")
      mixList[[length(mixList) + 1]] <- currTime
    }
    mixDF <- suppressWarnings(Reduce(function(x, y) merge(x, y, by=c("race1", "race2"), all = TRUE), mixList))
    mixDF[is.na(mixDF)] <- 0
    #mixDF <- do.call(cbind, mixList)
    mixDF$avgNumEdges <- rowSums(mixDF[sapply(mixDF, is.numeric)]) / length(mixList)
    mixDF <- mixDF[, c(1, 2, ncol(mixDF))]
    mixDF$percentEdges <- round(mixDF$avgNumEdges / sum(mixDF$avgNumEdges), 3) * 100
    mixDF$avgNumEdges <- round(mixDF$avgNumEdges, 2)

    mixDF$RaceOfIndividual <- races[j]
    mixDF$RaceOfContact <- c("Black", "Other", "White")
    mixDF <- mixDF[, c(5, 6, 3, 4)]

    raceMixList[[length(raceMixList) + 1]] <- mixDF
  }

  raceMixDF <- do.call(cbind, raceMixList)

  return(raceMixDF)
}

#JANUARY
filenames <- list.files("data/output/edges/Jan", pattern="*.RData", full.names=TRUE)
lapply(filenames,load,.GlobalEnv)
cRaceMix.Jan2 <- calcRaceMix2(cEdges)
bRaceMix.Jan2 <- calcRaceMix2(bEdges)
rm(list=setdiff(ls(), c("cRaceMix.Jan", "bRaceMix.Jan", "cRaceMix.Apr", "bRaceMix.Apr", "cRaceMix.All", "bRaceMix.All", "cRaceMix.Jan2", "bRaceMix.Jan2", lsf.str())))

#APRIL
filenames <- list.files("data/output/edges/Apr", pattern="*.RData", full.names=TRUE)
lapply(filenames,load,.GlobalEnv)
cRaceMix.Apr2 <- calcRaceMix2(cEdges)
bRaceMix.Apr2 <- calcRaceMix2(bEdges)
rm(list=setdiff(ls(), c("cRaceMix.Jan", "bRaceMix.Jan", "cRaceMix.Apr", "bRaceMix.Apr", "cRaceMix.All", "bRaceMix.All", "cRaceMix.Jan2", "bRaceMix.Jan2", "cRaceMix.Apr2", "bRaceMix.Apr2", lsf.str())))

#ALL
filenames <- list.files("data/output/edges/All", pattern="*.RData", full.names=TRUE)
lapply(filenames,load,.GlobalEnv)
cRaceMix.All2 <- calcRaceMix2(cEdges)
bRaceMix.All2 <- calcRaceMix2(bEdges)
rm(list=setdiff(ls(), c("cRaceMix.Jan", "bRaceMix.Jan", "cRaceMix.Apr", "bRaceMix.Apr", "cRaceMix.All", "bRaceMix.All", "cRaceMix.Jan2", "bRaceMix.Jan2", "cRaceMix.Apr2", "bRaceMix.Apr2", "cRaceMix.All2", "bRaceMix.All2")))




# Approach 3 --------------------------------------------------------------
#Doubled Edgelist -> mixing matrix (same approach as age mixing)

calcRaceMix3 <- function(edges){
  temp1 <- edges[ ,c("head", "tail", "onset", "terminus")]
  temp2 <- edges[ ,c("head", "tail", "onset", "terminus")]
  temp2[1:2] <- temp2[2:1]

  doubledEdges <- rbind(temp1, temp2)
  remove(temp1, temp2)
  doubledEdges <- merge(doubledEdges, ids[, c("id", "Race")], by.x = "head", by.y = "id")
  doubledEdges <- rename(doubledEdges, head.race = Race)
  doubledEdges[doubledEdges$head.race != "White" & doubledEdges$head.race != "Black", ]$head.race <- "Other"

  doubledEdges <- merge(doubledEdges, ids[, c("id", "Race")], by.x = "tail", by.y = "id")
  doubledEdges <- rename(doubledEdges, tail.race = Race)
  doubledEdges[doubledEdges$tail.race != "White" & doubledEdges$tail.race != "Black", ]$tail.race <- "Other"

  mixList <- list()
  for (i in seq_len(nrow(dates))) {
    currTime <- doubledEdges[doubledEdges$onset <= dates$DayNum[i] & doubledEdges$terminus >= (dates$DayNum[i] + 1), ]
    currTime <- currTime %>% group_by(head.race, tail.race, .drop = FALSE) %>% summarise(numEdges = n(), .groups = "drop_last")
    #currTime.props <- currTime %>% group_by(head.race) %>% mutate(propEdges = numEdges / sum(numEdges))
    #currTime.props$numEdges <- NULL

    mixList[[length(mixList) + 1]] <- currTime
  }

  mixDF <- suppressWarnings(Reduce(function(x, y) merge(x, y, by=c("head.race", "tail.race"), all = TRUE), mixList))
  mixDF[is.na(mixDF)] <- 0
  #mixDF <- do.call(cbind, mixList)
  mixDF$avgNumEdges <- rowSums(mixDF[sapply(mixDF, is.numeric)]) / length(mixList)
  mixDF <- mixDF[, c(1, 2, ncol(mixDF))]
  mixDF <- mixDF %>% group_by(head.race) %>% mutate(prop = avgNumEdges / sum(avgNumEdges)) %>% ungroup

  return(mixDF)
}

#JANUARY
filenames <- list.files("data/output/edges/Jan", pattern="*.RData", full.names=TRUE)
lapply(filenames,load,.GlobalEnv)
cRaceMix.Jan3 <- calcRaceMix3(cEdges)
bRaceMix.Jan3 <- calcRaceMix3(bEdges)
rm(list=setdiff(ls(), c("cRaceMix.Jan", "bRaceMix.Jan", "cRaceMix.Apr", "bRaceMix.Apr", "cRaceMix.All", "bRaceMix.All", "cRaceMix.Jan2", "bRaceMix.Jan2", "cRaceMix.Apr2", "bRaceMix.Apr2", "cRaceMix.All2", "bRaceMix.All2", "cRaceMix.Jan3", "bRaceMix.Jan3", lsf.str())))

#APRIL
filenames <- list.files("data/output/edges/Apr", pattern="*.RData", full.names=TRUE)
lapply(filenames,load,.GlobalEnv)
cRaceMix.Apr3 <- calcRaceMix3(cEdges)
bRaceMix.Apr3 <- calcRaceMix3(bEdges)
rm(list=setdiff(ls(), c("cRaceMix.Jan", "bRaceMix.Jan", "cRaceMix.Apr", "bRaceMix.Apr", "cRaceMix.All", "bRaceMix.All", "cRaceMix.Jan2", "bRaceMix.Jan2", "cRaceMix.Apr2", "bRaceMix.Apr2", "cRaceMix.All2", "bRaceMix.All2", "cRaceMix.Jan3", "bRaceMix.Jan3", "cRaceMix.Apr3", "bRaceMix.Apr3", lsf.str())))


#ALL
filenames <- list.files("data/output/edges/All", pattern="*.RData", full.names=TRUE)
lapply(filenames,load,.GlobalEnv)
cRaceMix.All3 <- calcRaceMix3(cEdges)
bRaceMix.All3 <- calcRaceMix3(bEdges)
rm(list=setdiff(ls(), c("cRaceMix.Jan", "bRaceMix.Jan", "cRaceMix.Apr", "bRaceMix.Apr", "cRaceMix.All", "bRaceMix.All", "cRaceMix.Jan2", "bRaceMix.Jan2", "cRaceMix.Apr2", "bRaceMix.Apr2", "cRaceMix.All2", "bRaceMix.All2", "cRaceMix.Jan3", "bRaceMix.Jan3", "cRaceMix.Apr3", "bRaceMix.Apr3", "cRaceMix.All3", "bRaceMix.All3")))

list_df <- mget(ls())
lapply(seq_along(list_df), function(i) write.csv(list_df[[i]], paste0(names(list_df)[i], ".csv")))
