#BLOCK-LEVEL

#Assign a numerical id to each date
dates <- unique(contacts[c("Date")])
dates$DayIndex <- 1:nrow(dates)
dates <- dates %>% mutate(DayNum = as.numeric(difftime(Date, lag(Date,1))))
dates$DayNum[1] <- 1
dates$DayNum <- cumsum(dates$DayNum)

#Merge in ids; combine floor, tower, and block into one column
bDailyLoc <- merge(m_stdLoc, ids, by = "SoNum")[,
                            c("id", "Date", "Floor", "Tower", "Block")]
bDailyLoc$loc <- paste(bDailyLoc$Floor, bDailyLoc$Tower, bDailyLoc$Block)
bDailyLoc <- bDailyLoc[ , c("id", "Date", "loc")]

#Create pairs of residents who were ever in the same block on the same day
bPairs <- merge(bDailyLoc, bDailyLoc, by = c("Date", "loc"))
bPairs <- subset(bPairs, (id.x < id.y))
bPairs <- merge(bPairs, dates, by = "Date")

#Flag rows if the pair is the same as the previous row and there's no time gap
bPairs <- bPairs[with(bPairs, order(id.x, id.y, DayIndex)), ]
ind.b <- which(bPairs$id.x==lag(bPairs$id.x) & bPairs$id.y==lag(bPairs$id.y) &
                bPairs$loc==lag(bPairs$loc) &
                bPairs$DayIndex==(lag(bPairs$DayIndex) + 1))
bPairs$newContact <- "True"
bPairs[ind.b, ]$newContact <- "False"

#Create a counter that increments for every new contact; remove helper col
bPairs$contactCounter <- cumsum(bPairs$newContact == "True")
bPairs$newContact <- NULL

#List out each contact w/head, tail, start, end, and censoring flags
bEdges <- bPairs %>% group_by(contactCounter) %>%
    summarise(head = first(id.x), tail = first(id.y),
              firstTime = min(DayNum), lastTime = max(DayNum),
              location = first(loc))
bEdges$contactCounter <- NULL
bEdges$startCensored <- "N"
bEdges$startCensored[bEdges$firstTime == min(bEdges$firstTime) ] <- "Y"
bEdges$endCensored <- "N"
bEdges$endCensored[bEdges$lastTime == max(bEdges$lastTime) ] <- "Y"

remove(bPairs, ind.b)




#CELL-LEVEL

#Merge in ids; combine floor, tower, and block into one column
cDailyLoc <- merge(m_stdLoc, ids, by = "SoNum")[,
                            c("id", "Date", "Floor", "Tower", "Block", "Cell")]
cDailyLoc$loc <- paste(cDailyLoc$Floor, cDailyLoc$Tower, cDailyLoc$Block,
                       cDailyLoc$Cell)
cDailyLoc <- cDailyLoc[ , c("id", "Date", "loc")]

#Create pairs of residents who were ever in the same cell on the same day
cPairs <- merge(cDailyLoc, cDailyLoc, by = c("Date", "loc"))
cPairs <- subset(cPairs, (id.x < id.y))
cPairs <- merge(cPairs, dates, by = "Date")

#Flag rows if the pair is the same as the previous row and there's no time gap
cPairs <- cPairs[with(cPairs, order(id.x, id.y, DayIndex)), ]
ind.c <- which(cPairs$id.x==lag(cPairs$id.x) & cPairs$id.y==lag(cPairs$id.y) &
                 cPairs$loc==lag(cPairs$loc) &
                 cPairs$DayIndex==(lag(cPairs$DayIndex) + 1))
cPairs$newContact <- "True"
cPairs[ind.c, ]$newContact <- "False"

#Create a counter that increments for every new contact; remove helper col
cPairs$contactCounter <- cumsum(cPairs$newContact == "True")
cPairs$newContact <- NULL

#List out each contact w/head, tail, start, end, and censoring flags
cEdges <- cPairs %>% group_by(contactCounter) %>%
  summarise(head = first(id.x), tail = first(id.y),
            firstTime = min(DayNum), lastTime = max(DayNum),
            location = first(loc))
cEdges$contactCounter <- NULL
cEdges$startCensored <- "N"
cEdges$startCensored[cEdges$firstTime == min(cEdges$firstTime) ] <- "Y"
cEdges$endCensored <- "N"
cEdges$endCensored[cEdges$lastTime == max(cEdges$lastTime) ] <- "Y"

remove(cPairs, ind.c)
