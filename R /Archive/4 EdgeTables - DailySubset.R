m_stdLoc_s <- subset(m_stdLoc, (Date >= as.Date("2022-01-11") &
                               Date <= as.Date("2022-01-22")))

#CELL-LEVEL

#Assign resident IDs for residents in this subset
m_stdLoc_s <- m_stdLoc_s[with(m_stdLoc_s, order(SoNum)), ]
m_stdLoc_s <- m_stdLoc_s %>% group_by(SoNum) %>%
  mutate(subsetID = cur_group_id()) %>% ungroup()

#Combine floor, tower, block, and cell into one column
cDailyLoc_s <- m_stdLoc_s[, c("subsetID", "Date", "Floor", "Tower", "Block",
                              "Cell")]
cDailyLoc_s$loc <- paste(cDailyLoc_s$Floor, cDailyLoc_s$Tower,
                         cDailyLoc_s$Block, cDailyLoc_s$Cell)
cDailyLoc_s <- cDailyLoc_s[ , c("subsetID", "Date", "loc")]

#Create pairs of residents who were ever in the same cell on the same day
cPairs_s <- merge(cDailyLoc_s, cDailyLoc_s, by = c("Date", "loc"))
cPairs_s <- subset(cPairs_s, (subsetID.x < subsetID.y))

#Convert dates to date ids
cPairs_s <- cPairs_s %>% group_by(Date) %>% mutate(Day = cur_group_id()) %>%
  ungroup()
remove(cDailyLoc_s)

#Flag rows if the pair is the same as the previous row and there's no time gap
cPairs_s <- cPairs_s[with(cPairs_s, order(subsetID.x, subsetID.y, Day)), ]
ind.c <- which(cPairs_s$subsetID.x==lag(cPairs_s$subsetID.x) &
                 cPairs_s$subsetID.y==lag(cPairs_s$subsetID.y) &
                 cPairs_s$Day == (lag(cPairs_s$Day) + 1))
cPairs_s$newContact <- "True"
cPairs_s[ind.c, ]$newContact <- "False"

#Create a counter that increments for every new contact; remove helper col
cPairs_s$contactCounter <- cumsum(cPairs_s$newContact == "True")
cPairs_s$newContact <- NULL

#List out each contact w/head, tail, start, end, and censoring flags
cEdges_s <- cPairs_s %>% group_by(contactCounter) %>%
  summarise(head = first(subsetID.x), tail = first(subsetID.y),
            firstTime = min(Day), lastTime = max(Day))
cEdges_s$contactCounter <- NULL
cEdges_s$startCensored <- "N"
cEdges_s$startCensored[cEdges_s$firstTime == min(cEdges_s$firstTime) ] <- "Y"
cEdges_s$endCensored <- "N"
cEdges_s$endCensored[cEdges_s$lastTime == max(cEdges_s$lastTime) ] <- "Y"

remove(cPairs_s, ind.c)




#BLOCK-LEVEL

#Combine floor, tower, and block into one column
bDailyLoc_s <- m_stdLoc_s[, c("subsetID", "Date", "Floor", "Tower", "Block")]
bDailyLoc_s$loc <- paste(bDailyLoc_s$Floor, bDailyLoc_s$Tower,
                         bDailyLoc_s$Block)
bDailyLoc_s <- bDailyLoc_s[ , c("subsetID", "Date", "loc")]

#Create pairs of residents who were ever in the same block on the same day
bPairs_s <- merge(bDailyLoc_s, bDailyLoc_s, by = c("Date", "loc"))
bPairs_s <- subset(bPairs_s, (subsetID.x < subsetID.y))

#Convert dates to date ids
bPairs_s <- bPairs_s %>% group_by(Date) %>% mutate(Day = cur_group_id()) %>%
  ungroup()
remove(bDailyLoc_s)

#Flag rows if the pair is the same as the previous row and there's no time gap
bPairs_s <- bPairs_s[with(bPairs_s, order(subsetID.x, subsetID.y, Day)), ]
ind.b <- which(bPairs_s$subsetID.x==lag(bPairs_s$subsetID.x) &
                 bPairs_s$subsetID.y==lag(bPairs_s$subsetID.y) &
                 bPairs_s$Day == (lag(bPairs_s$Day) + 1))
bPairs_s$newContact <- "True"
bPairs_s[ind.b, ]$newContact <- "False"

#Create a counter that increments for every new contact; remove helper col
bPairs_s$contactCounter <- cumsum(bPairs_s$newContact == "True")
bPairs_s$newContact <- NULL

#List out each contact w/head, tail, start, end, and censoring flags
bEdges_s <- bPairs_s %>% group_by(contactCounter) %>%
  summarise(head = first(subsetID.x), tail = first(subsetID.y),
            firstTime = min(Day), lastTime = max(Day))
bEdges_s$contactCounter <- NULL
bEdges_s$startCensored <- "N"
bEdges_s$startCensored[bEdges_s$firstTime == min(bEdges_s$firstTime) ] <- "Y"
bEdges_s$endCensored <- "N"
bEdges_s$endCensored[bEdges_s$lastTime == max(bEdges_s$lastTime) ] <- "Y"

remove(bPairs_s, ind.b)
