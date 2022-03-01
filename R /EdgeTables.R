#BLOCK-LEVEL

#Assign a numerical id to each resident and each date
counts$id <- 1:nrow(counts)
dates <- unique(contacts[c("Date")])
dates$Day <- 1:nrow(dates)
#Merge in the new resident IDs
dailyLoc_block <- merge(data_m_standardLoc,
                        counts, by = "SoNum")[,
                            c("id", "Date", "Floor", "Tower", "Block")]
#Combine floor, tower, and block into one column
dailyLoc_block$loc <- paste(dailyLoc_block$Floor, dailyLoc_block$Tower,
                            dailyLoc_block$Block)
dailyLoc_block <- dailyLoc_block[ , c("id", "Date", "loc")]
#Create pairs of residents who were ever in the same block on the same day
pairs_block <- merge(dailyLoc_block, dailyLoc_block, by = c("Date", "loc"))
#Remove duplicate pairs
pairs_block <- subset(pairs_block, (id.x < id.y))
#Merge in the new date IDs
pairs_block <- merge(pairs_block, dates, by = "Date")

#Create an id for each pair
pairs_block <- pairs_block[with(pairs_block, order(id.x, id.y, Day)), ]
pairs_block <- pairs_block %>%
  group_by(id.x, id.y) %>%
    mutate(contact = cur_group_id()) %>%
      ungroup()
#Flag rows if the pair is the same as the previous row and there's no time gap
ind2 <- which(pairs_block$contact==lag(pairs_block$contact) &
                pairs_block$Day == (lag(pairs_block$Day) + 1))
pairs_block$newContact <- "True"
pairs_block[ind2, ]$newContact <- "False"
#Create a counter that increments for every new contact
pairs_block$contactCounter <- cumsum(pairs_block$newContact == "True")
#Remove the helper columns
pairs_block$newContact <- NULL
pairs_block$contact <- NULL

#List out each contact w/head, tail, start, end, and censoring flags
edges_block <- pairs_block %>%
  group_by(contactCounter) %>%
    summarise(head = first(id.x), tail = first(id.y),
              startTime = min(Day), endTime = max(Day))
edges_block$contactCounter <- NULL
edges_block$startCensored <- "N"
edges_block$startCensored[edges_block$startTime ==
                            min(edges_block$startTime) ] <- "Y"
edges_block$endCensored <- "N"
edges_block$endCensored[edges_block$endTime
                        == max(edges_block$endTime) ] <- "Y"




# #FLOOR/TOWER-LEVEL
#
# #Merge in the resident IDs again
# dailyLoc_floor <- merge(data_m_standardLoc,
#                         counts, by = "SoNum")[,
#                                           c("id", "Date", "Floor", "Tower")]
# #Combine floor and tower into one column
# dailyLoc_floor$loc <- paste(dailyLoc_floor$Floor, dailyLoc_floor$Tower)
# dailyLoc_floor <- dailyLoc_floor[ , c("id", "Date", "loc")]
# #Create pairs of residents who were ever on the same flor on the same day
# pairs_floor <- merge(dailyLoc_floor, dailyLoc_floor, by = c("Date", "loc"))
# #Remove duplicate pairs
# pairs_floor <- subset(pairs_floor, (id.x < id.y))
# #Merge in the new date IDs
# pairs_floor <- merge(pairs_floor, dates, by = "Date")
#
# #Create an id for each pair
# pairs_floor <- pairs_floor[with(pairs_floor, order(id.x, id.y, Day)), ]
# pairs_floor <- pairs_floor %>%
#   group_by(id.x, id.y) %>%
#   mutate(contact = cur_group_id()) %>%
#   ungroup()
# #Flag rows if the pair is the same as the previous row and there's no time gap
# ind2.f <- which(pairs_floor$contact==lag(pairs_floor$contact) &
#                   pairs_floor$Day == (lag(pairs_floor$Day) + 1))
# pairs_floor$newContact <- "True"
# pairs_floor[ind2.f, ]$newContact <- "False"
# #Create a counter that increments for every new contact
# pairs_floor$contactCounter <- cumsum(pairs_floor$newContact == "True")
# #Remove the helper columns
# pairs_floor$newContact <- NULL
# pairs_floor$contact <- NULL
#
# #List out each contact w/head, tail, start, end, and censoring flags
# edges_floor <- pairs_floor %>%
#   group_by(contactCounter) %>%
#   summarise(head = first(id.x), tail = first(id.y),
#             startTime = min(Day), endTime = max(Day))
# edges_floor$contactCounter <- NULL
# edges_floor$startCensored <- "N"
# edges_floor$startCensored[edges_floor$startTime ==
#                             min(edges_floor$startTime) ] <- "Y"
# edges_floor$endCensored <- "N"
# edges_floor$endCensored[edges_floor$endTime
#                         == max(edges_floor$endTime) ] <- "Y"



#CELL-LEVEL

#Merge in the resident IDs again
dailyLoc_cell <- merge(data_m_standardLoc,
                        counts, by = "SoNum")[,
                            c("id", "Date", "Floor", "Tower", "Block", "Cell")]
#Combine floor, tower, block, and cell into one column
dailyLoc_cell$loc <- paste(dailyLoc_cell$Floor, dailyLoc_cell$Tower,
                           dailyLoc_cell$Block, dailyLoc_cell$Cell)
dailyLoc_cell <- dailyLoc_cell[ , c("id", "Date", "loc")]
#Create pairs of residents who were ever in the same cell on the same day
pairs_cell <- merge(dailyLoc_cell, dailyLoc_cell, by = c("Date", "loc"))
#Remove duplicate pairs
pairs_cell <- subset(pairs_cell, (id.x < id.y))
#Merge in the new date IDs
pairs_cell <- merge(pairs_cell, dates, by = "Date")

#Create an id for each pair
pairs_cell <- pairs_cell[with(pairs_cell, order(id.x, id.y, Day)), ]
pairs_cell <- pairs_cell %>%
  group_by(id.x, id.y) %>%
  mutate(contact = cur_group_id()) %>%
  ungroup()
#Flag rows if the pair is the same as the previous row and there's no time gap
ind2.c <- which(pairs_cell$contact==lag(pairs_cell$contact) &
                  pairs_cell$Day == (lag(pairs_cell$Day) + 1))
pairs_cell$newContact <- "True"
pairs_cell[ind2.c, ]$newContact <- "False"
#Create a counter that increments for every new contact
pairs_cell$contactCounter <- cumsum(pairs_cell$newContact == "True")
#Remove the helper columns
pairs_cell$newContact <- NULL
pairs_cell$contact <- NULL

#List out each contact w/head, tail, start, end, and censoring flags
edges_cell <- pairs_cell %>%
  group_by(contactCounter) %>%
  summarise(head = first(id.x), tail = first(id.y),
            startTime = min(Day), endTime = max(Day))
edges_cell$contactCounter <- NULL
edges_cell$startCensored <- "N"
edges_cell$startCensored[edges_cell$startTime ==
                            min(edges_cell$startTime) ] <- "Y"
edges_cell$endCensored <- "N"
edges_cell$endCensored[edges_cell$endTime
                        == max(edges_cell$endTime) ] <- "Y"

remove(dailyLoc_block, dailyLoc_cell, pairs_block,
       pairs_cell, ind2, ind2.c)
