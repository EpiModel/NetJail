library(readxl)
library(tidyr)
library(dplyr)

#Load all excel files from the data/input/FJC folder into the files list
filenames <- list.files("data/input/FJC", pattern="*.xlsx", full.names=TRUE)
files <- lapply(filenames, read_excel)
#Name the sub-lists (one for each file) based on the file names
names(files) <- substr(gsub(" ", "_",
                            gsub("\\(", "",
                                 gsub("\\)", "", filenames))), 16, 40)

#Basic data cleaning
for (i in seq_along(files)){
  #Keep only the SO Number, DOB, Gender, Race, and Cell/Cell Location columns
  files[[i]] <- files[[i]][ , grepl( "So|SO|DOB|Gender|Race|Cell" ,
                                     names( files[[i]] ) ) ]
  #Check that each sub-list has the right number of columns
  if (ncol(files[[i]]) != 5){
    stop("Wrong number of columns selected")
  }
  #Add a column for the date of data collection
  files[[i]]$Date <- substr(names(files)[i], 18, 25)
  #Standardize column names and date formats
  colnames(files[[i]])[grepl('So|SO',colnames(files[[i]]))] <- 'SoNum'
  colnames(files[[i]])[grepl('Cell',colnames(files[[i]]))] <- 'Location'
  files[[i]]$Date <- as.Date(files[[i]]$Date, "%Y%m%d")
  files[[i]]$DOB <- as.Date(files[[i]]$DOB)
}

#Concatenate the sub-lists into one long data frame
all_data = do.call(rbind, files)
remove(files, filenames, i)

#Can create a data frame for each sub-list if needed:
#list2env(files, .GlobalEnv)

#Check for missing values
sapply(all_data, function(x) sum(is.na(x)))
#Check for uniqueness of SO Numbers
check_unique_so <- all_data %>%
  group_by(SoNum) %>%
  summarise(DOBs = n_distinct(DOB), Genders = n_distinct(Gender),
            Races = n_distinct(Race))
sapply(check_unique_so, function(x) sum(x > 1))
remove(check_unique_so)
#Check for reasonableness of data
summary(all_data$DOB)
table(all_data$Gender)
table(all_data$Race)

#For now: Separate out women and men with special locations
data_m <-subset(all_data, Gender == "M")
data_f <-subset(all_data, Gender == "F")
data_m$Location <- gsub(" - Male", "", data_m$Location)
data_f$Location <- gsub(" - Female", "", data_f$Location)
if (nrow(all_data) != (nrow(data_f) + nrow(data_m))){
  warning("Check for missing values in the Gender column")}
data_m_standardLoc <- data_m[!grepl("[A-M]|[O-R]|[T-Z]|[a-z]",
                                    data_m$Location), ]
data_m_specialLoc <- data_m[grepl("[A-M]|[O-R]|[T-Z]|[a-z]", data_m$Location), ]
remove(data_m)
#Do other things (TBD)



#Continuing only with men with standard locations...

#Calculate how many times each person appears in the data and in how many places
counts <- data_m_standardLoc %>% group_by(SoNum) %>%
  summarise(DaysOfData = n(), FirstDate = min(Date), LastDate = max(Date),
            NumLocs = n_distinct(Location), DOB = last(DOB),
            Gender = last(Gender), Race = last(Race))
counts$Duration <- counts$LastDate - counts$FirstDate + 1

#Split the cell location column into floor, tower, block, and cell
data_m_standardLoc <- separate(data = data_m_standardLoc, col = 5,
                               into = c("Floor", "Tower", "Block", "Cell"),
                               sep = c(1, 2, 3))

#Contacts by Block and Cell
blockContacts_temp <- data_m_standardLoc %>%
  group_by(Date, Floor, Tower, Block) %>%
    summarise(BlockContacts = n_distinct(SoNum))
cellContacts_temp <- data_m_standardLoc %>%
  group_by(Date, Floor, Tower, Block, Cell) %>%
    summarise(CellContacts = n_distinct(SoNum))

blockContacts <- merge(data_m_standardLoc, blockContacts_temp,
              by = c("Date", "Floor", "Tower", "Block"))[,
                                        union(names(data_m_standardLoc),
                                              names(blockContacts_temp))]
blockContacts$BlockContacts <- blockContacts$BlockContacts - 1
contacts <- merge(blockContacts, cellContacts_temp,
                  by = c("Date", "Floor", "Tower", "Block", "Cell"))[,
                                            union(names(blockContacts),
                                                names(cellContacts_temp))]
contacts$CellContacts <- contacts$CellContacts - 1
remove(blockContacts_temp, cellContacts_temp, blockContacts)

contacts$CSum_Block <- ave(contacts$BlockContacts, contacts$SoNum, FUN=cumsum)
contacts$CSum_Cell <- ave(contacts$CellContacts, contacts$SoNum, FUN=cumsum)



