library(readxl)
library(tidyr)
library(dplyr)

#Load all excel files from the data/input/FJC folder into the files list
filenames <- list.files("data/input/FJC", pattern="*.xlsx", full.names=TRUE)
files <- lapply(filenames, read_excel)
#Name the sub-lists (one for each file) based on the file names
names(files) <- substr(gsub(" ", "_",
                            gsub("\\(", "",
                                 gsub("\\)", "", filenames))), 33, 40)

#Basic data cleaning
for (i in seq_along(files)){
  #Keep only the SO Number, DOB, Gender, Race, and Cell/Cell Location columns
  files[[i]] <- files[[i]][ , grepl( "So|SO|DOB|Gender|Race|Cell" ,
                                     names( files[[i]] ) ) ]
  #Check that each sub-list has the right number of columns
  if (ncol(files[[i]]) != 5){
    files[[i]]$Cell <- NA
  }
  #Add a column for the date of data collection
  files[[i]]$Date <- names(files)[i]
  #Standardize column names and date formats
  colnames(files[[i]])[grepl('So|SO',colnames(files[[i]]))] <- 'SoNum'
  colnames(files[[i]])[grepl('Cell',colnames(files[[i]]))] <- 'Location'
  files[[i]]$Date <- as.Date(files[[i]]$Date, "%Y%m%d")
  files[[i]]$DOB <- as.Date(files[[i]]$DOB)
}

#Concatenate the sub-lists into one long data frame
all_data = do.call(rbind, files)
remove(files, filenames, i)

#First and second datasets
set1 <- all_data[all_data$Date <= as.Date("2022-02-04"), ]
set2 <- all_data[all_data$Date > as.Date("2022-02-04"), ]

#Select most recent DOB/Race and initial Gender for those with multiple
all_data <- all_data %>% group_by(SoNum) %>%
  mutate(DOB = last(DOB), Race = last(Race), Gender = first(Gender)) %>% ungroup

#Remove extra designations from location column
all_data$Location <- gsub(" - Male", "", all_data$Location)
all_data$Location <- gsub(" - Female", "", all_data$Location)
all_data$Location <- gsub(" - Padded Cell", "", all_data$Location)

#Separate data with missing locations
all_data_incl_missing <- all_data
missingLocs <- all_data[is.na(all_data$Location), ]
all_data <- all_data[!is.na(all_data$Location), ]

#Separate standard from special locations
stdLoc <- all_data[!grepl("[A-M]|[O-R]|[T-Z]|[a-z]", all_data$Location), ]
specialLoc <- all_data[grepl("[A-M]|[O-R]|[T-Z]|[a-z]", all_data$Location), ]
loc3LD <- specialLoc[grepl("3LD", specialLoc$Location), ]
specialLoc <- specialLoc[!(grepl("3LD", specialLoc$Location)), ]
stdLoc <- rbind(stdLoc, loc3LD)
remove(loc3LD)

#Separate men from women
m_stdLoc <- subset(stdLoc, Gender == "M")
f_stdLoc <- subset(stdLoc, Gender == "F")

#Jan and Apr subsets
Jan <- m_stdLoc[m_stdLoc$Date >= as.Date("2022-01-11") & m_stdLoc$Date <= as.Date("2022-01-21"), ]
Apr <- m_stdLoc[m_stdLoc$Date >= as.Date("2022-04-01") & m_stdLoc$Date <= as.Date("2022-04-11"), ]

#Count SoNumbers instead of rows
length(unique(Jan$SoNum))
length(unique(Apr$SoNum))
length(unique(m_stdLoc$SoNum))
length(unique(f_stdLoc$SoNum))
length(unique(stdLoc$SoNum))
length(unique(specialLoc$SoNum))
length(unique(all_data$SoNum))
length(unique(all_data_incl_missing$SoNum))
length(unique(missingLocs$SoNum))
length(unique(set1$SoNum))
length(unique(set2$SoNum))
