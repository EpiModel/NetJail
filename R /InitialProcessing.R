library(readxl)
library(tidyr)

filenames <- list.files("data/input/FJC", pattern="*.xlsx", full.names=TRUE)
files <- lapply(filenames, read_excel)
names(files) <- substr(gsub(" ", "_",
                            gsub("\\(", "",
                                 gsub("\\)", "", filenames))), 16, 40)

for (i in seq_along(files)){
  files[[i]] <- files[[i]][ , grepl( "So|SO|DOB|Gender|Race|Cell" , names( files[[i]] ) ) ]
  files[[i]] <- separate(data = files[[i]], col = 5, into = c("Floor", "Tower", "Block", "Cell"), sep = c(1, 2, 3))
}

list2env(files, .GlobalEnv)
