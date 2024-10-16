# Baby Name Wrangling
library(tidyverse)
## set working directory
setwd("C:/Github/App-A-Day/04_baby_names")
## get all .txt files in directory
dir <- list.files(pattern="\\.txt$")
## read txt files, separating by comma
data <- lapply(dir, read.delim, sep=",", header=FALSE)
## initialize dataframe
datjoin <- as.data.frame(data[1]) %>%
  rename(Name = "V1",
         Gender = "V2",
         Count = "V3") %>%
  mutate(Year = 1880)
mdat <- subset(datjoin, Gender == "M")
mdat <- mdat[c(1:50),]
fdat <- subset(datjoin, Gender == "F")
fdat <- fdat[c(1:50),]
datjoin <- rbind(fdat, mdat)
## set variables for year and year range
## to make this more robust we could get the year from the file name and include
## it in the original data frame, but for now I'm not going to do that.
Year = 1880
yr_range <- c(1:144)
## for each in year range, we get the data from the list and 
## add it to the joined data frame
for (x in yr_range) {
  Year <- Year + 1
  newdata <- as.data.frame(data[x]) %>%
    rename(Name = "V1",
           Gender = "V2",
           Count = "V3") %>%
    mutate(Year = Year)
  mdat <- subset(newdata, Gender == "M")
  mdat <- mdat[c(1:50),]
  fdat <- subset(newdata, Gender == "F")
  fdat <- fdat[c(1:50),]
  newdata <- rbind(fdat, mdat)
  datjoin <- rbind(datjoin, newdata)
}

## write to a csv
write.csv(datjoin, "Top50BabyNames1880to2024.csv")
