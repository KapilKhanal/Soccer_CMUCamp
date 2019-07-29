library(tidyverse)
library(rjson)
library(data.table)
library(magrittr)


data_path <- 'open-data-master/data/events'

data_path_matches<-'open-data-master/data/matches'

files <- dir(path = data_path,pattern = "*.json")

data <- tibble(match = files) %>% # create a data frame
  # holding the file names
  mutate(data = map(files,function(x) jsonlite::fromJSON(txt = file.path(data_path, x) , flatten = TRUE) ) # a new data column
  )

data<-data %>% unnest()

View(head(data))

#Getting the data only for the certain matches
files_matches <- dir(path = data_path_matches,pattern = "*.json")

matches<- map(files_matches,function(x) jsonlite::fromJSON(txt = file.path(data_path_matches, x))) %>% reduce(rbind)

data_matches <- tibble(match = files_matches) %>% # create a data frame
  # holding the file names
  mutate(data = map(files_matches,function(x) jsonlite::fromJSON(txt = file.path(data_path_matches, x) , flatten = TRUE) ) # a new data column
  ) %>% unnest()

data<-data %>% mutate(matchID = gsub(".json","",match))
#Putting only FIFA WORLD CUP WOMEN

"Women's World Cup"

WomenSoccerMatches<-data_matches %>% filter(data_matches$competition.competition_name=="Women's World Cup")

WomenMatchID<-WomenSoccerMatches$match_id
#Checking for only womens world cup ID in original data
WomenSoccer<-data %>% mutate(matchID = gsub(".json","",match)) %>% filter(matchID %in% WomenMatchID)
write.csv(WomenSoccer,"/Users/qe5106hi/OneDrive - MNSCU/CMSACamp-master/open-data-master/SoccerCMSA_CMU/Scripts/Data_Wrangling/Data_after_script/WomenSoccer.csv")

