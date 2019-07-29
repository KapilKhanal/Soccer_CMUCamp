# Data Management 

#Begins with WomenSoccer df. 

# -----------------------------------------------------------------------------
# Remove .name columns that have a corresponding .id column. 
# -----------------------------------------------------------------------------

WomenSoccer.2 <- within(WomenSoccer, rm('player.id','position.id','pass.recipient.id','pass.height.id','pass.body_part.id','pass.type.id','pass.outcome.id','ball_receipt.outcome.id','dribble.outcome.id','shot.body_part.id',
                           'shot.type.id','shot.outcome.id','goalkeeper.technique.id','goalkeeper.position.id',
                           'goalkeeper.body_part.id','goalkeeper.outcome.id','goalkeeper.type.id',
                           'interception.outcome.id','foul_committed.card.id','foul_committed.type.id', 
                           'duel.type.id','duel.outcome.id','50_50.outcome.id','substitution.outcome.id',
                           'substitution.replacement.id','bad_behaviour.card.id'))


# -----------------------------------------------------------------------------
# Separating by just passes and shots.
# -----------------------------------------------------------------------------

passes_and_shots<-WomenSoccer.2 %>% filter(type.name == "Pass" | type.name == "Shot")

# ------------------------------------------------------------------------------------------
# Create a column that denotes the order of a pass/shot (within a possession), counting down.
# ------------------------------------------------------------------------------------------

passes_and_shots %>% group_by(possession,match) %>% mutate(length = rev(seq(1:n()))) -> passes_and_shots


# ------------------------------------------------------------------------------------------
# Convert NA values in the pass.cross and pass.switch columns to FALSE.
# ------------------------------------------------------------------------------------------

passes_and_shots$pass.cross[which(is.na(passes_and_shots$pass.cross))] <- F
passes_and_shots$pass.switch[which(is.na(passes_and_shots$pass.switch))] <- F


# --------------------------------------------------------------------------------------------------------------------------------
# Assign '0' for shots.statbomb_xg to passes in a chain with no shot taken. Otherwise, assign the value of the shot in the chain.
# --------------------------------------------------------------------------------------------------------------------------------

temp <- head(passes_and_shots, 100)

f <- function(df) {
  current.match <- df$match[nrow(df)]
  current.pos <- df$possession[nrow(df)]
  current.fill <- ifelse(is.na(df$shot.statsbomb_xg[nrow(df)]), 0, df$shot.statsbomb_xg[nrow(df)])
  
  for (ii in nrow(df):1) {
    if (!is.na(df$shot.statsbomb_xg[ii]) && abs(df$shot.statsbomb_xg[ii] - current.fill) > 0.0001) {
      current.fill <- df$shot.statsbomb_xg[ii]
    }
    if (df$match[ii] != current.match || df$possession[ii] != current.pos) {
      current.match <- df$match[ii]
      current.pos <- df$possession[ii]
      current.fill <- ifelse(is.na(df$shot.statsbomb_xg[ii]), 0, df$shot.statsbomb_xg[ii])
    }
    df$shot.statsbomb_xg[ii] <- current.fill
  }
  return(df)
}
#test on first 100 rows of data.

temp <- f(passes_and_shots)
df.adjusted <- temp
View(temp)
View(head(passes_and_shots, 100))

#run for entire dataset. 
df.adjusted <- f(passes_and_shots)
# --------------------------------------------------------------------------------------------------------------------------------
# Give the negative of the shot chain expected goals to passes.
# --------------------------------------------------------------------------------------------------------------------------------
df.adjusted$shot.statsbomb_xg[nrow(df.adjusted)]
df.adjusted$possession[nrow(df.adjusted)]

## WITHIN THREE EVENTS

View(head(df.adjusted, 40))
#order of possession which will be useful later
df.adjusted %>% group_by(possession,match) %>% mutate(chain.length = seq(1:n())) -> df.adjusted
#lead the chain length to start the order the event before possession change
df.adjusted %>% group_by(match,possession) %>% mutate(shot.chain.length = lead(chain.length)) -> df.adjusted
#give 1 to all NAs of lead
df.adjusted$shot.chain.length = ifelse(is.na(df.adjusted$shot.chain.length), 1, df.adjusted$shot.chain.length)
#events before a possession that are not shots (passes) are 1 and shots become .5
df.adjusted$shot.chain.length = ifelse(df.adjusted$type.name == "Shot", .5, df.adjusted$shot.chain.length)
#get rid of all other numbers in shot.chain.length except 1 and .5
df.adjusted$shot.chain.length = ifelse(df.adjusted$shot.chain.length == 1 |df.adjusted$shot.chain.length == .5 ,
                                       df.adjusted$shot.chain.length, 0)

#lead the possession so that it starts the event before possession change
df.adjusted$possession.lead <- lead(df.adjusted$possession, 1)

#create a possession pass chain length counting backwards, derivative of length earlier 
df.adjusted %>% group_by(possession.lead,match) %>% mutate(lead.chain.length = rev(seq(1:n())) ) -> df.adjusted

#possession length is 0 if not an event before change of possession
df.adjusted$lead.chain.length = ifelse(df.adjusted$shot.chain.length == 1 |df.adjusted$shot.chain.length == .5
                                       ,df.adjusted$lead.chain.length , 0)

#lead expected goals column this way xG of next shot will be on pass before change
df.adjusted$shot.statsbomb_nxg <- lead(df.adjusted$shot.statsbomb_xg, 1)

#lead pass.type.name this way pass type of next pass will be on pass before change
df.adjusted$lead_pass.type.name <- lead(df.adjusted$pass.type.name, 1)

#if a pass and the pass chain length var is less than 4 (3 events in chain):
#give the negative of the xG of the next shot. otherwise remain same.
df.adjusted$shot.statsbomb_xg <- ifelse(df.adjusted$shot.chain.length == 1 & df.adjusted$lead.chain.length <= 4 &
                                        !(df.adjusted$pass.type.name %in% c("Kick Off"))
                                        ,-1*df.adjusted$shot.statsbomb_nxg, df.adjusted$shot.statsbomb_xg)

View(head(df.adjusted,40))

## ENTIRE SEQUENCE PRECEDING A SEQUENCE WITH A SHOT. (with three events code)

#define a temporary xG column constructed similarly to the original xG column created for 3 event code.
df.adjusted$shot.statsbomb_txg <- ifelse(df.adjusted$shot.chain.length == 1 & df.adjusted$lead.chain.length <= 4 &
                                          !(df.adjusted$pass.type.name %in% c("Kick Off"))
                                        ,-1*df.adjusted$shot.statsbomb_nxg, df.adjusted$shot.statsbomb_xg)

#make all non negatives in this column zero.
df.adjusted$shot.statsbomb_txg <- ifelse(df.adjusted$shot.statsbomb_txg <0, df.adjusted$shot.statsbomb_txg, 0)

#end possession xG Function

endPossXG <- function(df) {
  
  lastRow = df[nrow(df), ]$shot.statsbomb_txg
  df$ExpectedGoal = lastRow
  return(df)
}

df.adjusted <- df.adjusted %>% group_by(match,possession) %>% nest() %>%
  mutate(goal = purrr:: map(data, endPossXG)) %>% unnest()

View(head(df.adjusted,40))


#combine negative and positives into final column.
df.adjusted$ExpectedGoal<- ifelse(df.adjusted$shot.statsbomb_xg > 0, df.adjusted$shot.statsbomb_xg, df.adjusted$ExpectedGoal)



# --------------------------------------------------------------------------------------------------------------------------------
# Select Desired Variables and create a new data frame. 
# --------------------------------------------------------------------------------------------------------------------------------

library(tidyverse)

#name to a new file named soccer. 
df.adjusted %>% select(match, possession, team.name, type.name, ExpectedGoal, possession_team.name,position.name,player.name, 
                       pass.recipient.name, pass.outcome.name, pass.angle, pass.length, pass.switch,
                        pass.cross, duration, minute,second,position.name,pass.height.name, 
                        pass.body_part.name,pass.type.name,location,length,pass.end_location,shot.end_location) -> soccer




# --------------------------------------------------------------------------------------------------------------------------------
# Create separate x and y location columns for both passes and shots. 
# --------------------------------------------------------------------------------------------------------------------------------


soccer$x.location = 1:nrow(soccer)
soccer$y.location = 1:nrow(soccer)

for (i in 1:nrow(soccer)) {
  soccer$x.location[i] = soccer$location[[i]][1]
  soccer$y.location[i] = soccer$location[[i]][2]
}


View(head(data))

# --------------------------------------------------------------------------------------------------------------------------------
# Create combined end x location, end y location columns for both passes and shots. 
# --------------------------------------------------------------------------------------------------------------------------------


true_false<- unlist(map(.x = soccer$pass.end_location,.f = is.null))
true_false

soccer$end.location <- ifelse(true_false, soccer$shot.end_location, soccer$pass.end_location)


soccer$end.x.location <- 1:nrow(soccer)
soccer$end.y.location <- 1:nrow(soccer)

for (i in 1:nrow(soccer)) {
  soccer$end.x.location[i] = soccer$end.location[[i]][1]
  soccer$end.y.location[i] = soccer$end.location[[i]][2]
}


# --------------------------------------------------------------------------------------------------------------------------------
# Add degree centrality and pagerank to data. 
# --------------------------------------------------------------------------------------------------------------------------------

### Make player name and pass recipient names identical to those in pagerank/centrality csv. 

#adapted from N-gram code
strReplace<- function(s,pattern = " |-", replacement = "_")
  
{ return (str_replace_all(s,pattern,replacement))}

#string functions 
soccer %>% mutate(player.name = map(player.name, strReplace), pass.recipient.name = map(pass.recipient.name, strReplace)) -> soccer
soccer %>% mutate(player.name = map(player.name, str_to_lower), pass.recipient.name = map(pass.recipient.name, str_to_lower)) -> soccer

#read-in rates (file can be obtained by running through 'EDA- Bigrams' code)
weights <- read.csv("https://raw.githubusercontent.com/KapilKhanal/Soccer/master/Player_with_weights_per_match.csv")

##PASS ORIGIN

#rename column to prepare for join.
weights %>% rename(player.name = label) -> weights

#preparing for join further 
weights$player.name <- as.character(unlist(weights$player.name))
soccer$player.name <- as.character(unlist(soccer$player.name))
soccer$pass.recipient.name <- as.character(unlist(soccer$pass.recipient.name))
within(weights, rm(id, X)) -> weights

View(head(soccer,40))
#perform left join 
soccer <- left_join(soccer, weights)
#rename to prepare for pass end join
soccer %>% rename(centrality_Origin = centrality, pageRank_Origin = pageRank) -> soccer


##PASS END
weights %>% rename(pass.recipient.name = player.name) -> weights
soccer <- left_join(soccer, weights)
soccer %>% rename(centrality_End = centrality, pageRank_End = pageRank) -> soccer
soccer
#Make NAs 0 for modeling
soccer$centrality_Origin <-ifelse(is.na(soccer$centrality_Origin), 0, soccer$centrality_Origin)
soccer$centrality_End <- ifelse(is.na(soccer$centrality_End), 0, soccer$centrality_End)
soccer$pageRank_Origin <- ifelse(is.na(soccer$pageRank_Origin), 0, soccer$pageRank_Origin)
soccer$pageRank_End <- ifelse(is.na(soccer$pageRank_End), 0, soccer$pageRank_End)


str(weights)
str(soccer)
View(soccer)


# --------------------------------------------------------------------------------------------------------------------------------
# Add close to shot column
# --------------------------------------------------------------------------------------------------------------------------------

close.to.shot <- function(df, INTERVAL = 15) {
  
  df %>% mutate(time = (60 * minute) + second) -> df
  
  lastShotTime <- 99999999
  current.match <- df$match[nrow(df)]
  current.pos <- df$possession[nrow(df)]
  
  for (ii in nrow(df):1) {
    if (df$type.name[ii] == "Shot") {
      lastShotTime <- df$time[ii]
      current.match <- df$match[ii]
      current.pos <- df$possession[ii]
      df$close_to_shot[ii] <- 1
    } else if (lastShotTime - df$time[ii] <= INTERVAL &&
               df$match[ii] == current.match &&
               df$possession[ii] == current.pos) {
      df$close_to_shot[ii] <- 1
    } else {
      df$close_to_shot[ii] <- 0
    }
  }
  
  return(df)
}

soccer$close_to_shot <- 1:nrow(soccer)

soccer <- close.to.shot(soccer)


# --------------------------------------------------------------------------------------------------------------------------------
# Write to Final File.  
# --------------------------------------------------------------------------------------------------------------------------------

write.csv(soccer %>% select(-c(location,pass.end_location,shot.end_location,end.location, time)), file = "WomenSoccer3.csv")


------------------------------------------------------------------------------
# Split data
------------------------------------------------------------------------------
df <- soccer

train_perc <- 0.8

set.seed(25644)
values <- sample(1:nrow(df), train_perc * nrow(df), replace = FALSE)

train <- df[values,]
test <- df[-values,]

View(head(train))

# -----------------------------------------------------------------------------
# Write results
# -----------------------------------------------------------------------------
write_csv(train, "train3-1.csv")
write_csv(test, "test3-2.csv")





