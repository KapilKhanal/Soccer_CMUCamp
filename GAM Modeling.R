#Model 1. Generalized Additive Model

# -----------------------------------------------------------------------------
# Load libraries/data/etc.
# -----------------------------------------------------------------------------


library(mgcv)
library(tidyverse)

View(head(soccer, 40))
soccer <- read.csv("WomenSoccer2.csv")
View(head(soccer))
within(soccer, rm(X.4, X.3, X.2, X.1, X, possession.lead)) -> soccer

#Make NAs 0 for modeling
soccer$centrality_Origin <-ifelse(is.na(soccer$centrality_Origin), 0, soccer$centrality_Origin)
soccer$centrality_End <- ifelse(is.na(soccer$centrality_End), 0, soccer$centrality_End)
soccer$pageRank_Origin <- ifelse(is.na(soccer$pageRank_Origin), 0, soccer$pageRank_Origin)
soccer$pageRank_End <- ifelse(is.na(soccer$pageRank_End), 0, soccer$pageRank_End)

#no negative shot values
soccer$shot.statsbomb_xg <- ifelse(soccer$shot.statsbomb_xg < 0, 0, soccer$shot.statsbomb_xg)
#Passes only
soccer %>% filter(type.name == "Pass") -> passes

df<- passes

# -----------------------------------------------------------------------------
# Split data
# -----------------------------------------------------------------------------
train_perc <- 0.8

set.seed(25644)
values <- sample(1:nrow(df), train_perc * nrow(df), replace = FALSE)

train <- df[values,]
test <- df[-values,]


# -----------------------------------------------------------------------------
# Write results
# -----------------------------------------------------------------------------
write_csv(train, "train.csv")
write_csv(test, "test.csv")

# --------------------------------------------------------------------------------------------------
# Run generalized additive model,check diagnostics, and append fitted values to original data frame.
# --------------------------------------------------------------------------------------------------

gam.mod.1 <- gam(shot.statsbomb_xg ~  s(x.location,y.location), data = passes, method = "REML")

gam.mod.2 <- gam(shot.statsbomb_xg ~ s(x.location, y.location) +
                   s(end.x.location, end.y.location), 
                 data = train, method = "REML")

gam.mod.3 <-gam(shot.statsbomb_xg ~ s(x.location, y.location,
                  end.x.location, end.y.location),
                data = train, method = "REML")

#_______________________________________ GAM Modeling after addition of new variables (no negatives) 
gam.mod.3 <- gam(shot.statsbomb_xg ~ s(x.location, y.location) +
                   s(end.x.location, end.y.location) + s(centrality_Origin, centrality_End), data = train, method = "REML")

gam.mod.4 <- gam(shot.statsbomb_xg ~ s(x.location, y.location,
                   end.x.location, end.y.location) + s(centrality_Origin, centrality_End), data = train, method = "REML")

gam.mod.5 <- gam(shot.statsbomb_xg ~ s(x.location, y.location,
                                       end.x.location, end.y.location, centrality_Origin, centrality_End), data = train, method = "REML")

#_______________________________________ GAM Modeling  with Page Rank and Centrality 
gam.mod.9 <- gam(shot.statsbomb_xg ~ s(x.location, y.location) +
                   s(end.x.location, end.y.location) + s(centrality_Origin, centrality_End) + s(pageRank_Origin, pageRank_End), data = train, method = "REML")

gam.mod.10 <- gam(shot.statsbomb_xg ~ s(x.location, y.location, end.x.location, end.y.location) + s(centrality_Origin, centrality_End) + 
                    s(pageRank_Origin, pageRank_End) , data = train, method = "REML")

#________________________________________Page Rank only 
gam.mod.11 <- bam(shot.statsbomb_xg ~ s(x.location, y.location, end.x.location, end.y.location) +
        s(pageRank_Origin, pageRank_End) , data = train, method = "REML")

gam.mod.12 <- bam(shot.statsbomb_xg ~ s(x.location, y.location, end.x.location, end.y.location,
                    pageRank_Origin, pageRank_End) , data = train, method = "REML")

#_______________________________________ Everything!
gam.mod.13 <- bam(shot.statsbomb_xg ~ s(x.location, y.location,
                                       end.x.location, end.y.location, centrality_Origin, centrality_End,
                                       pageRank_Origin, pageRank_End), data = train, method = "REML")
#_______________________________________ 


#'heat' map of expected goals at end of chain by location
plot1 <- plot(gam.mod.1, scheme = 2, xlab = "Width", ylab = "Length")
library(jsonlite)

plot1.df <- data.frame()


#Residual Plots and other diagnostics
gam.check(gam.mod.1)
gam.check(gam.mod.2)
plot1 <- plot(gam.mod.2, scheme = 2)
#append fitted values to pass data frame
passes$fittedxG <- gam.mod.1$fitted.values
passes$fittedXG.2<- gam.mod.2$fitted.values


train$fittedXG <- gam.mod.12$fitted.values

train$
# -----------------------------------------------------------------------------------------
# Diagnostic Plot
# -----------------------------------------------------------------------------------------

ggplot(train, aes(x = fittedXG, y = shot.statsbomb_xg)) + geom_point() + 
  labs(title = "Expected Goals vs Fitted Expected Goals", subtitle = "GAM, Start/End Location", x = "Fitted Expected Goals",
       y= "Expected Goals") + theme_minimal()

# -----------------------------------------------------------------------------------------
# Function to calculate adjusted Expected Goal (Expected Goal at end of Pass Chain Per Pass)
# -----------------------------------------------------------------------------------------


sum_fitted <- function(df) {
  sumValues <- sum(df$fittedxG)
  df$fittedXG <- sumValues
  df$fittedXG
}

passes %>% group_by(match, possession, shot.statsbomb_xg) %>% nest() %>% mutate(summ = map(data, sum_fitted)) %>% unnest() -> passes
View(passes)
passes %>% mutate(adjusted = (fittedxG/summ)*shot.statsbomb_xg) -> passes.adj
View(passes)

# -----------------------------------------------------------------------------------------
# Adding an effect per pass column 
# -----------------------------------------------------------------------------------------

#create lag column
passes.adj$lag <- lag(passes.adj$adjusted, n = 1)

#then make sure rows where adjusted = 0 have 0 retroactively
passes.adj$lag <- ifelse(passes.adj$adjusted == 0 , 0, passes.adj$lag)

#difference the two columns
passes.adj$Inc.Per.Pass <- passes.adj$adjusted - passes.adj$lag

#if lag is zero, increase per pass zero
passes.adj$Inc.Per.Pass <- ifelse(passes.adj$lag == 0 , 0, passes.adj$Inc.Per.Pass)

#remove lag and make increase per pass NA if zero.
within(passes.adj, rm(lag)) -> passes.adj
passes.adj$Inc.Per.Pass <- ifelse(passes.adj$Inc.Per.Pass == 0 , NA, passes.adj$Inc.Per.Pass)


# -----------------------------------------------------------------------------------------
# Creating Average Increase Per Pass from Position to Position
# -----------------------------------------------------------------------------------------


###CREATING AVERAGE INC.PER.PASS FROM POSITION TO POSITION
#create a to position variable
passes.adj$to.position.name <- lead(passes.adj$position.name, n = 1)


passes.adj %>% filter(Inc.Per.Pass != 0) %>% group_by(position.name, to.position.name) -> Positions_Inc



# -----------------------------------------------------------------------------
# Test models
# -----------------------------------------------------------------------------


mse.1 <- mean((predict.gam(gam.mod.1, test) - test$shot.statsbomb_xg)^2)
mse.2 <- mean((predict.gam(gam.mod.2, test) - test$shot.statsbomb_xg)^2)
mse.2a <-mean((predict.gam(gam.mod.3, test) - test$shot.statsbomb_xg)^2) #start and end all together

#Models gam.mod 3, 4, and 5, which include negative xG
mse.3 <- mean((predict.gam(gam.mod.3, test) - test$shot.statsbomb_xg)^2) #start +  end + centrality
mse.4 <- mean((predict.gam(gam.mod.4, test) - test$shot.statsbomb_xg)^2) #start, end + centrality
mse.5 <- mean((predict.gam(gam.mod.5, test) - test$shot.statsbomb_xg)^2) # start,end,centrality

#Models without negative xG.
mse.6 <- mean((predict.gam(gam.mod.3, test) - test$shot.statsbomb_xg)^2)
mse.7 <- mean((predict.gam(gam.mod.4, test) - test$shot.statsbomb_xg)^2)
mse.8 <- mean((predict.gam(gam.mod.5, test) - test$shot.statsbomb_xg)^2) #model 5 no XG

#Models without negative xG, including pageRank.
mse.9 <- mean((predict.gam(gam.mod.9, test) - test$shot.statsbomb_xg)^2) #start, end, centrality, page all separate
mse.10 <-mean((predict.gam(gam.mod.10, test) - test$shot.statsbomb_xg)^2) #start end together and centralty, page rank separate

mse.11 <- mean((predict.gam(gam.mod.11, test) - test$shot.statsbomb_xg)^2)
mse.12 <- mean((predict.gam(gam.mod.12, test) - test$shot.statsbomb_xg)^2)

#All information
mse.13 <- mean((predict.gam(gam.mod.13, test) - test$shot.statsbomb_xg)^2)

gam.check(gam.mod.12)







