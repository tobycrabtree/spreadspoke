# NFL read game data

# Read the data for NFL games, teams, and stadiums
rm(list=ls())
nfl <- read.csv("nfl_scores.csv", stringsAsFactors = F) # game scores data
nfl$schedule_date<-as.Date(nfl$schedule_date, "%m/%d/%Y") # format as date
nfl$stadium_type <- as.factor(nfl$stadium_type) # format as factor
teams <- read.csv("nfl_teams.csv",stringsAsFactors= F) # team data
stadiums <- read.csv("nfl_stadiums.csv",stringsAsFactors=F) # stadium data

# Add team IDs which are a 2 or 3 letter team id for each team
team_names <- teams$team_name # vector of team names
team_ids <- teams$team_id # vector of team IDs

nfl$team_away_id <- NA  # initialize the team away id
nfl$team_home_id <- NA # initialize the team home id

for (i in 1:nrow(nfl)) {
        for(j in 1:length(team_ids)){
                if(nfl$team_home[i]==team_names[j]){
                        nfl$team_home_id[i]<-team_ids[j]
                }
        }
}

for (i in 1:nrow(nfl)) {
        for(j in 1:length(team_ids)){
                if(nfl$team_away[i]==team_names[j]){
                        nfl$team_away_id[i]<-team_ids[j]
                }
        }
}

nfl$team_away_id <- as.factor(nfl$team_away_id)  # factor class team away id
nfl$team_home_id <- as.factor(nfl$team_home_id)  # factor class team home id

# game sequence and unique id
library(stringr)
nfl$game_order <- 0
nfl$game_id <- NA 
schedule_first_game_date <- min(nfl$schedule_date)
for(i in 1:nrow(nfl)){
        nfl$game_order[i] <- i # first game is set at 1 with sequence of dates
        nfl$game_id[i] <- paste(as.Date(nfl$schedule_date[i],format='%m/%d/%Y'),nfl$team_away_id[i],nfl$team_home_id[i],sep="") # set unique id for each game 
}  
nfl$game_id <- gsub("-","",nfl$game_id) # remove - from game id

# Create dummy variables for first 4 weeks of season and last week of season [consider week after bye week, or short week ie, sunday>thursday game]
nfl$schedule_week_1to4<-ifelse(nfl$schedule_week<4.1,TRUE,FALSE) # first 4 weeks of season
nfl$schedule_week_last<-NA
for (i in 1:nrow(nfl)) {
        if(nfl$schedule_season[i]==1993|1999){
                nfl$schedule_week_last[i]<-ifelse(nfl$schedule_week[i]==18,TRUE,FALSE)
        }
} # 1993 & 1998 seasons had 18 weeks

for (i in 1:nrow(nfl)) {
        if(nfl$schedule_season[i]==1987){
                nfl$schedule_week_last[i]<-ifelse(nfl$schedule_week[i]==16,TRUE,FALSE)
        }
} # 1987 seasons had week 3 cancelled, weeks 4-6 used replacement players which are excluded from data set, and 16 weeks total

for(i in 1:nrow(nfl)){
        if(nfl$schedule_season[i]!=1987|1993|1999){
                nfl$schedule_week_last[i]<-ifelse(nfl$schedule_week[i]==17,TRUE,FALSE)        
        }
}

# add day of week info
require(lubridate)
nfl$schedule_day <- wday(nfl$schedule_date, label=TRUE) 
nfl$schedule_month <- month(nfl$schedule_date, label=TRUE)

# divisional game True/False
team_divisions <- teams$team_division
nfl$team_home_division <- NA
nfl$team_away_division <- NA

for (i in 1:nrow(nfl)) {
        for(j in 1:length(team_divisions)){
                if(nfl$team_home[i]==team_names[j]){
                        nfl$team_home_division[i]<-team_divisions[j]
                }
        }
}

for (i in 1:nrow(nfl)) {
        for(j in 1:length(team_ids)){
                if(nfl$team_away[i]==team_names[j]){
                        nfl$team_away_division[i]<-team_divisions[j]
                }
        }
}

nfl$team_away_division <- as.factor(nfl$team_away_division)  # factor class team away id
nfl$team_home_division <- as.factor(nfl$team_home_division)  # factor class team home id

## 2002 division and ## pre2002 division
team_divisions_pre2002 <- teams$team_division_pre2002

for (i in 1:nrow(nfl)) {
        for(j in 1:length(team_divisions_pre2002)){
                if(nfl$team_home[i]==team_names[j]){
                        nfl$team_home_division_pre2002[i]<-team_divisions_pre2002[j]
                }
        }
}

for (i in 1:nrow(nfl)) {
        for(j in 1:length(team_divisions_pre2002)){
                if(nfl$team_away[i]==team_names[j]){
                        nfl$team_away_division_pre2002[i]<-team_divisions_pre2002[j]
                }
        }
}

nfl$team_away_division_pre2002 <- as.factor(nfl$team_away_division)  # factor class team away id
nfl$team_home_division_pre2002 <- as.factor(nfl$team_home_division)  # factor class team home id

### division matchup true or false
nfl$division_matchup <- NA
nfl$division_matchup <- ifelse(nfl$team_away_division==nfl$team_home_division,
                               TRUE, ifelse(nfl$team_away_division_pre2002==nfl$team_home_division_pre2002,TRUE,FALSE))


# playoff game
nfl$playoff <- !is.finite(nfl$schedule_week) # if not a week number

nfl <- nfl[c(
        "game_id",
        "game_order",
        "schedule_season",
        "schedule_week",
        "schedule_date",
        "team_home",
        "team_home_id",
        "team_away",
        "team_away_id",
        "stadium",
        "stadium_type",
        "stadium_location",
        "spread",
        "over_under_line",
        "score_home",
        "score_away",
        "schedule_week_1to4",         
        "schedule_week_last",        
        "schedule_day",               
        "schedule_month",             
        "team_home_division",
        "team_away_division",         
        "team_home_division_pre2002", 
        "team_away_division_pre2002",
        "division_matchup",
        "playoff")]
write.csv(nfl,"nfl_gameids.csv")
