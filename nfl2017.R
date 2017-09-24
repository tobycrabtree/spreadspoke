# current NFL season data processing
nflCurrent <- read.csv("spreadspoke_scores.csv", stringsAsFactors = F) # game scores data
nflCurrent$schedule_date<-as.Date(nflCurrent$schedule_date, "%m/%d/%Y") # format as date
teams <- read.csv("nfl_teams.csv",stringsAsFactors= F) # team data
stadiums <- read.csv("nfl_stadiums.csv",stringsAsFactors=F) # stadium data

#Schedule data
# game sequence and unique id
require(stringr)
#nflCurrent$game_order <- 0
nflCurrent$game_id <- NA 
schedule_first_game_date <- min(nflCurrent$schedule_date)
for(i in 1:nrow(nflCurrent)){
        #nflCurrent$game_order[i] <- i # first game is set at 1 with sequence of dates
        nflCurrent$game_id[i] <- paste(as.Date(nflCurrent$schedule_date[i],format='%m/%d/%Y'),nflCurrent$team_away_id[i],nflCurrent$team_home_id[i],sep="") # set unique id for each game 
}  
nflCurrent$game_id <- gsub("-","",nflCurrent$game_id) # remove - from game id

# Add Day of week and month
require(lubridate)
nflCurrent$schedule_day <- wday(nflCurrent$schedule_date, label=TRUE) 
nflCurrent$schedule_month <- month(nflCurrent$schedule_date, label=TRUE)

# Elo ratings from Elo package
nflCurrent$team_away_id <- as.character(nflCurrent$team_away_id)  # factor class team away id
nflCurrent$team_home_id <- as.character(nflCurrent$team_home_id)  # factor class team home id

elo_current <- read.csv("elo_current_2017.csv")
nflCurrent$team_home_elo_predicted <- NA
nflCurrent$team_away_elo_predicted <- NA

for (i in 1:nrow(nflCurrent)) {
        for(j in 1:length(elo_current$team)){
                if(nflCurrent$team_home_id[i]==elo_current$team[j]){
                        nflCurrent$team_home_elo_predicted[i]<-elo_current$elo[j]
                }
        }
}

for (i in 1:nrow(nflCurrent)) {
        for(j in 1:length(elo_current$team)){
                if(nflCurrent$team_away_id[i]==elo_current$team[j]){
                        nflCurrent$team_away_elo_predicted[i]<-elo_current$elo[j]
                }
        }
}


nflCurrent$team_elo_pick <- ifelse(nflCurrent$team_home_elo_predicted==nflCurrent$team_away_elo_predicted,
                               "Pick Em",ifelse(nflCurrent$team_home_elo_predicted>nflCurrent$team_away_elo_predicted,
                                                nflCurrent$team_home_id,nflCurrent$team_away_id) )

# Spreads 
nflCurrent$spread_predicted <- 0.9427486-0.0245085*nflCurrent$team_home_elo_pre+0.0206702*nflCurrent$team_away_elo_pre
nflCurrent$spread_predicted <- ifelse(is.na(nflCurrent$spread_predicted),-2.5,round((nflCurrent$spread_predicted*2))/2)
nflCurrent$spread_predicted_winner_id <- ifelse(nflCurrent$spread_predicted-nflCurrent$spread==0,"Pick Em",
                                          ifelse(nflCurrent$spread_predicted-nflCurrent$spread<0,
                                                 as.character(nflCurrent$team_home_id),
                                                 as.character(nflCurrent$team_away_id)))
nflCurrent$spread_predicted_winner <- ifelse(nflCurrent$spread_predicted-nflCurrent$spread==0,"Pick Em",
                                       ifelse(nflCurrent$spread_predicted-nflCurrent$spread<0,
                                              paste0(nflCurrent$team_home_id,c(" ("),nflCurrent$spread,c(")")),
                                              paste0(nflCurrent$team_away_id,c(" ("),-nflCurrent$spread,c(")"))))

#nflCurrent$team_ats_pick <- ifelse(nflCurrent$team_ats_pick==c("Pick Em"),"Pick Em",
#                                   ifelse(nflCurrent$team_ats_pick==nflCurrent$team_home_id,
#                                   paste0(nflCurrent$team_ats_pick,c(" "),nflCurrent$spread),
#                                   paste0(nflCurrent$team_ats_pick,c(" +"),nflCurrent$spread)))

# factors over under = median total 41, look at wind, cold weather, teams avg pts scored, teams run tendency (more runs uses more time), 


team_pts_current <- read.csv("nfl_season_pts.csv")
team_pts_current <- subset(team_pts_current,season==2016)

nflCurrent$over_under_predicted <- NA # add this

for (i in 1:nrow(nflCurrent)) {
        for(j in 1:length(team_pts_current$team)){
                if(nflCurrent$team_home_id[i]==team_pts_current$team[j]){
                        nflCurrent$score_offense_home_predicted[i]<-team_pts_current$avgptsfor[j]
                        nflCurrent$score_defense_home_predicted[i]<-team_pts_current$avgptsagainst[j]
                        nflCurrent$team_offense_home[i]<-as.character(team_pts_current$team_offense[j])
                        nflCurrent$team_defense_home[i]<-as.character(team_pts_current$team_offense[j])
                        
                }
        }
}

for (i in 1:nrow(nflCurrent)) {
        for(j in 1:length(team_pts_current$team)){
                if(nflCurrent$team_away_id[i]==team_pts_current$team[j]){
                        nflCurrent$score_offense_away_predicted[i]<-team_pts_current$avgptsfor[j]
                        nflCurrent$score_defense_away_predicted[i]<-team_pts_current$avgptsagainst[j]
                        nflCurrent$team_offense_away[i]<-as.character(team_pts_current$team_offense[j])
                        nflCurrent$team_defense_away[i]<-as.character(team_pts_current$team_offense[j])
                }
        }
}

nflCurrent$score_predicted <- round(((nflCurrent$score_offense_home_predicted+nflCurrent$score_offense_away_predicted
                                   +nflCurrent$score_defense_home_predicted+nflCurrent$score_defense_away_predicted)/2),digits=0)

nflCurrent$over_under_predicted <- round(34.7 +
                                           0.001934*nflCurrent$team_home_elo_predicted +
                                           0.004540*nflCurrent$team_away_elo_predicted +
                                           ifelse(nflCurrent$team_offense_home=="strong",3.495672,ifelse(nflCurrent$team_offense_home=="weak",-1.387331,0))+
                                           ifelse(nflCurrent$team_defense_away=="strong",-1.880359,ifelse(nflCurrent$team_defense_away=="weak",1.888973,0))+
                                           ifelse(nflCurrent$team_offense_away=="strong",2.691029,ifelse(nflCurrent$team_offense_away=="weak",-1.176916,0))+
                                           ifelse(nflCurrent$team_defense_home=="strong",-1.867485,ifelse(nflCurrent$team_defense_home=="weak",2.280479,0)),
                                   digits=0)

nflCurrent$over_under_pick <- ifelse(nflCurrent$over_under_line==nflCurrent$over_under_predicted,
                                   "Pick Em",ifelse(nflCurrent$over_under_predicted>nflCurrent$over_under_line,
                                                    "Over","Under") )
nflCurrent$over_under_pick <- paste0(nflCurrent$over_under_pick,c(" ("),nflCurrent$over_under_line,c(")"),sep=" ")

# write csv file

nflCurrent2 <- nflCurrent[c("schedule_week","schedule_date","team_home_id","team_away_id",
       "team_elo_pick","spread_predicted_winner","over_under_pick")]
nflCurrent2Names <- c("schedule_week","Date","Home Team","Away Team","Winner Pick","Spread Pick","Over/Under Pick")
colnames(nflCurrent2) <- nflCurrent2Names        
write.csv(nflCurrent2, "games.csv")

