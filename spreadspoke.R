# Spreadspoke 

nfl <- read.csv("spreadspoke_scores.csv", stringsAsFactors=F)
nfl$schedule_date<-as.Date(nfl$schedule_date, "%m/%d/%Y") # format as date
nflForecast <- subset(nfl,nfl$schedule_date > as.Date('2017-09-19')) # data for forecast games
nfl <- subset(nfl,nfl$schedule_date < as.Date('2017-09-19')) # data for played games

# Add team IDs which are a 2 or 3 letter team id for each team
teams <- read.csv("nfl_teams.csv",stringsAsFactors= F) # team data
team_names <- teams$team_name # vector of team names
team_ids <- teams$team_id # vector of team IDs

nfl$team_home_id <- NA # initialize the team home id
nfl$team_away_id <- NA  # initialize the team away id

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

# game unique id
library(stringr)
nfl$game_id <- NA 
schedule_first_game_date <- min(nfl$schedule_date)
for(i in 1:nrow(nfl)){
        nfl$game_id[i] <- paste(as.Date(nfl$schedule_date[i],format='%m/%d/%Y'),nfl$team_away_id[i],nfl$team_home_id[i],sep="") # set unique id for each game 
}  
nfl$game_id <- gsub("-","",nfl$game_id) # remove - from game id

# stadium type info
stadiums <- read.csv("nfl_stadiums.csv",stringsAsFactors=F) # stadium data
stadiums_names <- as.character(stadiums$stadium_name)
stadiums_types <- as.character(stadiums$stadium_type)
nfl$stadium_type <- NA # initialize
nfl$stadium <- as.character(nfl$stadium)

for (i in 1:nrow(nfl)) {
        for(j in 1:length(stadiums)){
                if(as.character(nfl$stadium[i])==stadiums_names[j]){
                        nfl$stadium_type[i]<-stadiums_types[j]
                }
        }
}

nfl$stadium_type <- as.factor(nfl$stadium_type) # initialize

# playoff game
nfl$schedule_playoff <- !is.finite(nfl$schedule_week) # if not a week number

# Create dummy variables for first 4 weeks of season and last week of season [consider week after bye week, or short week ie, sunday>thursday game]
nfl$schedule_week_1to4<-ifelse(nfl$schedule_week<4.1,TRUE,FALSE) # first 4 weeks of season
nfl$schedule_week_last<-NA
for (i in 1:nrow(nfl)) {
        if(nfl$schedule_season[i]==1993|1999){
                nfl$schedule_week_last[i]<-ifelse(nfl$schedule_week[i]==18,TRUE,FALSE)
        }
} # 1993 & 1998 seasons had 18 weeks

for (i in 1:nrow(nfl)) {
        if(nfl$schedule_season[i] %in% 1987){
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
nfl$schedule_sunday <- ifelse(nfl$schedule_day%in%c("Sun"),TRUE,FALSE) 

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
nfl$team_home_division <- NULL # no longer need
nfl$team_away_division <- NULL # no longer need
nfl$team_home_division_pre2002 <- NULL # no longer need
nfl$team_away_division_pre2002 <- NULL # no longer need

# spread types
nfl$team_home_favorite <- as.character(nfl$team_favorite_id)==as.character(nfl$team_home_id) 
nfl$spread_home <- ifelse(nfl$team_home_favorite==TRUE, nfl$spread_favorite,-nfl$spread_favorite)
nfl$spread_away <- -nfl$spread_home

nfl$spread_type<-ifelse(nfl$spread_home==0,'PICK',
                           ifelse(nfl$spread_home>0,'home underdog','home favorite'))
nfl$spread_type<-as.factor(nfl$spread_type)
nfl$spread_outlier <- ifelse(abs(nfl$spread_favorite) > 14.1, '2TD+',
                                ifelse(abs(nfl$spread_favorite) > 10.1, '1TD1FG+',
                                       ifelse(abs(nfl$spread_favorite) > 7.1, '1TD+','no outlier')))

# over under types
nfl$over_under_outlier <- ifelse(nfl$over_under_line<33,"under 2sd",
                                    ifelse(nfl$over_under_line<37,"under 1sd",
                                           ifelse(nfl$over_under_line>50,"over 2sd",
                                                  ifelse(nfl$over_under_line>46,"over 1sd","no outlier"))))
nfl$over_under_outlier <- as.factor(nfl$over_under_outlier)

# elo ratings
require(EloRating) # use the elo rating package
require(zoo)
nfl$tie <- nfl$score_away==nfl$score_home
nfl$team_winner <- ifelse(nfl$score_away==nfl$score_home,as.character(nfl$team_home_id),
                           ifelse(nfl$score_away>nfl$score_home,as.character(nfl$team_away_id),as.character(nfl$team_home_id)))
nfl$team_loser <- ifelse(nfl$score_away==nfl$score_home,as.character(nfl$team_away_id),
                          ifelse(nfl$score_away<nfl$score_home,as.character(nfl$team_away_id),as.character(nfl$team_home_id)))
nfl$team_winner <- as.factor(nfl$team_winner)
nfl$team_loser <- as.factor(nfl$team_loser)

# Home/Away team result-----
nfl$team_home_result<-ifelse(nfl$score_home>nfl$score_away,"Win",
                              ifelse(nfl$score_home==nfl$score_away,"Tie", "Loss"))
nfl$team_away_result<-ifelse(nfl$score_home<nfl$score_away,"Win",
                              ifelse(nfl$score_home==nfl$score_away,"Tie", "Loss"))
nfl$team_home_result<-as.factor(nfl$team_home_result)
nfl$team_away_result<-as.factor(nfl$team_away_result)

seqcheck(winner=nfl$team_winner,loser=nfl$team_loser,Date=nfl$schedule_date,draw=nfl$tie)
seq <- elo.seq(winner=nfl$team_winner, loser=nfl$team_loser,draw=nfl$tie,Date=nfl$schedule_date)
#eloplot(seq, from="2016-09-01",interpolate="no") #plots the elo ratings

nfl$team_win_elo_pre<-1000 # sets the initial elo rating at 1000
nfl$team_lose_elo_pre<-1000
elo_winners<-seq[[6]][4] # elo score before game for winning team
nfl$team_win_elo_pre <- as.numeric(unlist(elo_winners))
elo_losers<-seq[[6]][5] # elo score before game for losing team
nfl$team_lose_elo_pre <- as.numeric(unlist(elo_losers))

# home team and away team elo scores
nfl$team_home_elo_pre <- NA
nfl$team_away_elo_pre <- NA
nfl$team_home_id <- as.character(nfl$team_home_id)
nfl$team_winner <- as.character(nfl$team_winner)
for(i in 1:nrow(nfl)){
        nfl$team_home_elo_pre[i]<-ifelse(nfl$tie[i]==TRUE,nfl$team_win_elo_pre[i],ifelse(nfl$team_home_id[i]==nfl$team_winner[i],nfl$team_win_elo_pre[i],nfl$team_lose_elo_pre[i]))
        nfl$team_away_elo_pre[i]<-ifelse(nfl$tie[i]==TRUE,nfl$team_lose_elo_pre[i],ifelse(nfl$team_away_id[i]==nfl$team_winner[i],nfl$team_win_elo_pre[i],nfl$team_lose_elo_pre[i]))        
}

# difference between home team's pre-game elo and away team's pre-game elo
nfl$elo_pre_difference<-0
for(i in 1:nrow(nfl)){
        nfl$elo_pre_difference[i] <- nfl$team_home_elo_pre[i]-nfl$team_away_elo_pre[i] #  value of difference in pre game elo scores between home and away team)
}
nfl$team_home_elo_pre_diff <- nfl$elo_pre_difference
nfl$team_away_elo_pre_diff <- -nfl$elo_pre_difference
nfl$team_home_win_prob <- winprob(nfl$team_home_elo_pre,nfl$team_away_elo_pre)
nfl$team_away_win_prob <- 1-nfl$team_home_win_prob
nfl$team_home_win_prob_diff <- nfl$team_home_win_prob-nfl$team_away_win_prob

# score total & post game win/loss/tie
nfl$score_total <- nfl$score_home + nfl$score_away
nfl$team_home_win_count <- ifelse(nfl$team_home_result %in% c("Tie"),0.5,
                                      ifelse(nfl$team_home_result %in% c("Win"),1,0)) # 1 = win, 0.5 = tie, 0 = loss
nfl$team_away_win_count <- ifelse(nfl$spread_away_result %in% c("Tie"),0.5,
                                      ifelse(nfl$team_away_result %in% c("Win"),1,0)) # 1 = win, 0.5 = tie, 0 = loss


# over/under analysis
nfl$over_under_result <- ifelse(nfl$score_total==nfl$over_under_line, 'Push', 
                                   ifelse(nfl$score_total > nfl$over_under_line,
                                          'Over','Under'))
# spread analysis
nfl$spread_home_result<-nfl$score_away-nfl$score_home # spread home team result, i.e., away score less home score
nfl$spread_away_result<-nfl$score_home-nfl$score_away # spread away team result

nfl$score_favorite <- ifelse(nfl$team_favorite_id %in% c("PICK"),0,
                                    ifelse(nfl$team_favorite_id==nfl$team_home_id,nfl$score_home,nfl$score_away)) # favorite spread result = underdog score - favorite score
nfl$score_underdog <- ifelse(nfl$team_favorite_id %in% c("PICK"),0,
                             ifelse(nfl$team_favorite_id==nfl$team_home_id,nfl$score_away,nfl$score_home)) # favorite spread result = underdog score - favorite score

nfl$spread_favorite_result <- ifelse(nfl$team_favorite_id %in% c("PICK"),"Push",
                                     ifelse(nfl$spread_home_result==nfl$spread_favorite,"Push",
                                     ifelse(nfl$team_favorite_id==nfl$team_home_id,nfl$spread_home_result,nfl$spread_away_result))) # favorite spread result = underdog score - favorite score
nfl$spread_favorite_cover_result <- ifelse(nfl$spread_favorite_result %in% c("PICK"),"Push",
                                          ifelse(nfl$spread_home_result==nfl$spread_favorite,"Push",
                                          ifelse((nfl$score_favorite+nfl$spread_favorite)>nfl$score_underdog,"Cover","Did Not Cover"))) # 1 = cover, 0.5 = push, 0 = did not cover
nfl$spread_favorite_cover_count <- ifelse(nfl$spread_favorite_result %in% c("Push"),0.5,
                                    ifelse(nfl$spread_favorite_result %in% c("Cover"),1,0)) # 1 = cover, 0.5 = push, 0 = did not cover
nfl$spread_underdog_cover_result <- ifelse(nfl$spread_favorite_result %in% c("Push"),"Push",
                                           ifelse(nfl$spread_away_result==nfl$spread_favorite,"Push",
                                           ifelse((nfl$score_favorite+nfl$spread_favorite)>nfl$score_underdog,"Did Not Cover","Cover"))) # 1 = cover, 0.5 = push, 0 = did not cover
nfl$spread_underdog_cover_count <- ifelse(nfl$spread_favorite_result %in% c("Push"),0.5,
                                          ifelse(nfl$spread_favorite_result %in% c("Cover"),0,1)) # 1 = cover, 0.5 = push, 0 = did not cover

nfl$spread_home_cover_result <- ifelse(nfl$team_home_favorite==TRUE,
                                          nfl$spread_favorite_cover_result,
                                          nfl$spread_underdog_cover_result)
nfl$spread_away_cover_result <- ifelse(nfl$team_home_favorite==FALSE,
                                          nfl$spread_favorite_cover_result,
                                          nfl$spread_underdog_cover_result)  

nfl$spread_home_cover_result <- as.factor(nfl$spread_home_cover_result)
nfl$spread_away_cover_result <- as.factor(nfl$spread_away_cover_result)

nfl$spread_home_cover_count <- ifelse(nfl$spread_home_cover_result %in% c("Push"),0.5,
      ifelse(nfl$spread_home_cover_result %in% c("Cover"),1,0)) # 1 = cover, 0.5 = push, 0 = did not cover
nfl$spread_away_cover_count <- ifelse(nfl$spread_away_cover_result %in% c("Push"),0.5,
                                      ifelse(nfl$spread_away_cover_result %in% c("Cover"),1,0)) # 1 = cover, 0.5 = push, 0 = did not cover

# team rolling stats data prep team v opponent format
require(plyr)
require(dplyr)
require(FSA)
require(zoo)

nflCalc<-rbind(
        nflHome=data.frame(game_id=nfl[,'game_id'],
                           season=nfl[,'schedule_season'], 
                           schedule_week=nfl[,'schedule_week'], 
                           team=nfl[,'team_home_id'], 
                           opponent=nfl[,'team_away_id'],
                           schedule_date=nfl[,'schedule_date'],
                           venue=rep("home", n=nrow(nfl)), 
                           score=nfl$score_home,
                           score_against=nfl$score_away,
                           score_margin=nfl$score_home-nfl$score_away,
                           spread=nfl$spread_home,
                           over=nfl$over_under_line,
                           elo=nfl$team_home_elo_pre,
                           wins=nfl$team_home_win_count,
                           covers=nfl$spread_home_cover_count,
                           game_count=1
        )
        ,
        
        nflAway=data.frame(game_id=nfl[,'game_id'],
                           season=nfl[,'schedule_season'],
                           schedule_week=nfl[,'schedule_week'], 
                           team=nfl[,'team_away_id'], 
                           opponent=nfl[,'team_home_id'],
                           schedule_date=nfl[,'schedule_date'],
                           venue=rep("away", n=nrow(nfl)), 
                           score=nfl$score_away,
                           score_against=nfl$score_home,
                           score_margin=nfl$score_away-nfl$score_home,
                           spread=nfl$spread_away,
                           over=nfl$over_under_line,
                           elo=nfl$team_away_elo_pre,
                           wins=nfl$team_away_win_count,
                           covers=nfl$spread_away_cover_count,
                           game_count=1
        )
)

nflCalc <- arrange(nflCalc,schedule_date)

# team offense/defense avg pts scored/allowed + offense type
team_points <- ddply(nflCalc,.(team,season),summarize,avgptsfor=round(mean(score),digits=1),avgptsagainst=round(mean(score_against),digits=1))

#nfl$team_offense_home <- ifelse(is.na(nflStreakCalc2$score_avg_pts_for_roll_lag_home)==TRUE,"neutral",ifelse(nflStreakCalc2$score_avg_pts_for_roll_lag_home>24,"strong",ifelse(nflStreakCalc2$score_avg_pts_for_roll_lag_home<18,"weak","neutral")))
#nfl$team_defense_home <- ifelse(is.na(nflStreakCalc2$score_avg_pts_against_roll_lag_home)==TRUE,"neutral",ifelse(nflStreakCalc2$score_avg_pts_against_roll_lag_home>24,"weak",ifelse(nflStreakCalc2$score_avg_pts_against_roll_lag_home<18,"strong","neutral")))
#nfl$team_offense_away <- ifelse(is.na(nflStreakCalc2$score_avg_pts_for_roll_lag_away)==TRUE,"neutral",ifelse(nflStreakCalc2$score_avg_pts_for_roll_lag_away>24,"strong",ifelse(nflStreakCalc2$score_avg_pts_for_roll_lag_away<18,"weak","neutral")))
#nfl$team_defense_away <- ifelse(is.na(nflStreakCalc2$score_avg_pts_against_roll_lag_away)==TRUE,"neutral",ifelse(nflStreakCalc2$score_avg_pts_against_roll_lag_away>24,"weak",ifelse(nflStreakCalc2$score_avg_pts_against_roll_lag_away<18,"strong","neutral")))

# team win-loss record, win-loss record against the spread
nflSeasonWins <- ddply(nflCalc,.(team,season),summarize,wins_pct=sum(wins)/sum(game_count)) # need counter for game played

# team over %
nflSeasonCovers <- ddply(nflCalc,.(team,season),summarize,covers_pct=sum(covers)/sum(game_count)) # need counter for game played

# weather variables   NEED TO UPDATE FROM PRE 2009 WEATHER DETAIL INFO
nfl$weather_cold <- ifelse(is.na(nfl$weather_temperature),FALSE,ifelse(nfl$weather_temperature < 36,TRUE,FALSE))       
nfl$weather_wind_bad <- ifelse(is.na(nfl$weather_wind_mph),FALSE,ifelse(nfl$weather_wind_mph > 12,TRUE,FALSE))        
nfl$weather_rain <- grepl(c("Rain"),nfl$weather_detail, ignore.case=TRUE)    
nfl$weather_snow <- grepl(c("Snow"),nfl$weather_detail, ignore.case=TRUE)      
nfl$weather_fog <- grepl(c("Fog"),nfl$weather_detail, ignore.case=TRUE)      

# elo predicted scores
for (i in 1:nrow(nfl)) {
        for(j in 1:length(elo_current$team)){
                if(nfl$team_home_id[i]==elo_current$team[j]){
                        nfl$team_home_elo_predicted[i]<-elo_current$elo[j]
                }
        }
}

for (i in 1:nrow(nfl)) {
        for(j in 1:length(elo_current$team)){
                if(nfl$team_away_id[i]==elo_current$team[j]){
                        nfl$team_away_elo_predicted[i]<-elo_current$elo[j]
                }
        }
}


nfl$team_elo_pick <- ifelse(nfl$team_home_elo_pre==nfl$team_away_elo_pre,
                                   "Pick Em",ifelse(nfl$team_home_elo_pre-nfl$team_away_elo_pre>0,
                                                    as.character(nfl$team_home_id),as.character(nfl$team_away_id)))

nfl$team_winner_pick <- paste0(as.character(nfl$team_elo_pick)," (",
                               ifelse(nfl$team_home_win_prob>nfl$team_away_win_prob,round(100*nfl$team_home_win_prob,digits=1),round(100*nfl$team_away_win_prob,digits=1)),"%)")

# Spreads 
nfl$spread_home_predicted <- -2.1315572-0.0210213*nfl$team_home_elo_pre+0.0205236*nfl$team_away_elo_pre
nfl$spread_home_predicted <- ifelse(is.na(nfl$spread_home_predicted),-2.5,round((nfl$spread_home_predicted*2))/2)
nfl$spread_away_predicted <- -nfl$spread_home_predicted
nfl$spread_predicted_winner_id <- ifelse(nfl$spread_home_predicted==nfl$spread_home,"PICK",
                                                ifelse(nfl$spread_home_predicted-nfl$spread_home<0,
                                                       as.character(nfl$team_home_id),
                                                       as.character(nfl$team_away_id)))
nfl$spread_predicted_winner <- ifelse(nfl$spread_predicted==nfl$spread_home,"PICK",
                                             ifelse(nfl$spread_home_predicted-nfl$spread_home<0,
                                                    paste0(nfl$team_home_id,c(" ("),nfl$spread_home,c(")")),
                                                    paste0(nfl$team_away_id,c(" ("),-nfl$spread_home,c(")"))))

