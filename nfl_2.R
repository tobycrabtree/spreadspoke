# calculate rolling stats for points scored, points allowed
library(plyr)
library(dplyr)
library(FSA)
library(zoo)
# read nfl data
nflStreak <- read.csv("nfl_gameids.csv") #subset(nfl,nfl$schedule_season==2016) # if you want to subset for a season
nflStreak$X <- NULL

# Concantinate home/away into long format
nflStreakCalc<-rbind(
        nflHome=data.frame(game_id=nflStreak[,'game_id'],
                        season=nflStreak[,'schedule_season'], 
                        schedule_week=nflStreak[,'schedule_week'], 
                        team=nflStreak[,'team_home_id'], 
                        opponent=nflStreak[,'team_away_id'],
                        schedule_date=nflStreak[,'schedule_date'],
                        venue=rep("home", n=nrow(nflStreak)), 
                        score=nflStreak$score_home,
                        score_against=nflStreak$score_away,
                        score_margin=nflStreak$score_home-nflStreak$score_away
                        )
        ,
        
        nflAway=data.frame(game_id=nflStreak[,'game_id'],
                        season=nflStreak[,'schedule_season'],
                        schedule_week=nflStreak[,'schedule_week'], 
                        team=nflStreak[,'team_away_id'], 
                        opponent=nflStreak[,'team_home_id'],
                        schedule_date=nflStreak[,'schedule_date'],
                        venue=rep("away", n=nrow(nflStreak)), 
                        score=nflStreak$score_away,
                        score_against=nflStreak$score_home,
                        score_margin=nflStreak$score_away-nflStreak$score_home
                        
                        )
        )

nflStreakCalc <- arrange(nflStreakCalc,schedule_date)
#nflStreakCalc$season <- as.factor(nflStreakCalc$season)

# dummy variable if team is playing after bye week (i.e., didn’t play week before) ----
nflStreakCalc<-nflStreakCalc[order(nflStreakCalc$team,nflStreakCalc$schedule_date),]
nflStreakCalc$week_diff<-c(0,diff(nflStreakCalc$schedule_week,1))
nflStreakCalc$schedule_play_after_bye_week<-ifelse(nflStreakCalc$week_diff==1, FALSE, TRUE) ## correct for first 16 games in data set


# dummy variable for playing on a short week (i.e., played a game less than 6 days before)----
nflStreakCalc$day_diff<-c(0,as.numeric(diff(nflStreakCalc$schedule_date)))
nflStreakCalc$schedule_play_short_week<-ifelse(nflStreakCalc$day_diff %in% c(4,5),TRUE,FALSE)

# team points scored for and against 
k=16 # constant for trailing 16 games (can adjust)
nflStreakCalc <- nflStreakCalc %>%
        group_by(team) %>%
        arrange(schedule_date) %>%
        mutate(score_avg_pts_for=cummean(score))%>% 
        mutate(score_avg_pts_for_roll=rollapply(score,width=k,FUN=mean,fill=NA,align="right"))%>%
        mutate(score_avg_pts_for_roll_lag=lag(rollapply(score,width=k,FUN=mean,fill=NA,align="right")))%>%
        mutate(score_avg_pts_against=cummean(score_against))%>%
        mutate(score_avg_pts_against_roll=rollapply(score_against,width=k,FUN=mean,fill=NA,align="right"))%>%
        mutate(score_avg_pts_against_roll_lag=lag(rollapply(score_against,width=k,FUN=mean,fill=NA,align="right")))%>%
        mutate(score_avg_margin_of_victory=cummean(score_margin))%>%
        mutate(score_avg_margin_of_victory_roll=rollapply(score_margin,width=k,FUN=mean,fill=NA,align="right"))%>%
        mutate(score_avg_margin_of_victory_roll_lag=lag(rollapply(score_margin,width=k,FUN=mean,fill=NA,align="right")))%>%
        group_by(team,venue) %>% # avg pts home/away by venue
        arrange(schedule_date) %>%
        mutate(score_avg_pts_for_venue=cummean(score))%>%
        mutate(score_avg_pts_for_roll_venue=rollapply(score,width=8,FUN=mean,fill=NA,align="right"))%>%
        mutate(score_avg_pts_against_venue=cummean(score_against))%>%
        mutate(score_avg_pts_against_roll_venue=rollapply(score_against,width=8,FUN=mean,fill=NA,align="right"))%>%
        mutate(score_avg_margin_of_victory_venue=cummean(score_margin))%>%
        mutate(score_avg_margin_of_victory_roll_venue=rollapply(score_margin,width=8,FUN=mean,fill=NA,align="right"))%>%
        #group_by(team,season) %>% # avg pts home/away by season
        #mutate(score_avg_pts_for_season=cummean(score))%>%
        #mutate(score_avg_pts_for_roll_season=rollapply(score,width=k,FUN=mean,fill=NA,align="right"))%>%
        #mutate(score_avg_pts_against_season=cummean(score_against))%>%
        #mutate(score_avg_pts_against_roll_season=rollapply(score_against,width=k,FUN=mean,fill=NA,align="right"))%>%
        #mutate(score_avg_margin_of_victory_season=cummean(score_margin))%>%
        #mutate(score_avg_margin_of_victory_roll_season=rollapply(score_margin,width=k,FUN=mean,fill=NA,align="right"))%>%
        arrange(team)

nflStreakCalc$One <- NULL
nflStreakCalcHome <- nflStreakCalc[(nflStreakCalc$venue=="home"),]
colnames(nflStreakCalcHome) <- c("game_id",
                             "season",                                 
                             "schedule_week",
                             "team_home_id",                                   
                             "team_away_id",                               
                             "schedule_date",                         
                             "venue",                                  
                             "score_home",                                  
                             "score_away",                         
                             "score_margin_home",                           
                             "schedule_week_diff_home",
                             "schedule_play_after_bye_week_home",
                             "schedule_day_diff_home",
                             "schedule_play_short_week_home",
                             "score_avg_pts_for_home",                      
                             "score_avg_pts_for_roll_home",  
                             "score_avg_pts_for_roll_lag_home",
                             "score_avg_pts_against_home",                  
                             "score_avg_pts_against_roll_home",
                             "score_avg_pts_against_roll_lag_home",             
                             "score_avg_margin_of_victory_home",           
                             "score_avg_margin_of_victory_roll_home",   
                             "score_avg_margin_of_victory_roll_lag_home",       
                             "score_avg_pts_for_venue_home",                
                             "score_avg_pts_for_roll_venue_home",          
                             "score_avg_pts_against_venue_home",           
                             "score_avg_pts_against_roll_venue_home",      
                             "score_avg_margin_of_victory_venue_home"
                             )

nflStreakCalcAway <- nflStreakCalc[(nflStreakCalc$venue=="away"),]
colnames(nflStreakCalcAway) <- c("game_id",
                                 "season",                                 
                                 "schedule_week",
                                 "team_away_id",                                   
                                 "team_home_id",                               
                                 "schedule_date",                         
                                 "venue",                                  
                                 "score_away",                                  
                                 "score_home",                         
                                 "score_margin_away",  
                                 "schedule_week_diff_away",
                                 "schedule_play_after_bye_week_away",
                                 "schedule_day_diff_away",
                                 "schedule_play_short_week_away",
                                 "score_avg_pts_for_away",                      
                                 "score_avg_pts_for_roll_away",
                                 "score_avg_pts_for_roll_lag_away",                
                                 "score_avg_pts_against_away",                  
                                 "score_avg_pts_against_roll_away",             
                                 "score_avg_pts_against_roll_lag_away",             
                                 "score_avg_margin_of_victory_away",           
                                 "score_avg_margin_of_victory_roll_away",       
                                 "score_avg_margin_of_victory_roll_lag_away",       
                                 "score_avg_pts_for_venue_away",                
                                 "score_avg_pts_for_roll_venue_away",          
                                 "score_avg_pts_against_venue_away",           
                                 "score_avg_pts_against_roll_venue_away",      
                                 "score_avg_margin_of_victory_venue_away"
                                 )

nflStreakCalc2 <- merge(nflStreakCalcHome,nflStreakCalcAway,by=c("game_id","team_home_id","team_away_id","season","team_home_id","team_away_id"))
nflStreakCalc2 <- nflStreakCalc2[c("game_id", 
                                   "team_home_id",
                                   "team_away_id",
                                   "schedule_play_after_bye_week_home",
                                   "schedule_play_short_week_home",
                                   "schedule_play_after_bye_week_away",
                                   "schedule_play_short_week_away",
                                   "score_avg_pts_for_home",                      
                                   "score_avg_pts_for_roll_home",                
                                   "score_avg_pts_for_roll_lag_home",                
                                   "score_avg_pts_against_home",                  
                                   "score_avg_pts_against_roll_home",             
                                   "score_avg_pts_against_roll_lag_home",             
                                   "score_avg_margin_of_victory_home",           
                                   "score_avg_margin_of_victory_roll_home",       
                                   "score_avg_margin_of_victory_roll_lag_home",       
                                   "score_avg_pts_for_venue_home",                
                                   "score_avg_pts_for_roll_venue_home",          
                                   "score_avg_pts_against_venue_home",           
                                   "score_avg_pts_against_roll_venue_home",      
                                   "score_avg_margin_of_victory_venue_home",
                                   "score_avg_pts_for_away",                      
                                   "score_avg_pts_for_roll_away",                
                                   "score_avg_pts_for_roll_lag_away",                
                                   "score_avg_pts_against_away",                  
                                   "score_avg_pts_against_roll_away",             
                                   "score_avg_pts_against_roll_lag_away",             
                                   "score_avg_margin_of_victory_away",           
                                   "score_avg_margin_of_victory_roll_away",       
                                   "score_avg_margin_of_victory_roll_lag_away",       
                                   "score_avg_pts_for_venue_away",                
                                   "score_avg_pts_for_roll_venue_away",          
                                   "score_avg_pts_against_venue_away",           
                                   "score_avg_pts_against_roll_venue_away",      
                                   "score_avg_margin_of_victory_venue_away"
                                   )]

# create offense/defense attribute (strong = high scoring offense/allow few pts, neutral, weak = low scoring offense/allow a lot of pts) based on pts scored/pts allowed last k games
nflStreakCalc2$team_offense_home <- ifelse(is.na(nflStreakCalc2$score_avg_pts_for_roll_lag_home)==TRUE,"neutral",ifelse(nflStreakCalc2$score_avg_pts_for_roll_lag_home>24,"strong",ifelse(nflStreakCalc2$score_avg_pts_for_roll_lag_home<18,"weak","neutral")))
nflStreakCalc2$team_defense_home <- ifelse(is.na(nflStreakCalc2$score_avg_pts_against_roll_lag_home)==TRUE,"neutral",ifelse(nflStreakCalc2$score_avg_pts_against_roll_lag_home>24,"weak",ifelse(nflStreakCalc2$score_avg_pts_against_roll_lag_home<18,"strong","neutral")))
nflStreakCalc2$team_offense_away <- ifelse(is.na(nflStreakCalc2$score_avg_pts_for_roll_lag_away)==TRUE,"neutral",ifelse(nflStreakCalc2$score_avg_pts_for_roll_lag_away>24,"strong",ifelse(nflStreakCalc2$score_avg_pts_for_roll_lag_away<18,"weak","neutral")))
nflStreakCalc2$team_defense_away <- ifelse(is.na(nflStreakCalc2$score_avg_pts_against_roll_lag_away)==TRUE,"neutral",ifelse(nflStreakCalc2$score_avg_pts_against_roll_lag_away>24,"weak",ifelse(nflStreakCalc2$score_avg_pts_against_roll_lag_away<18,"strong","neutral")))

write.csv(nflStreakCalc2,"nfl_avg_pts.csv")

# create stats by season
nflSeasonPoints <- ddply(nflStreakCalc,.(team,season),summarize,avgptsfor=round(mean(score),digits=1),avgptsagainst=round(mean(score_against),digits=1))
nflSeasonPoints$team_offense <- ifelse(nflSeasonPoints$avgptsfor>24,"strong",ifelse(nflSeasonPoints$avgptsfor<18,"weak","neutral"))
nflSeasonPoints$team_defense <- ifelse(nflSeasonPoints$avgptsagainst>24,"weak",ifelse(nflSeasonPoints$avgptsagainst<18,"strong","neutral"))
write.csv(nflSeasonPoints,"nfl_season_pts.csv")
#nflSeasonPoints2 <- ddply(nflStreakCalc,.(team,season),mutate,mean(score))
