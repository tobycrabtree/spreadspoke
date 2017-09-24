# nfl_3  calculate wins %, % games where cover the spread and % overs
library(plyr)
library(dplyr)
library(FSA)
library(zoo)
# read nfl data
nflATS <- read.csv("nfl_gameids.csv") #subset(nfl,nfl$schedule_season==2016)
nflATS$X <- NULL

# wins, losses, ties
nflATS$team_home_result<-ifelse(nflATS$score_home>nflATS$score_away,"Win",
                             ifelse(nflATS$score_home==nflATS$score_away,"Tie", "Loss"))
nflATS$team_away_result<-ifelse(nflATS$score_home<nflATS$score_away,"Win",
                             ifelse(nflATS$score_home==nflATS$score_away,"Tie", "Loss"))
nflATS$team_home_win<-ifelse(nflATS$team_home_result=="Win",1,0)
nflATS$team_home_loss<-ifelse(nflATS$team_home_result=="Loss",1,0)
nflATS$team_home_tie <- ifelse(nflATS$team_home_result=="Tie",1,0)
nflATS$team_away_win<-ifelse(nflATS$team_away_result=="Win",1,0)
nflATS$team_away_loss<-ifelse(nflATS$team_away_result=="Loss",1,0)
nflATS$team_away_tie <- ifelse(nflATS$team_away_result=="Tie",1,0)
#nflATSCalc$spread_cover<-ifelse(nflATSCalc$spread %in% c("Cover"),1,0)
#nflATSCalc$spread_loss<-ifelse(nflATSCalc$spread %in% c("Loss"),1,0)
#nflATSCalc$spread_push<-ifelse(nflATSCalc$spread %in% c("Push"),1,0)
#nflATSCalc$over<-ifelse(nflATSCalc$over_under_result=="Over",1,0)
#nflATSCalc$under<-ifelse(nflATSCalc$over_under_result=="Under",1,0)
#nflATSCalc$over_under_push<-ifelse(nflATSCalc$over_under_result=="Push",1,0)

# spreads covers, pushes
nflATS$spread_actual<-nflATS$score_away-nflATS$score_home # spread final result, spread -7 means home is favored by 7 points
nflATS$spread_cover<- ifelse(nflATS$spread_actual==nflATS$spread,c("Push"),
                          ifelse(nflATS$spread_actual>nflATS$spread, c("Underdog"), c("Cover")))
nflATS$spread_home<-ifelse(nflATS$spread_cover=='Push', 'Push', 
                        ifelse(nflATS$spread_cover=='Cover', 'Cover','Loss'))
nflATS$spread_away<-ifelse(nflATS$spread_cover=='Push', 'Push', 
                        ifelse(nflATS$spread_cover=='Cover', 'Loss','Cover'))
nflATS$spread_type<-ifelse(nflATS$spread==0,'Push',
                        ifelse(nflATS$spread>0,'home underdog','home favorite'))
nflATS$spread_type<-as.factor(nflATS$spread_type)
nflATS$spread_outlier <- ifelse(abs(nflATS$spread) > 14.1, '2TD+',
                             ifelse(abs(nflATS$spread) > 10.1, '1TD1FG+',
                                    ifelse(abs(nflATS$spread) > 7.1, '1TD+','no outlier')))
nflATS$spread_outlier <- as.factor(nflATS$spread_outlier)
nflATS$spread_outlier_home<-ifelse(nflATS$spread<= -15,'home really big favorite',
                                ifelse(nflATS$spread<= -9.5,'home big favorite',
                                       ifelse(nflATS$spread >= 15,'home really big underdog',
                                              ifelse(nflATS$spread >= 9.5,'home big underdog', 'no outlier')))) # st dev spread = 5.9; mean+/-2sd=+/-9.5 
nflATS$spread_cover <- as.factor(nflATS$spread_cover)
nflATS$spread_home <- as.factor(nflATS$spread_home)
nflATS$spread_away <- as.factor(nflATS$spread_away)
nflATS$spread_type <- as.factor(nflATS$spread_type)
nflATS$spread_outlier <- as.factor(nflATS$spread_outlier)
nflATS$spread_outlier_home <- as.factor(nflATS$spread_outlier_home)

# over under analysis
nflATS$score_total <- nflATS$score_away + nflATS$score_home
nflATS$over_under_result <- ifelse(nflATS$score_total==nflATS$over_under_line, 'Push', 
                                                       ifelse(nflATS$score_total > nflATS$over_under_line,
                                                              'Over','Under'))
nflATS$over_under_outlier <- ifelse(nflATS$over_under_line<33,"under 2sd",
                                 ifelse(nflATS$over_under_line<37,"under 1sd",
                                        ifelse(nflATS$over_under_line>50,"over 2sd",
                                               ifelse(nflATS$over_under_line>46,"over 1sd","no outlier"))))
nflATS$over_under_outlier <- as.factor(nflATS$over_under_outlier)

nflSeasonHomeWins <- ddply(nflATS,.(team_home_id,schedule_season),summarize,home_wins=sum(team_home_win),home_ties=sum(team_home_tie),home_losses=sum(team_home_loss))
nflSeasonAwayWins <- ddply(nflATS,.(team_away_id,schedule_season),summarize,away_wins=sum(team_away_win),away_ties=sum(team_away_tie),away_losses=sum(team_away_loss))
nflSeasonWins <- merge(nflSeasonHomeWins,nflSeasonAwayWins,by.x=c("team_home_id","schedule_season"),by.y=c("team_away_id","schedule_season"))
nflSeasonWins$wins <- nflSeasonWins$home_wins + nflSeasonWins$away_wins
nflSeasonWins$losses <- nflSeasonWins$home_losses + nflSeasonWins$away_losses
nflSeasonWins$ties <- nflSeasonWins$home_ties + nflSeasonWins$away_ties
nflSeasonWins$wins_pct <- round(100*((nflSeasonWins$wins+0.5*nflSeasonWins$ties)/(nflSeasonWins$wins+nflSeasonWins$ties+nflSeasonWins$losses)),digits=1)
write.csv(nflSeasonWins,"nfl_season_wins.csv")

nflATS$spread_cover_count <- ifelse(nflATS$spread_cover==c("Cover"),1,0)
nflATS$spread_push_count <- ifelse(nflATS$spread_cover==c("Push"),1,0)
nflATS$spread_nocover_count <- ifelse(nflATS$spread_cover==c("Underdog"),1,0)
nflSeasonHomeATS <- ddply(nflATS,.(team_home_id,schedule_season),summarize,covers_home=sum(spread_cover_count),pushes_home=sum(spread_push_count),nocover_home=sum(spread_nocover_count))
nflSeasonAwayATS <- ddply(nflATS,.(team_away_id,schedule_season),summarize,covers_away=sum(spread_nocover_count),pushes_away=sum(spread_push_count),nocover_away=sum(spread_cover_count))
nflSeasonATS <- merge(nflSeasonHomeATS,nflSeasonAwayATS,by.x=c("team_home_id","schedule_season"),by.y=c("team_away_id","schedule_season"))
nflSeasonATS$covers <- nflSeasonATS$covers_home + nflSeasonATS$covers_away
nflSeasonATS$nocovers <- nflSeasonATS$nocover_home + nflSeasonATS$nocover_away
nflSeasonATS$pushes <- nflSeasonATS$pushes_home + nflSeasonATS$pushes_away
nflSeasonATS$cover_pct <- round(100*(nflSeasonATS$covers+0.5*nflSeasonATS$pushes)/(nflSeasonATS$covers+nflSeasonATS$pushes+nflSeasonATS$nocovers),digits=1)
write.csv(nflSeasonATS,"nfl_season_ats.csv")

nflATS$overs_count <- ifelse(nflATS$over_under_result==c("Over"),1,0)
nflATS$unders_count <- ifelse(nflATS$over_under_result==c("Under"),1,0)
nflATS$overs_push_count <- ifelse(nflATS$over_under_result==c("Push"),1,0)
nflSeasonHomeOver <- ddply(nflATS,.(team_home_id,schedule_season),summarize,overs_home=sum(overs_count),pushes_home=sum(overs_push_count),unders_home=sum(unders_count))
nflSeasonAwayOver <- ddply(nflATS,.(team_away_id,schedule_season),summarize,overs_away=sum(unders_count),pushes_away=sum(overs_push_count),unders_away=sum(overs_count))
nflSeasonOver <- merge(nflSeasonHomeOver,nflSeasonAwayOver,by.x=c("team_home_id","schedule_season"),by.y=c("team_away_id","schedule_season"))
nflSeasonOver$overs <- nflSeasonOver$overs_home + nflSeasonOver$overs_away
nflSeasonOver$unders <- nflSeasonOver$unders_home + nflSeasonOver$unders_away
nflSeasonOver$over_pushes <- nflSeasonOver$pushes_home + nflSeasonOver$pushes_away
nflSeasonOver$over_pct <- round(100*(nflSeasonOver$overs+0.5*nflSeasonOver$over_pushes)/(nflSeasonOver$overs+nflSeasonOver$over_pushes+nflSeasonOver$unders),digits=1)
write.csv(nflSeasonOver,"nfl_season_overs.csv")

nflTeams1 <- merge(nflSeasonOver,nflSeasonATS,by.x=c("team_home_id","schedule_season"),by.y=c("team_home_id","schedule_season"))
nflTeams2 <- merge(nflTeams1,nflSeasonWins,by.x=c("team_home_id","schedule_season"),by.y=c("team_home_id","schedule_season"))
nflTeams2$w_l_t <- paste0(c(" "),nflTeams2$wins,c("-"),nflTeams2$losses,c("-"),nflTeams2$ties)
nflTeams3 <- nflTeams2[c("team_home_id","schedule_season","wins","losses","ties","wins_pct","cover_pct","over_pct")]
nflTeams3 <- nflTeams3[order(nflTeams3$schedule_season,decreasing=TRUE),]
nflTeams3 <- nflTeams3[order(nflTeams3$team_home_id),]

colnames(nflTeams3) <- c("Team","Season","W","L","T","Win %","Cover %","Over %")
write.csv(nflTeams3,"nfl_teams_info.csv")
#nflATS <- nflATS[c("game_id","team_home_id","team_away_id","team_home_result","team_away_result","spread_actual","spread_cover","spread_home","spread_away","spread_type","spread_outlier","spread_outlier_home","score_total","over_under_result","over_under_outlier")]
write.csv(nflATS,"nfl_spread_over_results.csv")


# Concantinate home/away into long format----
nflATSCalc<-rbind(
        nflHome=data.frame(game_id=nflATS[,'game_id'],
                           season=nflATS[,'schedule_season'], 
                           schedule_week=nflATS[,'schedule_week'], 
                           team=nflATS[,'team_home_id'], 
                           opponent=nflATS[,'team_away_id'],
                           schedule_date=nflATS[,'schedule_date'],
                           venue=rep("home", n=nrow(nflATS)), 
                           score=nflATS$score_home,
                           score_against=nflATS$score_away,
                           result=nflATS$team_home_result,
                           spread=nflATS$spread_home, 
                           spread_num=nflATS$spread,
                           over_under=nflATS$over_under_line,
                           score_total=nflATS$score_total,
                           over_under_result=nflATS$over_under_result
        )
        ,
        
        nflAway=data.frame(game_id=nflATS[,'game_id'],
                           season=nflATS[,'schedule_season'],
                           schedule_week=nflATS[,'schedule_week'], 
                           team=nflATS[,'team_away_id'], 
                           opponent=nflATS[,'team_home_id'],
                           schedule_date=nflATS[,'schedule_date'],
                           venue=rep("away", n=nrow(nflATS)), 
                           score=nflATS$score_away,
                           score_against=nflATS$score_home,
                           result=nflATS$team_away_result,
                           spread=nflATS$spread_away, 
                           spread_num=nflATS$spread,
                           over_under=nflATS$over_under_line,
                           score_total=nflATS$score_total,
                           over_under_result=nflATS$over_under_result
        )
)

nflATSCalc <-arrange(nflATSCalc,schedule_date)
nflATSCalc$win<-ifelse(nflATSCalc$result=="Win",1,0)
nflATSCalc$loss<-ifelse(nflATSCalc$result=="Loss",1,0)
nflATSCalc$tie <- ifelse(nflATSCalc$result=="Tie",1,0)
nflATSCalc$spread_cover<-ifelse(nflATSCalc$spread %in% c("Cover"),1,0)
nflATSCalc$spread_loss<-ifelse(nflATSCalc$spread %in% c("Loss"),1,0)
nflATSCalc$spread_push<-ifelse(nflATSCalc$spread %in% c("Push"),1,0)
nflATSCalc$over<-ifelse(nflATSCalc$over_under_result=="Over",1,0)
nflATSCalc$under<-ifelse(nflATSCalc$over_under_result=="Under",1,0)
nflATSCalc$over_under_push<-ifelse(nflATSCalc$over_under_result=="Push",1,0)

k=16 # constant for number of games i.e., 4 = last 4 games
nflATSCalc<-nflATSCalc %>%
        group_by(team)%>%
        mutate(wins_roll=cumsum(win)) %>% # wins
        mutate(losses_roll=cumsum(loss)) %>% # losses
        mutate(ties_roll=cumsum(tie)) %>% # ties
        mutate(win_pct=(cumsum(win)+0.5*cumsum(tie))/
                       (cumsum(win)+cumsum(loss)+cumsum(tie))) %>% # winning %
        mutate(win_pct_roll_lag=(pcumsum(win)+0.5*pcumsum(tie))/
                       (pcumsum(win)+pcumsum(loss)+pcumsum(tie))) %>% # winning % prior-to
        mutate(win_pct_roll=rollapply(win+0.5*tie, k, FUN=sum, 
                                              fill=NA, align="right")/k) %>% # winning % last k games
       
        mutate(covers_roll=cumsum(spread_cover)) %>% # covers
        mutate(pushes_roll=cumsum(spread_push)) %>% # pushes
        mutate(nocovers_roll=cumsum(spread_loss)) %>% # did not cover
        mutate(cover_pct=(cumsum(spread_cover)+0.5*cumsum(spread_push))/(cumsum(spread_cover)+cumsum(spread_loss)+cumsum(spread_push))) %>% # % covers the spreads 
        mutate(cover_pct_roll_lag=(pcumsum(spread_cover)+0.5*pcumsum(spread_push))/(pcumsum(spread_cover)+pcumsum(spread_loss)+pcumsum(spread_push))) %>% # % covers the spreads prior-to 
        mutate(cover_pct_roll=rollapply(spread_cover+0.5*spread_push, k, FUN=sum, 
                                                  fill=NA, align="right")/k) %>% # % covers last k games
        mutate(overs_roll=cumsum(over)) %>% # overs
        mutate(over_pushes_roll=cumsum(over_under_push)) %>% # over under pushes
        mutate(unders_roll=cumsum(under)) %>% # unders
        mutate(over_pct=(cumsum(over)+0.5*cumsum(over_under_push))/(cumsum(over)+cumsum(under)+cumsum(over_under_push))) %>% # % overs
        mutate(over_pct_roll_lag=(pcumsum(over)+0.5*pcumsum(over_under_push))/(pcumsum(over)+pcumsum(under)+pcumsum(over_under_push))) %>% # % overs prior-to
        mutate(over_pct_roll=rollapply(over+0.5*over_under_push, k, FUN=sum, 
                                          fill=NA, align="right")/k) %>% # % overs last k games
arrange(team,schedule_date)

nflATSCalc2 <- nflATSCalc[(nflATSCalc$schedule_week==1&nflATSCalc$season==2017),] # only last week of season
#nflATSCalc2 <- nflATSCalc2[c("team","win_pct","win_pct_roll_lag","win_pct_roll",
#                             "cover_pct","cover_pct_roll_lag","cover_pct_roll",
#                             "over_pct","over_pct_roll_lag","over_pct_roll")]
write.csv(nflATSCalc2,"nfl_ats.csv")
