# spreadspoke tables
eloTemp <- read.csv("nfl_elo.csv") # nfl ELO info
eloTemp <- eloTemp[c("game_id","team_home_elo_pre","team_away_elo_pre")]
spreadTemp <- read.csv("nfl_spread_over_results.csv")
spreadTemp$X<-NULL
pointsTemp <- read.csv("nfl_avg_pts.csv")
pointsTemp <- pointsTemp[c("game_id","team_offense_home","team_defense_home","team_offense_away","team_defense_away")]
spreadTemp <- merge(spreadTemp,pointsTemp)
weatherTemp <- read.csv("nfl_weather.csv")
dfTemp <- merge(spreadTemp,weatherTemp,by=c("game_id"))
dfTemp$X <- NULL
df2 <- merge(dfTemp,eloTemp,by=c("game_id"))
df2$spread_result <- df2$spread_cover #as.factor(ifelse(df2$spread_cover%in% c("Cover","Push"),"Cover","Underdog")) # test if cover or not (combine cover + push)
df2$over_under_result <- df2$over_under_result #as.factor(ifelse(df2$over_under_result%in%c("Over","Push"),"Over","Under")) # test if over/push
df2$elo_combo <- df2$team_home_elo_pre * df2$team_away_elo_pre
df2$team_elo_favorite <- ifelse(df2$team_home_elo_pre==df2$team_away_elo_pre,"Push",ifelse(df2$team_home_elo_pre>df2$team_away_elo_pre,as.character(df2$team_home_id),as.character(df2$team_away_id)))
df2$team_win <- as.character(ifelse(df2$team_home_result=="Tie","Tie",ifelse(df2$team_home_result=="Win",as.character(df2$team_home_id),as.character(df2$team_away_id))))
df2$elo_favorite_win <- ifelse(df2$team_elo_favorite=="Push","Push",ifelse(df2$team_win==df2$team_elo_favorite,"TRUE","FALSE"))
df2$team_spread_favorite <- as.character(ifelse(df2$spread==0,"Push",ifelse(df2$spread<0,as.character(df2$team_home_id),as.character(df2$team_away_id))))
df2$team_spread_underdog <- as.character(ifelse(df2$spread==0,"Push",ifelse(df2$spread>0,as.character(df2$team_home_id),as.character(df2$team_away_id))))
df2$team_covers <- as.character(ifelse(df2$spread_result=="Push","Push",ifelse(df2$spread_result=="Cover",as.character(df2$team_spread_favorite),as.character(df2$team_spread_underdog))))
df2$team_favorite_covers <- as.factor(ifelse(df2$team_covers=="Push","Push",ifelse(df2$team_covers==df2$team_spread_favorite,"TRUE","FALSE")))

nflT <- df2

# games table
nflT$spread_predicted <- 0.9427486-0.0245085*nflT$team_home_elo_pre+0.0206702*nflT$team_away_elo_pre
nflT$spread_predicted <- ifelse(is.na(nflT$spread_predicted),-2.5,round((nflT$spread_predicted*2))/2)
nflT$spread_predicted_winner_id <- ifelse(abs(nflT$spread_predicted)<1,"Pick Em",
                                       ifelse(nflT$spread_predicted>nflT$spread,
                                              as.character(nflT$team_home_id),
                                              as.character(nflT$team_away_id)))
nflT$spread_predicted_winner <- ifelse(abs(nflT$spread_predicted)<1,"Pick Em",
                                ifelse(nflT$spread_predicted>nflT$spread,
                                       paste0(nflT$team_home_id,c("("),nflT$spread,c(")")),
                                       paste0(nflT$team_away_id,c("("),-nflT$spread,c(")"))))
nflT$spread_winner_id <- ifelse(nflT$spread_cover=="Push","None",
                                ifelse(nflT$spread_cover=="Cover",
                                       as.character(nflT$team_home_id),
                                       as.character(nflT$team_away_id)))
write.csv(nflT,"nflT.csv")

nflT$over_under_predicted <- round(34.7 +
        0.001934*nflT$team_home_elo_pre +
        0.004540*nflT$team_away_elo_pre +
        ifelse(nflT$team_offense_home=="strong",3.495672,ifelse(nflT$team_offense_home=="weak",-1.387331,0))+
        ifelse(nflT$team_defense_away=="strong",-1.880359,ifelse(nflT$team_defense_away=="weak",1.888973,0))+
        ifelse(nflT$team_offense_away=="strong",2.691029,ifelse(nflT$team_offense_away=="weak",-1.176916,0))+
        ifelse(nflT$team_defense_home=="strong",-1.867485,ifelse(nflT$team_defense_home=="weak",2.280479,0)),
        digits=0)
nflT$over_under_predicted_result <- ifelse(nflT$over_under_predicted==nflT$over_under_line,"Push",
                                           ifelse(nflT$over_under_predicted>nflT$over_under_line,"Over","Under"))     


nflGames <- nflT[c("schedule_week","schedule_date","team_home_id","team_away_id",
                            "team_elo_pick","team_ats_pick","over_under_pick","strategy")]
nflCurrent2Names <- c("schedule_week","Date","Home Team","Away Team","Winner Pick","ATS Pick","Over/Under Pick","Strategy")
colnames(nflCurrent2) <- nflCurrent2Names        
#write.csv(nflCurrent2, "./nfl_2017.csv")


# team table
nflPoints <- read.csv("nfl_season_pts.csv") # from nfl_2.R script
nflWinsATSOvers <- read.csv("nfl_teams_info.csv") # from nfl_3.R script
nflTables <- merge(nflPoints,nflWinsATSOvers,by.x=c("team","season"),by.y=c("Team","Season"))
nflTables <- nflTables[c("team","season","W","L","T","Win..","Cover..","Over..","avgptsfor","avgptsagainst")]
nflTables <- subset(nflTables,nflTables$season>2011)
nflTables <- nflTables[order(nflTables$season,decreasing=TRUE),]
nflTables <- nflTables[order(nflTables$team),]
colnames(nflTables) <- c("Team","Season","W","L","T","Win %","Cover %","Over %","Off Pts/G","Def Pts/G")
write.csv(nflTables,"teams.csv")

# strategies table
