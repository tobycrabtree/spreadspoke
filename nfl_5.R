### Elo ratings from Elo package

## need to fix games that are ties that aren't being assigned the Elo scores

require(EloRating) # use the elo rating package
require(zoo)

nflE <- read.csv("nfl_gameids.csv")

# Win/lose/tie  # tie adjusted for home time
nflE$tie <- nflE$score_away==nflE$score_home
nflE$team_winner <- ifelse(nflE$score_away==nflE$score_home,as.character(nflE$team_home_id),
                          ifelse(nflE$score_away>nflE$score_home,as.character(nflE$team_away_id),as.character(nflE$team_home_id)))
nflE$team_loser <- ifelse(nflE$score_away==nflE$score_home,as.character(nflE$team_away_id),
                         ifelse(nflE$score_away<nflE$score_home,as.character(nflE$team_away_id),as.character(nflE$team_home_id)))
nflE$team_winner <- as.factor(nflE$team_winner)
nflE$team_loser <- as.factor(nflE$team_loser)

# Home/Away team result-----
nflE$team_home_result<-ifelse(nflE$score_home>nflE$score_away,"Win",
                             ifelse(nflE$score_home==nflE$score_away,"Tie", "Loss"))
nflE$team_away_result<-ifelse(nflE$score_home<nflE$score_away,"Win",
                             ifelse(nflE$score_home==nflE$score_away,"Tie", "Loss"))
nflE$team_home_result<-as.factor(nflE$team_home_result)
nflE$team_away_result<-as.factor(nflE$team_away_result)

seqcheck(winner=nflE$team_winner,loser=nflE$team_loser,Date=nflE$schedule_date,draw=nflE$tie)
seq <- elo.seq(winner=nflE$team_winner, loser=nflE$team_loser,draw=nflE$tie,Date=nflE$schedule_date)
#eloplot(seq, from="2016-09-01",interpolate="no") #plots the elo ratings

nflE$team_win_elo_pre<-1000 # sets the initial elo rating at 1000
nflE$team_lose_elo_pre<-1000
elo_winners<-seq[[6]][4] # elo score before game for winning team
nflE$team_win_elo_pre <- as.numeric(unlist(elo_winners))
elo_losers<-seq[[6]][5] # elo score before game for losing team
nflE$team_lose_elo_pre <- as.numeric(unlist(elo_losers))

# home team and away team elo scores
nflE$team_home_elo_pre <- NA
nflE$team_away_elo_pre <- NA
nflE$team_home_id <- as.character(nflE$team_home_id)
nflE$team_winner <- as.character(nflE$team_winner)
for(i in 1:nrow(nflE)){
        nflE$team_home_elo_pre[i]<-ifelse(nflE$tie[i]==TRUE,nflE$team_win_elo_pre[i],ifelse(nflE$team_home_id[i]==nflE$team_winner[i],nflE$team_win_elo_pre[i],nflE$team_lose_elo_pre[i]))
        nflE$team_away_elo_pre[i]<-ifelse(nflE$tie[i]==TRUE,nflE$team_lose_elo_pre[i],ifelse(nflE$team_away_id[i]==nflE$team_winner[i],nflE$team_win_elo_pre[i],nflE$team_lose_elo_pre[i]))        
}

# difference between home team's pre-game elo and away team's pre-game elo
nflE$elo_pre_difference<-0
for(i in 1:nrow(nflE)){
        nflE$elo_pre_difference[i] <- nflE$team_home_elo_pre[i]-nflE$team_away_elo_pre[i] #  value of difference in pre game elo scores between home and away team)
}
nflE$team_home_elo_pre_diff <- nflE$elo_pre_difference
nflE$team_away_elo_pre_diff <- -nflE$elo_pre_difference
nflE$team_home_win_prob <- winprob(nflE$team_home_elo_pre,nflE$team_away_elo_pre)
nflE$team_away_win_prob <- 1-nflE$team_home_win_prob

#nflE$team_win_elo_pre <- NULL # no longer need
#nflE$team_lose_elo_pre <- NULL # no longer need
#nflE$elo_pre_difference <- NULL # no longer need
nflE$X <- NULL

nflELO <- nflE[c("game_id","team_winner","team_loser","team_home_result","team_away_result","team_home_elo_pre","team_away_elo_pre","team_home_elo_pre_diff","team_away_elo_pre_diff","team_home_win_prob","team_away_win_prob")]
write.csv(nflELO,"nfl_elo.csv")
