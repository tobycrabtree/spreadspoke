# model for spread and over under prediction
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

# train and test 
train <- df2[df2$schedule_season<=2013,]
test <- df2[df2$schedule_season>2013,]

fmOver <- over_under_result ~ schedule_week_1to4 + schedule_week_last + schedule_day + schedule_month + stadium_type + division_matchup + team_home_elo_pre*team_away_elo_pre + spread + spread_type + spread_outlier + over_under_line + over_under_outlier + weather_cold + weather_rain + weather_snow + weather_wind_bad + team_offense_home*team_defense_away + team_offense_away*team_defense_home
fmOver3 <- over_under_result ~ schedule_month + stadium_type + team_home_elo_pre*team_away_elo_pre + spread + over_under_line 
fmOver2 <- score_total ~ schedule_week_1to4 + schedule_day + stadium_type + division_matchup + team_home_elo_pre*team_away_elo_pre + spread + spread_outlier + over_under_outlier + spread_type + weather_cold + weather_wind_bad
fmSpread <- spread_result ~ schedule_week_1to4 + schedule_week_last + schedule_day + schedule_month + stadium_type + division_matchup + team_home_elo_pre*team_away_elo_pre + spread + spread_type + spread_outlier + over_under_line + over_under_outlier + spread_type + weather_cold + weather_rain + weather_snow + weather_wind_bad
fmSpread2 <- spread_result ~ schedule_day + stadium_type + elo_combo + over_under_line + spread_outlier 

# random forest over/under
library(randomForest)
library(caret)
library(e1071)
set.seed(2017)
rfOver <- randomForest(fmOver, data = train, ntree=1500, na.action=na.omit, importance=TRUE)
importance(rfOver)
rfOver_class <- predict(rfOver, test, type = "class")
rfOver_accuracy <- confusionMatrix(rfOver_class, test$over_under_result)$overall['Accuracy']
confusionMatrix(rfOver_class, test$over_under_result)
library(ROCR)
rfOver_probs <- predict(rfOver, newdata = test, type = "prob")
p_test_rfOver <- prediction(rfOver_probs[, 1], test$over_under_result) # fix for 2 categories
perf_rfOver <- performance(p_test_rfOver, "tpr", "fpr")
# The ROC curve
plot(perf_rfOver)
# Find Area under the curve (AUC)
rf_auc <- performance(p_test_rfOver, "auc")@y.values
rf_auc

# rf regression total points
rfOver2 <- randomForest(fmOver2, data=train, na.action=na.exclude, importance=TRUE)

# svm model over/under
library(e1071)
library(rpart)
svm1<-svm(fmOver, data=train, kernel="radial", cost=1, probability=TRUE, na.action=na.omit)
predict_svm1<-predict(svm1, test, type = "class")
confusionMatrix(predict_svm1, test$over_under_result)
predict_svm_prob1<-predict(svm1, newdata=test, probability=TRUE)
predict_svm_prob1 <- attr(predict_svm_prob1, "probabilities")
p_test_svm1 <- ROCR::prediction(predict_svm_prob1[, 2], test$over_under_result)
perf_svm <- performance(p_test_svm1, "tpr", "fpr")
performance(p_test_svm1, "auc")@y.values

# classification over/under
fitOver <- rpart(over_under_result ~ schedule_week_1to4 + schedule_week_last + schedule_day + schedule_month + stadium_type + division_matchup + team_home_elo_pre+team_away_elo_pre + spread + spread_type + spread_outlier + over_under_line + over_under_outlier + weather_cold + weather_rain + weather_snow + weather_wind_bad + team_offense_home+team_defense_away + team_offense_away+team_defense_home, method="class",data=train)
plot(fitOver)
text(fitOver, cex=.5, use.n=TRUE, all=TRUE)
summary(fitOver)
fitOverPredicted <- rpart(score_total ~ schedule_month + stadium_type + team_home_elo_pre + team_away_elo_pre + spread + over_under_line+team_offense_home+team_defense_away + team_offense_away+team_defense_home, method="anova",data=test)
summary(fitOverPredicted)
plot(fitOverPredicted)
text(fitOverPredicted, cex=.5, use.n=TRUE, all=TRUE)

# random forest spreads
set.seed(2017)
rfSpread <- randomForest(fmSpread, data = train, ntree=1500, na.action=na.exclude)
importance(rfSpread)
rfSpread_class <- predict(rfSpread, test, type = "class")
rfSpread_accuracy <- confusionMatrix(rfSpread_class, test$spread_result)$overall['Accuracy']
confusionMatrix(rfSpread_class, test$spread_result)
library(ROCR)
rfSpread_probs <- predict(rfSpread, newdata = test, type = "prob")
p_test_rfSpread <- prediction(rfSpread_probs[, 1], test$spread_result) # fix for 2 categories
perf_rfSpread <- performance(p_test_rfSpread, "tpr", "fpr")
# The ROC curve
plot(perf_rfSpread)
# Find Area under the curve (AUC)
rfSpread_auc <- performance(p_test_rfSpread, "auc")@y.values
rfSpread_auc

# svm spreads
svm1<-svm(fmSpread, data=train, kernel="radial", cost=1, probability=TRUE, na.action=na.exclude)
predict_svm1<-predict(svm1, newdata=test)
confusionMatrix(predict_svm1, test$spread_result, positive="Cover")
predict_svm_prob1<-predict(svm1, newdata=test, probability=TRUE)
predict_svm_prob1 <- attr(predict_svm_prob1, "probabilities")
p_test_svm1 <- ROCR::prediction(predict_svm_prob1[, 2], test$spread_result)
perf_svm <- performance(p_test_svm1, "tpr", "fpr")
performance(p_test_svm1, "auc")@y.values

# classification spreads
fitSpread <- rpart(spread_result ~ schedule_week_1to4 + schedule_week_last + schedule_day + schedule_month + stadium_type + division_matchup + team_home_elo_pre + team_away_elo_pre + over_under_line + over_under_outlier + weather_cold + weather_rain + weather_snow + weather_wind_bad+team_offense_home+team_defense_away + team_offense_away+team_defense_home, method="class",data=df2)
plot(fitSpread)
text(fitSpread, cex=.5, use.n=TRUE, all=TRUE)
summary(fitSpread)

# lm spreads
lmSpread <- lm(spread_actual ~ team_home_elo_pre*team_away_elo_pre+team_offense_home*team_defense_away + team_offense_away*team_defense_home, data=df2)

