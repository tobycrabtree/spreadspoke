#  weather info
nflWeatherTemp <- read.csv("weather_data_4.csv")
nflWeatherTemp <- subset(nflWeatherTemp,nflWeatherTemp$schedule_season>1978)

nflWeatherTemp$game_id <- NULL

for(i in 1:nrow(nflWeatherTemp)){
        nflWeatherTemp$game_id[i] <- paste(as.Date(nflWeatherTemp$schedule_date[i],format='%m/%d/%Y'),nflWeatherTemp$team_away_id[i],nflWeatherTemp$team_home_id[i],sep="") # set unique id for each game 
}  
nflWeatherTemp$game_id <- gsub("-","",nflWeatherTemp$game_id) # remove - from game id

nflTemp <- read.csv("nfl_gameids.csv")
nflW <- merge(nflTemp,nflWeatherTemp, by=c("game_id","schedule_date","schedule_season","team_home_id","team_away_id"),all.y=TRUE)
#nflW$weather_forecast2 <- paste0(nflW$weather_detail.x,nflW$weather_forecast,sep="-")

nflW$weather_cold <- ifelse(is.na(nflW$weather_temperature),FALSE,ifelse(nflW$weather_temperature < 36,TRUE,FALSE))       
nflW$weather_wind_bad <- ifelse(is.na(nflW$weather_wind_mph),FALSE,ifelse(nflW$weather_wind_mph > 12,TRUE,FALSE))        
nflW$weather_rain <- grepl(c("Rain"),nflW$weather_forecast2, ignore.case=TRUE)    
nflW$weather_snow <- grepl(c("Snow"),nflW$weather_forecast2, ignore.case=TRUE)      
nflW$weather_fog <- grepl(c("Fog"),nflW$weather_forecast2, ignore.case=TRUE)      

nflWeather <- nflW[c("game_id","weather_temperature","weather_cold","weather_wind_bad","weather_rain","weather_snow","weather_fog")]
write.csv(nflWeather,"nfl_weather.csv")

# noaa weather data prep
require(plyr)
weatherFileNames <- c("1071861.csv","1071857.csv","1071836.csv","1071828.csv","1072181.csv","1072179.csv","1072145.csv","1072154.csv")

#noaaTemp1 <- read.csv(weatherFileNames[1])
#noaaTemp2 <- read.csv(weatherFileNames[2])
#noaaTemp3 <- read.csv(weatherFileNames[3])
#noaaTemp4 <- read.csv(weatherFileNames[4])
#noaaTemp5 <- read.csv(weatherFileNames[5])
noaaTemp <- read.csv(weatherFileNames[6])
#noaaTemp <- read.csv(weatherFileNames[7])
#noaaTemp <- read.csv(weatherFileNames[8])
#noaaTemp <- rbind(noaaTemp6,noaaTemp4,noaaTemp1)
#noaaTemp <- merge(noaaTemp,noaaTemp2)
#noaaTemp <- do.call(rbind.fill(),lapply(weatherFileNames,read.csv))

noaaTemp$DATE <- as.Date(noaaTemp$DATE)
days <- unique(nfl$schedule_date) # from nfl file nfl_1.R process first
noaaTemp <- noaaTemp[noaaTemp$DATE %in% days, ]
noaaTemp$weather_temperature <- ifelse(is.na(noaaTemp$TAVG==TRUE),noaaTemp$TMAX,noaaTemp$TAVG)
noaaTemp$weather_snow <- ifelse(noaaTemp$SNOW>0,TRUE,FALSE)
noaaTemp$weather_rain <- ifelse(noaaTemp$PRCP>0,TRUE,FALSE)
noaaTemp$weather_fog <- ifelse(noaaTemp$WT01 %in% 1|noaaTemp$WT02 %in% 1|noaaTemp$WT21 %in% 1,TRUE,FALSE)

noaaTemp$weather_detail <- paste0(noaaTemp$weather_temperature,"F ",
                                  "-Wind ",noaaTemp$AWND," mph -",
                                  ifelse(noaaTemp$weather_snow==TRUE,"Snow ",""),
                                  ifelse(noaaTemp$weather_rain==TRUE,"Rain ",""),
                                  ifelse(noaaTemp$WT04 %in% 1|noaaTemp$WT17 %in% 1,"Ice, Sleet, Freezing Rain",""))

noaaTemp2 <- noaaTemp[c("STATION","NAME","LATITUDE","LONGITUDE","ELEVATION","DATE")]
write.csv(noaaTemp2,"noaa_stations.csv")
