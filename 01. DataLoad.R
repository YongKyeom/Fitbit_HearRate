
#####################
#### 0. Library  ####
#####################

if( !("fitbitr" %in% installed.packages()[, 1]) ){
  # install.packages("devtools")
  devtools::install_github("teramonagi/fitbitr")
  library(fitbitr)
}

if( !("pathological" %in% installed.packages()[, 1]) ){
  # install.packages("devtools")
  devtools::install_github("richierocks/pathological")
  library(pathological)
}

package <- c("data.table", "DescTools", "reshape2", "tidyverse", "stringr",
             "ggplot2", "lubridate", "pathological", "progress", "fitbitr")
sapply(package, require, character.only = T)


######################
#### 1. 환경세팅  ####
######################

options(stringsAsFactors = FALSE, 
        scipen = 100,        # 100자리까지 표시
        max.print = 999999)   

Sys.setenv(tz = "Asia/Seoul")

## As a global variable
Sys.setenv(FITBIT_KEY    = "22DR29", 
           FITBIT_SECRET = "1c524fb8df4755c7bf8a0b447f3eb060")

## Get token
token <- fitbitr::oauth_token()

#####################
#### 1. Activity ####
#####################
START_DATE <- "2017-02-01"
END_DATE   <- as.character(Sys.Date())

## Get daily activity summary
Daily_Activity_Summary <- get_activity_summary(token, END_DATE)
Daily_Activity_Summary$summary

######################################
#### .. Get Activity Time Series  ####
######################################
Daily_Step     <- get_activity_time_series(token, "steps",    
                                           base_date = START_DATE, 
                                           end_date  = END_DATE,
                                           period    = "max")
Daily_Calories <- get_activity_time_series(token, "calories", 
                                           base_date = START_DATE, 
                                           end_date  = END_DATE,
                                           period = "max")
# activityCalories

## Get activity intraday time series
fncGetActivity_TimeSeries <- function(token, 
                                      Resource_path = "steps",
                                      Base_date,
                                      End_date,
                                      Detail_level){
  # Resource_path = "steps"
  # Base_date     = START_DATE
  # End_date      = END_DATE
  # Detail_level  = "15min"
  
  DateList <- c(as.numeric(gsub("-", "", Base_date)):as.numeric(gsub("-", "", End_date)))
  DateList <- ymd(DateList)
  DateList <- as.character(DateList[!is.na(DateList)])
  
  Result_DF <- NULL
  
  fileDir <- paste0("01.data/", Resource_path, "/")
  if ( !dir.exists(fileDir)  ){ create_dirs(fileDir) }
  
  Progress_Bar <- progress_bar$new(total = NROW(DateList))
  
  for( i0 in 1:NROW(DateList)){
    
    # i0 <- 1
    if( i0 == 1 ){ i0_2 <- 1 }
    
    Progress_Bar$tick()

    tmpDate <- DateList[i0]

    tmpResult_DF          <- get_activity_intraday_time_series(token, Resource_path, tmpDate, detail_level = Detail_level)
    
    if( NROW(tmpResult_DF) == 0 ){ next; }
    
    tmpResult_DF$DateTime <- ymd_hms(paste0(tmpResult_DF$dateTime, " ", tmpResult_DF$dataset_time))
    tmpResult_DF$Category <- Resource_path

    tmpResult_DF <- tmpResult_DF %>%
      dplyr::rename(Date = dateTime, DataValue = dataset_value) %>%
      dplyr::select(Date, DateTime, Category, DataValue)

    fwrite(tmpResult_DF,
           file = paste0(fileDir, "Data_", Resource_path, "_", Detail_level, "_", gsub("-", "", tmpDate), ".csv"),
           row.names = FALSE)

    Result_DF <- rbind(Result_DF, tmpResult_DF)
    
    if( i0_2 == 140 ){
      Sys.sleep(60 * 60 + 3) ## Sleep 1 hours Since we only can request 150 times per hour
      i0_2 <- 0
      
      token <<- fitbitr::oauth_token()
      
    }
    
    i0_2 <- i0_2 + 1
    
  }
  
  return(Result_DF)
}


fncGetHeartRate_TimeSeries <- function(token, 
                                       Base_date,
                                       End_date,
                                       Detail_level){
  Resource_path = "hearts" # Default
  
  # Base_date     = START_DATE
  # End_date      = END_DATE
  # Detail_level  = "1min"
  
  DateList <- c(as.numeric(gsub("-", "", Base_date)):as.numeric(gsub("-", "", End_date)))
  DateList <- ymd(DateList)
  DateList <- as.character(DateList[!is.na(DateList)])
  
  Result_DF <- NULL
  
  fileDir <- paste0("01.data/", Resource_path, "/")
  if ( !dir.exists(fileDir)  ){ create_dirs(fileDir) }
  
  Progress_Bar <- progress_bar$new(total = NROW(DateList))
  
  for( i0 in 1:NROW(DateList)){
    
    # i0 <- 1
    if( i0 == 1 ){ i0_2 <- 1 }
    
    Progress_Bar$tick()
    
    tmpDate <- DateList[i0]
    
    tmpResult_DF          <- get_heart_rate_intraday_time_series(token, date = tmpDate, detail_level = Detail_level)
    
    if( NROW(tmpResult_DF) == 0 ){ next; }
    
    tmpResult_DF$Date     <- tmpDate
    tmpResult_DF$DateTime <- ymd_hms(paste0(tmpDate, " ", tmpResult_DF$time))
    tmpResult_DF$Category <- Resource_path
    
    tmpResult_DF <- tmpResult_DF %>%
      as.data.frame() %>% 
      dplyr::rename(HeartRate = value) %>%
      dplyr::select(Date, DateTime, Category, HeartRate)
    
    fwrite(tmpResult_DF,
           file = paste0(fileDir, "Data_", Resource_path, "_", Detail_level, "_", gsub("-", "", tmpDate), ".csv"),
           row.names = FALSE)
    
    Result_DF <- rbind(Result_DF, tmpResult_DF)
    
    if( i0_2 == 140 ){
      Sys.sleep(60 * 60 + 3) ## Sleep 1 hours Since we only can request 150 times per hour
      i0_2 <- 0
      
      token <<- fitbitr::oauth_token()
      
    }
    
    i0_2 <- i0_2 + 1
    
  }
  
  return(Result_DF)
}


Sys.sleep(60 * 60 + 3) ## Sleep 1 hours Since we only can request 150 times per hour
Detail_Step         <- fncGetActivity_TimeSeries(token, 
                                                 Resource_path = "steps",
                                                 Base_date     = START_DATE,
                                                 End_date      = END_DATE,
                                                 Detail_level  = "15min")
Sys.sleep(60 * 60 + 3) ## Sleep 1 hours Since we only can request 150 times per hour
Detail_Calories     <- fncGetActivity_TimeSeries(token, 
                                                 Resource_path = "calories",
                                                 Base_date     = START_DATE,
                                                 End_date      = END_DATE,
                                                 Detail_level  = "15min")
Sys.sleep(60 * 60 + 3) ## Sleep 1 hours Since we only can request 150 times per hour
Daily_HeartRate <- fncGetHeartRate_TimeSeries(token, 
                                              Base_date     = START_DATE,
                                              End_date      = END_DATE,
                                              Detail_level  = "1min")



# Get Frequent Activities
get_frequent_activities(token)

# Get Recent Activities
get_recent_activity_types(token)

get_activity_goals(token, period = "daily")
get_activity_goals(token, period = "weekly")

## Get Lifetime Stats
get_lifetime_stats(token) %>% glimpse

# Set a date for example
date <- as.character(Sys.Date() - 1)
# Get heart rate time series
Daily_HeartRate_Summary <- get_heart_rate_time_series(token, date = date, period = "max")
Daily_HeartRate_Summary <- Daily_HeartRate_Summary %>% 
  dplyr::filter(dateTime >= START_DATE)
Daily_HeartRate_Summary %>% glimpse
Daily_HeartRate_Summary$value$heartRateZones[[1]]



# Get Sleep Logs(date is character or Date)
Daily_Sleep_Log <- get_sleep_logs(token, "2019-06-06")
Daily_Sleep_Log$sleep %>% glimpse
Daily_Sleep_Log$sleep %>% tail
#  1 ("asleep"), 2 ("awake"), or 3 ("really awake").
#>   totalMinutesAsleep totalSleepRecords totalTimeInBed
#> 1                197                 1            357
Daily_Sleep_Log$summary
Daily_Sleep_Log$sleep %>% group_by(value) %>% summarise(CNT = n())
#Get the current sleep goal.
get_sleep_goal(token)
#>   awakeRestlessPercentage flowId recommendedSleepGoal typicalDuration
#> 1               0.5631363      0                  465             444
#>   typicalWakeupTime minDuration           updatedOn
#> 1             07:33         380 2017-12-16 10:50:32

#Get Sleep Time Series
get_sleep_time_series(token, "minutesAsleep", date = date, period="7d")
#>     dateTime value
#> 1 2016-03-27     0
#> 2 2016-03-28     0
#> 3 2016-03-29   714
#> 4 2016-03-30   357
#> 5 2016-03-31   552
#> 6 2016-04-01   326
#> 7 2016-04-02   434


# Get deice information you registerd
get_devices(token)
#>   battery deviceVersion features        id        lastSyncTime
#> 1  Medium       Alta HR     NULL 424040354 2017-12-31 11:22:51
#>            mac    type
#> 1 9F1F7466C3DA TRACKER


# Add alarms
tracker_id <- get_devices(token)$id[1]
add_alarm(token, tracker_id, "07:15-08:00", "MONDAY")
#>     alarmId deleted enabled recurring snoozeCount snoozeLength
#> 1 562558099   FALSE    TRUE     FALSE           3            9
#>   syncedToDevice        time    vibe weekDays
#> 1          FALSE 00:15+09:00 DEFAULT
alarm <- get_alarms(token, tracker_id)
alarm