library(lubridate)  #for preprocessing datas and times

preprocessing = function(old_data){
  #drops two variables: 'casual','registered'
  cf = c("datetime","season","holiday","workingday",
         "weather","temp","atemp","humidity","windspeed") 
  new_data = old_data[, cf]
  
  #add more variances, from datetime
  new_data$hour= hour(old_data$datetime) #hour is an integer
  new_data$year = as.factor(year(old_data$datetime))
  new_data$month = as.factor(month(old_data$datetime))
  new_data$mday =  as.factor(mday(old_data$datetime))
  new_data$wday = ifelse(wday(old_data$datetime)==1, 7, wday(old_data$datetime)-1)
  new_data$wday = as.factor(new_data$wday)
  
  #season
  new_data$season = as.factor(new_data$season)
  
  #weather, make 4 to 3
  new_data$weather[new_data$weather == 4] = 3
  new_data$weather = as.factor(new_data$weather)
  
  #make holiday,workingday factor
  new_data$holiday = as.factor(new_data$holiday)
  new_data$workingday = as.factor(new_data$workingday)
  
  return(new_data)
}


batches = function(dataset){ #find the rows corresponds to the start and end of each month
  
  batch_index = matrix(nrow = 2, ncol = 24)
  cnt = 1
  tt_index = 1:dim(dataset)[1]
  
  for (y in unique(dataset$year)){
    for (m in unique(dataset$month)) { 
      tmp = tt_index[dataset$year == y & dataset$month == m]
      batch_index[1,cnt] = min(tmp)
      batch_index[2,cnt] = max(tmp)
      cnt = cnt + 1
    }
  }
  return(batch_index)
}


output = function(test_datetime, test_count, filename){#output
  
  result = data.frame(datetime = test_datetime, count = test_count)
  write.csv(result, file = filename, row.names=FALSE)
  
}


