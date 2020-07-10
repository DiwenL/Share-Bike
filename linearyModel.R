library(MASS)
library(ggplot2)
library(sqldf)
library(Matrix)
library(glmnet)
setwd("C:/Users/12535/Desktop/457")
source("preprocessing.R")

origin_train = read.csv("train.csv")
origin_test = read.csv("test.csv")

train = preprocessing(origin_train)
test = preprocessing(origin_test)
train_bat = batches(train)
test_bat = batches(test)

count = origin_train$count
train$count = count
train = train[,-1]
attach(train)

result = data.frame(datetime = test$datetime,count = NA)

#plot hour~count group by season
season_hour_count = sqldf('select season, hour, avg(count) as count from train group by season, hour')

ggplot(train, aes(x=hour, y=count, color=season))+
  geom_point(data = season_hour_count, aes(group = season))+
  geom_line(data = season_hour_count, aes(group = season))+
  ggtitle("Count By Season")+
  scale_colour_hue('Season',breaks = levels(train$season), labels=c('spring', 'summer', 'fall', 'winter'))

#plot hour~count group by weekday
weekday_hour_count = sqldf('select wday, hour, avg(count) as count from train group by wday, hour')

ggplot(train, aes(x=hour, y=count, color=wday))+
  geom_point(data = weekday_hour_count, aes(group = wday))+
  geom_line(data = weekday_hour_count, aes(group = wday))+
  ggtitle("Count By Weekday")+
  scale_colour_hue('wday',breaks = levels(train$wday), labels=c('Mon', 'Tue', 'Wed', 'Thur','Fri','Sat','Sun'))


#######simple lineary regression model
for (m in 1:24){
  cat("month: ",m,"\n")
  monthlydata = train[train_bat[1,m]:train_bat[2,m],]
  monthlytest = test[test_bat[1,m]:test_bat[2,m],]
  monthlycount = count[train_bat[1,m]:train_bat[2,m]]
  
  #using lm without "year", "month", "season","mday"
  fit.lm = lm(monthlydata$count~weather+temp+atemp+humidity+windspeed+wday+hour,
              data = monthlydata)
  fit.lm.AIC = stepAIC(fit.lm,direction = "both")
  pred.lm = predict(fit.lm.AIC,monthlytest[,-12])
  result[test_bat[1,m]:test_bat[2,m],"count"] = exp(pred.lm)
}

output(result$datetime,result$count,"simplelm_fitlog_fulldata.csv")

##using ridge and lasso(FAIL)
X = train[,-14]
X$season = as.integer(X$season)
X$holiday = as.integer(X$season)
X$workingday = as.integer(X$workingday)
X$weather = as.integer(X$weather)
X$temp = as.integer(X$temp)
X$atemp = as.integer(X$atemp)
X$temp = ceiling(X$temp)
X$atemp = ceiling(X$atemp)
X$windspeed = as.integer(X$windspeed)
X$windspeed = ceiling(X$windspeed)
X$year = as.integer(X$year)
X$month = as.integer(X$month)
X$mday = as.integer(X$mday)
X$wday = as.integer(X$wday)
Y = train[,14]
fit.ridge = glmnet(X,Y,alpha = 0)

