library(dplyr)
library(randomForest)
setwd("C:/Users/12535/Desktop/STAT457/midterm_project")
source('preprocessing.R')

##### this file may be a little bit mass because i tried all kind of test and didnt deleted them all.
##### my finally model is in the bottom

originData = read.csv("train.csv")
originTest = read.csv(("test.csv"))
Train = preprocessing(originData)
#also drop datetime and wday

Test = preprocessing(originTest)

#finding the batches by month
train_bat = batches(Train)
test_bat = batches(Test)

Train = Train[,-c(1,12)]
Test = Test[,-c(1,12)]

attach(Train)
count = originData$count

#init the result matrix
result = data.frame(datetime = originTest$datetime,count = NA)

ntrees = seq(from=50,to=500,by=50)
mtrys = seq(2,10,2)

###################################################
for (m in 1:24){
  ErrMatrix = matrix(NA,1,length(mtrys))
  for(mt in 1:length(mtrys)){
    cat("month ",m," mtry ",mt,"\n")
    train_idx = c(train_bat[1,1]:train_bat[2,m])
    train_subset = Train[train_idx,]
    count_subset = count[train_idx]
    test_idx = c(test_bat[1,m]:test_bat[2,m])
    test_subset = Test[test_idx,]
    
    rf.fit = randomForest(count_subset~.,
                          data = train_subset,
                          trian = count_subset,
                          importance = TRUE,
                          ntree = 500,
                          mtry = mtrys[mt])
    pred.rf.train = predict(rf.fit,train_subset)
    res = sum((pred.rf.train - count_subset)^2)
    ErrMatrix[mt] = res
  }
  min.err.mtry = mtrys[which.min(ErrMatrix)]
  cat("mtry = ",min.err.mtry,"\n")
  rf.fit = randomForest(count_subset~.,
                        data = train_subset,
                        trian = count_subset,
                        importance = TRUE,
                        ntree = 500,
                        mtry = min.err.mtry)
  result[test_idx,"count"] = predict(rf.fit,test_subset)
}


###################################################################
for(m in 1:24){
  cat("month: ",m,"\n")
  train_idx = c(train_bat[1,1]:train_bat[2,m])
  train_subset = Train[train_idx,]
  count_subset = count[train_idx]
  test_idx = c(test_bat[1,m]:test_bat[2,m])
  test_subset = Test[test_idx,]
  rf.fit = randomForest(count_subset~.,
                        data = train_subset,
                        trian = count_subset,
                        importance = TRUE,
                        ntree = 500,
                        mtry = 20)
  pred.rf.test = predict(rf.fit,test_subset)
  result[test_idx,"count"] = pred.rf.test
}

output(result$datetime,result$count,"rf_ntree500_mtry20_monthly.csv")





###################################################################
#using CV10 to find the best training parameter ntree
nfold = 10
infold = sample(rep(1:nfold, length.out=nrow(Bike)))

# save prediction errors of each fold
errorMatrix = matrix(NA, length(ntrees), nfold) 

# loop across all possible ntree and all folds
for (l in 1:nfold){
  for (k in 1:length(ntrees)){
    cat("fold",l,"  ntree",ntrees[k],"\n")
    rf.fit = randomForest(count[infold != l]~season+holiday+workingday+weather+temp+atemp+humidity+windspeed+hour+year+month+wday,
    data = Bike[infold != l,],
    train = count[infold != l],
    importance = TRUE,
    ntree = ntrees[k])

    pred = predict(rf.fit,Bike[infold == l,])  
    errorMatrix[k, l] = mean((count[infold ==l] - pred)^2)
  }
}
plot(rowMeans(errorMatrix))

###############################################################
nt = ntrees[which.min(rowMeans(errorMatrix))]
#ntree = 250
rf = randomForest(count~season+holiday+workingday+weather+temp+atemp+humidity+windspeed+hour+year+month+wday,
                  data = Bike,
                  train = count,
                  importance = TRUE,
                  ntree = nt)

pred = predict(rf,testData)
result[,"count"] = pred
output(result$datetime,result$count, file = "rf_fullmodel_cv10.csv")



rf.fit1 = randomForest(count~.-mday,
                       data = Bike[,-1],
                       train = count,
                       ntree = 500)
pred.rf.fit1 = predict(rf.fit1,testData[,-1])

output(testData[,1],pred.rf.fit1,"rf_ntree500.csv")


ntrees = seq(50,500,50)
errorMat = matrix(NA,24,length(ntrees))
###########################################################################


for (m in 1:24){
  cat("month: ",m,"\n")
  monthlydata = Train[train_bat[1,1]:train_bat[2,m],]
  monthlytest = Test[test_bat[1,m]:test_bat[2,m],]
  monthlycount = count[train_bat[1,1]:train_bat[2,m]]

  rf.fit = randomForest(monthlycount~.,
                        data = monthlydata,
                        trian = monthlycount,
                        ntree = 500,
                        mtry = 20)
  monthlypred = predict(rf.fit,monthlytest)
  result[test_bat[1,m]:test_bat[2,m],"count"] = monthlypred
}
output(result$datetime,result$count,"monthly_rf_ntree500.csv")
