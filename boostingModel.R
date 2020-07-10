library(MASS)
library(gbm)
source("preprocessing.R")

oriTrain = read.csv("train.csv")
oriTest = read.csv("test.csv")

train = preprocessing(oriTrain)
test = preprocessing(oriTest)
count = oriTrain$count

train_bat = batches(train)
test_bat = batches(test)

ntrees = seq(100,500,40)
depths = seq(5,25,2)
errorMatrix = matrix(NA, length(ntrees), length(depths)) 

result = data.frame(test$datetime,count = NA)




for (m in 1:24){
  cat("month ",m,"\n")
  train_subset = train[train_bat[1,1]:train_bat[2,m],-c(1,2,11,12,13)]
  count_subset = count[train_bat[1,1]:train_bat[2,m]]
  test_subset = test[test_bat[1,m]:test_bat[2,m],-c(1,2,11,12,13)]
  fit.boost = gbm(log(count_subset)~.,
                  data = train_subset,
                  distribution = "gaussian",
                  n.trees = 1000,
                  interaction.depth = 20,
                  cv.folds = 10)
  cv.num = gbm.perf(fit.boost)
  pred.boost = predict(fit.boost,test_subset,n.trees = cv.num)
  result[test_bat[1,m]:test_bat[2,m],"count"] = exp(pred.boost)
}


output(result$test.datetime,result$count,"boostingtree_cv10_depth20_cutModel.csv")
