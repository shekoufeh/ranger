rm(list=ls())
library(tidyr)
library(ranger)
library(missMDA)

library(randomForestSRC)
dataDir<-"C:/Users/gorizadeh/Documents/GitHub/ranger/"
saveDir<-"C:/Users/gorizadeh/Documents/GitHub/ranger/evaluations/"

median_imputation <- function(data){
  for(i in 1:(ncol(data)-1)){
    m<-median(data[,i],na.rm = TRUE)
    dcol<-data[,i]
    dcol[is.na(dcol)]<-m
    data[,i]<-dcol
  }
  return(data)
}

# impute missing with median, then use ranger
use_median_RF <- function ( data, trainRatio = 0.7, seed = NULL){
 
  result <- list()
  result$method <- "median"
  # split data into train, test
  trainRows  <- sample(nrow(data),nrow(data)*trainRatio)
  trainData <- data[trainRows,]
  testData  <- data[-trainRows,]
  
  # median imputation  
  trainData<-median_imputation(trainData)
  testData<-median_imputation(testData)
  
  # train
  rg.reg  <- ranger(Y ~ .,num.trees=500,data = trainData,seed = seed)
  result$trainPredictionError <- rg.reg$prediction.error
  result$R2 <- rg.reg$r.squared
  
  # test
  pred    <- predict(rg.reg,testData[,1:ncol(testData)-1])
  result$testPredictionError <-mean((testData$Y - pred$predictions)^2)
  
  return ( result )
}

use_PCA_RF <- function ( data, trainRatio = 0.7, seed = NULL){
 
  result <- list()
  result$method <- "PCA"
  # split data into train, test
  trainRows  <- sample(nrow(data),nrow(data)*trainRatio)
  trainData <- data[trainRows,]
  testData  <- data[-trainRows,]
  
  # PCA imputation  
  trainData<-data.frame(imputePCA(trainData,ncp=10)$completeObs)
  testData <-data.frame(imputePCA(testData,ncp=10)$completeObs)
  
  # train
  rg.reg  <- ranger(Y ~ .,num.trees=500,data = trainData,seed = seed)
  result$trainPredictionError <- rg.reg$prediction.error
  result$R2 <- rg.reg$r.squared
  
  # test
  pred    <- predict(rg.reg,testData[,1:ncol(testData)-1])
  result$testPredictionError <-mean((testData$Y - pred$predictions)^2)
  
  return(result)
}

use_empirical_distribution_RF <- function ( data, trainRatio = 0.7, seed = NULL){
  
  result <- list()
  result$method <- "Ishwaran"
  # split data into train, test
  trainRows  <- sample(nrow(data),nrow(data)*trainRatio)
  trainData <- data[trainRows,]
  testData  <- data[-trainRows,]
  
  # Ischwaran RF imputation  
  trainData<-impute(data = trainData)
  testData <-impute(data = testData)
  
  # train
  rg.reg  <- ranger(Y ~ .,num.trees=500,data = trainData,seed = seed)
  result$trainPredictionError <- rg.reg$prediction.error
  result$R2 <- rg.reg$r.squared
  
  # test
  pred    <- predict(rg.reg,testData[,1:ncol(testData)-1])
  result$testPredictionError <-mean((testData$Y - pred$predictions)^2)
  
  return(result)
}

use_random_RF <- function ( data, trainRatio = 0.7, seed = NULL){
  
  result <- list()
  result$method <- "random"
  # split data into train, test
  trainRows  <- sample(nrow(data),nrow(data)*trainRatio)
  trainData <- data[trainRows,]
  testData  <- data[-trainRows,]
  
  # No imputation  
  
  # train
  rg.reg  <- ranger(Y ~ .,num.trees=500,data = trainData,seed = seed)
  result$trainPredictionError <- rg.reg$prediction.error
  result$R2 <- rg.reg$r.squared
  
  # test
  pred    <- predict(rg.reg,testData[,1:ncol(testData)-1])
  result$testPredictionError <-mean((testData$Y - pred$predictions)^2)
  
  return(result)
}

use_tree_weighted_RF <- function ( data, trainRatio = 0.7, seed = NULL, tree.weight = 0.5){
  
  
  result <- list()
  result$method <- "randomWeightedTree"
  # split data into train, test
  trainRows  <- sample(nrow(data),nrow(data)*trainRatio)
  trainData <- data[trainRows,]
  testData  <- data[-trainRows,]
  
  # No imputation  
  
  # train
  rg.reg  <- ranger(Y ~ .,num.trees=500,data = trainData,missing.tree.weight=0.5, seed = seed)
  result$trainPredictionError <- rg.reg$prediction.error
  result$R2 <- rg.reg$r.squared
  
  # test
  pred    <- predict(rg.reg,testData[,1:ncol(testData)-1])
  result$testPredictionError <-mean((testData$Y - pred$predictions)^2)
  
  return(result)
}

use_tree_and_forest_weighted_RF <- function ( data, trainRatio = 0.7, 
                                              seed = NULL, tree.weight = 0.5,
                                              forest.weight = 0.5){
  
  
  result <- list()
  result$method <- "randomWeightedTreeAndForest"
  # split data into train, test
  trainRows  <- sample(nrow(data),nrow(data)*trainRatio)
  trainData <- data[trainRows,]
  testData  <- data[-trainRows,]
  
  # No imputation  
  
  # train
  rg.reg  <- ranger(Y ~ .,num.trees=500,data = trainData,
                    missing.tree.weight=0.5, 
                    missing.forest.weight = forest.weight, 
                    seed = seed)
  
  result$trainPredictionError <- rg.reg$prediction.error
  result$R2 <- rg.reg$r.squared
  
  # test
  pred    <- predict(rg.reg,testData[,1:ncol(testData)-1])
  result$testPredictionError <-mean((testData$Y - pred$predictions)^2)
  
  return(result)
}

trainRatio<-0.7
allMissingTypes<-c("MCAR","MAR","MNAR")
missingPercentages <- c(0.05,0.25,0.5,0.75,0.9)
ks<-100
for(mp in missingPercentages){
for(missingType in allMissingTypes){
  print(mp)
  print(missingType)
for(repId in 1:100){
  print(repId)
seed <- ks
set.seed(seed)
resultList<-list()

dataOrig <- read.csv(file = paste(dataDir,"/simulated_missing_data/",missingType,"/",repId,"-",mp,".csv",sep = ""))

data<-data.frame(dataOrig)
result<-use_median_RF(data,trainRatio,seed)
resultList[[1]]<-result

data<-data.frame(dataOrig)
result<-use_PCA_RF(data,trainRatio,seed)
resultList[[2]]<-result

data<-data.frame(dataOrig)
result<-use_empirical_distribution_RF(data,trainRatio,seed)
resultList[[3]]<-result

data<-data.frame(dataOrig)
result<-use_random_RF(data,trainRatio,seed)
resultList[[4]]<-result

data<-data.frame(dataOrig)
result<-use_tree_weighted_RF(data,trainRatio,seed,0.5)
resultList[[5]]<-result

data<-data.frame(dataOrig)
result<-use_tree_and_forest_weighted_RF(data,trainRatio,seed,0.5,0.5)
resultList[[6]]<-result

finResult = do.call(rbind, resultList)
write.csv(finResult, file =  paste(saveDir,"/",missingType,"/",repId,"-",mp,".txt",sep = ""), row.names = FALSE)
ks<-ks+1
}
}
}