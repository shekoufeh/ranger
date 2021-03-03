run_all_methods_once <- function(kk,missingPercentage,missingType,repId){
library(tidyr)
library(ranger)
library(missMDA)
library(data.table)
#library(randomForestSRC)


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
  rg.reg  <- ranger(Y ~ .,num.trees=400,data = trainData,seed = seed)
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
  rg.reg  <- ranger(Y ~ .,num.trees=400,data = trainData,seed = seed)
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
  rg.reg  <- ranger(Y ~ .,num.trees=400,data = trainData,seed = seed)
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
  rg.reg  <- ranger(Y ~ .,num.trees=400,data = trainData,seed = seed)
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
  rg.reg  <- ranger(Y ~ .,num.trees=400,data = trainData,missing.tree.weight=0.5, seed = seed)
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
  rg.reg  <- ranger(Y ~ .,num.trees=400,data = trainData,
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

use_median_at_node_RF <- function ( data, trainRatio = 0.7, seed = NULL){
  
  result <- list()
  result$method <- "medianAtNode"
  # split data into train, test
  trainRows  <- sample(nrow(data),nrow(data)*trainRatio)
  trainData <- data[trainRows,]
  testData  <- data[-trainRows,]
  
  # No imputation  
  
  # train
  rg.reg  <- ranger(Y ~ .,num.trees=400,data = trainData,seed = seed,impute.missing="median")
  result$trainPredictionError <- rg.reg$prediction.error
  result$R2 <- rg.reg$r.squared
  
  # test
  pred    <- predict(rg.reg,testData[,1:ncol(testData)-1])
  result$testPredictionError <-mean((testData$Y - pred$predictions)^2)
  
  return(result)
}

use_median_at_node_tree_weighted_RF <- function ( data, trainRatio = 0.7, seed = NULL, tree.weight = 0.5){
  
  
  result <- list()
  result$method <- "medianAtNodeRandomWeightedTree"
  # split data into train, test
  trainRows  <- sample(nrow(data),nrow(data)*trainRatio)
  trainData <- data[trainRows,]
  testData  <- data[-trainRows,]
  
  # No imputation  
  
  # train
  rg.reg  <- ranger(Y ~ .,num.trees=400,data = trainData,missing.tree.weight=0.5, 
                    seed = seed,impute.missing="median")
  result$trainPredictionError <- rg.reg$prediction.error
  result$R2 <- rg.reg$r.squared
  
  # test
  pred    <- predict(rg.reg,testData[,1:ncol(testData)-1])
  result$testPredictionError <-mean((testData$Y - pred$predictions)^2)
  
  return(result)
}

use_median_at_node_tree_and_forest_weighted_RF <- function ( data, trainRatio = 0.7, 
                                              seed = NULL, tree.weight = 0.5,
                                              forest.weight = 0.5){
  
  
  result <- list()
  result$method <- "medianAtNodeRandomWeightedTreeAndForest"
  # split data into train, test
  trainRows  <- sample(nrow(data),nrow(data)*trainRatio)
  trainData <- data[trainRows,]
  testData  <- data[-trainRows,]
  
  # No imputation  
  
  # train
  rg.reg  <- ranger(Y ~ .,num.trees=400,data = trainData,
                    missing.tree.weight=0.5, 
                    missing.forest.weight = forest.weight, 
                    seed = seed,impute.missing="median")
  
  result$trainPredictionError <- rg.reg$prediction.error
  result$R2 <- rg.reg$r.squared
  
  # test
  pred    <- predict(rg.reg,testData[,1:ncol(testData)-1])
  result$testPredictionError <-mean((testData$Y - pred$predictions)^2)
  
  return(result)
}


corVec <-c(0.5,0.05,0.8,0.8,0.05,0.5)
rsqrVec<-c(0.6,0.6,0.6,0.9,0.9,0.9)
missingPercentages <- c(0.05,0.25,0.5,0.75,0.9)
 
dataDir<-"/home/gorgizadeh/scratch" 
fname  <-paste(kk,"-evaluation-cor",corVec[kk],"-Rsqr",rsqrVec[kk],"-300",sep="")
dataDir<-paste(dataDir,"/",fname,"/",sep="")
sDir <-getwd()
dir.create(paste(sDir,"/",fname,"/evaluations/MCAR/",sep = ""), showWarnings = FALSE,recursive=TRUE)
dir.create(paste(sDir,"/",fname,"/evaluations/MAR/",sep = ""), showWarnings = FALSE,recursive=TRUE)
dir.create(paste(sDir,"/",fname,"/evaluations/MNAR/",sep = ""), showWarnings = FALSE,recursive=TRUE)

saveDir<-paste(sDir,"/",fname,"/evaluations/",sep="")

trainRatio<-0.7
ks<-kk*1500
print(missingPercentage)
print(missingType)
print(kk)
print(repId)
seed <- ks+repId
set.seed(seed)
resultList<-list()
if(TRUE){
dataOrig <- read.csv(file = paste(dataDir,"/simulated_missing_data/",missingType,"/",repId,"-",missingPercentage,".csv",sep = ""))

print("median")
data<-data.frame(dataOrig)

result <- list()
result$method <- "median"
# split data into train, test
trainRows  <- sample(nrow(data),nrow(data)*trainRatio)
trainData <- data[trainRows,]
testData  <- data[-trainRows,]
# median imputation  
for(i in 1:(ncol(trainData)-1)){
  m<-median(trainData[,i],na.rm = TRUE)
  dcol<-trainData[,i]
  dcol[is.na(dcol)]<-m
  trainData[,i]<-dcol
}

for(i in 1:(ncol(testData)-1)){
  m<-median(testData[,i],na.rm = TRUE)
  dcol<-testData[,i]
  dcol[is.na(dcol)]<-m
  testData[,i]<-dcol
}

# train
rg.reg  <- ranger(Y ~ .,num.trees=400,data = trainData,seed = seed)
result$trainPredictionError <- rg.reg$prediction.error
result$R2 <- rg.reg$r.squared

# test
pred    <- predict(rg.reg,testData[,1:ncol(testData)-1])
result$testPredictionError <-mean((testData$Y - pred$predictions)^2)


resultList[[1]]<-data.frame(result)
print("PCA")
data<-data.frame(dataOrig)
result<-use_PCA_RF(data,trainRatio,seed)
resultList[[2]]<-data.frame(result)
# print("Emp")
# data<-data.frame(dataOrig)
# result<-use_empirical_distribution_RF(data,trainRatio,seed)
# resultList[[3]]<-data.frame(result)
print("rand RF")
data<-data.frame(dataOrig)
result<-use_random_RF(data,trainRatio,seed)
resultList[[3]]<-data.frame(result)
print("rand wT RF")
data<-data.frame(dataOrig)
result<-use_tree_weighted_RF(data,trainRatio,seed,0.5)
resultList[[4]]<-data.frame(result)
print("rand wF wT RF")
data<-data.frame(dataOrig)
result<-use_tree_and_forest_weighted_RF(data,trainRatio,seed,0.5,0.5)
resultList[[5]]<-data.frame(result)
print("median RF")
data<-data.frame(dataOrig)
result<-use_median_at_node_RF(data,trainRatio,seed)
resultList[[6]]<-data.frame(result)
print("rand wT RF")
data<-data.frame(dataOrig)
result<-use_median_at_node_tree_weighted_RF(data,trainRatio,seed,0.5)
resultList[[7]]<-data.frame(result)
print("rand wF wT RF")
data<-data.frame(dataOrig)
result<-use_median_at_node_tree_and_forest_weighted_RF(data,trainRatio,seed,0.5,0.5)
resultList[[8]]<-data.frame(result)

finResult = do.call(rbind, resultList)

#out <- tryCatch(
#  {
#    resPart1 <- read.csv(file = paste(saveDir,"/",missingType,"/",repId,"-",mp,".txt",sep = ""))
#    resPart1<-data.frame(resPart1)
#    
#    },
#  error=function(cond) {
#    message("Here's the original error message:")
#    message(cond)
#    # Choose a return value in case of error
#    return(NA)
#  },
#  warning=function(cond) {
#    message(cond)
#    # Choose a return value in case of warning
#    return(NULL)
#  },
#  finally={
#    if(dim(resPart1)[1]==9){
#      finResult<-rbind(resPart1[1:6,],resPart2)
#    } else if(dim(resPart1)[1]==6){
#      finResult<-rbind(resPart1,resPart2)
#    }else{
#      finResult<-resPart2
#    }
#  }
#)    
write.csv(finResult, file =  paste(saveDir,"/",missingType,"/",repId,"-",missingPercentage,".txt",sep = ""), row.names = FALSE)
}
}
