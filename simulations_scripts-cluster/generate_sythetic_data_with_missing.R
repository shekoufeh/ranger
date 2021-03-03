rm(list=ls())
setwd("C:/Users/gorizadeh/Documents/GitHub/ranger/")
source("C:/Users/gorizadeh/Documents/GitHub/ranger/simulations_scripts/missing_generating_functions.R")
library(readxl)
set.seed(23425)

corVec <-c(0.5,0.05,0.8,0.8,0.05,0.5)
rsqrVec<-c(0.6,0.6,0.6,0.9,0.9,0.9)
missingPercentages <- c(0.05,0.25,0.5,0.75,0.9)


for(kk in 1:6){
mydata <- read_excel(paste("synth-cor",corVec[kk],"-Rsqr",rsqrVec[kk],"-3000.xlsx",sep=""))
for(repId in 1:1){
  
print("repeation ID")
print(repId)
cpMyData <- data.frame(mydata)
cNames<-names(cpMyData)
varIDs <- seq(1,ncol(cpMyData)-1)
skipColumns<-sample(varIDs,5)


# Generate Missing completely at random dataset
for(mp in missingPercentages){
  cpMyData <- data.frame(mydata)
  # Set missing values at each column randomly
  for(i in 1:(ncol(cpMyData)-1)){
    if(i %in% skipColumns){
      next
    }
    cpMyData <- MCAR(cpMyData,cNames[i],mp)
  }
  print(mp)
  dirName<-paste(kk,"-evaluation-cor",corVec[kk],"-Rsqr",rsqrVec[kk],"-300",sep="")
  dir.create(paste(getwd(),"/",dirName,"/simulated_missing_data/MCAR/",sep = ""), showWarnings = FALSE,recursive=TRUE)
  write.csv(cpMyData,paste(getwd(),"/",dirName,"/simulated_missing_data/MCAR/",repId,"-",mp,".csv",sep = ""), row.names = FALSE)
}

# Generate missing at random
for(mp in missingPercentages){
  cpMyData <- data.frame(mydata)
  varIDs <- seq(1,ncol(cpMyData)-1)
  # Set missing values at each column randomly
  for(i in 1:(ncol(cpMyData)-3)){
    
    cId <- sample(varIDs,1)
    if(cId %in% skipColumns){
      i<-i-1
      next
    }
    varIDs <- varIDs[!varIDs %in% c(cId)]
    refID <- sample(varIDs,1)
    
    cpMyData <- MAR(cpMyData,cNames[cId],cNames[refID],mp)
  }
  print(mp)
  dirName<-paste(kk,"-evaluation-cor",corVec[kk],"-Rsqr",rsqrVec[kk],"-300",sep="")
  dir.create(paste(getwd(),"/",dirName,"/simulated_missing_data/MAR/",sep = ""), showWarnings = FALSE,recursive=TRUE)
  write.csv(cpMyData,paste(getwd(),"/",dirName,"/simulated_missing_data/MAR/",repId,"-",mp,".csv",sep = ""), row.names = FALSE)
}

# Generate missing not at random
for(mp in missingPercentages){
  cpMyData <- data.frame(mydata)
  # Set missing values at each column randomly
  for(i in 1:(ncol(cpMyData)-1)){
    if(i %in% skipColumns){
      next
    }
    cpMyData <- MNAR(cpMyData,cNames[i],mp)
  }
  print(mp)
  
  dirName<-paste(kk,"-evaluation-cor",corVec[kk],"-Rsqr",rsqrVec[kk],"-300",sep="")
  dir.create(paste(getwd(),"/",dirName,"/simulated_missing_data/MNAR/",sep = ""), showWarnings = FALSE,recursive=TRUE)
  write.csv(cpMyData,paste(getwd(),"/",dirName,"/simulated_missing_data/MNAR/",repId,"-",mp,".csv",sep = ""), row.names = FALSE)
}
}
}