# Read Athero data, and run the modified random forest on it
library(ranger)
context("ranger_reg")
rm(list=ls())
library(readxl)
mydata <- read_excel("synth.xlsx")


cpMyData <- data.frame(mydata)
completeCaseRatio <- c(100,90,80,70,60,50,40,30,20,10)
#completeCaseRatio <- c(100,90,80)
numRepeat <- 100

#xxOOBMSE<- vector("list", length(completeCaseRatio)*numRepeat)
#yyOOBMSE<- vector("list", length(completeCaseRatio)*numRepeat)
#zzOOBMSE<- vector("list", length(completeCaseRatio)*numRepeat)

#xxTestMSE<- vector("list", length(completeCaseRatio)*numRepeat)
#yyTestMSE<- vector("list", length(completeCaseRatio)*numRepeat)
#zzTestMSE<- vector("list", length(completeCaseRatio)*numRepeat)

#xxOOBR<- vector("list", length(completeCaseRatio)*numRepeat)
#yyOOBR<- vector("list", length(completeCaseRatio)*numRepeat)
#zzOOBR<- vector("list", length(completeCaseRatio)*numRepeat)

#xxOOBMSENo<- vector("list", length(completeCaseRatio)*numRepeat)
#yyOOBMSENo<- vector("list", length(completeCaseRatio)*numRepeat)
#zzOOBMSENo<- vector("list", length(completeCaseRatio)*numRepeat)

#xxTestMSENo<- vector("list", length(completeCaseRatio)*numRepeat)
#yyTestMSENo<- vector("list", length(completeCaseRatio)*numRepeat)
#zzTestMSENo<- vector("list", length(completeCaseRatio)*numRepeat)

#xxOOBRNo<- vector("list", length(completeCaseRatio)*numRepeat)
#yyOOBRNo<- vector("list", length(completeCaseRatio)*numRepeat)
#zzOOBRNo<- vector("list", length(completeCaseRatio)*numRepeat)

for(repId in 1:numRepeat){
  set.seed(repId)
  allOOBMSE  <- vector("list", length(completeCaseRatio))
  allOOBRSqr <- vector("list", length(completeCaseRatio))
  allTestMSE <- vector("list", length(completeCaseRatio))
  allTestMSESameData <- vector("list", length(completeCaseRatio))
  
  allOOBMSENoMiss  <- vector("list", length(completeCaseRatio))
  allOOBRSqrNoMiss <- vector("list", length(completeCaseRatio))
  allTestMSENoMiss <- vector("list", length(completeCaseRatio))
  
  missingRate <- vector("list", length(completeCaseRatio))
  
for(i in 1:length(completeCaseRatio)){

ratio <- completeCaseRatio[i]/100.0


# First pick random samples to remain unchanged
#iris[sample(1:nrow(iris),80,replace=F),2]<-NA
numNoMissingSamples <- nrow(mydata)*ratio
numMissingSamples   <- nrow(mydata) - numNoMissingSamples

indxNoMissingSample <- sample(1:nrow(mydata),numNoMissingSamples,replace = F)
noMissingSamples    <- mydata[indxNoMissingSample,]
missingSamples      <- mydata[-indxNoMissingSample,]
cmpMissingSamples   <- data.frame(missingSamples)

# From the remaining data, for variable set 50% of the samples to missing
for(c in 1:(ncol(mydata)-1)){
  missIndx <- sample(1:nrow(missingSamples),numMissingSamples*0.5,replace = F)
  missingSamples[missIndx,c] <- NA
}

# Check if there are cases where all variables have become 
# missing, randomly restore one value
indAllMissing  <- rowSums(is.na(missingSamples[,1:ncol(mydata)-1])) == (ncol(mydata)-1)
numAllMissing  <- sum(indAllMissing)
undoMissingInd <- sample(1:ncol(missingSamples),numAllMissing,replace = T)
valIndAllMissing <- which(indAllMissing %in% c(TRUE))
if(numAllMissing>0){
for(j in 1:numAllMissing){
  missingSamples[valIndAllMissing[j],undoMissingInd[j]] <- cmpMissingSamples[valIndAllMissing[j],undoMissingInd[j]]
}}

# Break dataset into train, test (70%, 30%)
indxTrainNoMissing <- sample(1:nrow(noMissingSamples),numNoMissingSamples*0.7,replace = F)
indxTrainMissing   <- sample(1:nrow(missingSamples),  numMissingSamples*0.7,replace = F)

train <- rbind(noMissingSamples[indxTrainNoMissing,], missingSamples[indxTrainMissing,])
test  <- rbind(noMissingSamples[-indxTrainNoMissing,],missingSamples[-indxTrainMissing,])
rows  <- sample(nrow(train))
train <- train[rows,]
rows  <- sample(nrow(test))
test  <- test[rows,]

missingRate[[i]] <- round(sum(is.na(train)) / ((dim(train)[1]) * (dim(train)[2]-1)),digits = 2)*100

#k <- i+((repId-1)*length(completeCaseRatio))
#yyOOBMSE[[k]]  <- missingRate[[i]]
#yyTestMSE[[k]] <- missingRate[[i]]
#yyOOBR[[k]]    <- missingRate[[i]]

rg.reg  <- ranger(Y ~ .,num.trees=500,data = train,seed = repId)
allOOBMSE[[i]]  <- rg.reg$prediction.error
allOOBRSqr[[i]] <- rg.reg$r.squared

pred    <- predict(rg.reg,test[,1:ncol(test)-1])
allTestMSE[[i]] <-mean((test$Y - pred$predictions)^2)

#zzOOBMSE[[k]]  <- 2
#zzTestMSE[[k]] <- 2
#zzOOBR[[k]]    <- 2

#xxOOBMSE[[k]]  <- allOOBMSE[[i]]
#xxTestMSE[[k]] <- allTestMSE[[i]]
#xxOOBR[[k]]    <- allOOBRSqr[[i]]


removeMissingRows <- TRUE

if(removeMissingRows){
  # Remove any missing values
  train <- train[rowSums(is.na(train)) == 0, ]
  test  <- test[ rowSums(is.na(test)) == 0, ]
}

#yyOOBMSENo[[k]]  <- missingRate[[i]]
#yyTestMSENo[[k]] <- missingRate[[i]]
#yyOOBRNo[[k]]    <- missingRate[[i]]

pred    <- predict(rg.reg,test[,1:ncol(test)-1])
allTestMSESameData[[i]] <-mean((test$Y - pred$predictions)^2)


rg.reg  <- ranger(Y ~ .,num.trees=500,data = train,seed = repId)
allOOBMSENoMiss[[i]]  <- rg.reg$prediction.error
allOOBRSqrNoMiss[[i]] <- rg.reg$r.squared

pred    <- predict(rg.reg,test[,1:ncol(test)-1])
allTestMSENoMiss[[i]] <-mean((test$Y - pred$predictions)^2)

#zzOOBMSENo[[k]]  <- 1
#zzTestMSENo[[k]] <- 1
#zzOOBRNo[[k]]    <- 1

#xxOOBMSENo[[k]]  <- allOOBMSENoMiss[[i]]
#xxTestMSENo[[k]] <- allTestMSENoMiss[[i]]
#xxOOBRNo[[k]]    <- allOOBRSqrNoMiss[[i]]
# print(k)
}
missingRate        <- unlist(missingRate, use.names=FALSE)
allOOBMSE          <- unlist(allOOBMSE, use.names=FALSE)
allOOBRSqr         <- unlist(allOOBRSqr, use.names=FALSE)
allTestMSE         <- unlist(allTestMSE, use.names=FALSE)
allTestMSESameData <- unlist(allTestMSESameData, use.names=FALSE)
allOOBMSENoMiss    <- unlist(allOOBMSENoMiss, use.names=FALSE)
allOOBRSqrNoMiss   <- unlist(allOOBRSqrNoMiss, use.names=FALSE)
allTestMSENoMiss   <- unlist(allTestMSENoMiss, use.names=FALSE)


#print(resultOOBR)
results <- data.frame(cbind(missingRate,allOOBMSE,allOOBRSqr,allTestMSE,allTestMSESameData,allOOBMSENoMiss,allOOBRSqrNoMiss,allTestMSENoMiss))

write.csv(results,paste("C:/Users/gorizadeh/Documents/missRanger/evaluations/result",repId,".csv",sep = ""), row.names = FALSE)
}

#xxOOBMSE  <- unlist(xxOOBMSE, use.names=FALSE)
#yyOOBMSE  <- unlist(yyOOBMSE, use.names=FALSE)
#zzOOBMSE  <- unlist(zzOOBMSE, use.names=FALSE)
#xxTestMSE <- unlist(xxTestMSE, use.names=FALSE)
#yyTestMSE <- unlist(yyTestMSE, use.names=FALSE)
#zzTestMSE <- unlist(zzTestMSE, use.names=FALSE)
#xxOOBR    <- unlist(xxOOBR, use.names=FALSE)
#yyOOBR    <- unlist(yyOOBR, use.names=FALSE)
#zzOOBR    <- unlist(zzOOBR, use.names=FALSE)

#xxOOBMSENo  <- unlist(xxOOBMSENo, use.names=FALSE)
#yyOOBMSENo  <- unlist(yyOOBMSENo, use.names=FALSE)
#zzOOBMSENo  <- unlist(zzOOBMSENo, use.names=FALSE)
#xxTestMSENo <- unlist(xxTestMSENo, use.names=FALSE)
#yyTestMSENo <- unlist(yyTestMSENo, use.names=FALSE)
#zzTestMSENo <- unlist(zzTestMSENo, use.names=FALSE)
#xxOOBRNo    <- unlist(xxOOBRNo, use.names=FALSE)
#yyOOBRNo    <- unlist(yyOOBRNo, use.names=FALSE)
#zzOOBRNo    <- unlist(zzOOBRNo, use.names=FALSE)

#resultOOBMSE  <- cbind(c(xxOOBMSE,xxOOBMSENo),c(yyOOBMSE,yyOOBMSENo),c(zzOOBMSE,zzOOBMSENo))
#resultTestMSE <- cbind(c(xxTestMSE,xxTestMSENo),c(yyTestMSE,yyTestMSENo),c(zzTestMSE,zzTestMSENo))
#resultOOBR    <- cbind(c(xxOOBR,xxOOBRNo),c(yyOOBR,yyOOBRNo),c(zzOOBR,zzOOBRNo))

if(FALSE){
#####################################
############ PLOT MSE ###############
#####################################
#dev.new(width = 400, height = 300, unit = "px")
png(file="C:/Users/gorizadeh/Documents/missRanger/figures/MSE.png",
    width=600, height=500,unit = "px",res=100)

x<-missingRate; y1=allOOBMSE; y2=allOOBMSENoMiss; 
y3<-allTestMSE; y4=allTestMSESameData; y5=allTestMSENoMiss;

plot(x, y1, type="b", pch=19, col="blue",lty=2, xlab="Missing Value Rate (%)", ylab="MSE")#xlim = c(0,60)
# Add a line
lines(x, y2, pch=19, col="red", type="b", lty=2)
lines(x, y3, pch=19, col="#00DBD8", type="b", lty=2)
lines(x, y5, pch=19, col="#FFA600", type="b", lty=2)

# Add a legend
legend(0, 48000, legend=c("Only Complete Samples (OOB)", "All Samples (OOB)",
                          "Only Complete Samples (test)","All Samples (test)"),
       col=c("red", "blue","#FFA600","#00DBD8"), lty=c(2,2,2,2),pch=c(19,19,19,19), cex=0.8)

dev.off()

#####################################
############# PLOT R2 ###############
#####################################
#require(tikzDevice)
#dev.new(width = 400, height = 300, unit = "px")
png(file="C:/Users/gorizadeh/Documents/missRanger/figures/R2.png",
    width=600, height=500,unit = "px",res=100)

x<-missingRate; y1=allOOBRSqr; y2=allOOBRSqrNoMiss; 

plot(x, y1, type="b", pch=19, col="blue",lty=2, 
     xlab="Missing Value Rate (%)", 
     ylab=expression(R^2))#xlim = c(0,60)
# Add a line
lines(x, y2, pch=19, col="red", type="b", lty=2)

# Add a legend
legend(0, 0.2, legend=c("Only Complete Samples (OOB)", "All Samples (OOB)"),
       col=c("red", "blue"), lty=c(2,2),pch=c(19,19), cex=0.8)

dev.off()
}