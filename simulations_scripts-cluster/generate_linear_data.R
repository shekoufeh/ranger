setwd("C:/Users/gorizadeh/Documents/GitHub/ranger/")
library("writexl")
library(MASS)

saveData<-TRUE
seedVec<-c(12345,1234,123,12,1,2)
corVec <-c(0.5,0.05,0.8,0.8,0.05,0.5)
rsqrVec<-c(0.6,0.6,0.6,0.9,0.9,0.9)
noisVec<-c(3.65,4.5,5.1,0.94,1.86,1.43)

for(i in 1:6){

set.seed(seedVec[i])
numParams<-300
numInformative<-100
n <- 3000

mu <- rep(0, numParams)
Sigma <- matrix(corVec[i], ncol = numParams, nrow = numParams)
diag(Sigma) <- 1

X <- mvrnorm(n, mu = mu, Sigma = Sigma)

coefs <- c(runif(numInformative, -1, 1), rep(0, numParams-numInformative))
eta <- apply(t(t(X) * coefs), 1, sum)
# for R2 0.6, use 1.15 *** 
Y <- eta + rnorm(n, sd = noisVec[i])

model1 <- lm(Y ~ X)

# R2
rsqr<-cor(Y,eta)^2
# MSE
mse<-mean((Y - eta)^2)

if(saveData){
# saveData in 
df <- data.frame(X,Y)

write_xlsx(df,paste("synth-cor",corVec[i],"-Rsqr",rsqrVec[i],"-3000.xlsx",sep=""))
}
}