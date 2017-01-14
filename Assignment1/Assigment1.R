##1
#3

#only normal
plot(density(rnorm(n = 600,mean = 0.008,sd = 0.063)))

#normalBernoullimix
normalBernoulliMix <- function(normalMean,normalSD,bernProb,jumpMean,jumpSD,n)
{
  SecondTerm <- jumpMean + jumpSD*rnorm(n)
  jt <- rbinom(n,1,bernProb)*(SecondTerm)
  normalMean + normalSD * rnorm(n) + jt
}

normalBernoulliDist <- normalBernoulliMix(0.012,0.05,0.15,-0.03,0.1,600)  

#density
plot(density(normalBernoulliDist),type="l",col="blue")
lines(density(rnorm(600,0.012,0.05)),col="red")

#actual values
plot(normalBernoulliDist,type="l",col="blue")
lines(rnorm(600,0.012,0.05),type="l",col="red")


##2
this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)
library("readxl")
library("xts")
dbv <- read_excel("DBV.xlsx")
gspc <- read_excel("GSPC.xlsx")

dbv$Date <- as.Date(dbv$Date)
gspc$Date <- as.Date(gspc$Date)
dbv.xts <- xts(dbv[,-1],order.by = dbv$Date)
gspc.xts <- xts(gspc[,-1],order.by = gspc$Date)

#logreturns
dbv.returns <- (dbv.xts$'Adj Close'[-1,] - lag(dbv.xts$'Adj Close')[-1,])/lag(dbv.xts$'Adj Close')[-1,]
gspc.returns <- (gspc.xts$'Adj Close'[-1,] - lag(gspc.xts$'Adj Close')[-1,])/lag(gspc.xts$'Adj Close')[-1,]
dbv.logreturns <- log(dbv.xts$'Adj Close'[-1,]/lag(dbv.xts$'Adj Close')[-1,])
gspc.logreturns <- log(gspc.xts$'Adj Close'[-1,]/lag(gspc.xts$'Adj Close')[-1,])

#1
plot(dbv.logreturns)
plot(gspc.logreturns)

hist(dbv.logreturns)
hist(gspc.logreturns)

#2


library(moments)

#a
skewNullCheck <- function(returns,alpha=.05){
  criticalt <- qt(1-(alpha/2),df = length(returns))
  skewcap <- skewness(returns)
  skewt <- skewcap/(sqrt(6/length(returns)))
  returnVal <- c(skewcap,skewt,abs(skewt) > criticalt) #TRUE, so reject normal distribution, no skewness Null
  names(returnVal) <- c("Sample Skewness","Skewness t","Reject Null?")
  returnVal
}

skewNullCheck(dbv.logreturns)
skewNullCheck(gspc.logreturns)

#b
kurtosisNullCheck <- function(returns,alpha=.05){
  criticalt <- qt(1-(alpha/2),df = length(returns))
  kurtosiscap <- kurtosis(returns)
  kurtosist <- (kurtosiscap-3)/(sqrt(24/length(returns)))
  returnVal <- c(kurtosiscap,kurtosist,abs(kurtosist) > criticalt) #TRUE, so reject normal distribution, no skewness Null
  names(returnVal) <- c("Sample Kurtosis","Kurtosis t","Reject Null?")
  returnVal
}

kurtosisNullCheck(dbv.logreturns)
kurtosisNullCheck(gspc.logreturns)

#c
jbTest <- function(returns,alpha=.05){
  criticalchi <- qchisq(1-alpha,df = 2)
  skewcap <- skewness(returns)
  kurtosiscap <- kurtosis(returns)
  jbt <- skewcap^2/(6/length(returns)) + (kurtosiscap-3)^2/(24/length(returns))
  abs(jbt) > criticalchi #TRUE, so reject normal distribution, no skewness Null
}

jbTest(dbv.logreturns)
jbTest(gspc.logreturns)

#3
skewKurtMat <- matrix(c(skewNullCheck(dbv.logreturns)[1],skewNullCheck(gspc.logreturns)[1],kurtosisNullCheck(dbv.logreturns)[1],kurtosisNullCheck(gspc.logreturns)[1]),nrow = 2)
colnames(skewKurtMat) <- c("Skewness","Kurtosis")
row.names(skewKurtMat) <- c("DBV","GSPC")
skewKurtMat


#4
## Explain - Return/Standard Deviation covers only the first 2 moments. 
## DBV and GSPC vary even in the 3rd (skewness) and 4th (kurtosis) moments
## As GSPC has lower negative skewness, there is lesser chance of getting a negative tail value. 
## It also has lower kurtosis, which means there is lesser chance of getting an extreme value  

#5
#a Both assumptions valid (i.e. homoskedastic and normal)
reg1Summ <- summary(lm(dbv.logreturns ~ gspc.logreturns))
reg1Summ$coefficients[,2]

#b Heterskedastic errors
## I think we need to calculate covariance matrix and then perform Y (sigma)^(-0.5) = X (sigma)^(-0.5) + error

#c Non normal errors