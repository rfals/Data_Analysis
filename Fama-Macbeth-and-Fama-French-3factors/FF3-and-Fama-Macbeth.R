
################################################################

          #Fama-Macbeth regression analysis

################################################################

#packages
library(plm)
library(dplyr)
library(ggplot2)
library(stargazer)
library(PerformanceAnalytics)

################################################################

#set working directory
setwd("")

#choose the .csv file
returns <- read.table(file = "", header = T, sep = ",")

################################################################

#Fama-Macbeth analysis

################################################################

dim(returns)

summary(returns)


data_is.na <- returns[!is.na(returns$deutsche), ]



L <- 21
J <- L - 6


myBetas <- rep(0,J)
myAlphas <- rep(0,J)
myTs <- rep(0, J)
myAver <- rep(0, J)



Rj <- returns[ ,7] 

Rm <- returns[ ,6]


myResult <- lm(Rj ~ Rm)
mySummary <- summary(myResult)
myCoef <- coef(mySummary)

myBetas[1] <- myCoef[2,1]
myAlphas[1] <- myCoef[1,1]
myTs[1] <- myCoef[1,3]
myAver[1] <- mean(Rj)

for (j in 1:15) {
  Rj <- returns[ ,6+j]
  myResult <- lm(Rj ~ Rm)
  mySummary <- summary(myResult)
  myCoef <- coef(mySummary)
  
  myBetas[j] <- myCoef[2,1]
  myAlphas[j] <- myCoef[1,1]
  myTs[j] <- myCoef[1,3]
  myAver[j] <- mean(Rj)
}

myCoef
myResult
mySummary
myBetas
myAlphas
myTs
myAver


#######mean returns



######

plot(myBetas, myAver)

x <- c(1:250)/100
points(x, mean(Rm)*x, col="red", type = "l")

myNames <- colnames(returns)
myNames2 <- myNames[7:21]

mydata3 <- data.frame(myNames2, myBetas)
mydata4 <- data.frame(myNames2, myAlphas)


##############



################################################################

#Fama-French 3-factor analysis

################################################################

setwd("")
returns <- read.table(file = "", header = T, sep = ",")

################################################################


dim(returns)



L <- ncol(returns)
J <- L - 6


myBetas <- rep(0,J)
myBetas2 <- rep(0,J)
myBetas3 <- rep(0,J)
myAlphas <- rep(0,J)
myTa <- rep(0, J)
myTb <- rep(0, J)
myTb2 <- rep(0, J)
myTb3 <- rep(0, J)
myAver <- rep(0, J)
myR2 <- rep(0, J)



Rj <- returns[ ,7] 

Rm <- returns[ ,6]

Fm <- returns[ ,2]

Fs <- returns[ ,3]

Fh <- returns[ ,4]

myResult <- lm(Rj ~ Rm + Fm + Fs + Fh)
mySummary <- summary(myResult)
myCoef <- coef(mySummary)
myR <- mySummary$r.squared


for (j in 1:J) {
  Rj <- returns[ ,6+j]
  myResult <- lm(Rj ~ Rm + Fm + Fs + Fh)
  mySummary <- summary(myResult)
  myCoef <- coef(mySummary)
  myR <- mySummary$r.squared
  
  myBetas[j] <- myCoef[2,1]
  myBetas2[j] <- myCoef[3,1]
  myBetas3[j] <- myCoef[4,1]
  myAlphas[j] <- myCoef[1,1]
  myTa[j] <- myCoef[1,3]
  myTb[j] <- myCoef[2,3]
  myTb2[j] <- myCoef[3,3]
  myTb3[j] <- myCoef[4,3]
  myAver[j] <- mean(Rj)
  myR2[j] <- myR
}

myCoef
myResult
mySummary
myBetas
myBetas2
myBetas3
myAlphas
myTa
myTb
myTb2
myTb3
myAver
myR2


myNames <- colnames(returns)
myNames2 <- myNames[7:21]

mydata11 <- data.frame(myNames2, myAlphas, myTa, myBetas,myTb, myBetas2, myTb2, myBetas3, myTb3, myR2)




