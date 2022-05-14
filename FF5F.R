###############################################################################

                    #Fama-French 5-factor regressions

###############################################################################

#Packages
library(dplyr)
library(ggplot2)
library(stargazer)
library(PerformanceAnalytics)

###############################################################################

#set working directory and input a .csv file with market data

setwd("/Users/reinisfals/Desktop/FE/Term Paper")
returns <- read.table(file = "IFUND_FF5F.csv", header = T, sep = ",")

###############################################################################

#Check the dimensions of your dataframe
dim(returns)

#count of columns
L <- ncol(returns)

#the start of market data
J <- L - 8

###############################################################################

#create storage for relevant variables
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

###############################################################################

#create storage for market data

#Rj = stock_1 returns
Rj <- returns[ ,9] 

#Rm = benchmark market return
Rm <- returns[ ,8]

#Fm = risk free rate
Fm <- returns[ ,2]

#Fs = Size factor 
Fs <- returns[ ,3]

#Fh = High - Low book-to-market factor
Fh <- returns[ ,4]

#Fr = Momentum factor
Fr <- returns[ ,5]

#Fc = Lag effect factor
Fc <- returns[ ,6]

###############################################################################

#Compute the first regression, note that the output is stored
myResult <- lm(Rj ~ Rm + Fm + Fs + Fh + Fr + Fc)
mySummary <- summary(myResult)
myCoef <- coef(mySummary)
myR <- mySummary$r.squared

###############################################################################

#for loop to collect data for all of the stocks

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

###############################################################################

#individual outputs by variable
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

###############################################################################

#to get all data stored in one dataframe
myNames <- colnames(returns)

#in myNames2 input the dimensions
myNames2 <- myNames[7:21]

mydata11 <- data.frame(myNames2, myAlphas, myTa, myBetas,myTb, myBetas2, myTb2, myBetas3, myTb3, myR2)

###############################################################################

#TROUBLESHOOTING

###############################################################################

#The first commands might not create a valid dataframe in that case try getting
#the data this way:

#set working directory
setwd("")

#load data
data <- read.csv("", header=TRUE, sep=",", na.strings = "")
summary(data)

#change date formating
data <- data %>%
  mutate(date = as.Date(date, format="%d/%m/%Y")) %>%
  arrange(date)





