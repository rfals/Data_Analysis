remove(list=ls())

library(ggplot2)
library(dynlm)
library(ggfortify)
library(forecast)
library(vars)
library(fGarch)
library(rugarch)
library(dplyr)
library(urca)

#set working directory
setwd("")

Data <- read.csv("Expectations.csv", header=TRUE, sep=",")

Data <- Data %>%
  mutate(date = as.Date(date, format="%d.%m.%Y"))

Data_ts <- Data %>%
  dplyr::select(eurusd, conf, euribor, hicp, infl_exp) %>%
  ts(start=c(1999,1), frequency=12)

#Question 1

Data_ts[, c("conf","infl_exp")] %>%
  autoplot(facets=FALSE)

d_Data_ts <- Data_ts - stats::lag(Data_ts,-1)
colnames(d_Data_ts) <- colnames(Data_ts)

ggAcf(Data_ts[,"infl_exp"], lag.max=12, plot=TRUE)
ggAcf(d_Data_ts[,"infl_exp"], lag.max=12, plot=TRUE)

#Question 2
Data_ts[,"infl_exp"] %>%
  ur.df(type="drift", selectlags="AIC") %>%
  summary
#not good, not order zero, not stationary

d_Data_ts[,"infl_exp"] %>%
  ur.df(type="drift", selectlags="AIC") %>%
  summary
#yes good, order one

d_Data_ts %>% 
  dynlm(infl_exp ~ L(infl_exp,1), 
        data=., start=c(1999,5)) %>%
  BIC
#choose 1 lag only, it is best

d_Data_ts %>% 
  dynlm(infl_exp ~ L(infl_exp,1:2), 
        data=., start=c(1999,5)) %>%
  BIC

d_Data_ts %>% 
  dynlm(infl_exp ~ L(infl_exp,1:3), 
        data=., start=c(1999,5)) %>%
  BIC

d_Data_ts %>% 
  dynlm(infl_exp ~ L(infl_exp), data=.) %>% 
  summary
#interpret

#Question 3
# d(infl_exp) ~ d(log_cp)
log_Data <- Data %>% 
  mutate(log_hicp=log(hicp), 
         log_eurusd=log(eurusd)) %>% 
  dplyr::select(-hicp,-eurusd)

log_Data_ts <- log_Data %>% 
  dplyr::select(conf,euribor,log_hicp,log_eurusd,infl_exp) %>% 
  ts(start=c(1999,1), frequency=12)

dlog_Data_ts <- log_Data_ts - stats::lag(log_Data_ts,-1)
colnames(dlog_Data_ts) <- colnames(log_Data_ts)

dlog_Data_ts %>%
  dynlm(infl_exp ~ d(log_hicp), 
        data=., start=c(1999,5)) %>%
  BIC

dlog_Data_ts %>%
  dynlm(infl_exp ~ L(d(log_hicp),1), 
        data=., start=c(1999,5)) %>%
  BIC

dlog_Data_ts %>%
  dynlm(infl_exp ~ L(d(log_hicp),1:2), 
        data=., start=c(1999,5)) %>%
  BIC

dlog_Data_ts %>%
  dynlm(infl_exp ~ L(d(log_hicp),1:3), 
        data=., start=c(1999,5)) %>%
  BIC

#we should use one lag of hicp

ADLmodel1 <- dlog_Data_ts %>% 
  dynlm(infl_exp ~ L(infl_exp) + L(log_hicp,0:1),data=.)
summary(ADLmodel1)

#long run effects of hicp

sum(ADLmodel1$coefficients[3:4])/(1-ADLmodel1$coefficients[2])

#Question 4
log_Data4_ts <- log_Data %>% 
  dplyr::select(conf,log_hicp,log_eurusd,infl_exp) %>% 
  ts(start=c(1999,1), frequency=12)

dlog_Data4_ts <- log_Data4_ts - stats::lag(log_Data4_ts,-1)
colnames(dlog_Data4_ts) <- colnames(log_Data4_ts)

var1 <- dlog_Data4_ts %>%
  VAR(p=2)
summary(var1)

causality(var1, cause="infl_exp")

#Question5

forecasts <- var1 %>%
  predict(n.ahead=3)
forecasts

autoplot(forecasts)

#Question 6

question6 <- Data %>% 
  select(infl_exp) %>% 
  garchFit(~garch(1,1), data=.) %>% 
  summary()


autoplot(question6$residual)

#Question 7

ILevels <- Data_ts %>% 
  dynlm(infl_exp ~ conf + euribor, data=.)
summary(ILevels)

autoplot(ILevels$residual)
#Test stationarity for the residual


#Question 8

#We create pre Crisis subset, from 1999-2007 December, 

pre2008 <- window(Data_ts, start=c(1999,1), end=c(2008,1))

PREcILevels <- pre2008 %>% 
  dynlm(infl_exp ~ conf + euribor, data=.)
summary(PREcILevels)

PREcILevels$residuals %>% 
  ur.df(type = "none", selectlags = "AIC") %>% 
  summary
