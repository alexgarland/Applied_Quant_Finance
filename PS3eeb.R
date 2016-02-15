library(readr)
library(MASS)
library(ggplot2)

setwd("C:/Users/Emily/Documents/AppliedQuantFin")
rm(list=ls())

market_proxy <- read_csv("ps3marketProxyTrimmed.csv")
market_proxy <- as.matrix(market_proxy[,2:3])
vw_returns <- read_csv("ps3vwReturns.csv")
vw_returns <- as.matrix(vw_returns[,2:49])
firm_size <- read_csv("ps3avgFirmSize.csv")
firm_size  <- as.matrix(firm_size[,2:49])
be_me <- read.csv("ps3beme.csv")
be_me <- as.matrix(be_me[,2:49])

# 1b)
average_returns <- apply(vw_returns,2,mean, na.rm = TRUE)

cov <- cov(vw_returns)
var_market <- var(market_proxy[,1]);
covariances <- 1 : 48
covariances <- as.matrix(covariances)
for(i in 1 : 48) {
  covariances[i,] = cov(market_proxy[,1], vw_returns[,i])
}

beta <- covariances / var_market
b <- coefficients(model)[2,]

model <- lm(vw_returns ~ market_proxy[,1])
gamma0 <- 1 : 1069
gammaM <- 1 : 1069
eta <- 1 : 1069

for(i in 1 : 1069) {
  m <- lm(vw_returns[i,] ~ b)
  gamma0[i] <- coefficients(m)[1]
  gammaM[i] <- coefficients(m)[2]
  #eta[i] <- coeff
}
g0 <- mean(gamma0)
gM <- mean(gammaM)

#1c
gamma0_time <- 1 : 48
gammaM_time <- 1 : 48
eta_time <- 1 : 48
for(i in 1 : 48) {
  m2 <- lm(vw_returns[,i] ~ market_proxy[,1])
  gamma0_time[i] <- coefficients(m2)[1]
  gammaM_time[i] <- coefficients(m2)[2]
  #something about eta
}

g0_time <- mean(gamma0_time)
gM_time <- mean(gammaM_time)

#1d
plot(beta, average_returns)
plot(b, average_returns)

#1e
for(i in 1 : 1069) {
  if(i-1 %% 12 == 1) {
    lm(vw_returns[,i] ~ market_proxy[,1] + firm_size[,i] + be_me[,i])
  }
}
