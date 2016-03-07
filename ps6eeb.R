library(readr)
library(ggplot2)
library(MASS)

setwd("C:/Users/Emily/Documents/AppliedQuantFin")
rm(list=ls())

ff <- read_csv("ps5ff.csv")
ff[,2:6] <- ff[,2:6] / 100
rf <- mean(ff$RF)
industry <- read_csv("ps6industry.csv")
industry[,2:31] <- industry[,2:31] / 100
industry <- as.matrix(industry)

#1a)
mom <- 1:1068
for(i in 2 : 1069) {
  temp_sorted <- sort(industry[i-1,])
  avg_losers <- mean(temp_sorted[1:3])
  avg_winners <- mean(temp_sorted[28:30])
  mom[i] <- avg_winners - avg_losers
}

mom_mean <- mean(mom)
mom_sd <- sd(mom)
mom_t_stat <- mom_mean/mom_sd
mom_sharpe <- (mom_mean - rf)/mom_sd

#b)
#calculating portfolio betas: 
betas <- 1:30
for(i in 2 : 31) {
  betas[i-1] <- cov(ff[1:1069,2],industry[,i]-rf)/var(ff$`Mkt-RF`)
}

cross_sec_ret <- 1 : 1068
cross_sec_beta <- 1 : 1068
cross_sec_res <- 1 : 1068
for(i in 2 : 1069) { 
  cross_sec_ret <- var(industry[i-1,])
  #betas <- lm(industry[i-1,2:31] ~ betas + ff$HML[i-1] + ff$SMB[i-1])
  
  
  cross_sec_beta <- betas %*% cov(ff[i,]$`Mkt-RF`, industry[i,])
}

#1c)
mom12 <- 1:1056
for(i in 13 : 1069) {
  temp_sorted <- sort(apply(industry[(i-12):(i-1),],2,sum))
  avg_losers <- mean(temp_sorted[1:3])
  avg_winners <- mean(temp_sorted[28:30])
  mom12[i] <- avg_winners - avg_losers
}

mom12_mean <- mean(mom12)
mom12_sd <- sd(mom12)
mom12_t_stat <- mom12_mean/mom12_sd
mom12_sharpe <- (mom12_mean - rf)/mom12_sd

#1d)
mom212 <- 1:1056
for(i in 13 : 1069) {
  temp_sorted <- sort(apply(industry[(i-12):(i-2),],2,sum))
  avg_losers <- mean(temp_sorted[1:3])
  avg_winners <- mean(temp_sorted[28:30])
  mom212[i] <- avg_winners - avg_losers
}

mom212_mean <- mean(mom212)
mom212_sd <- sd(mom212)
mom212_t_stat <- mom212_mean/mom212_sd
mom212_sharpe <- (mom212_mean - rf)/mom212_sd


