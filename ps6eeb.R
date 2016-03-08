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
  temp_sorted <- sort(industry[(i-1),])
  avg_losers <- mean(temp_sorted[1:3])
  avg_winners <- mean(temp_sorted[28:30])
  mom[(i-1)] <- avg_winners - avg_losers
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
mom12 <- 1:1057
for(i in 13 : 1069) {
  temp_sorted <- sort(apply(industry[(i-12):(i-1),],2,sum))
  avg_losers <- mean(temp_sorted[1:3])
  avg_winners <- mean(temp_sorted[28:30])
  mom12[i-12] <- avg_winners - avg_losers
}

mom12_mean <- mean(mom12)
mom12_sd <- sd(mom12)
mom12_t_stat <- mom12_mean/mom12_sd
mom12_sharpe <- (mom12_mean - rf)/mom12_sd

#1d)
mom212 <- 1:1057
for(i in 13 : 1069) {
  temp_sorted <- sort(apply(industry[(i-12):(i-2),],2,sum))
  avg_losers <- mean(temp_sorted[1:3])
  avg_winners <- mean(temp_sorted[28:30])
  mom212[i-12] <- avg_winners - avg_losers
}

mom212_mean <- mean(mom212)
mom212_sd <- sd(mom212)
mom212_t_stat <- mom212_mean/mom212_sd
mom212_sharpe <- (mom212_mean - rf)/mom212_sd

#1e)
m1 <- lm(mom-rf ~ ff[1:1069,]$`Mkt-RF` + ff[1:1069,]$SMB +ff[1:1069,]$HML)
m2 <- lm(mom12-rf ~ ff[1:1069,]$`Mkt-RF` + ff[1:1069,]$SMB +ff[1:1069,]$HML)
m3 <- lm(mom212-rf ~ ff[1:1069,]$`Mkt-RF` + ff[1:1069,]$SMB +ff[1:1069,]$HML)

#1f)
m1f <- lm(mom-rf ~ ff[1:1069,]$`Mkt-RF` + ff[1:1069,]$SMB +ff[1:1069,]$HML + ff[1:1069,]$UMD)
m2f <- lm(mom12-rf ~ ff[1:1069,]$`Mkt-RF` + ff[1:1069,]$SMB +ff[1:1069,]$HML + ff[1:1069,]$UMD)
m3f <- lm(mom212-rf ~ ff[1:1069,]$`Mkt-RF` + ff[1:1069,]$SMB +ff[1:1069,]$HML + ff[1:1069,]$UMD)

#1g) 
comm <- read_csv("ps6comm.csv")
comm <- comm[,1:33]
comm <- as.matrix(comm)

comm_mom <- 1 : 551
comm_mom12 <- 1 : 540
comm_mom212 <- 1 : 540
for(i in 13: 552) {
  temp_sorted <- sort(comm[i-1,])
  avg_losers <- mean(temp_sorted[1:3])
  avg_winners <- mean(temp_sorted[(length(temp_sorted)-4):(length(temp_sorted)-1)])
  comm_mom[i-1] <- avg_winners - avg_losers
  
  temp_sorted <- sort(apply(comm[(i-12):(i-1),],2,sum))
  avg_losers <- mean(temp_sorted[1:3])
  avg_winners <- mean(temp_sorted[(length(temp_sorted)-4):(length(temp_sorted)-1)])
  comm_mom12[i-12] <- avg_winners - avg_losers
  
  temp_sorted <- sort(apply(comm[(i-12):(i-2),],2,sum))
  avg_losers <- mean(temp_sorted[1:3])
  avg_winners <- mean(temp_sorted[(length(temp_sorted)-4):(length(temp_sorted)-1)])
  comm_mom212[i-12] <- avg_winners - avg_losers
}

#ii)
a1 <- coefficients(lm(comm_mom - rf ~ ff[523:1073,]$`Mkt-RF` + ff[523:1073,]$SMB +ff[523:1073,]$HML))[1]
a2 <- coefficients(lm(comm_mom12-rf ~ ff[534:1073,]$`Mkt-RF` + ff[534:1073,]$SMB +ff[534:1073,]$HML))[1]
a3 <- coefficients(lm(comm_mom212-rf ~ ff[534:1073,]$`Mkt-RF` + ff[534:1073,]$SMB +ff[534:1073,]$HML))[1]

a11 <- coefficients(lm(comm_mom - rf ~ ff[523:1073,]$`Mkt-RF` + ff[523:1073,]$SMB +ff[523:1073,]$HML +ff[523:1073,]$HML))[1]
a21 <- coefficients(lm(comm_mom12-rf ~ ff[534:1073,]$`Mkt-RF` + ff[534:1073,]$SMB +ff[534:1073,]$HML +ff[534:1073,]$HML))[1]
a31 <- coefficients(lm(comm_mom212-rf ~ ff[534:1073,]$`Mkt-RF` + ff[534:1073,]$SMB +ff[534:1073,]$HML +ff[534:1073,]$HML))[1]

#iii)
cor1 <- cor(mom[518:1068], comm_mom)
cor2 <- cor(mom12[518:1057], comm_mom12)
cor3 <- cor(mom212[518:1057], comm_mom212)

#iv)
model <- lm((mom12[523:1073]-rf) ~ (comm_mom-rf) + ff[523:1073,]$`Mkt-RF` + ff[523:1073,]$SMB +ff[523:1073,]$HML)
