library(readr)

setwd("~/Applied_Quant_Finance")
rm(list=ls())
source('~/Applied_Quant_Finance/PS6_helper.R')
industry <- read_csv("PS6_Industry.csv")

industry <- as.matrix(industry/100)

#Question A
best_performers <- t(apply(industry[,2:31], 1, find_three_highest))
worst_performers <- t(apply(industry[,2:31], 1, find_three_lowest))

mean_best <- c()
mean_worst <- c()
for (i in 2:1069){
  mean_best <- c(mean_best, mean(industry[i, best_performers[(i-1),]]))
  mean_worst <- c(mean_worst, mean(industry[i, worst_performers[(i-1),]]))
}

momentum_payoff <- mean_best - mean_worst

#Question B- come back to look at this

ff_factors <- read_csv("PS6_FF.csv") / 100
ff_factors[ff_factors <= -.99] <- NA
market_auto = cov(ff_factors$`Mkt-RF`[2:1073], ff_factors$`Mkt-RF`[1:1072])
cross_variance <- var(apply(industry[,2:31], 2, mean))
market_model <- lm(industry[,2:31] ~ ff_factors$`Mkt-RF`[1:1069])
betas <- coef(market_model)[2,]
var_beta <- var(betas)
resid <- residuals(market_model)
cross_ac <- mean(apply(resid, 2, return_acov))
predicted_mom <- cross_variance + var_beta*market_auto + cross_ac

#Question C
past_twelve_returns <- apply(industry, 2, collapse_year)
past_twelve_returns[,1] <- industry[,1]

best_performers_212 <- t(apply(past_twelve_returns[12:1069,2:31], 1, find_three_highest))
worst_performers_212 <- t(apply(past_twelve_returns[12:1069,2:31], 1, find_three_lowest))

mean_best_212 <- c()
mean_worst_212 <- c()

for (i in 13:1069){
  mean_best_212 <- c(mean_best_212, mean(industry[i, best_performers_212[(i-12),]]))
  mean_worst_212 <- c(mean_worst_212, mean(industry[i, worst_performers_212[(i-12),]]))
}

momentum_payoff_longer <- mean_best_212 - mean_worst_212

#Question D
past_212 <- apply(industry, 2, collapse_212)
past_212[,1] <- industry[,1]

best_performers_d <- t(apply(past_212[12:1069,2:31], 1, find_three_highest))
worst_performers_d <- t(apply(past_212[12:1069,2:31], 1, find_three_lowest))

mean_best_d <- c()
mean_worst_d <- c()

for (i in 13:1069){
  mean_best_d <- c(mean_best_d, mean(industry[i, best_performers_d[(i-12),]]))
  mean_worst_d <- c(mean_worst_d, mean(industry[i, worst_performers_d[(i-12),]]))
}

momentum_payoff_d <- mean_best_d - mean_worst_d

#Question E

mean_rf <- mean(ff_factors$RF)

industry_1 <- lm((momentum_payoff - mean_rf) ~ ff_factors$`Mkt-RF`[2:1069] + 
                   ff_factors$SMB[2:1069] + ff_factors$HML[2:1069])
industry_12 <- lm((momentum_payoff_longer - mean_rf) ~ ff_factors$`Mkt-RF`[13:1069] + 
                    ff_factors$SMB[13:1069] + ff_factors$HML[13:1069])
industry_212 <- lm((momentum_payoff_d - mean_rf) ~ ff_factors$`Mkt-RF`[13:1069] + 
                     ff_factors$SMB[13:1069] + ff_factors$HML[13:1069])

#Question F

industry_1_UMD <- lm((momentum_payoff - mean_rf) ~ ff_factors$`Mkt-RF`[2:1069] + 
                   ff_factors$SMB[2:1069] + ff_factors$HML[2:1069] + ff_factors$UMD[2:1069])

industry_12_UMD <- lm((momentum_payoff_longer - mean_rf) ~ ff_factors$`Mkt-RF`[13:1069] + 
                    ff_factors$SMB[13:1069] + ff_factors$HML[13:1069] + ff_factors$UMD[13:1069])

industry_212_UMD <- lm((momentum_payoff_d - mean_rf) ~ ff_factors$`Mkt-RF`[13:1069] + 
                     ff_factors$SMB[13:1069] + ff_factors$HML[13:1069] + ff_factors$UMD[13:1069])

#Question G
commodities <- read.csv("PS6_Commodity.csv")
commodities$time <- as.Date(commodities$time, "%m/%d/%Y")

best_commod_short <- t(apply(commodities[,2:33], 1, find_three_highest))
worst_commod_short <- t(apply(commodities[,2:33], 1, find_three_lowest))

mean_comm_short_good <- c()
mean_comm_short_bad <- c()
for (i in 2:552){
  mean_comm_short_good <- c(mean_comm_short_good, mean(as.numeric(commodities[i, best_commod_short[(i-1), ]], na.rm = T)))
  mean_comm_short_bad <- c(mean_comm_short_bad, mean(as.numeric(commodities[i, worst_commod_short[(i-1), ]], na.rm = T)))
}

momentum_comm_1 <- mean_comm_short_good - mean_comm_short_bad

past_twelve_comm <- apply(commodities[2:33], 2, collapse_year)

best_12_comm <- t(apply(past_twelve_comm[12:552,], 1, find_three_highest))
worst_12_comm <- t(apply(past_twelve_comm[12:552,], 1, find_three_lowest))

past_212_comm <- apply(commodities[2:33], 2, collapse_212)

best_212_comm <- t(apply(past_212_comm[12:552,], 1, find_three_highest))
worst_212_comm <- t(apply(past_212_comm[12:552,], 1, find_three_lowest))

mean_comm_12 <- c()
mean_comm_212 <- c()
for (i in 13:552){
  holder1 <- mean(as.numeric(commodities[i, best_12_comm[(i-12), ]], na.rm = T)) - mean(as.numeric(commodities[i, worst_12_comm[(i-12), ]], na.rm = T))
  holder2 <- mean(as.numeric(commodities[i, best_212_comm[(i-12), ]], na.rm = T)) - mean(as.numeric(commodities[i, worst_212_comm[(i-12), ]], na.rm = T))
  mean_comm_12 <- c(mean_comm_12, holder1)
  mean_comm_212 <- c(mean_comm_212, holder2)
}

comm_1 <- lm((momentum_comm_1 - mean_rf)[1:551] ~ ff_factors$`Mkt-RF`[523:1073] + 
               ff_factors$SMB[523:1073] + ff_factors$HML[523:1073])

comm_2 <- lm((mean_comm_12 - mean_rf)[1:539] ~ ff_factors$`Mkt-RF`[535:1073] + 
               ff_factors$SMB[535:1073] + ff_factors$HML[535:1073])

comm_3 <- lm((mean_comm_212- mean_rf)[1:539] ~ ff_factors$`Mkt-RF`[535:1073] + 
               ff_factors$SMB[535:1073] + ff_factors$HML[535:1073])


comm_1_UMD <- lm((momentum_comm_1 - mean_rf)[1:551] ~ ff_factors$`Mkt-RF`[523:1073] + 
                   ff_factors$SMB[523:1073] + ff_factors$HML[523:1073] + ff_factors$UMD[523:1073])

comm_2_UMD <- lm((mean_comm_12- mean_rf)[1:539] ~ ff_factors$`Mkt-RF`[535:1073] + 
                   ff_factors$SMB[535:1073] + ff_factors$HML[535:1073] + ff_factors$UMD[535:1073])

comm_3_UMD <- lm((mean_comm_212- mean_rf)[1:539] ~ ff_factors$`Mkt-RF`[535:1073] + 
                ff_factors$SMB[535:1073] + ff_factors$HML[535:1073] + ff_factors$UMD[535:1073])

corr_short <- cor(momentum_payoff[522:1068], momentum_comm_1[1:547])
corr_12 <- cor(momentum_payoff_longer[534:1069], mean_comm_12[1:536], use="complete")
corr_212 <- cor(momentum_payoff_d[534:1069], mean_comm_212[1:536], use="complete")
