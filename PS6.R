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

momentum_payoff_longer <- mean_best_212 - mean_worst_212

for (i in 13:1069){
  mean_best_212 <- c(mean_best_212, mean(industry[i, best_performers_212[(i-12),]]))
  mean_worst_212 <- c(mean_worst_212, mean(industry[i, worst_performers_212[(i-12),]]))
}

m_best_212 <- mean(mean_best_212)
m_worst_212 <- mean(mean_worst_212)

std_best_212 <- sd(mean_best_212) / sqrt(1057)
std_worst_212 <- sd(mean_worst_212) / sqrt(1057)

t_best_212 <- m_best_212/ std_best_212
t_worst_212 <- m_worst_212/ std_worst_212

sr_best_212 <- t_best_212 / sqrt(1057)
sr_worst_212 <- t_worst_212 / sqrt(1057)

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
