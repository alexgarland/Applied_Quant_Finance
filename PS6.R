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

m_best <- mean(mean_best)
m_worst <- mean(mean_worst)

std_best <- sd(mean_best) / sqrt(1068)
std_worst <- sd(mean_worst) / sqrt(1068)

t_best <- m_best/ std_best
t_worst <- m_worst/ std_worst

sr_best <- t_best / sqrt(1068)
sr_worst <- t_worst / sqrt(1068)


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