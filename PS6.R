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

ff_factors <- read_csv("PS6_FF.csv") / 100
market_auto = cov(ff_factors$`Mkt-RF`[2:1073], ff_factors$`Mkt-RF`[1:1072])
cross_variance <- var(apply(industry[,2:31], 2, mean))
market_model <- lm(industry[,2:31] ~ ff_factors$`Mkt-RF`[1:1069])
betas <- coef(market_model)[2,]
var_beta <- var(betas)
