library(readr)
library(ggplot2)
library(MASS)

setwd("~/Applied_Quant_Finance")
rm(list=ls())

#Question 1A
industry_returns <- read_csv("PS4_Industry.csv")
market_returns <- read_csv("PS4_Market.csv")

industry_returns <- industry_returns / 100
market_returns <- market_returns / 100
market_returns <- market_returns[1:1069,]

mean_rf <- mean(market_returns$RF)
mean_industry <- apply(industry_returns, 2, mean)
sd_industry <- apply(industry_returns, 2, sd)
mean_industry <- mean_industry[2:31]
sd_industry <- sd_industry[2:31]
sharpe_industry <- (mean_industry - mean_rf) / sd_industry

#Question 1B
excess_returns <- function(ind_return_vector){
  if (length(ind_return_vector) == 1069){
    return(ind_return_vector - market_returns$RF)
  }
  else{
    return(ind_return_vector - market_returns$RF[7:1069])
  }
}

excess_returns_arr <- apply(industry_returns, 2, excess_returns)
excess_returns_arr <- excess_returns_arr[,2:31]

model_list <- lm(excess_returns_arr ~ market_returns$`RM-RF`)
intercepts <- model_list$coefficients[1,]

vcov_mat <- cov(industry_returns[2:31])
v_inv <- ginv(vcov_mat)

market_sharpe <- mean(market_returns$`RM-RF`) / sd(market_returns$`RM-RF`)

test_stat <- (t(intercepts) %*% v_inv %*% intercepts) / (1+market_sharpe^2)
obs <- 1069
N <- 30
F_stat <- obs * (obs - N - 1) * test_stat / (N * (obs - 2))
p_val <- pf(F_stat, N, obs-N-1, 0, lower.tail = FALSE)

#Question 2e
past_return_port <- read_csv("PS4_PastPerformance.csv")
past_return_port <- past_return_port / 100

market_returns2 <- market_returns[7:1069,]
mean_pastperf <- apply(past_return_port, 2, mean)[2:11]
sd_pastperf <- apply(past_return_port, 2, sd)[2:11]

sharpe_pastperf <- (mean_pastperf - mean_rf) / sd_pastperf

excess_returns_arr2 <- apply(past_return_port, 2, excess_returns)
excess_returns_arr2 <- excess_returns_arr2[,2:11]
