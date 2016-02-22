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

mean_rf <- mean(market_returns$RF)
mean_industry <- apply(industry_returns, 2, mean)
sd_industry <- apply(industry_returns, 2, sd)
mean_industry <- mean_industry[2:31]
sd_industry <- sd_industry[2:31]
sharpe_industry <- (mean_industry - mean_rf) / sd_industry

#Question 1B
excess_returns <- function(ind_return_vector){
  if (length(ind_return_vector) == 1069){
    return(ind_return_vector - market_returns$RF[1:1069])
  }
  else if(length(ind_return_vector) == 1073){
    return(ind_return_vector - market_returns$RF) 
  }
  else{
    return(ind_return_vector - market_returns$RF[7:1069])
  }
}

excess_returns_arr <- apply(industry_returns, 2, excess_returns)
excess_returns_arr <- excess_returns_arr[,2:31]

model_list <- lm(excess_returns_arr ~ market_returns$`RM-RF`[1:1069])
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

mean_pastperf <- apply(past_return_port, 2, mean)[2:11]
sd_pastperf <- apply(past_return_port, 2, sd)[2:11]

sharpe_pastperf <- (mean_pastperf - mean_rf) / sd_pastperf

excess_returns_arr2 <- apply(past_return_port, 2, excess_returns)
excess_returns_arr2 <- excess_returns_arr2[,2:11]

model_list2 <- lm(excess_returns_arr2 ~ market_returns$`RM-RF`[7:1069])
intercepts2 <- model_list2$coefficients[1,]

vcov_mat2 <- cov(past_return_port[2:11])
v_inv2 <- ginv(vcov_mat2)

test_stat2 <- (t(intercepts2) %*% v_inv2 %*% intercepts2) / (1+market_sharpe^2)
obs <- 1063
N <- 10
F_stat2 <- obs * (obs - N - 1) * test_stat2 / (N * (obs - 2))
p_val2 <- pf(F_stat2, N, obs-N-1, 0, lower.tail = FALSE)

#Question 3f
beme_port <- read_csv("PS4_SizeBEME.csv")
beme_port <- beme_port / 100

mean_beme <- apply(beme_port, 2, mean)[2:26]
sd_beme <- apply(beme_port, 2, sd)[2:26]

sharpe_beme <- (mean_beme - mean_rf) / sd_beme

excess_returns_arr3 <- apply(beme_port, 2, excess_returns)
excess_returns_arr3 <- excess_returns_arr3[,2:26]

model_list3 <- lm(excess_returns_arr3 ~ market_returns$`RM-RF`)
intercepts3 <- model_list3$coefficients[1,]

vcov_mat3 <- cov(beme_port[2:26])
v_inv3 <- ginv(vcov_mat3)

test_stat3 <- (t(intercepts3) %*% v_inv3 %*% intercepts3) / (1+market_sharpe^2)
obs <- 1073
N <- 25
F_stat3 <- obs * (obs - N - 1) * test_stat3 / (N * (obs - 2))
p_val3 <- pf(F_stat3, N, obs-N-1, 0, lower.tail = FALSE)