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
  return(ind_return_vector - market_returns$RF)
}
excess_returns_arr <- apply(industry_returns, 2, excess_returns)
excess_returns_arr <- excess_returns_arr[,2:31]

model_list <- lm(excess_returns_arr ~ market_returns$`RM-RF`)
intercepts <- model_list$coefficients[1,]
