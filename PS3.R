library(readr)

setwd("~/Applied_Quant_Finance")
rm(list=ls())

#Question 1B
raw_market <- read_csv("PS3_Market.csv")
raw_market <- raw_market / 100
rf_return <- mean(raw_market$RF)
raw_industry <- read_csv("PS3_Industry_Returns.csv")
raw_industry <- raw_industry / 100

raw_industry[raw_industry <= -.99] <- NA
beta_vector = as.numeric()

for (i in 2:50){
  temp <- data.frame(Date = raw_industry$Date, industry = raw_industry[,i])
  merged_temp <- merge(temp, raw_market, by="Date")
  merged_temp <- subset(merged_temp, !is.na(merged_temp$industry))
  beta <- cov(merged_temp$industry, merged_temp$'Mkt-RF')/var(merged_temp$`Mkt-RF`)
  beta_vector <- c(beta_vector, beta)
}