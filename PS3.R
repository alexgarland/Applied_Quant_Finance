library(readr)

setwd("~/Applied_Quant_Finance")
rm(list=ls())

#Question 1B
raw_market <- read_csv("PS3_Market.csv")
raw_industry <- read_csv("PS3_Industry_Returns.csv")