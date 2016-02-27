library(readr)
library(ggplot2)
library(MASS)

setwd("~/Applied_Quant_Finance")
rm(list=ls())

beme_portfolio <- read_csv("PS3_Custom_Returns.csv")
beme_portfolio <- beme_portfolio / 100
beme_portfolio[beme_portfolio <= -.99] <- NA

beme_portfolio_size <- read_csv("PS3_Custom_Size.csv")
beme_portfolio_beme <- read_csv("PS3_Custom_BEME.csv")
beme_portfolio_size[beme_portfolio_size <= -.99] <- NA
beme_portfolio_beme[beme_portfolio_beme <= -.99] <- NA

expanded_beme <- portfolio_beme[1,]
for (i in 1:90){
  for (j in 1:12){
    expanded_beme <- rbind(expanded_beme, portfolio_beme[i,])
  }
}