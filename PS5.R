library(readr)
library(ggplot2)
library(MASS)

setwd("~/Applied_Quant_Finance")
rm(list=ls())

#Read in all of the relevant portfolios
beme_portfolio <- read_csv("PS3_Custom_Returns.csv")
beme_portfolio <- beme_portfolio / 100
beme_portfolio[beme_portfolio <= -.99] <- NA

beme_portfolio_size <- read_csv("PS3_Custom_Size.csv")
beme_portfolio_beme <- read_csv("PS3_Custom_BEME.csv")
beme_portfolio_size[beme_portfolio_size <= -.99] <- NA
beme_portfolio_beme[beme_portfolio_beme <= -.99] <- NA

momentum_portfolio <- read_csv("PS5_Size_Momentum_Returns.csv")
momentum_portfolio <- momentum_portfolio / 100
momentum_portfolio[momentum_portfolio <= -.99] <- NA

momentum_portfolio_size <- read_csv("PS5_Size_Momentum_Returns.csv")
momentum_portfolio_size[momentum_portfolio_size <= -.99] <- NA

momentum_portfolio_momentum <- read_csv("PS5_Size_Momentum_Returns.csv")
momentum_portfolio_momentum <- momentum_portfolio_momentum / 100
momentum_portfolio_momentum[momentum_portfolio_momentum <= -.99] <- NA

FF_Factors <- read_csv("PS5_ff_factors.csv")
FF_Factors <- FF_Factors / 100

#Expand the BEME out from an annualized to a monthly stat
expanded_beme <- beme_portfolio_beme[1,]
for (i in 1:90){
  for (j in 1:12){
    expanded_beme <- rbind(expanded_beme, beme_portfolio_beme[i,])
  }
}