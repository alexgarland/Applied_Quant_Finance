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
FF_Factors$`Recession indicator (1=recession, 0=non-recesstion)` <- 100*FF_Factors$`Recession indicator (1=recession, 0=non-recesstion)`
FF_Factors[FF_Factors <= -.99] <- NA

#Expand the BEME out from an annualized to a monthly stat
expanded_beme <- beme_portfolio_beme[1,]
for (i in 1:90){
  for (j in 1:12){
    expanded_beme <- rbind(expanded_beme, beme_portfolio_beme[i,])
  }
}

#Question A
reg_mkt <- lm(FF_Factors$`Mkt-RF`~ FF_Factors$`Recession indicator (1=recession, 0=non-recesstion)`)
reg_smb <- lm(FF_Factors$SMB~ FF_Factors$`Recession indicator (1=recession, 0=non-recesstion)`)
reg_hml <- lm(FF_Factors$HML ~ FF_Factors$`Recession indicator (1=recession, 0=non-recesstion)`)
reg_rf <- lm(FF_Factors$RF ~ FF_Factors$`Recession indicator (1=recession, 0=non-recesstion)`)
reg_umd <- lm(FF_Factors$UMD[7:1073] ~ FF_Factors$`Recession indicator (1=recession, 0=non-recesstion)`[7:1073])

#Question B
mean_rf <- mean(FF_Factors$`Mkt-RF`)

beme_portfolio <- beme_portfolio - mean_rf
momentum_portfolio <- momentum_portfolio - mean_rf
beme_portfolio$Date <- beme_portfolio$Date + mean_rf
momentum_portfolio$`Size and Ret212` <- momentum_portfolio$`Size and Ret212` + mean_rf

reg_small_value <- lm(beme_portfolio$`Small Size High BE/ME` ~ FF_Factors$`Recession indicator (1=recession, 0=non-recesstion)`)
reg_small_growth <- lm(beme_portfolio$`Small Size Low BE/ME` ~ FF_Factors$`Recession indicator (1=recession, 0=non-recesstion)`)
reg_big_growth <- lm(beme_portfolio$`Big Size Low BE/ME` ~ FF_Factors$`Recession indicator (1=recession, 0=non-recesstion)`)
reg_big_value  <- lm(beme_portfolio$`Big Size High BE/ME` ~ FF_Factors$`Recession indicator (1=recession, 0=non-recesstion)`)

reg_small_losers <- lm(momentum_portfolio$`Small Size  & Low Ret212`[7:1069] ~ FF_Factors$`Recession indicator (1=recession, 0=non-recesstion)`[7:1069])
reg_small_winners <- lm(momentum_portfolio$`Small Size  & High Ret212`[7:1069] ~ FF_Factors$`Recession indicator (1=recession, 0=non-recesstion)`[7:1069])
reg_big_losers <- lm(momentum_portfolio$`Big Size  & Low Ret212`[7:1069] ~ FF_Factors$`Recession indicator (1=recession, 0=non-recesstion)`[7:1069])
reg_big_winners <- lm(momentum_portfolio$`Big Size  & High Ret212`[7:1069] ~ FF_Factors$`Recession indicator (1=recession, 0=non-recesstion)`[7:1069])
