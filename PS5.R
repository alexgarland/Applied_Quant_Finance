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
momentum_portfolio$Date <- momentum_portfolio$Date + mean_rf

reg_small_value <- lm(beme_portfolio$`Small Size High BE/ME` ~ FF_Factors$`Recession indicator (1=recession, 0=non-recesstion)`)
reg_small_growth <- lm(beme_portfolio$`Small Size Low BE/ME` ~ FF_Factors$`Recession indicator (1=recession, 0=non-recesstion)`)
reg_big_growth <- lm(beme_portfolio$`Big Size Low BE/ME` ~ FF_Factors$`Recession indicator (1=recession, 0=non-recesstion)`)
reg_big_value  <- lm(beme_portfolio$`Big Size High BE/ME` ~ FF_Factors$`Recession indicator (1=recession, 0=non-recesstion)`)

reg_small_losers <- lm(momentum_portfolio$`Small Size  & Low Ret212`[7:1069] ~ FF_Factors$`Recession indicator (1=recession, 0=non-recesstion)`[7:1069])
reg_small_winners <- lm(momentum_portfolio$`Small Size  & High Ret212`[7:1069] ~ FF_Factors$`Recession indicator (1=recession, 0=non-recesstion)`[7:1069])
reg_big_losers <- lm(momentum_portfolio$`Big Size  & Low Ret212`[7:1069] ~ FF_Factors$`Recession indicator (1=recession, 0=non-recesstion)`[7:1069])
reg_big_winners <- lm(momentum_portfolio$`Big Size  & High Ret212`[7:1069] ~ FF_Factors$`Recession indicator (1=recession, 0=non-recesstion)`[7:1069])

#Question C
beme_port_betas <- data.frame(Beta1 = as.numeric(), Beta2 = as.numeric(), Beta3 = as.numeric())

for (i in 2:26){
  temp <- data.frame(Date = beme_portfolio$Date, Port = beme_portfolio[,i])
  merged_temp <- merge(temp, FF_Factors, by="Date")
  model <- lm(merged_temp$Port ~ merged_temp$`Mkt-RF` + merged_temp$SMB + merged_temp$HML)
  df <- data.frame(Beta1 = coef(model)[2], Beta2 = coef(model)[3], Beta3 = coef(model)[4])
  beme_port_betas <- rbind(beme_port_betas, df)
}

beme_gammas1 <- data.frame(gamma_zero = as.numeric(), gamma_mkt = as.numeric(), 
                           gamma_size = as.numeric(), gamma_beme = as.numeric())

beme_gammas2 <- data.frame(gamma_zero = as.numeric(), gamma_mkt = as.numeric(), 
                           gamma_smb = as.numeric(), gamma_hml = as.numeric())

beme_gammas3 <- data.frame(gamma_zero = as.numeric(), gamma_mkt = as.numeric(),
                           gamma_smb = as.numeric(), gamma_hml = as.numeric(),
                           gamma_size = as.numeric(), gamma_beme = as.numeric())

expanded_beme <- expanded_beme[8:1080,2:26]
expanded_beme <- as.matrix(expanded_beme)
array_beme <- as.matrix(beme_portfolio[2:26])

for(i in 1:1073){
  #Estimate the gammas
  model1 <- lm(array_beme[i,] ~ beme_port_betas$Beta1 + as.vector(as.matrix(beme_portfolio_size[i,2:26])) + expanded_beme[i,])
  model2 <- lm(array_beme[i,] ~ beme_port_betas$Beta1 + beme_port_betas$Beta2 + beme_port_betas$Beta3)
  model3 <- lm(array_beme[i,] ~ beme_port_betas$Beta1 + as.vector(as.matrix(beme_portfolio_size[i,2:26])) + 
                 expanded_beme[i,] + beme_port_betas$Beta2 + beme_port_betas$Beta3)
  
  #Store them in the temporary dfs
  temp1 <- data.frame(gamma_zero = coef(model1)[1], gamma_mkt = coef(model1)[2], 
                      gamma_size = coef(model1)[3], gamma_beme = coef(model1)[4])
  
  temp2 <- data.frame(gamma_zero = coef(model2)[1], gamma_mkt = coef(model2)[2], 
                      gamma_smb = coef(model2)[3], gamma_hml = coef(model2)[4])
  
  temp3 <- data.frame(gamma_zero = coef(model3)[1], gamma_mkt = coef(model3)[2],
                      gamma_smb = coef(model3)[5], gamma_hml = coef(model3)[6],
                      gamma_size = coef(model3)[3], gamma_beme = coef(model3)[4])
  
  beme_gammas1 <- rbind(beme_gammas1, temp1)
  beme_gammas2 <- rbind(beme_gammas2, temp2)
  beme_gammas3 <- rbind(beme_gammas3, temp3)
}

avg_1 <- apply(beme_gammas1, 2, mean)
avg_2 <- apply(beme_gammas2, 2, mean)
avg_3 <- apply(beme_gammas3, 2, mean)

std_1 <- apply(beme_gammas1, 2, sd) / sqrt(1073)
std_2 <- apply(beme_gammas2, 2, sd) / sqrt(1073)
std_3 <- apply(beme_gammas3, 2, sd) / sqrt(1073)

t_stat1 <- avg_1 / std_1
t_stat2 <- avg_2 / std_2
t_stat3 <- avg_3 / std_3

#Question E
beme_gammas1_2 <- data.frame(gamma_zero = as.numeric(), gamma_mkt = as.numeric(), 
                           gamma_size = as.numeric(), gamma_beme = as.numeric())

beme_gammas2_2 <- data.frame(gamma_zero = as.numeric(), gamma_mkt = as.numeric(), 
                           gamma_smb = as.numeric(), gamma_hml = as.numeric())

beme_gammas3_2 <- data.frame(gamma_zero = as.numeric(), gamma_mkt = as.numeric(),
                           gamma_smb = as.numeric(), gamma_hml = as.numeric(),
                           gamma_size = as.numeric(), gamma_beme = as.numeric())

for(i in 439:1073){
  #Estimate the gammas
  model1 <- lm(array_beme[i,] ~ beme_port_betas$Beta1 + as.vector(as.matrix(beme_portfolio_size[i,2:26])) + expanded_beme[i,])
  model2 <- lm(array_beme[i,] ~ beme_port_betas$Beta1 + beme_port_betas$Beta2 + beme_port_betas$Beta3)
  model3 <- lm(array_beme[i,] ~ beme_port_betas$Beta1 + as.vector(as.matrix(beme_portfolio_size[i,2:26])) + 
                 expanded_beme[i,] + beme_port_betas$Beta2 + beme_port_betas$Beta3)
  
  #Store them in the temporary dfs
  temp1 <- data.frame(gamma_zero = coef(model1)[1], gamma_mkt = coef(model1)[2], 
                      gamma_size = coef(model1)[3], gamma_beme = coef(model1)[4])
  
  temp2 <- data.frame(gamma_zero = coef(model2)[1], gamma_mkt = coef(model2)[2], 
                      gamma_smb = coef(model2)[3], gamma_hml = coef(model2)[4])
  
  temp3 <- data.frame(gamma_zero = coef(model3)[1], gamma_mkt = coef(model3)[2],
                      gamma_smb = coef(model3)[5], gamma_hml = coef(model3)[6],
                      gamma_size = coef(model3)[3], gamma_beme = coef(model3)[4])
  
  beme_gammas1_2 <- rbind(beme_gammas1_2, temp1)
  beme_gammas2_2 <- rbind(beme_gammas2_2, temp2)
  beme_gammas3_2 <- rbind(beme_gammas3_2, temp3)
}

avg_1_2 <- apply(beme_gammas1_2, 2, mean)
avg_2_2 <- apply(beme_gammas2_2, 2, mean)
avg_3_2 <- apply(beme_gammas3_2, 2, mean)

std_1_2 <- apply(beme_gammas1_2, 2, sd) / sqrt(1073-439)
std_2_2 <- apply(beme_gammas2_2, 2, sd) / sqrt(1073-439)
std_3_2 <- apply(beme_gammas3_2, 2, sd) / sqrt(1073-439)

t_stat1_2 <- avg_1_2 / std_1_2
t_stat2_2 <- avg_2_2 / std_2_2
t_stat3_2 <- avg_3_2 / std_3_2