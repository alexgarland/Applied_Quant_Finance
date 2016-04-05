library(readr)
library(MASS)

setwd("~/Applied_Quant_Finance/Problem Sets")
rm(list=ls())
set.seed(1)

source('PS7_Helpers.R')

one_month <- read_csv("PS7_1Month.csv")
one_year <- read_csv("PS7_212Month.csv")
five_years <- read_csv("PS7_1360Month.csv")

one_month <- one_month / 100
one_year <- one_year / 100
five_years <- five_years / 100

#Part A
one_month$Jan <- 0
one_year$Jan <- 0
five_years$Jan <- 0

one_month$Dec <- 0
one_year$Dec <- 0
five_years$Dec <- 0

month_indicator <- one_month$Date %% 1
month_list <- (month_indicator - .01 < .001)
month_list <- which(month_list %in% TRUE)
dec_list <- ((month_indicator - .12 < .001) & (month_indicator - .12 > -.001))
dec_list <- which(dec_list %in% TRUE)

one_month$Jan[month_list] <- 1
one_year$Jan[month_list] <- 1
five_years$Jan[month_list] <- 1

one_month$Dec[dec_list] <- 1
one_year$Dec[dec_list] <- 1
five_years$Dec[dec_list] <- 1

one_month[one_month < -.99] <- NA
one_year[one_year < -.99] <- NA
five_years[five_years < -.99] <- NA

ff_factors <- read_csv("PS6_FF.csv") / 100
ff_factors[ff_factors <= -.99] <- NA
mean_rf <- mean(ff_factors$RF)

one_month[,2:26] <- one_month[,2:26] - mean_rf
one_year[,2:26] <- one_year[,2:26] - mean_rf
five_years[,2:26] <- five_years[,2:26] - mean_rf

one_year <- one_year[12:1074,] 
five_years <- five_years[60:1074,]

one_month <- as.matrix(one_month)
one_year <- as.matrix(one_year)
five_years <- as.matrix(five_years)

month_jan <- lm(one_month[,2:26] ~ one_month[,27])
year_jan <- lm(one_year[,2:26] ~ one_year[,27])
five_years_jan <- lm(five_years[,2:26] ~ five_years[,27])

#Part B
month_both <- lm(one_month[,2:26] ~ one_month[,27] + one_month[,28])
year_both <- lm(one_year[,2:26] ~ one_year[,27] + one_year[,28])
five_years_both <- lm(five_years[,2:26] ~ five_years[,27] + five_years[,28])

#Part D
py_pm <- one_month[,2:26]
pf_pm <- one_month[,2:26]

py_py <- one_year[,2:26]
pf_py <- one_year[,2:26]

py_pf <- five_years[,2:26]
pf_pf <- five_years[,2:26]

for(i in 1:25){
  py_pm[,i] <- collapse_212(py_pm[,i])
  pf_pm[,i] <- collapse_1360(pf_pm[,i])
  
  py_py[,i] <- collapse_212(py_py[,i])
  pf_py[,i] <- collapse_1360(pf_py[,i])
  
  py_pf[,i] <- collapse_212(py_pf[,i])
  pf_pf[,i] <- collapse_1360(pf_pf[,i])
}

port_coef <- list()
for(i in 120:1074){
  reg_df <- data.frame(Current = as.numeric(), Past_Month = as.numeric(), 
                       Past_Year = as.numeric(), Past_Five = as.numeric())
  
  temp <- data.frame(Current = one_month[i,2:26], Past_Month = one_month[(i-1), 2:26], 
                       Past_Year = py_pm[(i-1),], Past_Five = pf_pm[(i-1),])
    
  temp2 <- data.frame(Current = one_year[(i-11), 2:26], Past_Month = one_year[(i-12), 2:26], 
                        Past_Year = py_py[(i-12),], Past_Five = pf_py[(i-12),])
    
  temp3 <- data.frame(Current = five_years[(i-59), 2:26], Past_Month = five_years[(i-60), 2:26], 
                        Past_Year = py_pf[(i-60),], Past_Five =pf_pf[(i-60),])
  
  reg_df <- rbind(reg_df, temp, temp2, temp3)
  model <- lm(reg_df$Current ~ reg_df$Past_Month + reg_df$Past_Year + reg_df$Past_Five)
  betas <- coef(model)[2:4]
  port_coef[[length(port_coef)+1]] <- betas
}

port_coef2 <- t(as.data.frame(port_coef))

mean_coef <- apply(port_coef2, 2, mean)
sd_coef <- apply(port_coef2, 2, sd)
sd_coef <- sd_coef / sqrt(955)
t_coef <- mean_coef / sd_coef

#Part E
#We want to do a conditional rebalancing each month
#So let's start with prediction of each portfolios conditional mean

month_coef <- coef(month_both)[2:3,]
year_coef <- coef(year_both)[2:3,]
fy_coef <- coef(five_years_both)[2:3,]

month_predicted_returns <- list()
year_predicted_returns <- list()
fy_predicted_returns <- list()

for(i in 120:1074){
  month_prediction <- one_month[(i-1), 2:26]*mean_coef[1] + py_pm[(i-1),]*mean_coef[2] + pf_pm[(i-1),]*mean_coef[3]
  year_prediction <- one_year[(i-12), 2:26]*mean_coef[1] + py_py[(i-12),]*mean_coef[2] + pf_py[(i-12),]*mean_coef[3]
  fy_prediction <- five_years[(i-60), 2:26]*mean_coef[1] + py_pf[(i-60),]*mean_coef[2] + pf_pf[(i-60),]*mean_coef[3]
  if (one_month[i,27] == 1){
    month_prediction <- month_prediction + month_coef[1,]
    year_prediction <- year_prediction + year_coef[1,]
    fy_prediction <- fy_prediction + fy_coef[1,]
  }
  else if(one_month[i,28] == 1){
    month_prediction <- month_prediction + month_coef[2,]
    year_prediction <- year_prediction + year_coef[2,]
    fy_prediction <- fy_prediction + fy_coef[2,]
  }
  month_predicted_returns[[length(month_predicted_returns)+1]] <- month_prediction
  year_predicted_returns[[length(year_predicted_returns)+1]] <- year_prediction
  fy_predicted_returns[[length(fy_predicted_returns)+1]] <- fy_prediction
}

#Calculate conditional volatility
conditionalvol <- list()
error <- list()
for(i in 120:1074){
  actual_returns <- c(one_month[i,2:26], one_year[(i-11),2:26], five_years[(i-59),2:26])
  predicted <- c(month_predicted_returns[[(i-119)]], year_predicted_returns[[(i-119)]], fy_predicted_returns[[(i-119)]])
  error[[length(error)+1]] <- (actual_returns - predicted)^2
}

error_df <- data.frame(current = as.numeric(), past = as.numeric())
for(i in 2:955){
  temp <- data.frame(current = error[[i]], past = error[[(i-1)]])
  error_df <- rbind(error_df, temp)
}

arch_coef <- list()
arch_coef[[1]] <- c(1,1)
for(i in 1:954){
  holder <- lm(error_df$current[1:(75*i)] ~ error_df$past[1:(75*i)])
  holder <- coef(holder)
  arch_coef[[(i + 1)]] <- holder
}

for(i in 1:955){
  conditionalvol[[i]] <- arch_coef[[i]][1] + arch_coef[[i]][2]* error[[i]]
}

weights <- list()
weights[[1]] <- rep(1, 75)/75
for(i in 2:955){
  predicted_means <- c(month_predicted_returns[[i]], year_predicted_returns[[i]], fy_predicted_returns[[i]])
  predicted_vol <- conditionalvol[[(i-1)]]
  
}

#Nah, let's use known covariance matrices
