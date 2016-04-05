library(readr)
library(MASS)
library(moments)

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
date <- list()

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
  date[[length(date)+1]] <- one_month[i,1]
}

all_info <- merge(one_month, one_year, by="Date")
all_info[,27] <- NULL
all_info[,27] <- NULL
all_info[,52] <- NULL
all_info[,52] <- NULL
all_info <- merge(all_info, five_years, by="Date")

all_info <- all_info[60:1015,]
one_v <- rep(1, 75)

test <- c()
for(i in 1:955){
  predicted_returns <- c(month_predicted_returns[[i]], year_predicted_returns[[i]], fy_predicted_returns[[i]])
  placeholder <- which.max(predicted_returns)
  placeholder2 <- which.min(predicted_returns)
  actual <- all_info[(i+1),(placeholder+1)] - all_info[(i+1),(placeholder2+1)]
  test <- c(test, actual)
}

mean_return <- mean(test) + mean_rf
sd_return <- sd(test)/sqrt(955)
sharpe <- mean(test) / sd(test)

#Do it with restrictions!
all_info <- merge(one_month, one_year, by="Date")
all_info[,27] <- NULL
all_info[,27] <- NULL
all_info[,52] <- NULL
all_info[,52] <- NULL
all_info <- merge(all_info, five_years, by="Date")

all_info <- all_info[60:1015,]

month_both2 <- lm(one_month[1:800,2:26] ~ one_month[1:800,27] + one_month[1:800,28])
year_both2 <- lm(one_year[200:1000,2:26] ~ one_year[200:1000,27] + one_year[200:1000,28])
five_years_both2 <- lm(five_years[100:900,2:26] ~ five_years[100:900,27] + five_years[100:900,28])

month_coef2 <- coef(month_both2)[2:3,]
year_coef2 <- coef(year_both2)[2:3,]
fy_coef2 <- coef(five_years_both2)[2:3,]

port_coef <- list()
for(i in 120:1074){
  if(i %% 2 == 1){
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
}

port_coef3 <- t(as.data.frame(port_coef))

mean_coef2 <- apply(port_coef3, 2, mean)
sd_coef2 <- apply(port_coef3, 2, sd)
sd_coef2 <- sd_coef2 / sqrt(955)
t_coef2 <- mean_coef2 / sd_coef2

month_predicted_returns2 <- list()
year_predicted_returns2 <- list()
fy_predicted_returns2 <- list()
date <- list()
  
for(i in 120:1074){
  if(i%%2 == 0){
    month_prediction <- one_month[(i-1), 2:26]*mean_coef2[1] + py_pm[(i-1),]*mean_coef2[2] + pf_pm[(i-1),]*mean_coef2[3]
    year_prediction <- one_year[(i-12), 2:26]*mean_coef2[1] + py_py[(i-12),]*mean_coef2[2] + pf_py[(i-12),]*mean_coef2[3]
    fy_prediction <- five_years[(i-60), 2:26]*mean_coef2[1] + py_pf[(i-60),]*mean_coef2[2] + pf_pf[(i-60),]*mean_coef2[3]
    if (one_month[i,27] == 1){
      month_prediction <- month_prediction + month_coef2[1,]
      year_prediction <- year_prediction + year_coef2[1,]
      fy_prediction <- fy_prediction + fy_coef2[1,]
    }
    else if(one_month[i,28] == 1){
      month_prediction <- month_prediction + month_coef2[2,]
      year_prediction <- year_prediction + year_coef2[2,]
      fy_prediction <- fy_prediction + fy_coef2[2,]
    }
    month_predicted_returns2[[length(month_predicted_returns2)+1]] <- month_prediction
    year_predicted_returns2[[length(year_predicted_returns2)+1]] <- year_prediction
    fy_predicted_returns2[[length(fy_predicted_returns2)+1]] <- fy_prediction
    date[[length(date)+1]] <- one_month[i,1]
  }
}

test2 <- c()
for(i in 1:955){
  if (i %% 2 == 0){
    predicted_returns <- c(month_predicted_returns2[[(i/2)]], year_predicted_returns2[[(i/2)]], fy_predicted_returns2[[(i/2)]])
    placeholder <- which.max(predicted_returns)
    placeholder2 <- which.min(predicted_returns)
    actual <- all_info[(i+1),(placeholder+1)] - all_info[(i+1),(placeholder2+1)]
    test2 <- c(test2, actual)
  }
}
mean_return2 <- mean(test2) + mean_rf
sd_return2 <- sd(test2)/sqrt(478)
sharpe2 <- mean(test2) / sd(test2)

#F
sd_norm <- (1+ mean(test)^2 / 2*sd(test)^2)/954
sd_general <- (1/954)*(1 + sharpe^2/4 * (kurtosis(test) - 1) - sharpe*skewness(test))

sd_norm2 <- (1+ mean(test2)^2 / 2*sd(test2)^2)/478
sd_general2 <- (1/478)*(1 + sharpe2^2/4 * (kurtosis(test2) - 1) - sharpe*skewness(test2))


conf_in_1 <- c(sharpe - sd_norm*1.96, sharpe + sd_norm*1.96)
conf_in_2 <- c(sharpe - sd_general*1.96, sharpe + sd_general*1.96)

conf_in_3 <- c(sharpe2 - sd_norm2*1.96, sharpe2 + sd_norm2*1.96)
conf_in_4 <- c(sharpe2 - sd_general2*1.96, sharpe2 + sd_general2*1.96)

#Question G
all_info <- all_info[2:956,]
time_series <- matrix(1:5000, ncol = 5000)
for(i in 1:955){
  nums <- sample(2:76, size = 5000, replace=T)
  random_returns <- as.matrix(all_info[i, nums])
  time_series <- rbind(time_series, unname(random_returns))
}

time_series <- time_series[2:954,]

gm_mean = function(a){prod(1+a, na.rm=T)^(1/length(a)) - 1}

mean_ts <- apply(time_series, 2, mean, na.rm=T)
mean_ts2 <- apply(time_series, 2, gm_mean)
sd_ts <- apply(time_series, 2, sd, na.rm=T)
se_ts <- sd_ts / sqrt(952)
t_ts <- mean_ts / se_ts
t_ts2 <- mean_ts2 / se_ts
sharpe_ts <- mean_ts / sd_ts
sharpe_ts2 <- mean_ts2 / sd_ts
