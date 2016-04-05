library(readr)
library(MASS)

setwd("~/Applied_Quant_Finance/Problem Sets")
rm(list=ls())

source('PS7_Helpers.R')

one_month <- read_csv("PS7_1Month.csv")
one_year <- read_csv("PS7_212Month.csv")
five_years <- read_csv("PS7_1360Month.csv")

one_month <- one_month / 100
one_year <- one_year / 100
five_years <- five_years / 100

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

one_month <- one_month - mean_rf
one_year <- one_year - mean_rf
five_years <- five_years - mean_rf

one_month$Date <- one_month$Date + mean_rf
one_year$Date <- one_year$Date + mean_rf
five_years$Date <- five_years$Date + mean_rf

one_year <- one_year[12:1074,] 
five_years <- five_years[60:1074,]

one_month <- as.matrix(one_month)
one_year <- as.matrix(one_year)
five_years <- as.matrix(five_years)

month_jan <- lm(one_month[,2:26] ~ one_month[,27])
year_jan <- lm(one_year[,2:26] ~ one_year[,27])
five_years_jan <- lm(five_years[,2:26] ~ five_years[,27])

month_both <- lm(one_month[,2:26] ~ one_month[,27] + one_month[,28])
year_both <- lm(one_year[,2:26] ~ one_year[,27] + one_year[,28])
five_years_both <- lm(five_years[,2:26] ~ five_years[,27] + five_years[,28])

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
