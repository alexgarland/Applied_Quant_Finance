library(readr)
library(MASS)

setwd("~/Applied_Quant_Finance/Problem Sets")
rm(list=ls())

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

all_info <- merge(one_month, one_year, by = "Date")
all_info <- merge(all_info, five_years, by = "Date")

port_coef <- list()

for (i in 2:1015){
  for (j in 2:26){
    model <- tryCatch({
      lm(all_info[i,j] ~ all_info[(i-1),j] + 
                           all_info[(i-1),(j+27)] + all_info[(i-1),(j+54)])}, 
      error = function(e){ return(NA) })
    if (!is.na(model)){
      
    }
  }
  for (j in 29:53){
    model <- tryCatch({
      lm(all_info[i,j] ~ all_info[(i-1),(j-27)] + 
           all_info[(i-1),(j)] + all_info[(i-1),(j+27)])},
      error = function(e){  return(NA)  })
  }
  for (j in 56:80){
    model <- tryCatch({
      lm(all_info[i,j] ~ all_info[(i-1),(j-54)] + 
           all_info[(i-1),(j-27)] + all_info[(i-1),(j)])},
      error = function(e){  return(NA)  })
  }
}

for (i in 2:1015){
  
}