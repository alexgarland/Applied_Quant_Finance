library(readr)
library(moments)
setwd("~/Applied_Quant_Finance")
rm(list=ls())

raw = read_csv("Prob_Set_1.csv")
data_mat = as.matrix(raw[2:51])

#Question 1a
first <- seq(1,50, 1)
second <- seq(1,50, 1)
third <- seq(1,50, 1)
fourth <- seq(1,50, 1)

first <- ifelse(first < 6, .2, 0)
second <- ifelse(second < 11, .1, 0)
third <- ifelse(third < 26, .04, 0)
fourth <- ifelse(fourth < 51, .02, 0)

returns_function1 <- function(x) {returns_arr = 1 + x/100}
returns <- as.matrix(apply(data_mat, 2, returns_function1))
returns2 <- as.list(as.data.frame(returns))
returns_vect <- sapply(returns2, prod)
returns_vect_month <- (returns_vect)^(1/179)
returns_vect_month <- returns_vect_month - 1
weighted_returns_first <- first %*% returns_vect_month
weighted_returns_second <- second %*% returns_vect_month
weighted_returns_third <- third %*% returns_vect_month
weighted_returns_fourth <- fourth %*% returns_vect_month

vcov_mat <- cov(data_mat/100)
st_dev_1 <- (t(first) %*% vcov_mat %*% first)^(1/2)
st_dev_2 <- (t(second) %*% vcov_mat %*% second)^(1/2)
st_dev_3 <- (t(third) %*% vcov_mat %*% third)^(1/2)
st_dev_4 <- (t(fourth) %*% vcov_mat %*% fourth)^(1/2)

st_dev_v = c(st_dev_1, st_dev_2, st_dev_3, st_dev_4)
plot(st_dev_v)

#Question 1b
var_only_mat <- vcov_mat
var_only_mat[upper.tri(var_only_mat) | lower.tri(var_only_mat)] = 0
var_o_1 <- (t(first) %*% var_only_mat %*% first) / (st_dev_1)^2
var_o_2 <- (t(second) %*% var_only_mat %*% second) / (st_dev_2)^2
var_o_3 <- (t(third) %*% var_only_mat %*% third) / (st_dev_3)^2
var_o_4 <- (t(fourth) %*% var_only_mat %*% fourth) / (st_dev_4)^2

#Question 1c

#Question 1d
zscore1 <- weighted_returns_first / (st_dev_1 / sqrt(180))
zscore2 <- weighted_returns_second / (st_dev_2 / sqrt(180))
zscore3 <- weighted_returns_third / (st_dev_3 / sqrt(180))
zscore4 <- weighted_returns_fourth / (st_dev_4 / sqrt(180))

#Question 1e
first_stock <- as.vector(raw$CTL/100)
prob <- shapiro.test(first_stock)
fs_dev <- sd(first_stock)
fs_range <- (max(first_stock) - min(first_stock))/fs_dev
fs_skew <- skewness(first_stock)
fs_kurt <- kurtosis(first_stock)

e_w_sr <- (returns-1) %*% fourth
ew_dev = sd(e_w_sr)
prob_ew <- shapiro.test(e_w_sr)
ew_range <- (max(e_w_sr) - min(e_w_sr))/ew_dev
ew_skew <- skewness(e_w_sr)
ew_kurt <- kurtosis(e_w_sr)

prob_market <- shapiro.test(raw$`Market (Value Weighted Index)`/100)
market <- raw$`Market (Value Weighted Index)`/100
market_dev <- sd(market)
market_range <- (max(market) - min(market))/market_dev
market_skew <- skewness(market)
market_kurt <- kurtosis(market)

#Question 1f
first_ten_stocks <- as.matrix(data_mat[,1:10])
model <- lm(first_ten_stocks ~ raw$`Market (Value Weighted Index)`)

sd_csco_all <- c()
sd_market_all <- c()
sd_csco_year <- c()
sd_market_year <- c()

#Question 2a
for (i in 1:180){
  sd_csco_all <- c(sd_csco_all, sd(raw$CSCO[1:i]))
  sd_market_all <- c(sd_market_all, sd(raw$`Market (Value Weighted Index)`[1:i]))
  if (i > 12){
    sd_csco_year <- c(sd_csco_year, sd(raw$CSCO[(i-11):i]))
    sd_market_year <- c(sd_market_year, sd(raw$`Market (Value Weighted Index)`[(i-11):i]))
  }
  else{
    sd_csco_year <- c(sd_csco_year, NA)
    sd_market_year <- c(sd_market_year, NA)
  }
}
plot(sd_csco_all, type = "l", col = "red")
lines(sd_csco_year, col = "blue")

plot(sd_market_all, type = "l", col = "black")
lines(sd_market_year, type = "l", col = "green")

#Question 2b
df_all <- data.frame(index = integer(), coeff = numeric(), top = numeric(), bottom = numeric())
df_year <- data.frame(index = integer(), coeff = numeric(), top = numeric(), bottom = numeric())

for (i in 2:180){
  mod_all <- lm(raw$CSCO[1:i] ~ raw$`Market (Value Weighted Index)`[1:i])
  st_err_all <- coef(summary(mod_all))[2,2]
  coeff_all <- coef(summary(mod_all))[2,1]
  df_temp_1 <- data.frame(index = i, coeff = coeff_all, top = coeff_all + 1.96*st_err_all, bottom = coeff_all - 1.96*st_err_all)
  df_all <- rbind(df_all, df_temp_1)
  if (i > 12){
    mod_year <- lm(raw$CSCO[(i-11):i] ~ raw$`Market (Value Weighted Index)`[(i-11):i])
    st_err_y <- coef(summary(mod_year))[2,2]
    coeff_y <- coef(summary(mod_year))[2,1]
    df_temp_2 <- data.frame(index = i, coeff = coeff_y, top = coeff_y + 1.96*st_err_y, bottom = coeff_y - 1.96*st_err_y)
    df_year <- rbind(df_year, df_temp_2)
  }
}

plot(y = df_year$coeff, x = df_year$index, type = "l", col = "red")
lines(y = df_year$top, x = df_year$index, type = "l", col = "green")
lines(y = df_year$bottom, x = df_year$index, type = "l", col = "green")

lines(y = df_all$coeff, x = df_all$index, type = "l", col = "black")
lines(y = df_all$top, x = df_all$index, type = "l", col = "blue")
lines(y = df_all$bottom, x = df_all$index, type = "l", col = "blue")

change_in_beta <- numeric()
for (i in 13:179){
  if (df_all$coeff[i] < df_year$bottom[(i-11)] || df_all$coeff[i] > df_year$top[(i-11)]){
    change_in_beta <- c(change_in_beta,1)
  }
  else{
    change_in_beta <- c(change_in_beta,0)
  }
}
total <- sum(change_in_beta)
percent_outside_error <- total / length(change_in_beta)