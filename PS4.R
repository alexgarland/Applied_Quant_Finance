library(readr)
library(ggplot2)
library(MASS)

setwd("~/Applied_Quant_Finance")
rm(list=ls())

#Question 1A
industry_returns <- read_csv("PS4_Industry.csv")
market_returns <- read_csv("PS4_Market.csv")

industry_returns <- industry_returns / 100
market_returns <- market_returns / 100

mean_rf <- mean(market_returns$RF)
mean_industry <- apply(industry_returns, 2, mean)
sd_industry <- apply(industry_returns, 2, sd)
mean_industry <- mean_industry[2:31]
sd_industry <- sd_industry[2:31]
sharpe_industry <- (mean_industry - mean_rf) / sd_industry

#Question 1B
excess_returns <- function(ind_return_vector){
  if (length(ind_return_vector) == 1069){
    return(ind_return_vector - market_returns$RF[1:1069])
  }
  else if(length(ind_return_vector) == 1073){
    return(ind_return_vector - market_returns$RF) 
  }
  else{
    return(ind_return_vector - market_returns$RF[7:1069])
  }
}

excess_returns_arr <- apply(industry_returns, 2, excess_returns)
excess_returns_arr <- excess_returns_arr[,2:31]

model_list <- lm(excess_returns_arr ~ market_returns$`RM-RF`[1:1069])
intercepts <- model_list$coefficients[1,]

vcov_mat <- cov(industry_returns[2:31])
v_inv <- ginv(vcov_mat)

market_sharpe <- mean(market_returns$`RM-RF`) / sd(market_returns$`RM-RF`)

test_stat <- (t(intercepts) %*% v_inv %*% intercepts) / (1+market_sharpe^2)
obs <- 1069
N <- 30
F_stat <- obs * (obs - N - 1) * test_stat / (N * (obs - 2))
p_val <- pf(F_stat, N, obs-N-1, 0, lower.tail = FALSE)

#Question 2e
past_return_port <- read_csv("PS4_PastPerformance.csv")
past_return_port <- past_return_port / 100

mean_pastperf <- apply(past_return_port, 2, mean)[2:11]
sd_pastperf <- apply(past_return_port, 2, sd)[2:11]

sharpe_pastperf <- (mean_pastperf - mean_rf) / sd_pastperf

excess_returns_arr2 <- apply(past_return_port, 2, excess_returns)
excess_returns_arr2 <- excess_returns_arr2[,2:11]

model_list2 <- lm(excess_returns_arr2 ~ market_returns$`RM-RF`[7:1069])
intercepts2 <- model_list2$coefficients[1,]

vcov_mat2 <- cov(past_return_port[2:11])
v_inv2 <- ginv(vcov_mat2)

test_stat2 <- (t(intercepts2) %*% v_inv2 %*% intercepts2) / (1+market_sharpe^2)
obs <- 1063
N <- 10
F_stat2 <- obs * (obs - N - 1) * test_stat2 / (N * (obs - 2))
p_val2 <- pf(F_stat2, N, obs-N-1, 0, lower.tail = FALSE)

#Question 3f
beme_port <- read_csv("PS4_SizeBEME.csv")
beme_port <- beme_port / 100

mean_beme <- apply(beme_port, 2, mean)[2:26]
sd_beme <- apply(beme_port, 2, sd)[2:26]

sharpe_beme <- (mean_beme - mean_rf) / sd_beme

excess_returns_arr3 <- apply(beme_port, 2, excess_returns)
excess_returns_arr3 <- excess_returns_arr3[,2:26]

model_list3 <- lm(excess_returns_arr3 ~ market_returns$`RM-RF`)
intercepts3 <- model_list3$coefficients[1,]

vcov_mat3 <- cov(beme_port[2:26])
v_inv3 <- ginv(vcov_mat3)

test_stat3 <- (t(intercepts3) %*% v_inv3 %*% intercepts3) / (1+market_sharpe^2)
obs <- 1073
N <- 25
F_stat3 <- obs * (obs - N - 1) * test_stat3 / (N * (obs - 2))
p_val3 <- pf(F_stat3, N, obs-N-1, 0, lower.tail = FALSE)

#Question 3g
one_v = rep(1, 25)
tan_weights <- (v_inv3 %*% (mean_beme - mean_rf)) / (as.vector(t(one_v) %*% v_inv3 %*% (mean_beme - mean_rf)))
tan_excess_return <- excess_returns_arr3 %*% tan_weights

model_list4 <- lm(excess_returns_arr3 ~ tan_excess_return)
intercepts4 <- model_list4$coefficients[1,]
tan_sharpe <- mean(tan_excess_return) / sd(tan_excess_return)

test_stat4 <- (t(intercepts4) %*% v_inv3 %*% intercepts4) / (1+tan_sharpe^2)
obs <- 1073
N <- 25
F_stat4 <- obs * (obs - N - 1) * test_stat4 / (N * (obs - 2))
p_val4 <- pf(F_stat4, N, obs-N-1, 0, lower.tail = FALSE)


#Question 3h
even_years <- subset(beme_port, as.integer(beme_port$Date) %% 2 == 0) 
odd_years <- subset(beme_port, as.integer(beme_port$Date) %% 2 == 1)

even_training <- subset(even_years, as.integer(even_years$Date * 100) %% 2 == 1)
even_test <- subset(even_years, as.integer(even_years$Date * 100) %% 2 == 0)
odd_training <- subset(odd_years, as.integer(odd_years$Date * 100) %% 2 == 0)
odd_test <- subset(odd_years, as.integer(odd_years$Date * 100) %% 2 == 1)

training_df <- rbind(even_training, odd_training)
test_df <- rbind(even_test, odd_test)

vcov_mat4 = cov(training_df[2:26])
v_inv4 = ginv(vcov_mat4)
training_ret = as.matrix(training_df[,2:26])
avg_train_rtn <- apply(training_df[,2:26], 2, mean, na.rm=TRUE)
avg_train_rtn2 <- avg_train_rtn - mean_rf
tan_weights2 <- (v_inv4 %*% avg_train_rtn2) / (as.vector(t(one_v) %*% v_inv4 %*% avg_train_rtn2))

vcov_mat5 = cov(test_df[2:26])
v_inv5 = ginv(vcov_mat5)
test_ret <- as.matrix(test_df[,2:26])
avg_test_rtn <- apply(test_df[,2:26], 2, mean)
avg_test_rtn2 <- avg_test_rtn - mean_rf
tan_weights3 <- (v_inv5 %*% avg_test_rtn2) / (as.vector(t(one_v) %*% v_inv5 %*% avg_test_rtn2))

first_half_of_returns <- test_ret %*% tan_weights2
second_half <- training_ret %*% tan_weights3

total_ret <- as.numeric()
for (i in 1:1073){
  if (as.integer(beme_port$Date) %% 2 == 0 && as.integer(beme_port$Date * 100) %% 2 == 1){
    holder <- excess_returns_arr3[i,] %*% tan_weights3
    total_ret <- c(total_ret, holder)
  }
  else if (as.integer(beme_port$Date) %% 2 == 1 && as.integer((beme_port$Date * 100) %% 2 == 0)){
    holder <- excess_returns_arr3[i,] %*% tan_weights3
    total_ret <- c(total_ret, holder)
  }
  else{
    holder <- excess_returns_arr3[i,] %*% tan_weights2
    total_ret <- c(total_ret, holder)
  }
}

tan_sharpe2 <- mean(total_ret) / sd(total_ret)

model_list5 <- lm(excess_returns_arr3 ~ total_ret)
intercepts5 <- model_list5$coefficients[1,]

test_stat5 <- (t(intercepts5) %*% v_inv3 %*% intercepts5) / (1+tan_sharpe2^2)
obs <- 1073
N <- 25
F_stat5 <- obs * (obs - N - 1) * test_stat5 / (N * (obs - 2))
p_val5 <- pf(F_stat5, N, obs-N-1, 0, lower.tail = FALSE)

#Question 3i
one_v <- rep(1, 30)
industry_tan <- (v_inv %*% (mean_industry - mean_rf)) / (as.vector(t(one_v) %*% v_inv %*% (mean_industry - mean_rf)))
ind_tan_returns <- excess_returns_arr %*% industry_tan

model_list6 <- lm(excess_returns_arr3[1:1069,] ~ ind_tan_returns)
intercepts6 <- model_list6$coefficients[1,]

tan_sharpe3 <- mean(ind_tan_returns) / sd(ind_tan_returns)

test_stat6 <- (t(intercepts6) %*% v_inv3 %*% intercepts6) / (1+tan_sharpe3^2)
obs <- 1069
N <- 25
F_stat6 <- obs * (obs - N - 1) * test_stat6 / (N * (obs - 2))
p_val6 <- pf(F_stat6, N, obs-N-1, 0, lower.tail = FALSE)

#Question 3j
one_v <- rep(1, 10)
pastperf_tan <- (v_inv2 %*% (mean_pastperf- mean_rf)) / (as.vector(t(one_v) %*% v_inv2 %*% (mean_pastperf - mean_rf)))
pp_tan_returns <- excess_returns_arr2 %*% pastperf_tan

model_list7 <- lm(excess_returns_arr3[7:1069,] ~ pp_tan_returns)
intercepts7 <- model_list6$coefficients[1,]

tan_sharpe4 <- mean(pp_tan_returns) / sd(pp_tan_returns)

test_stat7 <- (t(intercepts7) %*% v_inv3 %*% intercepts7) / (1+tan_sharpe4^2)
obs <- 1063
N <- 25
F_stat7 <- obs * (obs - N - 1) * test_stat7 / (N * (obs - 2))
p_val7 <- pf(F_stat7, N, obs-N-1, 0, lower.tail = FALSE)