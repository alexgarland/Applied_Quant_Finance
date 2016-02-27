library(readr)
library(ggplot2)
library(MASS)

setwd("C:/Users/Emily/Documents/AppliedQuantFin")
rm(list=ls())

repmat = function(X,m,n){
  ##R equivalent of repmat (matlab)
  mx = dim(X)[1]
  nx = dim(X)[2]
  matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)
}

market <- read_csv("ps4market.csv")
market <- as.matrix(market[,2:3])
market <- market / 100
rf <- mean(market[,2])
market <- market[1:1069,]
vw_returns <- read_csv("ps4vw.csv")
vw_returns <- as.matrix(vw_returns[,2:31])
vw_returns <- vw_returns / 100

#1A
portfolio_means <- apply(vw_returns, 2, mean)
portfolio_sds <- apply(vw_returns, 2, sd)
portfolio_sharpes <- (portfolio_means - rf) / portfolio_sds

#1B 
alphas <- 1 : 30
betas  <- 1 : 30
for(i in 1:30) {
  model <- lm(vw_returns[,i] - rf ~ market[,1])
  alphas[i] <- coefficients(model)[1]
  betas[i]  <- coefficients(model)[2]
  errors[,i] <- residuals(model)
}

#F-stat
T <- 1069 #num of observations
L <- 1 #num of factors
N <- 30 #num of portfolios
v_inv <- ginv(cov(vw_returns))
market_sharpe <- mean(market[,1]) / sd(market[,1])
test_stat <- (t(alphas) %*% v_inv %*% alphas) / (1+market_sharpe^2)
F_stat <- obs * (T - N - 1) * test_stat / (N * (T - 2))
p_val <- pf(F_stat, N, T-N-1, 0, lower.tail = FALSE)

#1E
past_returns <- read_csv("ps4past.csv")
past_returns <- as.matrix(past_returns[,2:11])
past_returns <- past_returns / 100

past_means <- apply(past_returns - rf, 2, mean)
past_sds <- apply(past_returns, 2, sd)
past_sharpes <- (past_means) / past_sds

past_alphas <- 1 : 10
past_betas  <- 1 : 10
past_errors <- matrix(nrow = 1063, ncol = 10)
for(i in 1:10) {
  model2 <- lm(past_returns[,i] - rf ~ market[7:1069,1])
  past_alphas[i] <- coefficients(model2)[1]
  past_betas[i]  <- coefficients(model2)[2]
  past_errors[,i] <- residuals(model2)
}

#F-stat
T <- 1063 #num of observations
L <- 1 #num of factors
N <- 10 #num of portfolios
v_inv <- ginv(cov(past_returns))
market_sharpe <- mean(market[,1]) / sd(market[,1])
test_stat <- (t(past_alphas) %*% v_inv %*% past_alphas) / (1+market_sharpe^2)
F_stat <- obs * (T - N - 1) * test_stat / (N * (T - 2))
p_val <- pf(F_stat, N, T-N-1, 0, lower.tail = FALSE)

#1F
T <- 1069 #num of observations
L <- 1 #num of factors
N <- 25 #num of portfolios
beme <- read_csv("ps4beme.csv")
beme <- as.matrix(beme[1:1069,2:26])
beme <- beme / 100

beme_means <- apply(beme, 2, mean)
beme_sds <- apply(beme, 2, sd)
beme_sharpes <- (beme_means - rf) / beme_sds

beme_alphas <- 1 : 25
beme_betas  <- 1 : 25
beme_errors <- matrix(nrow = 1069, ncol = 25)
for(i in 1:25) {
  model3 <- lm(beme[,i] - rf ~ market[,1])
  beme_alphas[i] <- coefficients(model3)[1]
  beme_betas[i]  <- coefficients(model3)[2]
  beme_errors[,i] <- residuals(model3)
}

##F-stat
T <- 1063 #num of observations
L <- 1 #num of factors
N <- 25 #num of portfolios
v_inv <- ginv(cov(beme))
market_sharpe <- mean(market[,1]) / sd(market[,1])
test_stat <- (t(beme_alphas) %*% v_inv %*% beme_alphas) / (1+market_sharpe^2)
F_stat <- obs * (T - N - 1) * test_stat / (N * (T - 2))
p_val <- pf(F_stat, N, T-N-1, 0, lower.tail = FALSE)

#1G
cov_beme <- cov(beme)
cov_beme_inv <- ginv(cov_beme)
one_v = rep(1, 25)
tan_weights <- (cov_beme_inv %*% (beme_means - rf)) / (as.vector(t(one_v) %*% cov_beme_inv %*% (beme_means - rf)))
tan_excess_return <- (beme - rf) %*% tan_weights

model_list <- lm((beme - rf) ~ tan_excess_return)
intercepts <- model_list$coefficients[1,]
tan_sharpe <- mean(tan_excess_return) / sd(tan_excess_return)

test_stat <- (t(intercepts) %*% cov_beme_inv %*% intercepts) / (1+tan_sharpe^2)
obs <- 1073
N <- 25
F_stat <- obs * (obs - N - 1) * test_stat / (N * (obs - 2))
p_val <- pf(F_stat, N, obs-N-1, 0, lower.tail = FALSE)

#1H
beme <- read_csv("ps4beme.csv")
beme <- beme / 100
even_years <- subset(beme, as.integer(beme$Size) %% 2 == 0) 
odd_years <- subset(beme, as.integer(beme$Size) %% 2 == 1)

even_training <- subset(even_years, as.integer(even_years$Size * 100) %% 2 == 1)
even_test <- subset(even_years, as.integer(even_years$Size * 100) %% 2 == 0)
odd_training <- subset(odd_years, as.integer(odd_years$Size * 100) %% 2 == 0)
odd_test <- subset(odd_years, as.integer(odd_years$Size * 100) %% 2 == 1)

training_df <- rbind(even_training, odd_training)
test_df <- rbind(even_test, odd_test)

vcov_mat4 = cov(training_df[2:26])
v_inv4 = ginv(vcov_mat4)
training_ret = as.matrix(training_df[,2:26])
avg_train_rtn <- apply(training_df[,2:26], 2, mean, na.rm=TRUE)
avg_train_rtn2 <- avg_train_rtn - rf
tan_weights2 <- (v_inv4 %*% avg_train_rtn2) / (as.vector(t(one_v) %*% v_inv4 %*% avg_train_rtn2))

vcov_mat5 = cov(test_df[2:26])
v_inv5 = ginv(vcov_mat5)
test_ret <- as.matrix(test_df[,2:26])
avg_test_rtn <- apply(test_df[,2:26], 2, mean)
avg_test_rtn2 <- avg_test_rtn - rf
tan_weights3 <- (v_inv5 %*% avg_test_rtn2) / (as.vector(t(one_v) %*% v_inv5 %*% avg_test_rtn2))

first_half_of_returns <- test_ret %*% tan_weights2
second_half <- training_ret %*% tan_weights3

beme_excess <- beme - rf
total_ret <- as.numeric()
for (i in 1:1073){
  if (as.integer(beme$Size) %% 2 == 0 && as.integer(beme$Size * 100) %% 2 == 1){
    holder <- beme_excess[i,] %*% tan_weights3
    total_ret <- c(total_ret, holder)
  }
  else if (as.integer(beme$Size) %% 2 == 1 && as.integer((beme$Size * 100) %% 2 == 0)){
    holder <- beme_excess[i,] %*% tan_weights3
    total_ret <- c(total_ret, holder)
  }
  else{
    holder <- beme_excess[i,] %*% tan_weights2
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
v_inv <- ginv(cov(vw_returns))
vw_excess <- vw_returns - rf
industry_tan <- (v_inv %*% (portfolio_means - rf)) / (as.vector(t(one_v) %*% v_inv %*% (portfolio_means - rf)))
ind_tan_returns <- vw_excess %*% industry_tan

i_alphas <- 1 : 25
i_betas  <- 1 : 25
beme <- read_csv("ps4beme.csv")
beme <- beme / 100
beme <- beme[1:1069,2:26]
for(i in 1:25) {
  model6 <- lm(beme[1:1069,i] - rf ~ ind_tan_returns)
  i_alphas[i] <- coefficients(model6)[1]
  i_betas[i]  <- coefficients(model6)[2]
}
tan_sharpe3 <- mean(ind_tan_returns) / sd(ind_tan_returns)

test_stat6 <- (t(i_alphas) %*% cov_beme_inv %*% i_alphas) / (1+tan_sharpe3^2)
obs <- 1069
N <- 25
F_stat6 <- obs * (obs - N - 1) * test_stat6 / (N * (obs - 2))
p_val6 <- pf(F_stat6, N, obs-N-1, 0, lower.tail = FALSE)

#Question 3j
one_v <- rep(1, 10)
v_inv2 <- ginv(cov(past_returns))
pastperf_tan <- (v_inv2 %*% (past_means)) / (as.vector(t(one_v) %*% v_inv2 %*% (past_means)))
pp_excess <- past_returns - rf
pp_tan_returns <- pp_excess %*% pastperf_tan

j_alphas <- 1 : 25
j_betas  <- 1 : 25
for(i in 1:25) {
  model7 <- lm(beme[1:1063,i] - rf ~ ind_tan_returns[1:1063])
  j_alphas[i] <- coefficients(model7)[1]
  j_betas[i]  <- coefficients(model7)[2]
}

tan_sharpe_j <- mean(pp_tan_returns) / sd(pp_tan_returns)

test_stat7 <- (t(j_alphas) %*% cov_beme_inv %*% j_alphas) / (1+tan_sharpe_j^2)
obs <- 1063
N <- 25
F_stat7 <- obs * (obs - N - 1) * test_stat7 / (N * (obs - 2))
p_val7 <- pf(F_stat7, N, obs-N-1, 0, lower.tail = FALSE)

#3K
returns_df <- data.frame(tan_ind = ind_tan_returns[7:1069], tan_pp = pp_tan_returns, tan_beme = tan_excess_return[7:1069])
cov_returns <- cov(returns_df)


