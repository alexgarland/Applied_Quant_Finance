library(readr)
library(MASS)
setwd("~/Applied_Quant_Finance")
rm(list=ls())

#Question 1A
raw <- read_csv("Prob_Set_2.csv")
raw <- raw/100
industry_returns <- as.matrix(raw[,2:11])

vcov_mat <- cov(industry_returns)
v_inv <- ginv(vcov_mat) 
mean_return <- sapply(as.list(as.data.frame(industry_returns)), mean)
mean_rf <- mean(raw$`Risk-free rate`)
mean_return2 <- mean_return - mean_rf

one_v <- rep(1, 10)
mvp_weights <- (v_inv %*% one_v) / as.vector((t(one_v) %*% v_inv %*% one_v))
tan_weights <- (v_inv %*% mean_return2) / (as.vector(t(one_v) %*% v_inv %*% mean_return2))

mvp_return <- mean_return %*% mvp_weights
tan_return <- mean_return %*% tan_weights

mvp_sd <- (t(mvp_weights) %*% vcov_mat %*% mvp_weights)^(1/2)
tan_sd <- (t(tan_weights) %*% vcov_mat %*% tan_weights)^(1/2)

monthly_mvp <- industry_returns %*% mvp_weights
monthly_tan <- industry_returns %*% tan_weights
cov_two_port <- cov(monthly_mvp, monthly_tan)

weights <- seq(-5, 5, .01)
efficient_frontier <- data.frame(ret = numeric(), sd = numeric())
for (w in weights){
  multi_sd <- (w^2 * mvp_sd^2 + (1-w)^2 * tan_sd^2 + 2*w*(1-w)*cov_two_port)^(1/2)
  multi_return <- w*mvp_return + (1-w)*tan_return
  temp = data.frame(ret = multi_return, sd = multi_sd)
  efficient_frontier <- rbind(efficient_frontier, temp)
}

plot(efficient_frontier$sd, efficient_frontier$ret, type = "l")

#Question 1B
industry_sd <- sapply(as.list(as.data.frame(industry_returns)), sd)
mean_return_inc <- mean_return2 + industry_sd
tan_weights2 <- (v_inv %*% mean_return_inc) / (as.vector(t(one_v) %*% v_inv %*% mean_return_inc))

tan_return2 <- (mean_return + industry_sd) %*% tan_weights2
tan_sd2 <- (t(tan_weights2) %*% vcov_mat %*% tan_weights2)^(1/2)

monthly_mvp2 <- industry_returns %*% mvp_weights
monthly_tan2 <- industry_returns %*% tan_weights2
cov_two_port <- cov(monthly_mvp2, monthly_tan2)

weights <- seq(-5, 5, .01)
efficient_frontier2 <- data.frame(ret = numeric(), sd = numeric())
for (w in weights){
  multi_sd <- (w^2 * mvp_sd^2 + (1-w)^2 * tan_sd2^2 + 2*w*(1-w)*cov_two_port)^(1/2)
  multi_return <- w*mvp_return + (1-w)*tan_return2
  temp = data.frame(ret = multi_return, sd = multi_sd)
  efficient_frontier2 <- rbind(efficient_frontier2, temp)
}
plot(efficient_frontier2$sd, efficient_frontier2$ret, type = "l")
abline(efficient_frontier$sd, efficient_frontier$ret)

#Question 1C
var_only_mat <- vcov_mat
var_only_mat[upper.tri(var_only_mat) | lower.tri(var_only_mat)] = 0
v_only_inv = ginv(var_only_mat)

tan_weights3 <- (v_only_inv %*% mean_return2) / (as.vector(t(one_v) %*% v_only_inv %*% 
                                                           mean_return2))
tan_return3 <- mean_return %*% tan_weights3
tan_sd3 <- (t(tan_weights3) %*% var_only_mat %*% tan_weights3)^(1/2)

mvp_weights3 <- (v_only_inv %*% one_v) / as.vector((t(one_v) %*% v_only_inv %*% one_v))
mvp_return3 <- mean_return %*% mvp_weights3
mvp_sd3 <- (t(mvp_weights3) %*% var_only_mat %*% mvp_weights3)^(1/2)

monthly_mvp3 <- industry_returns %*% mvp_weights3
monthly_tan3 <- industry_returns %*% tan_weights3
cov_two_port3 <- cov(monthly_mvp3, monthly_tan3)

weights <- seq(-5, 5, .01)
efficient_frontier3 <- data.frame(ret = numeric(), sd = numeric())
for (w in weights){
  multi_sd <- (w^2 * mvp_sd3^2 + (1-w)^2 * tan_sd3^2 + 2*w*(1-w)*cov_two_port3)^(1/2)
  multi_return <- w*mvp_return3 + (1-w)*tan_return3
  temp = data.frame(ret = multi_return, sd = multi_sd)
  efficient_frontier3 <- rbind(efficient_frontier3, temp)
}
plot(efficient_frontier3$sd, efficient_frontier3$ret, type = "l")

#FINISH THIS PART

#For Questions 1D and 1E
num_months <- 60

#Question 1D
total_portfolio_normal <- data.frame(var_return = mvp_return, var_sd = mvp_sd, tangency_return = tan_return, tangency_sd = tan_sd)
for (i in 1:1000){
  predicted_IR <- mvrnorm(n = num_months, mu = mean_return, Sigma = vcov_mat)
  vcov_mat_p <- cov(predicted_IR)
  v_inv_p <- ginv(vcov_mat_p) 
  mean_return_p <- sapply(as.list(as.data.frame(predicted_IR)), mean)
  mean_return_p2 <- mean_return_p - mean_rf
  
  mvp_weights_p <- (v_inv_p %*% one_v) / as.vector((t(one_v) %*% v_inv_p %*% one_v))
  tan_weights_p <- (v_inv_p %*% mean_return_p2) / (as.vector(t(one_v) %*% v_inv_p %*% mean_return_p2))
  
  mvp_return_p <- mean_return %*% mvp_weights_p
  tan_return_p <- mean_return %*% tan_weights_p
  
  mvp_sd_p <- (t(mvp_weights_p) %*% vcov_mat %*% mvp_weights_p)^(1/2)
  tan_sd_p <- (t(tan_weights_p) %*% vcov_mat %*% tan_weights_p)^(1/2)
  temp <- data.frame(var_return = mvp_return_p, var_sd = mvp_sd_p, tangency_return = tan_return_p, tangency_sd = tan_sd_p)
  total_portfolio_normal <- rbind(total_portfolio_normal, temp)
}

plot(total_portfolio_normal$var_sd, total_portfolio_normal$var_return)
plot(total_portfolio_normal$tangency_sd, total_portfolio_normal$tangency_return)

#Question 1E
total_portfolio_boot <- data.frame(var_return = mvp_return, var_sd = mvp_sd, tangency_return = tan_return, tangency_sd = tan_sd)
for (i in 1:1000){
  indices <- sample(1:1069, num_months)
  predicted_IR <- industry_returns[indices,]
  vcov_mat_p <- cov(predicted_IR)
  v_inv_p <- ginv(vcov_mat_p) 
  mean_return_p <- sapply(as.list(as.data.frame(predicted_IR)), mean)
  mean_return_p2 <- mean_return_p - mean_rf
  
  mvp_weights_p <- (v_inv_p %*% one_v) / as.vector((t(one_v) %*% v_inv_p %*% one_v))
  tan_weights_p <- (v_inv_p %*% mean_return_p2) / (as.vector(t(one_v) %*% v_inv_p %*% mean_return_p2))
  
  mvp_return_p <- mean_return %*% mvp_weights_p
  tan_return_p <- mean_return %*% tan_weights_p
  
  mvp_sd_p <- (t(mvp_weights_p) %*% vcov_mat %*% mvp_weights_p)^(1/2)
  tan_sd_p <- (t(tan_weights_p) %*% vcov_mat %*% tan_weights_p)^(1/2)
  temp <- data.frame(var_return = mvp_return_p, var_sd = mvp_sd_p, tangency_return = tan_return_p, tangency_sd = tan_sd_p)
  total_portfolio_boot <- rbind(total_portfolio_boot, temp)
}

plot(total_portfolio_boot$var_sd, total_portfolio_boot$var_return)
plot(total_portfolio_boot$tangency_sd, total_portfolio_boot$tangency_return)
