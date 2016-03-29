library(readr)
library(MASS)
library(ggplot2)
setwd("C:/Users/Emily/Documents/AppliedQuantFin")
rm(list=ls())

raw = read_csv("Problem_Set2_2016.csv")
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

industry_sd1 <- sapply(as.list(as.data.frame(industry_returns)), sd)

industries <- data.frame(mean_r = mean_return, i_sd = industry_sd1)

first_plot <- ggplot(data = efficient_frontier, aes(x = sd, y = ret)) + 
              geom_point(color="firebrick") + 
              geom_point(aes(x=i_sd, y = mean_r), data = industries, color="blue")
print(first_plot)

#Question 1B
industry_sd <- industry_sd1 / sqrt(1069)
mean_return_inc <- mean_return2 + industry_sd
tan_weights2 <- (v_inv %*% mean_return_inc) / (as.vector(t(one_v) %*% v_inv %*% mean_return_inc))

tan_return2 <- (mean_return + industry_sd) %*% tan_weights2
tan_sd2 <- (t(tan_weights2) %*% vcov_mat %*% tan_weights2)^(1/2)

mvp_return2 <-  (mean_return + industry_sd) %*% mvp_weights

monthly_mvp2 <- industry_returns %*% mvp_weights
monthly_tan2 <- industry_returns %*% tan_weights2
cov_two_port <- cov(monthly_mvp2, monthly_tan2)

weights <- seq(-5, 5, .01)
efficient_frontier2 <- data.frame(ret = numeric(), sd = numeric())
for (w in weights){
  multi_sd <- (w^2 * mvp_sd^2 + (1-w)^2 * tan_sd2^2 + 2*w*(1-w)*cov_two_port)^(1/2)
  multi_return <- w*mvp_return2 + (1-w)*tan_return2
  temp = data.frame(ret = multi_return, sd = multi_sd)
  efficient_frontier2 <- rbind(efficient_frontier2, temp)
}

second_plot <- ggplot(data = efficient_frontier, aes(x = sd, y = ret)) + 
              geom_point(color="firebrick") + 
              geom_point(aes(x=i_sd, y = mean_r), data = industries, color="blue") +
              geom_point(aes(x=sd, y=ret), data=efficient_frontier2, color = "pink")
print(second_plot)

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
cov_two_port3 <- t(mvp_weights3) %*% var_only_mat %*% tan_weights
#cov_two_port3 <- 0 #cov(monthly_mvp3, monthly_tan3)

weights <- seq(-5, 5, 5)
efficient_frontier3 <- data.frame(ret = numeric(), sd = numeric())
for (w in weights){
  multi_sd <- (w^2 * mvp_sd3^2 + (1-w)^2 * tan_sd3^2 + 2*w*(1-w)*cov_two_port3)^(1/2) 
  multi_return <- w*mvp_return3 + (1-w)*tan_return3
  temp = data.frame(ret = multi_return, sd = multi_sd)
  efficient_frontier3 <- rbind(efficient_frontier3, temp)
}

var_only_mat2 <- diag(10)
v_only_inv2 = ginv(var_only_mat2)

tan_weights3b <- (v_only_inv2 %*% mean_return2) / (as.vector(t(one_v) %*% v_only_inv2 %*% 
                                                             mean_return2))
tan_return3b <- mean_return %*% tan_weights3b
tan_sd3b <- (t(tan_weights3b) %*% var_only_mat2 %*% tan_weights3b)^(1/2)

mvp_weights3b <- (v_only_inv2 %*% one_v) / as.vector((t(one_v) %*% v_only_inv2 %*% one_v))
mvp_return3b <- mean_return %*% mvp_weights3b
mvp_sd3b <- (t(mvp_weights3b) %*% var_only_mat2 %*% mvp_weights3b)^(1/2)

monthly_mvp3b <- industry_returns %*% mvp_weights3b
monthly_tan3b <- industry_returns %*% tan_weights3b
cov_two_port3b <- t(tan_weights3b) %*% var_only_mat2 %*% mvp_weights
#cov_two_port3b <- 0 # cov(monthly_mvp3b, monthly_tan3b)

weights <- seq(-5, 1, 5)
efficient_frontier3b <- data.frame(ret = numeric(), sd = numeric())
for (w in weights){
  multi_sd <- (w^2 * mvp_sd3b^2 + (1-w)^2 * tan_sd3b^2 + 2*w*(1-w)*cov_two_port3b)^(1/2) 
  multi_return <- w*mvp_return3b + (1-w)*tan_return3b
  temp = data.frame(ret = multi_return, sd = multi_sd)
  efficient_frontier3b <- rbind(efficient_frontier3b, temp)
}

third_plot <- ggplot(data = efficient_frontier, aes(x = sd, y = ret)) + 
              geom_point(color="firebrick") + 
              geom_point(aes(x=i_sd, y = mean_r), data = industries, color="blue") +
              geom_point(aes(x=sd, y=ret), data=efficient_frontier2, color = "pink") +
              geom_point(aes(x=sd, y=ret), data=efficient_frontier3, color = "green") +
              geom_point(aes(x=sd, y=ret), data=efficient_frontier3b,color="yellow")
print(third_plot)

#FQuestion 1C Continued

#For Questions 1D and 1E
num_months <- 60

#Question 1D
total_portfolio_normal <- data.frame(var_return = mvp_return, var_sd = mvp_sd, tangency_return = tan_return, tangency_sd = tan_sd)
highest_weight1 <- numeric()
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
  
  highest_weight1 <- c(highest_weight1, max(tan_weights_p))
  temp <- data.frame(var_return = mvp_return_p, var_sd = mvp_sd_p, tangency_return = tan_return_p, tangency_sd = tan_sd_p)
  total_portfolio_normal <- rbind(total_portfolio_normal, temp)
}

fourth_plot <- ggplot(data = total_portfolio_normal, aes(x=var_sd, y=var_return)) +
               geom_point(color = "firebrick")
fifth_plot <- ggplot(data = total_portfolio_normal, aes(x=tangency_sd, y=tangency_return)) +
              geom_point(color = "firebrick")
sixth_plot <- ggplot(data = total_portfolio_normal, aes(x=tangency_sd, y=tangency_return)) +
              geom_point(color = "firebrick") + xlim(0,1) + ylim(-.05, .05)
print(fifth_plot)
print(sixth_plot)


#Question 1E
highest_weight2 <- numeric()
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
  
  highest_weight2 <- c(highest_weight2, max(tan_weights_p))
  temp <- data.frame(var_return = mvp_return_p, var_sd = mvp_sd_p, tangency_return = tan_return_p, tangency_sd = tan_sd_p)
  total_portfolio_boot <- rbind(total_portfolio_boot, temp)
}

seventh_plot <- ggplot(data = total_portfolio_boot, aes(x=var_sd, y=var_return)) +
                geom_point(color = "firebrick")
eigth_plot <- ggplot(data = total_portfolio_boot, aes(x=tangency_sd, y=tangency_return)) +
              geom_point(color = "firebrick")
ninth_plot <- ggplot(data = total_portfolio_boot, aes(x=tangency_sd, y=tangency_return)) +
              geom_point(color = "firebrick") + xlim(0,.5) + ylim(-.025, .025)
