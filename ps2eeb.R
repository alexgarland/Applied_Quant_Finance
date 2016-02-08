library(readr)
library(moments)
library(MASS)
library(quadprog)
setwd("C:/Users/Emily/Documents/AppliedQuantFin")
rm(list=ls())

raw = read_csv("Problem_Set2_2016.csv")
data_mat = as.matrix(raw[2:11])

# 1a)
portfolio = as.matrix(raw[2:11])/100 #just the stocks, ignoring rf
rf = as.matrix(raw[12])
rf <- mean(rf)/100
v = cov(portfolio)
v_inv = ginv(v)
sigmasq = diag(v)^2
R = mean(portfolio)
mean_return <- sapply(as.list(as.data.frame(portfolio)), mean)
mean_return <- mean_return - rf

one_v <- rep(1, 10)
mvp_weights <- (v_inv %*% one_v) / as.vector((t(one_v) %*% v_inv %*% one_v))
tan_weights <- (v_inv %*% mean_return) / (as.vector(t(one_v) %*% v_inv %*% mean_return))

mvp_r <- (mean_return + rf) %*% mvp_weights
tan_r <- (mean_return + rf) %*% tan_weights

mvp_sd <- (t(mvp_weights) %*% v %*% mvp_weights)^(1/2)
tan_sd <- (t(tan_weights) %*% v %*% tan_weights)^(1/2)

monthly_mvp <- portfolio %*% mvp_weights
monthly_tan <- portfolio %*% tan_weights
cov_two_port <- cov(monthly_mvp, monthly_tan)

weights <- seq(-5, 5, .01)
efficient_frontier <- data.frame(ret = numeric(), sd = numeric())
for (w in weights){
  multi_sd <- (w^2 * mvp_sd^2 + (1-w)^2 * tan_sd^2 + 2*w*(1-w)*cov_two_port)^(1/2)
  multi_return <- w*mvp_r + (1-w)*tan_r
  temp = data.frame(ret = multi_return, sd = multi_sd)
  efficient_frontier <- rbind(efficient_frontier, temp)
}

plot(efficient_frontier$sd, efficient_frontier$ret, type = "l")
points(apply(portfolio,2,sd),mean_return+rf)

# 1b)
sds <- apply(portfolio, 2, sd)
std_err <- sds / sqrt(1069)

high_mean_return <- mean_return + std_err;

one_v <- rep(1, 10)
high_mvp_weights <- (v_inv %*% one_v) / as.vector((t(one_v) %*% v_inv %*% one_v))
high_tan_weights <- (v_inv %*% high_mean_return) / (as.vector(t(one_v) %*% v_inv %*% high_mean_return))

high_mvp_r <- (high_mean_return + rf) %*% high_mvp_weights
high_tan_r <- (high_mean_return + rf) %*% high_tan_weights

high_mvp_sd <- (t(high_mvp_weights) %*% v %*% high_mvp_weights)^(1/2)
high_tan_sd <- (t(high_tan_weights) %*% v %*% high_tan_weights)^(1/2)

high_monthly_mvp <- portfolio %*% high_mvp_weights
high_monthly_tan <- portfolio %*% high_tan_weights
high_cov_two_port <- cov(high_monthly_mvp, high_monthly_tan)

weights <- seq(-5, 5, .01)
high_efficient_frontier <- data.frame(ret = numeric(), sd = numeric())
for (w in weights){
  high_multi_sd <- (w^2 * high_mvp_sd^2 + (1-w)^2 * high_tan_sd^2 + 2*w*(1-w)*high_cov_two_port)^(1/2)
  high_multi_return <- w*high_mvp_r + (1-w)*high_tan_r
  temp = data.frame(ret = high_multi_return, sd = high_multi_sd)
  high_efficient_frontier <- rbind(high_efficient_frontier, temp)
}

plot(high_efficient_frontier$sd, high_efficient_frontier$ret, type = "l", col = "red")
points(sds, mean_return + rf)

# 1c) 
v_diag = cov(portfolio)
v_diag[upper.tri(v_diag) | lower.tri(v_diag)] = 0
v_diag_inv = ginv(v_diag)

mvp_weights <- (v_diag_inv %*% one_v) / as.vector((t(one_v) %*% v_diag_inv %*% one_v))
tan_weights <- (v_diag_inv %*% mean_return) / (as.vector(t(one_v) %*% v_diag_inv %*% mean_return))

mvp_r <- (mean_return + rf) %*% mvp_weights
tan_r <- (mean_return + rf) %*% tan_weights

mvp_sd <- (t(mvp_weights) %*% v_diag %*% mvp_weights)^(1/2)
tan_sd <- (t(tan_weights) %*% v_diag %*% tan_weights)^(1/2)

monthly_mvp <- portfolio %*% mvp_weights
monthly_tan <- portfolio %*% tan_weights
cov_two_port <- cov(monthly_mvp, monthly_tan)

weights <- seq(-5, 5, .01)
efficient_frontier_diag <- data.frame(ret = numeric(), sd = numeric())
for (w in weights){
  multi_sd <- (w^2 * mvp_sd^2 + (1-w)^2 * tan_sd^2 + 2*w*(1-w)*cov_two_port)^(1/2)
  multi_return <- w*mvp_r + (1-w)*tan_r
  temp = data.frame(ret = multi_return, sd = multi_sd)
  efficient_frontier_diag <- rbind(efficient_frontier_diag, temp)
}

plot(efficient_frontier_diag$sd, efficient_frontier_diag$ret, type = "l")

# 1d) 

