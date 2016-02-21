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
market <- market[1:1069,]
rf <- mean(market[,2])
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
errors <- matrix(nrow = 1069, ncol = 30)
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
cov_e <- (t(errors) %*% errors) / (T - L - 1)
factor_means <- as.matrix(portfolio_means - rf)
factor_returns <- vw_returns - rf
factor_means_bar <- t(repmat(factor_means,1,1069))
factor_cov <- (t(factor_returns - factor_means_bar) %*% (factor_returns - factor_means_bar)) / (T-1)

grs <- (T/N) * ((T-N-L)/(T-L-1)) * (t(alphas) %*% ginv(cov_e) %*% alphas) / (1+ t(factor_means) %*% ginv(factor_cov) %*% factor_means)

#1E
past_returns <- read_csv("ps4past.csv")
past_returns <- as.matrix(past_returns[,2:11])

past_means <- apply(past_returns, 2, mean)
past_sds <- apply(past_returns, 2, sd)
past_sharpes <- (past_means - rf) / past_sds

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
cov_e <- (t(past_errors) %*% past_errors) / (T - L - 1)
factor_means <- as.matrix(past_means - rf)
factor_returns <- past_returns - rf
factor_means_bar <- t(repmat(factor_means,1,T))
factor_cov <- (t(factor_returns - factor_means_bar) %*% (factor_returns - factor_means_bar)) / (T-1)

grs <- (T/N) * ((T-N-L)/(T-L-1)) * (t(past_alphas) %*% ginv(cov_e) %*% past_alphas) / (1+ t(factor_means) %*% ginv(factor_cov) %*% factor_means)

#1F
T <- 1069 #num of observations
L <- 1 #num of factors
N <- 25 #num of portfolios
beme <- read_csv("ps4beme.csv")
beme <- as.matrix(beme[1:1069,2:26])

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

#F-stat
cov_e <- (t(beme_errors) %*% beme_errors) / (T - L - 1)
factor_means <- as.matrix(beme_means - rf)
factor_returns <- beme - rf
factor_means_bar <- t(repmat(factor_means,1,1069))
factor_cov <- (t(factor_returns - factor_means_bar) %*% (factor_returns - factor_means_bar)) / (T-1)

grs <- (T/N) * ((T-N-L)/(T-L-1)) * (t(beme_alphas) %*% ginv(cov_e) %*% beme_alphas) / (1+ t(factor_means) %*% ginv(factor_cov) %*% factor_means)

