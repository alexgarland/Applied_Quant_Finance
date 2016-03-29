library(readr)
library(MASS)
library(ggplot2)

setwd("C:/Users/Emily/Documents/AppliedQuantFin")
rm(list=ls())

market_proxy <- read_csv("ps3marketProxyTrimmed.csv")
market_proxy <- as.matrix(market_proxy[,2:3])
vw_returns <- read_csv("ps3vwReturns.csv")
vw_returns[vw_returns <= -99] <- NA
vw_returns <- as.matrix(vw_returns[,2:50])
firm_size <- read_csv("ps3avgFirmSize.csv")
firm_size  <- as.matrix(firm_size[,2:50])
be_me <- read.csv("ps3beme.csv")
be_me <- as.matrix(be_me[,2:50])

# 1b)
average_returns <- apply(vw_returns,2,mean, na.rm = TRUE)
rf = mean(market_proxy[,2])/100

cov_returns <- cov(vw_returns)
var_market <- var(market_proxy[,1]);
covariances <- 1 : 49
#covariances <- as.matrix(covariances)
for(i in 1 : 49) {
  merged_df <- data.frame(industry = vw_returns[,i])
  merged_df$market <- market_proxy[,1]
  merged_df <- subset(merged_df, !is.na(merged_df$industry))
  covariances[i] = cov(merged_df$industry - rf, merged_df$market - rf)/var(merged_df$market)
}

beta <- covariances

model <- lm(vw_returns ~ market_proxy[,1])
gamma0 <- 1 : 1069
gammaM <- 1 : 1069

for(i in 1 : 1069) {
  m <- lm(vw_returns[i,] ~ beta)
  gamma0[i] <- coefficients(m)[1]
  gammaM[i] <- coefficients(m)[2]
}
g0 <- mean(gamma0)
gM <- mean(gammaM)
g0_err <- (sd(gamma0))/sqrt(1069)
gM_err <- (sd(gammaM))/sqrt(1069)
g0_t <- g0/g0_err
gM_t <- gM/gM_err 

#1c
model <- lm(average_returns ~ beta)
g0_time <- coefficients(model)[1]
gM_time <- coefficients(model)[2]
g0_time_err <- coefficients(summary(model))[1,2]
gM_time_err <- coefficients(summary(model))[2,2]

#1d
plot(beta, average_returns)
plot_df = data.frame(returns = average_returns, b = beta)
first_plot <- ggplot(data = plot_df, aes(x = beta, y = returns)) + 
  geom_point(color = "firebrick") +
  geom_abline(slope = gM, intercept = g0, color = "blue")
print(first_plot)

#1f
gamma0_f <- 1 : 1069
gammaM_f <- 1 : 1069
gammaS_f <- 1 : 1069
gammaB_f <- 1 : 1069

expanded_be_me <- be_me[1,]
for (i in 1:90){
  for (j in 1:12){
    expanded_be_me <- rbind(expanded_be_me, be_me[i,])
  }
}
expanded_be_me <- expanded_be_me[8:1081,1:49]
expanded_be_me <- expanded_be_me[1:1069, 1:49]

size_curr <- firm_size[1,]
be_me_curr <- expanded_be_me[1,]

for(i in 13 : 1069) {
  if(i > 13) {
    be_me_curr <- expanded_be_me[i-12,]
    size_curr <- firm_size[i-1,]
  }
  m3 <- lm(vw_returns[i,] ~ beta + log(size_curr) + log(be_me_curr))
  gamma0_f[i] <- coefficients(m3)[1]
  gammaM_f[i] <- coefficients(m3)[2]
  gammaS_f[i] <- coefficients(m3)[3]
  gammaB_f[i] <- coefficients(m3)[4]
}
g0_f <- mean(gamma0_f)
gM_f <- mean(gammaM_f)
gS_f <- mean(gammaS_f)
gB_f <- mean(gammaB_f)

g0_f_err <- sd(gamma0_f)/sqrt(1069)
gM_f_err <- sd(gammaM_f)/sqrt(1069)
gS_f_err <- sd(gamma0_f)/sqrt(1069)
gB_f_err <- sd(gammaM_f)/sqrt(1069)

## PART TWO

vw_returns_25 <- read_csv("ps3vw25.csv")
vw_returns_25[vw_returns_25 <= -99] <- NA
vw_returns_25 <- as.matrix(vw_returns_25[,2:25])
firm_size_25 <- read_csv("ps3size25.csv")
firm_size_25  <- as.matrix(firm_size_25[,2:25])
be_me_25 <- read.csv("ps3beme25.csv")
be_me_25 <- as.matrix(be_me_25[,2:25])

# 2b)
average_returns <- apply(vw_returns_25,2,mean, na.rm = TRUE)
rf = mean(market_proxy[,2])/100

covariances <- 1 : 49
#covariances <- as.matrix(covariances)
for(i in 1 : 49) {
  merged_df <- data.frame(industry = vw_returns_25[,i])
  merged_df$market <- market_proxy[,1]
  merged_df <- subset(merged_df, !is.na(merged_df$industry))
  covariances[i] = cov(merged_df$industry - rf, merged_df$market - rf)/var(merged_df$market)
}

beta <- covariances

model <- lm(vw_returns_25 ~ market_proxy[,1])
gamma0 <- 1 : 1069
gammaM <- 1 : 1069

for(i in 1 : 1069) {
  m <- lm(vw_returns[i,] ~ beta)
  gamma0[i] <- coefficients(m)[1]
  gammaM[i] <- coefficients(m)[2]
}
g0 <- mean(gamma0)
gM <- mean(gammaM)
g0_err <- (sd(gamma0))/sqrt(1069)
gM_err <- (sd(gammaM))/sqrt(1069)
g0_t <- g0/g0_err
gM_t <- gM/gM_err 

#2c
model <- lm(average_returns ~ beta)
g0_time <- coefficients(model)[1]
gM_time <- coefficients(model)[2]
g0_time_err <- coefficients(summary(model))[1,2]
gM_time_err <- coefficients(summary(model))[2,2]

#2d
plot(beta, average_returns)
plot_df = data.frame(returns = average_returns, b = beta)
first_plot <- ggplot(data = plot_df, aes(x = beta, y = returns)) + 
  geom_point(color = "firebrick") +
  geom_abline(slope = gM, intercept = g0, color = "blue")
print(first_plot)

#2f
gamma0_f <- 1 : 1069
gammaM_f <- 1 : 1069
gammaS_f <- 1 : 1069
gammaB_f <- 1 : 1069

expanded_be_me <- be_me_25[1,]
for (i in 1:90){
  for (j in 1:12){
    expanded_be_me <- rbind(expanded_be_me, be_me_25[i,])
  }
}
expanded_be_me <- expanded_be_me[8:1081,1:49]
expanded_be_me <- expanded_be_me[1:1069, 1:49]

size_curr <- firm_size_25[1,]
be_me_curr <- expanded_be_me[1,]

for(i in 13 : 1069) {
  if(i > 13) {
    be_me_curr <- expanded_be_me[i-12,]
    size_curr <- firm_size_25[i-1,]
  }
  m3 <- lm(vw_returns[i,] ~ beta + log(size_curr) + log(be_me_curr))
  gamma0_f[i] <- coefficients(m3)[1]
  gammaM_f[i] <- coefficients(m3)[2]
  gammaS_f[i] <- coefficients(m3)[3]
  gammaB_f[i] <- coefficients(m3)[4]
}
g0_f <- mean(gamma0_f)
gM_f <- mean(gammaM_f)
gS_f <- mean(gammaS_f)
gB_f <- mean(gammaB_f)

g0_f_err <- sd(gamma0_f)/sqrt(1069)
gM_f_err <- sd(gammaM_f)/sqrt(1069)
gS_f_err <- sd(gamma0_f)/sqrt(1069)
gB_f_err <- sd(gammaM_f)/sqrt(1069)

