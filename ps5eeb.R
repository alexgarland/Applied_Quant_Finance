library(readr)
library(ggplot2)
library(MASS)

setwd("C:/Users/Emily/Documents/AppliedQuantFin")
rm(list=ls())

ff <- read_csv("ps5ff.csv")
#ff <- as.matrix(ff[,2:7])
ff[,2:6] <- ff[,2:6] / 100
rf <- mean(ff[,2])
recessions <- subset(ff, as.integer(ff$recession) == 1)
no_rec <- subset(ff, as.integer(ff$recession) == 0)

#1a
rec_means <- apply(recessions[,2:6], 2, mean, na.rm=TRUE)

#1b
vw <- read_csv("ps3vw25.csv")
vw[,2:26] <- vw[,2:26] / 100
small_value <- vw[,2] - rf
small_growth <- vw[,6] - rf
large_value <- vw[,21] - rf
large_growth <- vw[,26] -rf

four_corners_means <- c(mean(small_value), mean(small_growth), mean(large_value), mean(large_growth))

#1c
size <- read.csv("ps3size25.csv")
size[,2:26] <- size[,2:26] / 100
beme <- read.csv("ps3beme25.csv")
beme[,2:26] <- beme[,2:26] / 100
expanded_beme <- beme[1,]
for (i in 1:90){
  for (j in 1:12){
    expanded_beme <- rbind(expanded_beme, beme[i,])
  }
}
expanded_beme <- expanded_beme[8:1080,]

for(i in 1:25) {
  r1 <- lm(vw[,i+1] - rf ~ ff[,2] + log(size[,i+1]) + log(expanded_beme[,i+1]))
  r2 <- lm(vw[,i+1] - rf ~ ff[,2] + ff[,3] + ff[,4])
  r3 <- lm(vw[,i+1] - rf ~ ff[,2] + log(size[,i+1]) + log(expanded_beme[,i+1]) + ff[,3] + ff[,4])
}

cov(ff[,2],)
for(i in 1:1073){
  r11 <-lm(vw[i,2:26] ~ ff[i,2] + log(size[i,2:26]) + log(expanded_beme[i,2:26]))
}

T <- 1073 #num of observations
L <- 1 #num of factors
N <- 30 #num of portfolios
v_inv <- ginv(cov(vw))
market_sharpe <- mean(ff[,2]) / sd(ff[,2])
test_stat <- (t(alphas) %*% v_inv %*% alphas) / (1+market_sharpe^2)
F_stat <- obs * (T - N - 1) * test_stat / (N * (T - 2))
p_val <- pf(F_stat, N, T-N-1, 0, lower.tail = FALSE)


