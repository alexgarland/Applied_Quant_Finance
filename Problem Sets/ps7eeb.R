library(MASS)
library(readr)

rm(list=ls())
setwd("C:/Users/Emily/Documents/AppliedQuantFin")
ff <- read_csv("ps5ff.csv")
ff[,2:6] <- ff[,2:6] / 100
rf <- mean(ff$RF)
pr1m <- read_csv("pr1m.csv")
colnames(pr1m)[1] <- "date"
pr212 <- read_csv("pr212.csv")
colnames(pr212)[1] <- "date"
pr1360 <- read_csv("pr1360.csv")
colnames(pr1360)[1] <- "date"
full <- merge(pr1m, pr212, by="date")
full <- merge(full, pr1360, by="date")
full[full < -99] <- NA
full[,2:76] <- full[,2:76]/100
full <- full[60:1074,]

pr1m[,2:26] <- pr1m[,2:26] /100
pr1m <- as.matrix(pr1m)
pr212[pr212 < -99] <- NA
pr212[,2:26] <- pr212[,2:26] /100
pr212 <- pr212[12:1074,]
pr212 <- as.matrix(pr212)
pr1360[pr1360 < -99] <- NA
pr1360[,2:26] <- pr1360[,2:26] /100
pr1360 <- pr1360[60:1074,]
pr1360 <- as.matrix(pr1360)

#a)
jans_1m <- 1:1075
jans_1m <- jans_1m %% 12 == 1
jans_1m <- jans_1m[2:1075]
j1m <- coefficients(lm(pr1m[,2:26] -rf ~ jans_1m))
j1m_means <- apply(j1m, 1, mean)
jans_temp <- 1:1063 
jans_temp <- jans_temp %% 12 == 1
j212 <- coefficients(lm(pr212[,2:26] - rf ~ jans_temp))
j212_means <- apply(j212,1,mean)
j1360 <- coefficients(lm(pr1360[,2:26] -rf ~ jans_temp[1:1015]))
j1360_means <- apply(j1360,1,mean)

#1b) 
dec_temp <- 1:1075
dec_temp <- dec_temp %% 12 == 0
dec_temp <- dec_temp[2:1075]
d1m <- coefficients(lm(pr1m[,2:26] -rf ~ jans_1m + dec_temp))
dec_temp <- 1:1063
dec_temp <- dec_temp %% 12 == 0
d212 <- coefficients(lm(pr212[,2:26] -rf ~ jans_temp + dec_temp))
d1360 <- coefficients(lm(pr1360[,2:26] -rf~ jans_temp[1:1015] + dec_temp[1:1015]))
dm1_means <- apply(d1m,1,mean)
d212_means <- apply(d212,1,mean)
d1360_means <- apply(d1360, 1, mean)

#d) 
collapse212 <- function(col) {
  temp <- 1 + col[(i-12):(i-2)]
  twelve_returns <- prod(temp,na.rm=T) - 1
  return(twelve_returns)
}
collapse1360 <- function(col) {
  temp <- 1 + col[(i-60):(i-13)]
  returns <- prod(temp, na.rm=T) - 1
  return(returns)
}

alpha <- 1:955
b1 <- 1:955
b2 <- 1:955
b3 <- 1:955
full <- as.matrix(full)
for(i in 61:1015) {
  coeff1 <- 1:75
  coeff2 <- 1:75
  coeff3 <- 1:75
  for(j in 1 : 75) {
    coeff1[j] <- full[i-1,j+1]
    coeff2[j] <- t(collapse212(full[,j+1]))
    coeff3[j] <- t(collapse1360(full[,j+1]))
  }
  alpha[i-60] <- coefficients(lm(full[i,2:76] ~ coeff1 + coeff2 + coeff3))[1]
  b1[i-60] <- coefficients(lm(full[i,2:76] ~ coeff1 + coeff2 + coeff3))[2]
  b2[i-60] <- coefficients(lm(full[i,2:76] ~ coeff1 + coeff2 + coeff3))[3]
  b3[i-60] <- coefficients(lm(full[i,2:76] ~ coeff1 + coeff2 + coeff3))[4]
}

b1_mean <- mean(b1)
b2_mean <- mean(b2)
b3_mean <- mean(b3)
a_mean <- mean(alpha)

b1_sd <- sd(b1)/sqrt(955)
b2_sd <- sd(b2)/sqrt(955)
b3_sd <- sd(b3)/sqrt(955)

b1_t <- mean(b1)/(sd(b1)/sqrt(955))
b2_t <- mean(b2)/(sd(b2)/sqrt(955))
b3_t <- mean(b3)/(sd(b3)/sqrt(955))
a_t <- mean(alpha)/(sd(alpha)/sqrt(955))

sharpe <- 


#e) 

#f) 

  
#g)
  set.seed(1)
means <- 1 : 5000
sds <- 1 : 5000
sharpes <- 1 : 5000
tstats <- 1 : 5000
for(i in 1 : 5000) {
  rand <- sample(1:75, 1015, "replace" = TRUE)
  strat <- 1:1015
  for(j in 1 : 1015) {
    strat[j] <- full[j,(rand[j]+1)] - rf
  }
  means[i] <- prod((1 + strat), na.rm=TRUE)^(1/1015) - 1 #geom avg to get avg ret
  sds[i] <- sd(strat,na.rm=TRUE)
  sharpes[i] <- means[i]/sds[i]
  tstats[i] <- (means[i])/(sds[i]/sqrt(1015))
}

hist(tstats)



