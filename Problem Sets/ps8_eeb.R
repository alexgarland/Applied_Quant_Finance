library(MASS)
library(readr)
library(lubridate)
library(moments)

rm(list=ls())
setwd("C:/Users/Emily/Documents/AppliedQuantFin")

djcs <- read_csv("djcs_factors.csv")
hfri <- read_csv("hfri_factors.csv")
equity_factors <- read_csv("equity_factors.csv")
equity_factors <- equity_factors[,1:9]
asset_factors <- read_csv("asset_factors.csv")
mom <- read_csv("mom_factors.csv")
mom <- mom[,1:6]
ff <- read_csv("ff_factors.csv")
ff[ff < -90] <- NA
ff[,2:8] <- ff[,2:8] / 100
rf <- mean(ff$RF)

# a)
all <- merge(equity_factors, asset_factors, by="Date")

means <- 1:16
counts <- 1:16
for(i in 2:17) {
  counts[i-1] <- length(na.omit(all[,i]))
  means[i-1] <- (prod(all[,i] + 1, na.rm=T)^(1/length(na.omit(all[,i])))-1)
}
sds <- sapply(all[,2:17], sd, na.rm=T)
kurt <- sapply(all[,2:17], kurtosis, na.rm=T)
skew <- sapply(all[,2:17], skewness, na.rm=T)
sharpes <- means/sds

ts <- means/(sds/sqrt(counts))

interval_normal_mean <- means/sds
interval_normal_sd <- 1/(colSums(!is.na(all[,2:17]))) * (1 + (means)^2 / (2*sds^2))
in_high <- interval_normal_mean + 3*interval_normal_sd
in_low <- interval_normal_mean - 3*interval_normal_sd

interval_general_mean <- sharpes
interval_general_sd <- 1/(colSums(!is.na(all[,2:17]))) * (1 + (sharpes)^2/4 * (kurt - 1) - sharpes * skew)
ig_high <- interval_general_mean + 3*interval_general_sd
ig_low <- interval_general_mean - 3*interval_general_sd

#no outliers somehow.  No 3-sigma events????
instant_sharpes <- all[,2:17]/sds
n_out <- 0
total <- 0
for(i in 1:16) {
  temp <- na.omit(instant_sharpes[,i])
  total <- total + length(temp)
  n_out <- n_out + length(temp[temp > in_high[i] || temp < in_low[i]])
}

normal_outliers <- sharpes[sharpes > in_high || sharpes < in_low]
general_outliers <- sharpes[sharpes > ig_high || sharpes < ig_low]

# b) 
#seemingly not but that's not expected--look for FI discrepancies
odds <- rep(1,1,15)
evens<- rep(2,2,16)
mean_val <- means[odds]
mean_mom <- means[evens]
t_val_prem <- (max(mean_val) - mean(mean_val))/sd(mean_val)
t_mom_prem <- (min(mean_mom) - mean(mean_mom))/sd(mean_mom)

mean_val_eq <- mean_val[1:5]
mean_mom_eq <- mean_mom[1:5]

t_val_prem_eq <- (max(mean_val_eq) - mean(mean_val_eq))/sd(mean_val_eq)
t_mom_prem_eq <- (min(mean_mom_eq) - mean(mean_mom_eq))/sd(mean_mom_eq)


# c) 
max_drawdown <- function(returns) {
  local_max <- 0
  local_min <- 0
  time_of_max <- 1
  largest_drawdown <- 0
  current_drawdown <- 0
  for(i in 1 : length(returns)) {
    if(!is.na(returns[i])){
      if(returns[i] > local_max) {
        local_max <- returns[i]
        time_of_max <- i
      } else if (returns[i] < local_min) {
        local_min <- returns[i]
        time_of_dd <- i - time_of_max
        current_drawdown <- local_max - local_min
      }
      
      if(current_drawdown > largest_drawdown){
        largest_drawdown <- current_drawdown
        dd_time <- time_of_dd
      }
    }
  }
  output <- t(list(largest_drawdown, dd_time))
  return(output)
}

max_dds <- apply(all[,2:17], 2, max_drawdown)

# d) 
value_ports <- equity_factors[,c("Date","VAL^US^EQ","VAL^UK^EQ","VAL^EU^EQ","VAL^JP^EQ")]
value_ports <- merge(value_ports, asset_factors[,c("Date","VAL^EQ","VAL^FX","VAL^FI","VAL^CM")],by="Date")

mom_ports <- equity_factors[,c("Date","MOM^US^EQ","MOM^UK^EQ","MOM^EU^EQ","MOM^JP^EQ")]
mom_ports <- merge(mom_ports, asset_factors[,c("Date","MOM^EQ","MOM^FX","MOM^FI","MOM^CM")],by="Date")

value_means <- sapply(value_ports[,2:9], mean, na.rm=T)
value_sds <- sapply(value_ports[,2:9], sd, na.rm=T)
value_sd_sum <- sum(1/value_sds)
weighted_value_mean <- value_means %*% ((1/value_sds)/value_sd_sum)
weighted_value_t <- (value_means * (value_sds/value_sd_sum)) / (value_sds/sqrt(527))
weighted_value_port <- apply(na.omit(value_ports[,2:9] * (value_sds/value_sd_sum)), 1, sum)
wvp_sd <- sd(weighted_value_port)

mom_means <- sapply(mom_ports[,2:9], mean, na.rm=T)
mom_sds <- sapply(mom_ports[,2:9], sd, na.rm=T)
mom_sd_sum <- sum(1/mom_sds)
weighted_mom_mean <- mom_means %*% ((1/mom_sds)/mom_sd_sum)
weighted_mom_t <- (mom_means * (mom_sds/mom_sd_sum)) / (mom_sds/sqrt(527))
weighted_mom_port <- apply(na.omit(mom_ports[,2:9] * (mom_sds/mom_sd_sum)), 1, sum)
wmp_sd <- sd(weighted_mom_port)

# e) 
combined <- 0.5*value_ports + 0.5*mom_ports
count_comb <- apply(combined[,2:9], 2, function(x) length(which(!is.na(x))))
comb_means <- sapply(combined[,2:9], mean, na.rm=T)
comb_sds <- sapply(combined[,2:9], sd, na.rm=T)
comb_sharpes <- comb_means / (comb_sds)
comb_kurts <- sapply(combined[,2:9], kurtosis, na.rm=T)
comb_skews <- sapply(combined[,2:9], skewness, na.rm=T)
comb_ts <- comb_means/(comb_sds/sqrt(527))
weighted_value_t <- (comb_means) / (comb_sds/sqrt(527)) 
comb_dd <- sapply(combined[,2:9], max_drawdown)
nav_values_c <- apply(na.omit(combined), 2, build_nav)
drawdown_list_c <- apply(nav_values_c, 2, find_mdd)

comb_normal <- (1+comb_means^2 / 2*comb_sds^2) * 1/(count_comb-1)
comb_general <- (1/(count_comb-1)) * (1 + (comb_sharpes^2 / 4) * (comb_kurts - 1) -
                                    comb_sharpes * comb_skews)

# f) 
comb_sd_sums <- sum(1/comb_sds)
global_mean <- comb_means %*% ((1/comb_sds) / comb_sd_sums)
global_sd <- comb_sds %*% ((1/comb_sds) / comb_sd_sums)
global_sharpe <- global_mean/global_sd
global_t <- global_sharpe/sqrt(527)
global_skew <- comb_skews %*% ((1/comb_sds) / comb_sd_sums)
global_kurt <- comb_kurts %*% ((1/comb_sds) / comb_sd_sums)

glob_normal <- (1+global_mean^2 / 2*global_sd^2) * 1/(527-1)
glob_general <- (1/(527-1)) * (1 + (global_sharpe^2 / 4) * (global_kurt - 1) -
                                    global_sharpe * global_skew)

# g)
#holy sharpe ratios

# h)
covariances <- cov(na.omit(merge(value_ports, mom_ports, by="Date")[,2:17]))
covs <- cov(weighted_value_port, weighted_mom_port)
rets <- append(value_means, mom_means)
one_v <- rep(1,16)
tan_weights <- (ginv(covariances) %*% rets) / (as.vector(t(one_v) %*% ginv(covariances) %*% rets))
tan_rets <- means %*% tan_weights
tan_sds <- t(tan_weights) %*% covariances %*% tan_weights

weights <- seq(-5, 5, .01)
efficient_frontier <- data.frame(ret = numeric(), sd = numeric())
for (w in weights){
  multi_sd <- (w^2 * sd(weighted_value_port)^2 + (1-w)^2 * sd(weighted_mom_port)^2 + 2*w*(1-w)*covs)^(1/2)
  multi_return <- w*weighted_value_mean + (1-w)*weighted_mom_mean
  temp = data.frame(ret = multi_return, sd = multi_sd)
  efficient_frontier <- rbind(efficient_frontier, temp)
}
plot(efficient_frontier$sd, efficient_frontier$ret, type = "l")
points(mom_means, mom_sds)

# i)

# j)
covs <- cov(na.omit(all[,2:17]))

# k)




