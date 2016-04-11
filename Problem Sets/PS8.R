library(readr)
library(MASS)
library(moments)
library(stringr)

setwd("~/Applied_Quant_Finance/Problem Sets")
rm(list=ls())
set.seed(1)

#Part A
val_mom <- read.csv("PS8_Val_Mom.csv", stringsAsFactors = F)

holder <- apply(val_mom[,2:17], 2, str_replace_all, fixed("%"), "")
holder <- apply(holder, 2, as.numeric)/100
val_mom[,2:17] <- holder

mean_returns <- apply(holder, 2, mean, na.rm=T)
sd_returns <- apply(holder, 2, sd, na.rm=T)
count <- apply(holder, 2, function(x) length(which(!is.na(x))))
t_returns <- mean_returns / (sd_returns / sqrt(count))
sharpe_returns <- mean_returns / sd_returns
skew_returns <- apply(holder, 2, skewness, na.rm=T)
kurt_returns <- apply(holder, 2, kurtosis, na.rm=T)

sr_sd_normal <- (1+mean_returns^2 / 2*sd_returns^2) * 1/(count-1)
sr_sd_general <- (1/(count-1)) * (1 + (sharpe_returns^2 / 4) * (kurt_returns - 1) -
                                    sharpe_returns * skew_returns)

sr_t_norm <- sharpe_returns / sr_sd_normal
sr_t_general <- sharpe_returns / sr_sd_general

#Part B
even <- seq(2, 16, 2)
odd <- seq(1, 15, 2)
mean_val <- mean_returns[odd]
mean_mom <- mean_returns[even]
t_val_prem <- (max(mean_val) - mean(mean_val))/sd(mean_val)
t_mom_prem <- (min(mean_mom) - mean(mean_mom))/sd(mean_mom)

mean_val_eq <- mean_val[1:5]
mean_mom_eq <- mean_mom[1:5]

t_val_prem_eq <- (max(mean_val_eq) - mean(mean_val_eq))/sd(mean_val_eq)
t_mom_prem_eq <- (min(mean_mom_eq) - mean(mean_mom_eq))/sd(mean_mom_eq)

#Part C
nav_values <- apply(holder, 2, build_nav)
drawdown_list <- apply(nav_values, 2, find_mdd)

val_weights <- rep(1, 8)
mom_weights <- rep(1, 8)

val_weights <- val_weights / apply(holder[,odd], 2, sd, na.rm=T)
val_weights <- val_weights / sum(val_weights)

mom_weights <- mom_weights / apply(holder[,even], 2, sd, na.rm=T)
mom_weights <- mom_weights / sum(mom_weights)

val_only <- holder[,odd]
mom_only <- holder[,even]

weighted_val <- val_only * val_weights
weighted_mom <- mom_only * mom_weights

val_everywhere <- apply(weighted_val, 1, sum)
mom_everywhere <- apply(weighted_mom, 1, sum)
val_everywhere <- val_everywhere[!is.na(val_everywhere)]
mom_everywhere <- mom_everywhere[!is.na(mom_everywhere)]

mean_val_e <- mean(val_everywhere)
mean_mom_e <- mean(mom_everywhere)

sd_val_e <- sd(val_everywhere)
sd_mom_e <- sd(mom_everywhere)

t_val_e <- mean_val_e / (sd_val_e / sqrt(length(val_everywhere)))
t_mom_e <- mean_mom_e / (sd_mom_e / sqrt(length(mom_everywhere)))

sr_val_e <- mean_val_e / sd_val_e
sr_mom_e <- mean_mom_e / sd_mom_e

kurt_val_e <- kurtosis(val_everywhere)
kurt_mom_e <- kurtosis(mom_everywhere)

skew_val_e <- skewness(val_everywhere)
skew_mom_e <- skewness(mom_everywhere)

#Question E
combined_val_mom <- val_only * .5 + .5 * mom_only
vm_mean <- apply(combined_val_mom, 2, mean, na.rm=T)
vm_sd <- apply(combined_val_mom, 2, sd, na.rm=T)
vm_count <- apply(combined_val_mom, 2, function(x) length(which(!is.na(x))))
vm_t <- vm_mean / (vm_sd / sqrt(vm_count))
vm_sr <- vm_mean / vm_sd
