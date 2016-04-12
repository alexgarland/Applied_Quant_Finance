library(readr)
library(MASS)
library(moments)
library(stringr)
library(ggplot2)

setwd("~/Applied_Quant_Finance/Problem Sets")
rm(list=ls())
source('~/Applied_Quant_Finance/Problem Sets/PS8_Helpers.R')
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

#Part D
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

#Question F
combined_weights <- rep(1, 8)
combined_weights <- combined_weights / vm_sd
combined_weights <- combined_weights / sum(combined_weights)

weighted_val_mom <- combined_val_mom * combined_weights
val_mom2 <- apply(weighted_val_mom, 1, sum)
val_mom2 <- val_mom2[!is.na(val_mom2)]

mean_all <- mean(val_mom2)
sd_all <- sd(val_mom2)
t_all <- mean_all / (sd_all / sqrt(length(val_mom2)))
sr_all <- mean_all / sd_all
skew_all <- skewness(val_mom2)
kurt_all <- kurtosis(val_mom2)

#Question g
holder <- holder[133:527,]
vcov_mat <- cov(holder)
v_inv <- ginv(vcov_mat)
one_v <- rep(1, 16)
tan_weights <- (v_inv %*% mean_returns) / (as.vector(t(one_v) %*% v_inv %*% mean_returns))
mvp_weights <- (v_inv %*% one_v) / as.vector((t(one_v) %*% v_inv %*% one_v))

mvp_return <- mean_returns %*% mvp_weights
tan_return <- mean_returns %*% tan_weights

mvp_sd <- (t(mvp_weights) %*% vcov_mat %*% mvp_weights)^(1/2)
tan_sd <- (t(tan_weights) %*% vcov_mat %*% tan_weights)^(1/2)
cov_two_port <- t(tan_weights) %*% vcov_mat %*% mvp_weights

weights <- seq(-5, 5, .01)
efficient_frontier <- data.frame(ret = numeric(), sd = numeric())
for (w in weights){
  multi_sd <- (w^2 * mvp_sd^2 + (1-w)^2 * tan_sd^2 + 2*w*(1-w)*cov_two_port)^(1/2)
  multi_return <- w*mvp_return + (1-w)*tan_return
  temp = data.frame(ret = multi_return, sd = multi_sd)
  efficient_frontier <- rbind(efficient_frontier, temp)
}

factors <- data.frame(ret = mean_returns, sd = sd_returns)

first_plot <- ggplot(data = efficient_frontier, aes(x = sd, y = ret)) + 
  geom_point(color="firebrick") + 
  geom_point(aes(x=sd, y = ret), data = factors, color="blue") +
  geom_point(aes(x=sd_all, y=mean_all), color="green")
  labs(x="Standard Deviation", y="Return")

#Question i
holder <- val_mom[133:527,]
odd_months <- seq(1, 395, 2)
even_months <- seq(2, 394, 2)
odd_holder <- holder[odd_months,]
even_holder <- holder[even_months,]

odd_cov <- cov(odd_holder[,2:17])
even_cov <- cov(even_holder[,2:17])

odd_vinv <- ginv(odd_cov)
even_vinv <- ginv(even_cov)

odd_means <- apply(odd_holder[,2:17], 2, mean)
even_means <- apply(even_holder[,2:17], 2, mean)

odd_tan_weights <- (odd_vinv %*% odd_means) / (as.vector(t(one_v) %*% odd_vinv %*% odd_means))
even_tan_weights <- (even_vinv %*% even_means) / (as.vector(t(one_v) %*% even_vinv %*% even_means))

odd_returns <- even_holder[,2:17] * odd_tan_weights
even_returns <- odd_holder[,2:17] * even_tan_weights

oos_returns <- rbind(odd_returns, even_returns)
oos_returns <- apply(oos_returns, 1, sum)
mean_oos <- mean(oos_returns)
sd_oos <- sd(oos_returns)

second_plot <- ggplot(data = efficient_frontier, aes(x = sd, y = ret)) + 
  geom_point(color="firebrick") + 
  geom_point(aes(x=sd, y = ret), data = factors, color="blue") +
  geom_point(aes(x=sd_all, y=mean_all), color="green") +
  geom_point(aes(x=sd_oos, y=mean_oos), color="pink") + 
  labs(x="Standard Deviation", y="Return")

#Question j
cor_matrix <- cor(holder[,2:17])

#Question k
model <- lm(as.matrix(holder[,2:17]) ~ val_everywhere + mom_everywhere)
predictions <- predict(model)
mean_predicts <- apply(predictions, 2, mean)
plot(mean_predicts)
points(mean_returns, col ="red")

#Question l
historical_factors <- read_csv("PS8_FF.csv")
historical_factors <- historical_factors/100

#CAPM
capm_stats <- list()
ff4_stats <- list()
amp3_stats <- list()
amp_ts_stats <- list()
amp_all_stats <- list()

tsmom <- read_csv("PS8_TSMom.csv")
alex <- apply(tsmom[,2:6], 2, str_replace_all, fixed("%"), "")
alex <- apply(alex, 2, as.numeric)/100
tsmom[,2:6] <- alex

temp <- historical_factors[703:1073,]
holder <- holder[25:395,]

for (i in 2:17){
  capm <- lm(holder[,i] ~ temp$`Mkt-RF`)
  ff_4 <- lm(holder[,i] ~ temp$`Mkt-RF` + temp$SMB + temp$HML + temp$UMD)
  amp_3 <- lm(holder[,i] ~ temp$`Mkt-RF` + val_everywhere[25:395] + mom_everywhere[25:395])
  amp_ts <- lm(holder[,i] ~ temp$`Mkt-RF` + val_everywhere[25:395] + mom_everywhere[25:395] + tsmom$TSMOM)
  amp_ts_ex <- lm(holder[,i] ~ temp$`Mkt-RF` + val_everywhere[25:395] + mom_everywhere[25:395] + tsmom$TSMOM +
                    temp$STREV + temp$LTREV)
  capm_stats[[(i-1)]] <- c(coef(capm)[1], summary(capm)[8])
  ff4_stats[[(i-1)]] <- c(coef(ff_4)[1], summary(ff_4)[8])
  amp3_stats[[(i-1)]] <- c(coef(amp_3)[1], summary(amp_3)[8])
  amp_ts_stats[[(i-1)]] <- c(coef(amp_ts)[1], summary(amp_ts)[8])
  amp_all_stats[[(i-1)]] <- c(coef(amp_ts_ex)[1], summary(amp_ts_ex)[8])
}

temp_vcov <- cov(holder[,2:17])
market_sharpe <- mean(historical_factors$`Mkt-RF`) / sd(historical_factors$`Mkt-RF`)
temps <- as.data.frame(lapply(capm_stats, function(x) head(x, 1)))
temps <- t(temps)
f_capm <- our_f_test(temps[,1], ginv(temp_vcov), 361, market_sharpe, 16)

temps <- as.data.frame(lapply(ff4_stats, function(x) head(x, 1)))
temps <- t(temps)
f_ff4 <- our_f_test(temps[,1], ginv(temp_vcov), 361, market_sharpe, 16)

temps <- as.data.frame(lapply(amp3_stats, function(x) head(x, 1)))
temps <- t(temps)
f_amp3 <- our_f_test(temps[,1], ginv(temp_vcov), 361, market_sharpe, 16)

temps <- as.data.frame(lapply(amp_ts_stats, function(x) head(x, 1)))
temps <- t(temps)
f_amp3_ts <- our_f_test(temps[,1], ginv(temp_vcov), 361, market_sharpe, 16)

temps <- as.data.frame(lapply(amp_all_stats, function(x) head(x, 1)))
temps <- t(temps)
f_amp3_ex <- our_f_test(temps[,1], ginv(temp_vcov), 361, market_sharpe, 16)

#Question m
eq_model <- lm(holder$MOM.EQ ~ tsmom$`TSMOM^EQ` + historical_factors$`Mkt-RF`[703:1073])
cm_model <- lm(holder$MOM.CM ~ tsmom$`TSMOM^CM` + historical_factors$`Mkt-RF`[703:1073])
fi_model <- lm(holder$MOM.FI ~ tsmom$`TSMOM^FI` + historical_factors$`Mkt-RF`[703:1073])
fx_model <- lm(holder$MOM.FX ~ tsmom$`TSMOM^FX` + historical_factors$`Mkt-RF`[703:1073])

intercepts <- c(coef(eq_model)[1], coef(cm_model)[1], coef(fi_model)[1], coef(fx_model)[1])

eq2 <- lm(tsmom$`TSMOM^EQ` ~ holder$MOM.EQ + historical_factors$`Mkt-RF`[703:1073])
cm2 <- lm(tsmom$`TSMOM^CM` ~ holder$MOM.CM + historical_factors$`Mkt-RF`[703:1073])
fi2 <- lm(tsmom$`TSMOM^FI` ~ holder$MOM.FI + historical_factors$`Mkt-RF`[703:1073])
fx2 <- lm(tsmom$`TSMOM^FX` ~ holder$MOM.FX + historical_factors$`Mkt-RF`[703:1073])

intercepts2 <- c(coef(eq2)[1], coef(cm2)[1], coef(fi2)[1], coef(fx2)[1])

mom_holder <- data.frame(holder$MOM.EQ, holder$MOM.CM, holder$MOM.FI, holder$MOM.FX)
vcov_mom <- cov(mom_holder)
vcov_tsmom <- cov(tsmom[,3:6])

f_mom <- our_f_test(intercepts, ginv(vcov_mom), 371, market_sharpe, 4)
f_tsmom <- our_f_test(intercepts2, ginv(vcov_tsmom), 371, market_sharpe, 4)

#Question n
hfri_index <- read_csv("PS8_HFRI.csv")
djcs_index <- read_csv("PS8_DJCS.csv")

super_temp <- apply(hfri_index[,2:11], 2, str_replace_all, fixed("%"), "")
super_temp <- as.numeric(super_temp) / 100
hfri_index[,2:11] <- super_temp
hfri_index[,2:11] <- hfri_index[,2:11] - mean(historical_factors$RF)

super_temp <- apply(djcs_index[,2:11], 2, str_replace_all, fixed("%"), "")
super_temp <- as.numeric(super_temp) / 100
djcs_index[,2:11] <- super_temp
djcs_index[,2:11] <- djcs_index[,2:11] - mean(historical_factors$RF)

defense_stats <- read.csv("PS8_Bab.csv", stringsAsFactors = F)
super_temp <- apply(defense_stats[,2:4], 2, str_replace_all, fixed("%"), "")
super_temp[nchar(super_temp) == 0] <- NA
defense_stats$BAB <- as.numeric(super_temp[,1])/100
defense_stats$Defensive.Quality <- as.numeric(super_temp[,2])/100
defense_stats$Carry <- as.numeric(super_temp[,3])/100

#HFRI models
hfri_model1 <- lm(as.matrix(hfri_index[1:311,2:11]) ~ historical_factors$`Mkt-RF`[763:1073])

hfri_model2 <- lm(as.matrix(hfri_index[2:311,2:11]) ~ historical_factors$`Mkt-RF`[764:1073] +
                    historical_factors$`Mkt-RF`[763:1072])

hfri_model3 <- lm(as.matrix(hfri_index[2:311,2:11]) ~ historical_factors$`Mkt-RF`[764:1073] +
                    historical_factors$`Mkt-RF`[763:1072] + historical_factors$SMB[764:1073] +
                    historical_factors$HML[764:1073 + historical_factors$UMD[764:1073]])

hfri_model4 <- lm(as.matrix(hfri_index[2:311,2:11]) ~ historical_factors$`Mkt-RF`[764:1073] +
                    historical_factors$`Mkt-RF`[763:1072] + val_everywhere[86:395] + mom_everywhere[86:395])

hfri_model5 <- lm(as.matrix(hfri_index[2:311,2:11]) ~ historical_factors$`Mkt-RF`[764:1073] +
                    historical_factors$`Mkt-RF`[763:1072] + val_everywhere[86:395] + mom_everywhere[86:395] +
                    tsmom$TSMOM[62:371])

hfri_model6 <- lm(as.matrix(hfri_index[2:311,2:11]) ~ historical_factors$`Mkt-RF`[764:1073] +
                    historical_factors$`Mkt-RF`[763:1072] + val_everywhere[86:395] + mom_everywhere[86:395] +
                    tsmom$TSMOM[62:371] + historical_factors$STREV[764:1073] + historical_factors$LTREV[764:1073])

hfri_model7 <- lm(as.matrix(hfri_index[2:311,2:11]) ~ historical_factors$`Mkt-RF`[764:1073] +
                    historical_factors$`Mkt-RF`[763:1072] + val_everywhere[86:395] + mom_everywhere[86:395] +
                    tsmom$TSMOM[62:371] + historical_factors$STREV[764:1073] + historical_factors$LTREV[764:1073] +
                    defense_stats$BAB[764:1073] + defense_stats$Defensive.Quality[764:1073] + 
                    defense_stats$Carry[764:1073])

djcs_model1 <- lm(as.matrix(djcs_index[49:311,2:11]) ~ historical_factors$`Mkt-RF`[811:1073])

djcs_model2 <- lm(as.matrix(djcs_index[50:311,2:11]) ~ historical_factors$`Mkt-RF`[812:1073] +
                    historical_factors$`Mkt-RF`[811:1072])

djcs_model3 <- lm(as.matrix(djcs_index[50:311,2:11]) ~ historical_factors$`Mkt-RF`[812:1073] +
                    historical_factors$`Mkt-RF`[811:1072] + historical_factors$SMB[812:1073] +
                    historical_factors$HML[812:1073 + historical_factors$UMD[812:1073]])

djcs_model4 <- lm(as.matrix(djcs_index[50:311,2:11]) ~ historical_factors$`Mkt-RF`[812:1073] +
                    historical_factors$`Mkt-RF`[811:1072] + val_everywhere[134:395] + mom_everywhere[134:395])

djcs_model5 <-  lm(as.matrix(djcs_index[50:311,2:11]) ~ historical_factors$`Mkt-RF`[812:1073] +
                     historical_factors$`Mkt-RF`[811:1072] + val_everywhere[134:395] + mom_everywhere[134:395] +
                     tsmom$TSMOM[110:371])

djcs_model6 <- lm(as.matrix(hfri_index[50:311,2:11]) ~ historical_factors$`Mkt-RF`[812:1073] +
                    historical_factors$`Mkt-RF`[811:1072] + val_everywhere[134:395] + mom_everywhere[134:395] +
                    tsmom$TSMOM[110:371] + historical_factors$STREV[812:1073] + historical_factors$LTREV[812:1073])


djcs_model7 <- lm(as.matrix(hfri_index[50:311,2:11]) ~ historical_factors$`Mkt-RF`[812:1073] +
                    historical_factors$`Mkt-RF`[811:1072] + val_everywhere[134:395] + mom_everywhere[134:395] +
                    tsmom$TSMOM[110:371] + historical_factors$STREV[812:1073] + historical_factors$LTREV[812:1073] +
                    defense_stats$BAB[812:1073] + defense_stats$Defensive.Quality[812:1073] + 
                    defense_stats$Carry[812:1073])

intercepts1 <- coef(hfri_model1)[1,]
intercepts2 <- coef(hfri_model2)[1,]
intercepts3 <- coef(hfri_model3)[1,]
intercepts4 <- coef(hfri_model4)[1,]
intercepts5 <- coef(hfri_model5)[1,]
intercepts6 <- coef(hfri_model6)[1,]
intercepts7 <- coef(hfri_model7)[1,]

vcov_hedge <- cov(hfri_index[,2:11])

f1 <- our_f_test(intercepts1, ginv(vcov_hedge), 310, market_sharpe, 10)
f2 <- our_f_test(intercepts2, ginv(vcov_hedge), 310, market_sharpe, 10)
f3 <- our_f_test(intercepts3, ginv(vcov_hedge), 310, market_sharpe, 10)
f4 <- our_f_test(intercepts4, ginv(vcov_hedge), 310, market_sharpe, 10)
f5 <- our_f_test(intercepts5, ginv(vcov_hedge), 310, market_sharpe, 10)
f6 <- our_f_test(intercepts6, ginv(vcov_hedge), 310, market_sharpe, 10)
f7 <- our_f_test(intercepts7, ginv(vcov_hedge), 310, market_sharpe, 10)

#Question o
all_factors_df <- data.frame(date = historical_factors$Date[763:1073], RMRF = historical_factors$`Mkt-RF`[763:1073],
                             SMB = historical_factors$SMB[763:1073], val = val_everywhere[85:395],
                             mom = mom_everywhere[85:395], BAB = defense_stats$BAB[763:1073],
                             Qual = defense_stats$Defensive.Quality[763:1073], Carry = defense_stats$Carry[763:1073],
                             STREV = historical_factors$STREV[763:1073], LTEV = historical_factors$LTREV[763:1073])

one_v <- rep(1, 9)
mean_factors <- apply(all_factors_df[,2:10], 2, mean)
vcov_mat2 <- cov(all_factors_df[,2:10])
v_inv2 <- ginv(vcov_mat2)
tan_weights2 <- (v_inv2 %*% mean_factors) / (as.vector(t(one_v) %*% v_inv2 %*% mean_factors))
mvp_weights2 <- (v_inv2 %*% one_v) / as.vector((t(one_v) %*% v_inv2 %*% one_v))

mvp_return2 <- mean_factors %*% mvp_weights2
tan_return2 <- mean_factors %*% tan_weights2

mvp_sd2 <- (t(mvp_weights2) %*% vcov_mat2 %*% mvp_weights2)^(1/2)
tan_sd2 <- (t(tan_weights2) %*% vcov_mat2 %*% tan_weights2)^(1/2)
cov_two_port2 <- t(tan_weights2) %*% vcov_mat2 %*% mvp_weights2

weights <- seq(-5, 5, .01)
efficient_frontier2 <- data.frame(ret = numeric(), sd = numeric())
for (w in weights){
  multi_sd <- (w^2 * mvp_sd2^2 + (1-w)^2 * tan_sd2^2 + 2*w*(1-w)*cov_two_port2)^(1/2)
  multi_return <- w*mvp_return2 + (1-w)*tan_return2
  temp = data.frame(ret = multi_return, sd = multi_sd)
  efficient_frontier2 <- rbind(efficient_frontier2, temp)
}

this_plot <- ggplot(data = efficient_frontier2, aes(x = sd, y = ret)) + 
  geom_point(color="firebrick") + 
  geom_point(aes(x=sd, y = ret), data = factors, color="blue") + 
  geom_point(aes(x=tan_sd2, y=tan_return2), color="green")
labs(x="Standard Deviation", y="Return")
print(this_plot)

#Question p
even <- seq(2, 310, 2)
odd <- seq(1, 311, 2)

odd_factors <- all_factors_df[odd,]
even_factors <- all_factors_df[even,]

mean_odd <- apply(odd_factors[,2:10], 2, mean)
mean_even <- apply(even_factors[,2:10], 2, mean)

vcov_mat2o <- cov(odd_factors[,2:10])
v_inv2o <- ginv(vcov_mat2o)
tan_weights2o <- (v_inv2o %*% mean_odd) / (as.vector(t(one_v) %*% v_inv2o %*% mean_odd))


vcov_mat2e <- cov(even_factors[,2:10])
v_inv2e <- ginv(vcov_mat2e)
tan_weights2e <- (v_inv2e %*% mean_even) / (as.vector(t(one_v) %*% v_inv2e %*% mean_even))

odd_ret <-  odd_factors[,2:10] * tan_weights2e
even_ret <- even_factors[,2:10] * tan_weights2o
odd_ret <- apply(odd_ret, 1, sum)
even_ret <- apply(even_ret, 1, sum)
total <- c(odd_ret, even_ret)
total_mean <- mean(total)
total_sd <- sd(total)

this_plot2 <- ggplot(data = efficient_frontier2, aes(x = sd, y = ret)) + 
  geom_point(color="firebrick") + 
  geom_point(aes(x=sd, y = ret), data = factors, color="blue") + 
  geom_point(aes(x=tan_sd2, y=tan_return2), color="green") + 
  geom_point(aes(x=total_sd, y=total_mean), color="pink")
labs(x="Standard Deviation", y="Return")
print(this_plot2)
