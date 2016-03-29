library(readr)
library(ggplot2)
library(MASS)

setwd("~/Applied_Quant_Finance")
rm(list=ls())

#Question 1B
raw_market <- read_csv("PS3_Market.csv")
raw_market <- raw_market / 100
raw_market$excess <- raw_market$`Mkt-RF` - raw_market$RF
rf_return <- mean(raw_market$RF)
raw_industry <- read_csv("PS3_Industry_Returns.csv")
raw_industry <- raw_industry / 100

raw_industry[raw_industry <= -.99] <- NA
beta_vector = as.numeric()

for (i in 2:50){
  temp <- data.frame(Date = raw_industry$Date, industry = raw_industry[,i])
  merged_temp <- merge(temp, raw_market, by="Date")
  merged_temp <- subset(merged_temp, !is.na(merged_temp$industry)) #LOOK HERE EMILY
  beta <- cov(merged_temp$industry, merged_temp$excess)/var(merged_temp$excess)
  beta_vector <- c(beta_vector, beta)
}

gam_zero_vector = c()
gam_one_vector = c()
for (i in 1:1069){
  returns_vector <- t(as.matrix(raw_industry[i,2:50]))
  reg_df <- data.frame(returns = returns_vector[,1], betas = beta_vector)
  reg_df <- subset(reg_df, !is.na(reg_df$returns))
  model <- lm(returns ~ betas, data = reg_df)
  gamma_zero <- coef(summary(model))[1,1]
  gam_zero_vector <- c(gam_zero_vector, gamma_zero)
  gamma_one <- coef(summary(model))[2,1]
  gam_one_vector <- c(gam_one_vector, gamma_one)
}

gam_zero_mean <- mean(gam_zero_vector)
gam_one_mean <- mean(gam_one_vector)

gz_st_err <- sd(gam_zero_vector) / sqrt(1069)
go_st_err <- sd(gam_one_vector) / sqrt(1069)

gz_t <- gam_zero_mean / gz_st_err
go_t <- gam_one_mean / go_st_err

gz_t_2 <- (gam_zero_mean - rf_return) / gz_st_err
go_t_2 <- (gam_one_mean - mean(raw_market$excess)) / go_st_err

#1C
avg_rtn <- sapply(raw_industry[,2:50], mean, na.rm=TRUE)
model <- lm(avg_rtn ~ beta_vector)
coef_vector <- c(coef(summary(model))[1,1], coef(summary(model))[2,1])
st_err_vector <- c(coef(summary(model))[1,2], coef(summary(model))[2,2])

#1D
plot_df = data.frame(returns = avg_rtn, beta = beta_vector)
first_plot <- ggplot(data = plot_df, aes(x = beta, y = returns)) + 
              geom_point(color = "firebrick") +
              geom_abline(slope = coef_vector[2], intercept = coef_vector[1], color = "blue")


#1F
industry_size <- read_csv("PS3_Industry_Size.csv")
industry_beme <- read_csv("PS3_Industry_BEME.csv")
industry_size[industry_size <= -.99] <- NA
industry_beme[industry_beme <= -.99] <- NA
expanded_beme <- industry_beme[1,]
for (i in 1:90){
  for (j in 1:12){
    expanded_beme <- rbind(expanded_beme, industry_beme[i,])
  }
}
expanded_beme <- expanded_beme[8:1081,1:50]
expanded_beme <- expanded_beme[1:1069, 1:50]

gam_zero_vector2 = c()
gam_one_vector2 = c()
gam_size_vector2 = c()
gam_beme_vector2 = c()

for (i in 13:1069){
  returns_vector <- t(as.matrix(raw_industry[i,2:50]))
  beme_vector <- t(as.matrix(expanded_beme[(i-12), 2:50]))
  size_vector <- t(as.matrix(industry_size[(i-1), 2:50]))
  
  reg_df <- data.frame(returns = returns_vector[,1], betas = beta_vector, known_size = size_vector[,1], beme = beme_vector[,1])
  reg_df <- subset(reg_df, !is.na(reg_df$returns))
  reg_df <- subset(reg_df, !is.na(reg_df$known_size))
  reg_df <- subset(reg_df, !is.na(reg_df$beme))
  
  model <- lm(returns ~ betas + log(known_size) + log(beme), data = reg_df)
  
  gamma_zero <- coef(summary(model))[1,1]
  gam_zero_vector2 <- c(gam_zero_vector2, gamma_zero)
  gamma_one <- coef(summary(model))[2,1]
  gam_one_vector2 <- c(gam_one_vector2, gamma_one)
  gamma_size <- coef(summary(model))[3,1]
  gam_size_vector2 <- c(gam_size_vector2, gamma_size)
  gamma_beme <- coef(summary(model))[4,1]
  gam_beme_vector2 <- c(gam_beme_vector2, gamma_beme)
}

g_z_mean2 <- mean(gam_zero_vector2)
g_o_mean2 <- mean(gam_one_vector2)
g_size_mean2 <- mean(gam_size_vector2)
g_beme_mean2 <- mean(gam_beme_vector2)


#2B
raw_portfolio <- read_csv("PS3_Custom_Returns.csv")
raw_portfolio <- raw_portfolio / 100
raw_portfolio[raw_portfolio <= -.99] <- NA

port_beta_vector <- as.numeric()
for (i in 2:26){
  temp <- data.frame(Date = raw_portfolio$Date, portfolio = raw_portfolio[,i])
  merged_temp <- merge(temp, raw_market, by="Date")
  merged_temp <- subset(merged_temp, !is.na(merged_temp$portfolio))
  beta <- cov(merged_temp$portfolio, merged_temp$excess)/var(merged_temp$excess)
  port_beta_vector <- c(port_beta_vector, beta)
}

port_gam_zero_vector = c()
port_gam_one_vector = c()
for (i in 1:1073){
  returns_vector <- t(as.matrix(raw_portfolio[i,2:26]))
  reg_df <- data.frame(returns = returns_vector[,1], betas = port_beta_vector)
  reg_df <- subset(reg_df, !is.na(reg_df$returns))
  model <- lm(returns ~ betas, data = reg_df)
  gamma_zero <- coef(summary(model))[1,1]
  port_gam_zero_vector <- c(gam_zero_vector, gamma_zero)
  gamma_one <- coef(summary(model))[2,1]
  port_gam_one_vector <- c(gam_one_vector, gamma_one)
}

port_gam_zero_mean <- mean(port_gam_zero_vector)
port_gam_one_mean <- mean(port_gam_one_vector)

port_gz_st_err <- sd(port_gam_zero_vector) / sqrt(1069)
port_go_st_err <- sd(port_gam_one_vector) / sqrt(1069)

port_gz_t <- port_gam_zero_mean / port_gz_st_err
port_go_t <- port_gam_one_mean / port_go_st_err

#2C
avg_port_rtn <- sapply(raw_portfolio[,2:26], mean, na.rm=TRUE)
model <- lm(avg_port_rtn ~ port_beta_vector)
port_coef_vector <- c(coef(summary(model))[1,1], coef(summary(model))[2,1])
port_st_err_vector <- c(coef(summary(model))[1,2], coef(summary(model))[2,2])

#2D
plot_port_df = data.frame(returns = avg_port_rtn, beta = port_beta_vector)
second_plot <- ggplot(data = plot_port_df, aes(x = beta, y = returns)) + 
               geom_point(color = "firebrick") +
               geom_abline(slope = port_coef_vector[2], intercept = port_coef_vector[1], color = "blue")
#2F
portfolio_size <- read_csv("PS3_Custom_Size.csv")
portfolio_beme <- read_csv("PS3_Custom_BEME.csv")
portfolio_size[portfolio_size <= -.99] <- NA
portfolio_beme[portfolio_beme <= -.99] <- NA
expanded_beme <- portfolio_beme[1,]
for (i in 1:90){
  for (j in 1:12){
    expanded_beme <- rbind(expanded_beme, portfolio_beme[i,])
  }
}
expanded_beme <- expanded_beme[8:1081,1:26]

port_gam_zero_vector2 = c()
port_gam_one_vector2 = c()
port_gam_size_vector2 = c()
port_gam_beme_vector2 = c()

for (i in 13:1069){
  returns_vector <- t(as.matrix(raw_portfolio[i,2:26]))
  beme_vector <- t(as.matrix(expanded_beme[(i-12), 2:26]))
  size_vector <- t(as.matrix(industry_size[(i-1), 2:26]))
  
  reg_df <- data.frame(returns = returns_vector[,1], betas = port_beta_vector, known_size = size_vector[,1], beme = beme_vector[,1])
  reg_df <- subset(reg_df, !is.na(reg_df$returns))
  reg_df <- subset(reg_df, !is.na(reg_df$known_size))
  reg_df <- subset(reg_df, !is.na(reg_df$beme))
  
  model <- lm(returns ~ betas + log(known_size) + log(beme), data = reg_df)
  
  gamma_zero <- coef(summary(model))[1,1]
  port_gam_zero_vector2 <- c(port_gam_zero_vector2, gamma_zero)
  gamma_one <- coef(summary(model))[2,1]
  port_gam_one_vector2 <- c(port_gam_one_vector2, gamma_one)
  gamma_size <- coef(summary(model))[3,1]
  port_gam_size_vector2 <- c(port_gam_size_vector2, gamma_size)
  gamma_beme <- coef(summary(model))[4,1]
  port_gam_beme_vector2 <- c(port_gam_beme_vector2, gamma_beme)
}

port_g_z_mean2 <- mean(port_gam_zero_vector2)
port_g_o_mean2 <- mean(port_gam_one_vector2)
port_g_size_mean2 <- mean(port_gam_size_vector2)
port_g_beme_mean2 <- mean(port_gam_beme_vector2)

#2C with tangency portfolio
vcov_mat = cov(raw_portfolio[2:26])
v_inv = ginv(vcov_mat)
avg_port_rtn2 = avg_port_rtn - rf_return
one_v = rep(1, 25)
tan_weights <- (v_inv %*% avg_port_rtn2) / (as.vector(t(one_v) %*% v_inv %*% avg_port_rtn2))

port_returns <- as.matrix(raw_portfolio[2:26])
tan_return <- as.data.frame(port_returns %*% tan_weights)
tan_return$Date = raw_portfolio$Date

tan_beta_vector <- as.numeric()
for (i in 2:26){
  temp <- data.frame(Date = raw_portfolio$Date, portfolio = raw_portfolio[,i])
  merged_temp <- merge(temp, tan_return, by="Date")
  merged_temp <- subset(merged_temp, !is.na(merged_temp$portfolio))
  weight_holder = rep(0, 25)
  weight_holder[(i-1)] = 1
  beta <- cov(merged_temp$portfolio, merged_temp$V1)/var(merged_temp$V1)
  tan_beta_vector <- c(tan_beta_vector, beta)
}

model <- lm(avg_port_rtn ~ tan_beta_vector)
tan_coef_vector <- c(coef(summary(model))[1,1], coef(summary(model))[2,1])
tan_st_err_vector <- c(coef(summary(model))[1,2], coef(summary(model))[2,2])

#2D with Tangency
plot_tan_df = data.frame(returns = avg_port_rtn, beta = tan_beta_vector)
third_plot <- ggplot(data = plot_tan_df, aes(x = beta, y = returns)) + 
              geom_point(color = "firebrick") +
              geom_abline(slope = tan_coef_vector[2], intercept = tan_coef_vector[1], color = "blue")

#2C with training set
even_years <- subset(raw_portfolio, as.integer(raw_portfolio$Date) %% 2 == 0) 
odd_years <- subset(raw_portfolio, as.integer(raw_portfolio$Date) %% 2 == 1)

even_training <- subset(even_years, as.integer(even_years$Date * 100) %% 2 == 1)
even_test <- subset(even_years, as.integer(even_years$Date * 100) %% 2 == 0)
odd_training <- subset(odd_years, as.integer(odd_years$Date * 100) %% 2 == 0)
odd_test <- subset(odd_years, as.integer(odd_years$Date * 100) %% 2 == 1)

training_df <- rbind(even_training, odd_training)
test_df <- rbind(even_test, odd_test)

vcov_mat2 = cov(training_df[2:26])
v_inv2 = ginv(vcov_mat2)
training_ret = as.matrix(training_df[,2:26])
avg_train_rtn <- sapply(training_df[,2:26], mean, na.rm=TRUE)
avg_train_rtn2 <- avg_train_rtn - rf_return
tan_weights2 <- (v_inv2 %*% avg_train_rtn2) / (as.vector(t(one_v) %*% v_inv2 %*% avg_train_rtn2))

vcov_mat3 = cov(test_df[2:26])
v_inv3 = ginv(vcov_mat3)
test_ret <- as.matrix(test_df[,2:26])
avg_test_rtn <- sapply(test_df[,2:26], mean, na.rm=TRUE)
avg_test_rtn2 <- avg_test_rtn - rf_return
tan_weights3 <- (v_inv2 %*% avg_test_rtn2) / (as.vector(t(one_v) %*% v_inv3 %*% avg_test_rtn2))

first_half_of_returns <- test_ret %*% tan_weights2
second_half <- training_ret %*% tan_weights3

total_ret <- as.numeric()
for (i in 1:1073){
  if (as.integer(raw_portfolio$Date) %% 2 == 0 && as.integer(raw_portfolio$Date * 100) %% 2 == 1){
    holder <- port_returns[i,] %*% tan_weights3
    total_ret <- c(total_ret, holder)
  }
  else if (as.integer(raw_portfolio$Date) %% 2 == 1 && as.integer(raw_portfolio$Date * 100) %% 2 == 0){
    holder <- port_returns[i,] %*% tan_weights3
    total_ret <- c(total_ret, holder)
  }
  else{
    holder <- port_returns[i,] %*% tan_weights2
    total_ret <- c(total_ret, holder)
  }
}

tan_beta_vector2 <- as.numeric()
for (i in 2:26){
  temp <- data.frame(Date = raw_portfolio$Date, portfolio = raw_portfolio[,i])
  merged_temp <- temp
  merged_temp$V1 <- total_ret
  merged_temp <- subset(merged_temp, !is.na(merged_temp$portfolio))
  beta <- cov(merged_temp$portfolio, merged_temp$V1)/var(merged_temp$V1)
  tan_beta_vector2 <- c(tan_beta_vector2, beta)
}

model <- lm(avg_port_rtn ~ tan_beta_vector2)
tan_coef_vector2 <- c(coef(summary(model))[1,1], coef(summary(model))[2,1])
tan_st_err_vector2 <- c(coef(summary(model))[1,2], coef(summary(model))[2,2])

plot_tan_df2 = data.frame(returns = avg_port_rtn, beta = tan_beta_vector2)
fourth_plot <- ggplot(data = plot_tan_df2, aes(x = beta, y = returns)) + 
               geom_point(color = "firebrick") +
               geom_abline(slope = tan_coef_vector2[2], intercept = tan_coef_vector2[1], color = "blue")