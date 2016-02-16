library(readr)
library(ggplot2)

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
