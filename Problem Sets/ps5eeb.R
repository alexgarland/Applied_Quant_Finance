library(readr)
library(ggplot2)
library(MASS)

setwd("C:/Users/Emily/Documents/AppliedQuantFin")
rm(list=ls())

ff <- read_csv("ps5ff.csv")
#ff <- as.matrix(ff[,2:7])
ff[,2:6] <- ff[,2:6] / 100
rf <- mean(ff[,5])
recessions <- subset(ff, as.integer(ff$recession) == 1)
no_rec <- subset(ff, as.integer(ff$recession) == 0)

#1a
rec_means <- apply(recessions[,2:6], 2, mean, na.rm=TRUE)

#1b
vw <- read_csv("ps3vw25.csv")
vw[,2:26] <- vw[,2:26] / 100
mom_vw <- read_csv("ps5momvw.csv")
mom_vw[,2:26] <- mom_vw[,2:26] /100

#value is large book to market
small_value <- lm(vw[,6] - rf ~ ff$recession)
small_growth <- lm(vw[,2] - rf ~ ff$recession)
large_value <- lm(vw[,26] - rf ~ ff$recession)
large_growth <- lm(vw[,22] -rf ~ff$recession)

small_loser <- lm(mom_vw[7:1069,2] - rf ~ ff$recession[7:1069])
small_winner <- lm(mom_vw[7:1069,6] - rf ~ ff$recession[7:1069])
large_loser <- lm(mom_vw[7:1069,22] - rf ~ ff$recession[7:1069]) ##LOOK AT MEAN
large_winner <- lm(mom_vw[7:1069,26] - rf ~ ff$recession[7:1069])

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
expanded_beme <- expanded_beme[8:1080,];
expanded_beme <- as.matrix(expanded_beme);
array_beme <- as.matrix(vw[2:26])
array_beme <- array_beme - rf

betas <- data.frame(Beta1 = as.numeric(), Beta2 = as.numeric(), Beta3 = as.numeric())

for (i in 2:26){
  temp <- data.frame(Date = vw$Date, Port = vw[,i])
  merged_temp <- merge(temp, ff, by="Date")
  model <- lm(merged_temp$Port ~ merged_temp$`Mkt-RF` + merged_temp$SMB + merged_temp$HML)
  df <- data.frame(Beta1 = coef(model)[2], Beta2 = coef(model)[3], Beta3 = coef(model)[4])
  betas <- rbind(betas, df)
}

beme_gammas1 <- data.frame(gamma_zero = as.numeric(), gamma_mkt = as.numeric(), 
                           gamma_size = as.numeric(), gamma_beme = as.numeric())

beme_gammas2 <- data.frame(gamma_zero = as.numeric(), gamma_mkt = as.numeric(), 
                           gamma_smb = as.numeric(), gamma_hml = as.numeric())

beme_gammas3 <- data.frame(gamma_zero = as.numeric(), gamma_mkt = as.numeric(),
                           gamma_smb = as.numeric(), gamma_hml = as.numeric(),
                           gamma_size = as.numeric(), gamma_beme = as.numeric())

for(i in 1:1073){
  #Estimate the gammas
  model1 <- lm(array_beme[i,] ~ betas$Beta1 + log(as.vector(as.matrix(size[i,2:26]))) + log(expanded_beme[i,2:26]))
  model2 <- lm(array_beme[i,] ~ betas$Beta1 + betas$Beta2 + betas$Beta3)
  model3 <- lm(array_beme[i,] ~ betas$Beta1 + log(as.vector(as.matrix(size[i,2:26]))) + 
                 log(expanded_beme[i,2:26]) + betas$Beta2 +betas$Beta3)
  
  #Store them in the temporary dfs
  temp1 <- data.frame(gamma_zero = coef(model1)[1], gamma_mkt = coef(model1)[2], 
                      gamma_size = coef(model1)[3], gamma_beme = coef(model1)[4])
  
  temp2 <- data.frame(gamma_zero = coef(model2)[1], gamma_mkt = coef(model2)[2], 
                      gamma_smb = coef(model2)[3], gamma_hml = coef(model2)[4])
  
  temp3 <- data.frame(gamma_zero = coef(model3)[1], gamma_mkt = coef(model3)[2],
                      gamma_smb = coef(model3)[5], gamma_hml = coef(model3)[6],
                      gamma_size = coef(model3)[3], gamma_beme = coef(model3)[4])
  
  beme_gammas1 <- rbind(beme_gammas1, temp1)
  beme_gammas2 <- rbind(beme_gammas2, temp2)
  beme_gammas3 <- rbind(beme_gammas3, temp3)
}

avg_1 <- apply(beme_gammas1, 2, mean)
avg_2 <- apply(beme_gammas2, 2, mean)
avg_3 <- apply(beme_gammas3, 2, mean)

std_1 <- apply(beme_gammas1, 2, sd) / sqrt(1073)
std_2 <- apply(beme_gammas2, 2, sd) / sqrt(1073)
std_3 <- apply(beme_gammas3, 2, sd) / sqrt(1073)

t_stat1 <- avg_1 / std_1
t_stat2 <- avg_2 / std_2
t_stat3 <- avg_3 / std_3

#d)

#e)
rm(beme_gammas1, beme_gammas2, beme_gammas3)
beme_gammas1 <- data.frame(gamma_zero = as.numeric(), gamma_mkt = as.numeric(), 
                           gamma_size = as.numeric(), gamma_beme = as.numeric())

beme_gammas2 <- data.frame(gamma_zero = as.numeric(), gamma_mkt = as.numeric(), 
                           gamma_smb = as.numeric(), gamma_hml = as.numeric())

beme_gammas3 <- data.frame(gamma_zero = as.numeric(), gamma_mkt = as.numeric(),
                           gamma_smb = as.numeric(), gamma_hml = as.numeric(),
                           gamma_size = as.numeric(), gamma_beme = as.numeric())

for(i in 439:1073){
  #Estimate the gammas
  model1 <- lm(array_beme[i,] ~ betas$Beta1 + log(as.vector(as.matrix(size[i,2:26]))) + log(expanded_beme[i,2:26]))
  model2 <- lm(array_beme[i,] ~ betas$Beta1 + betas$Beta2 + betas$Beta3)
  model3 <- lm(array_beme[i,] ~ betas$Beta1 + log(as.vector(as.matrix(size[i,2:26]))) + 
                 log(expanded_beme[i,2:26]) + betas$Beta2 +betas$Beta3)
  
  #Store them in the temporary dfs
  temp1 <- data.frame(gamma_zero = coef(model1)[1], gamma_mkt = coef(model1)[2], 
                      gamma_size = coef(model1)[3], gamma_beme = coef(model1)[4])
  
  temp2 <- data.frame(gamma_zero = coef(model2)[1], gamma_mkt = coef(model2)[2], 
                      gamma_smb = coef(model2)[3], gamma_hml = coef(model2)[4])
  
  temp3 <- data.frame(gamma_zero = coef(model3)[1], gamma_mkt = coef(model3)[2],
                      gamma_smb = coef(model3)[5], gamma_hml = coef(model3)[6],
                      gamma_size = coef(model3)[3], gamma_beme = coef(model3)[4])
  
  beme_gammas1 <- rbind(beme_gammas1, temp1)
  beme_gammas2 <- rbind(beme_gammas2, temp2)
  beme_gammas3 <- rbind(beme_gammas3, temp3)
}

avg_1_e <- apply(beme_gammas1, 2, mean)
avg_2_e <- apply(beme_gammas2, 2, mean)
avg_3_e <- apply(beme_gammas3, 2, mean)

std_1_e <- apply(beme_gammas1, 2, sd) / sqrt(1073-439)
std_2_e <- apply(beme_gammas2, 2, sd) / sqrt(1073-439)
std_3_e <- apply(beme_gammas3, 2, sd) / sqrt(1073-439)

t_stat1_e <- avg_1_e / std_1_e
t_stat2_e <- avg_2_e / std_2_e
t_stat3_e <- avg_3_e / std_3_e

#f)
vw_212 <- read_csv("ps5momvw.csv")
vw_212[,2:26] <- vw_212[,2:26] /100
vw_212 <- vw_212[7:1069,]
colnames(vw_212)[1] <- "Date"
vw_212[vw_212 <= -.99] <- NA
vw_212 <- vw_212 - rf
size_212 <- read.csv("ps5momsize.csv")
size_212[,2:26] <- size_212[,2:26] /100
size_212 <- size_212[7:1069,]
size_212[size_212 <= -.99] <- NA
ret_212 <- read.csv("ps5mom212.csv")
ret_212[,2:26] <- ret_212[,2:26] / 100
ret_212 <- ret_212[7:1069,]
ret_212[ret_212 <= -.99] <- NA

betas <- data.frame(Beta1 = as.numeric(), Beta2 = as.numeric(), Beta3 = as.numeric())

for (i in 2:26){
  temp <- data.frame(Date = vw_212$Date, Port = vw_212[,i])
  merged_temp <- merge(temp, ff, by="Date")
  model <- lm(merged_temp$Port ~ merged_temp$`Mkt-RF` + merged_temp$SMB + merged_temp$UMD)
  df <- data.frame(Beta1 = coef(model)[2], Beta2 = coef(model)[3], Beta3 = coef(model)[4])
  betas <- rbind(betas, df)
}

beme_gammas1 <- data.frame(gamma_zero = as.numeric(), gamma_mkt = as.numeric(), 
                           gamma_size = as.numeric(), gamma_212 = as.numeric())

beme_gammas2 <- data.frame(gamma_zero = as.numeric(), gamma_mkt = as.numeric(), 
                           gamma_smb = as.numeric(), gamma_umd = as.numeric())

beme_gammas3 <- data.frame(gamma_zero = as.numeric(), gamma_mkt = as.numeric(),
                           gamma_smb = as.numeric(), gamma_umd = as.numeric(),
                           gamma_size = as.numeric(), gamma_212 = as.numeric())

vw_212 <- as.matrix(vw_212)
size_212 <- as.matrix(size_212)
ret_212 <- as.matrix(ret_212)

for(i in 1:1063){
  #Estimate the gammas
  model1 <- lm(vw_212[i,2:26] ~ betas$Beta1 + log(as.vector(as.matrix(size_212[i,2:26]))) + ret_212[i,2:26])
  model2 <- lm(vw_212[i,2:26] ~ betas$Beta1 + betas$Beta2 + betas$Beta3)
  model3 <- lm(vw_212[i,2:26] ~ betas$Beta1 + log(as.vector(as.matrix(size_212[i,2:26]))) + 
                 ret_212[i,2:26] + betas$Beta2 +betas$Beta3)
  
  #Store them in the temporary dfs
  temp1 <- data.frame(gamma_zero = coef(model1)[1], gamma_mkt = coef(model1)[2], 
                      gamma_size = coef(model1)[3], gamma_212 = coef(model1)[4])
  
  temp2 <- data.frame(gamma_zero = coef(model2)[1], gamma_mkt = coef(model2)[2], 
                      gamma_smb = coef(model2)[3], gamma_hml = coef(model2)[4])
  
  temp3 <- data.frame(gamma_zero = coef(model3)[1], gamma_mkt = coef(model3)[2],
                      gamma_smb = coef(model3)[5], gamma_hml = coef(model3)[6],
                      gamma_size = coef(model3)[3], gamma_212 = coef(model3)[4])
  
  beme_gammas1 <- rbind(beme_gammas1, temp1)
  beme_gammas2 <- rbind(beme_gammas2, temp2)
  beme_gammas3 <- rbind(beme_gammas3, temp3)
}

avg_1_f <- apply(beme_gammas1, 2, mean)
avg_2_f <- apply(beme_gammas2, 2, mean)
avg_3_f <- apply(beme_gammas3, 2, mean)

std_1_f <- apply(beme_gammas1, 2, sd) / sqrt(1063)
std_2_f <- apply(beme_gammas2, 2, sd) / sqrt(1063)
std_3_f <- apply(beme_gammas3, 2, sd) / sqrt(1063)

t_stat1_f <- avg_1_f / std_1_f
t_stat2_f <- avg_2_f / std_2_f
t_stat3_f <- avg_3_f / std_3_f

#h)
rm(beme_gammas1, beme_gammas2, beme_gammas3)
beme_gammas1 <- data.frame(gamma_zero = as.numeric(), gamma_mkt = as.numeric(), 
                           gamma_size = as.numeric(), gamma_beme = as.numeric())

beme_gammas2 <- data.frame(gamma_zero = as.numeric(), gamma_mkt = as.numeric(), 
                           gamma_smb = as.numeric(), gamma_hml = as.numeric())

beme_gammas3 <- data.frame(gamma_zero = as.numeric(), gamma_mkt = as.numeric(),
                           gamma_smb = as.numeric(), gamma_hml = as.numeric(),
                           gamma_size = as.numeric(), gamma_beme = as.numeric())

for(i in 390:1063){
  #Estimate the gammas
  model1 <- lm(vw_212[i,2:26] ~ betas$Beta1 + log(as.vector(as.matrix(size_212[i,2:26]))) + ret_212[i,2:26])
  model2 <- lm(vw_212[i,2:26] ~ betas$Beta1 + betas$Beta2 + betas$Beta3)
  model3 <- lm(vw_212[i,2:26] ~ betas$Beta1 + log(as.vector(as.matrix(size_212[i,2:26]))) + 
                 ret_212[i,2:26] + betas$Beta2 +betas$Beta3)
  
  #Store them in the temporary dfs
  temp1 <- data.frame(gamma_zero = coef(model1)[1], gamma_mkt = coef(model1)[2], 
                      gamma_size = coef(model1)[3], gamma_212 = coef(model1)[4])
  
  temp2 <- data.frame(gamma_zero = coef(model2)[1], gamma_mkt = coef(model2)[2], 
                      gamma_smb = coef(model2)[3], gamma_hml = coef(model2)[4])
  
  temp3 <- data.frame(gamma_zero = coef(model3)[1], gamma_mkt = coef(model3)[2],
                      gamma_smb = coef(model3)[5], gamma_hml = coef(model3)[6],
                      gamma_size = coef(model3)[3], gamma_212 = coef(model3)[4])
  
  beme_gammas1 <- rbind(beme_gammas1, temp1)
  beme_gammas2 <- rbind(beme_gammas2, temp2)
  beme_gammas3 <- rbind(beme_gammas3, temp3)
}

avg_1_h <- apply(beme_gammas1, 2, mean)
avg_2_h <- apply(beme_gammas2, 2, mean)
avg_3_h <- apply(beme_gammas3, 2, mean)

std_1_h <- apply(beme_gammas1, 2, sd) / sqrt(1063-390)
std_2_h <- apply(beme_gammas2, 2, sd) / sqrt(1063-390)
std_3_h <- apply(beme_gammas3, 2, sd) / sqrt(1063-390)

t_stat1_h <- avg_1_h / std_1_h
t_stat2_h <- avg_2_h / std_2_h
t_stat3_h <- avg_3_h / std_3_h

