library(readr)

setwd("~/Applied_Quant_Finance")
rm(list=ls())
source('~/Applied_Quant_Finance/PS6_helper.R')
industry <- read_csv("PS6_Industry.csv")

industry <- as.matrix(industry/100)
