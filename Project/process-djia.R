library(MASS)
library(readr)
rm(list=ls())
setwd("C:/Users/Emily/Documents/GitHub/Applied_Quant_Finance/Project")

retvol <- read.csv("retvol.csv")

prices <- retvol[,c("MMM_PX_LAST", "AAPL_PX_LAST", "AXP_PX_LAST", "T_PX_LAST",
                     "BA_PX_LAST", "CAT_PX_LAST", "CVX_PX_LAST", "CSCO_PX_LAST",
                     "KO_PX_LAST", "DD_PX_LAST", "XOM_PX_LAST", "GE_PX_LAST", "GOOG_PX_LAST",
                     "GS_PX_LAST", "HD_PX_LAST", "INTC_PX_LAST", "JNJ_PX_LAST", "JPM_PX_LAST",
                     "MRK_PX_LAST", "MSFT_PX_LAST", "NKE_PX_LAST",
                     "PFE_PX_LAST", "PG_PX_VOL", "TRV_PX_LAST",
                     "UNH_PX_LAST", "UTX_PX_LAST", "VZ_PX_LAST", "V_PX_LAST", "WMT_PX_LAST",
                     "DIS_PX_LAST")]
volumes <- retvol[,c("MMM_PX_VOL", "AAPL_PX_VOL", "AXP_PX_VOL", "T_PX_VOL",
                     "BA_PX_VOL", "CAT_PX_VOL", "CVX_PX_VOL", "CSCO_PX_VOL",
                     "KO_PX_VOL", "DD_PX_VOL", "XOM_PX_VOL", "GE_PX_VOL", "GOOG_PX_VOL",
                     "GS_PX_VOL", "HD_PX_VOL", "INTC_PX_VOL", "JNJ_PX_VOL", "JPM_PX_VOL",
                     "MRK_PX_VOL", "MSFT_PX_VOL", "NKE_PX_VOL",
                     "PFE_PX_VOL", "PG_PX_VOL", "TRV_PX_VOL",
                     "UNH_PX_VOL", "UTX_PX_VOL", "VZ_PX_VOL", "V_PX_VOL", "WMT_PX_VOL",
                     "DIS_PX_VOL")]

calc_rets <- function(column) {
  return ((column[2]-column[1])/column[2])
}

returns <- matrix(0, nrow=762, ncol=30)
for(i in 2:763) {
  returns[(i-1),] <- t(apply(prices[(i-1):763,], 2, calc_rets))
}


one_month_sd <- matrix(0, nrow=731, ncol=30)
for(i in 31:762) {
  one_month_sd[(i-31),] <- apply(returns[(i-30):i,], 2, sd)
}

volatilities <- 1:30

