library(copula)
library(rugarch)
library(readr)

setwd("~/Applied_Quant_Finance/Project")
rm(list=ls())

spec = ugarchspec(mean.model=list(armaOrder=c(5,5)), 
                  variance.model=list(model = "eGARCH", garchOrder=c(5,5)),
                  distribution.model = "sstd")

most_data <- read_csv("F-F_Research_Data_Factors_daily.CSV")
mom_data <- read_csv("F-F_Momentum_Factor_daily.CSV")

all_data <- merge(most_data, mom_data, by="Date")
all_data <- all_data[1:23597,]
colnames(all_data)[6] <- "Mom"

all_data$combined <- .5*all_data$HML + .5 * all_data$Mom

fit_value <- ugarchfit(data=all_data$HML, spec=spec, solver='hybrid')
fit_mom <- ugarchfit(data=all_data$Mom, spec=spec, solver='hybrid')
fit_both <- ugarchfit(data=all_data$combined, spec=spec, solver='hybrid')

value_shock <- residuals(fit_value, standardize=T)
value_shock <- as.vector(value_shock)

mom_shock <- as.vector(residuals(fit_mom, standardize=T))

both_shock <- as.vector(residuals(fit_both, standardize=T))

mom_both <- pobs(data.frame(mom = mom_shock, both = both_shock))
value_both <- pobs(data.frame(val = value_shock, both = both_shock))
mom_value <- pobs(data.frame(mom = mom_shock, val = value_shock))

fit_mb <- fitCopula(ellipCopula("t", dim=2), data=mom_both, method="ml")
fit_vb <- fitCopula(ellipCopula("t", dim=2), data=value_both, method="ml")
fit_vm <- fitCopula(ellipCopula("t", dim=2), data=mom_value, method="ml")

tailIndex(fit_mb@copula)
tailIndex(fit_vb@copula)
tailIndex(fit_vm@copula)

mom_value_obs <- pobs(data.frame(mom = all_data$Mom, val = all_data$HML))
fit_vm_obs <- fitCopula(ellipCopula("t", dim=2), data=mom_value_obs, method="ml")
tailIndex(fit_vm_obs@copula)

mom_arma <- arima(all_data$Mom, order=c(5,0,5))
val_arma <- arima(all_data$HML, order=c(5,0,5))

mom_val_res <- pobs(data.frame(mom = mom_arma$residuals, val = val_arma$residuals))
fit_vm_res <- fitCopula(ellipCopula("t", dim=2), data=mom_val_res, method="ml")
tailIndex(fit_vm_res@copula)

plot(mom_value, cex=.0001)
