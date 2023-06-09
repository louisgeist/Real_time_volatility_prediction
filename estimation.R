library(forecast)
library(tseries)
library(rugarch)
library(zoo)
library(fUnitRoots)
library(mfGARCH)
library(lubridate)

rm(list=ls())
source(file = "./data_import.R")


###----- fit of normal GARCH ---------
spec_garch = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(include.mean=FALSE), distribution.model = "norm")
model = ugarchfit(spec_garch, spx.ret)
model

plot(model@fit[["sigma"]], type="l", ylab = "Sigma")


### ----- GARCH-MIDAS --------
## Construction of the unique dataframe containing all the used data
df = merge(df_spx, df_vix, by = "date")

results = mfGARCH::fit_mfgarch(data = df, y = "spx", x = "vix", K = 3, low.freq = "date", weighting = "beta.restricted")

# some changes in order to have the same results as in the paper
df_2018 = filter(df, year(date) < 2019)

results_2018 = mfGARCH::fit_mfgarch(data = df_2018, y = "spx", x = "vix", K = 3, low.freq = "date", weighting = "beta.restricted")
round(results_2018$par,4)

sum(is.na(df_2018$spx))
