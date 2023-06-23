library(forecast)
library(tseries)
library(rugarch)
library(zoo)
library(fUnitRoots)
library(mfGARCH)
library(lubridate)

# ----- fit of normal GARCH ---------
spec_garch = ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(include.mean = FALSE),
  distribution.model = "norm"
)
model = ugarchfit(spec_garch, spx.ret)
model

plot(model@fit[["sigma"]], type = "l", ylab = "Sigma")


# ----- GARCH-MIDAS with one explanatory variable --------
## ----- Daily explanatory variables (VIX, Rvol22, vrp) -----

## s&p explained by vix
df = merge(df_spx, df_vix, by = "date")
GM_sp_vix = mfGARCH::fit_mfgarch(
  data = df,
  y = "spx",
  x = "vix",
  K = 3,
  low.freq = "date",
  weighting = "beta.restricted"
)
# plot_weighting_scheme(GM_sp_vix)

## s&p explained by Rvol22
df = merge(df_spx, df_Rvol22, by = "date")
GM_sp_Rvol22 = mfGARCH::fit_mfgarch(
  data = df,
  y = "spx",
  x = "Rvol22",
  K = 264,
  low.freq = "date",
  weighting = "beta.restricted"
)

## s&p explained by vrp
df = merge(df_spx, df_vrp, by = "date")
GM_sp_vrp = mfGARCH::fit_mfgarch(
  data = df,
  y = "spx",
  x = "vrp",
  K = 3,
  low.freq = "date",
  weighting = "beta.restricted"
)


# Focus on period 1990 - 2018 (as in the paper)
df_2018 = df_spx %>% merge(df_vix, by = "date") %>% filter(year(date) < 2019)

results_2018 = mfGARCH::fit_mfgarch(
  data = df_2018,
  y = "spx",
  x = "vix",
  K = 3,
  low.freq = "date",
  weighting = "beta.restricted"
)
round(results_2018$par, 4)

sum(is.na(df_2018$spx))

# Rvol(22)
df = df_spx %>% merge(df_Rvol22, by = "date") %>% filter(year(date) < 2019) %>% filter(1989 < year(date))
GM_sp_Rvol22_restriced_period = mfGARCH::fit_mfgarch(
  data = df,
  y = "spx",
  x = "Rvol22",
  K = 264,
  low.freq = "date",
  weighting = "beta.restricted"
)
round(GM_sp_Rvol22_restriced_period$par, 4)

## ------ Non daily explanatory variable -------
### dhoust
df = df_spx %>% merge(df_dhoust, by = "date")
GM_dhoust = mfGARCH::fit_mfgarch(
  data = df,
  y = "spx",
  x = "value",
  low.freq = "year_month",
  K =  36,
  weighting = "beta.unrestricted"
)

### nfci (weekly)
df = df_spx %>% merge(df_NFCI, by = "date")
GM_nfci = mfGARCH::fit_mfgarch(
  data = df,
  y = "spx",
  x = "value",
  low.freq = "year_week",
  K =  52,
  weighting = "beta.restricted"
)
GM_nfci$week_start <-
  5 # should be the same as indicated in data_import.R


### Industrial production
df = df_spx %>% merge(df_ip, by = "date")
GM_ip = mfGARCH::fit_mfgarch(
  data = df,
  y = "spx",
  x = "value",
  low.freq = "year_month",
  K = 36,
  weighting = "beta.restricted"
)

### Chicago Fed National Activity Index (CFNAI)
df = df_spx %>% merge(df_nai, by = "date") %>% filter(year(date)<2019)
GM_nai = mfGARCH::fit_mfgarch(
  data = df ,
  y = "spx",
  x = "value",
  low.freq  = "year_month",
  K = 36,
  weighting = "beta.restricted"
)

