library(alfred)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(zoo)
library(tseries)

source("./data_import_tools.R")
source("./forecast.R")

## S&P 500
df_spx = import_spx()

# ---- Explanatory variables ----
## HOUST
df_dhoust = import_houst()

# --- Example of use ----
# Estimation
df = df_spx %>% merge(df_dhoust, by = "date") %>% as_tibble()
GM_dhoust = mfGARCH::fit_mfgarch(
  data = df,
  y = "spx",
  x = "value",
  low.freq = "year_month",
  K =  36,
  weighting = "beta.unrestricted"
)

# Forecast
h = 20

dhoust_forecast = real_time_optimal_forecast(GM_dhoust,h,df_spx)

write.csv(dhoust_forecast, "./data_plot", row.names = FALSE)

print("real_time_data_import done !")