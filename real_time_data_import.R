library(alfred)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(zoo)
library(tseries)

## S&P 500
spx.raw <-
  get.hist.quote(
    instrument = "^GSPC",
    start = as.Date("1971-01-01"),
    quote = "Close"
  )
spx.ret = 100 * diff(log(spx.raw)) #stationary times series

df_spx = fortify.zoo(spx.ret) %>% dplyr::rename(c("date" = "Index", "spx" = "Close"))

# ---- Explanatory variables ----
## HOUST
df_HOUST = get_alfred_series(
  series_id = "HOUST",
  series_name = "HOUST",
  observation_start = "1959-01-01",
  realtime_start = ymd(today())
) %>% as_tibble()


df_HOUST$value =  c(NA, 100 * diff(log(df_HOUST$HOUST)))

df_dhoust = df_HOUST %>% fill_missing_dates(frequency = "month") %>% as_tibble()

# --- Example of use ----
# Estimation
df = df_spx %>% merge(df_dhoust, by = "date") %>% as_tibble() #%>% dplyr::rename(c("value" = "list_value"))
GM_dhoust = mfGARCH::fit_mfgarch(
  data = df,
  y = "spx",
  x = "value",
  low.freq = "year_month",
  K =  36,
  weighting = "beta.unrestricted"
)

# Forecast
source(file = "./forecast.R")
h = 20

tail(series_optimal_forecast(GM_dhoust, h))

ggplot(data = test) + geom_line(aes(x = horizon, y = optimal_prediction))
ggplot(data = test) + geom_line(aes(x = date, y = optimal_prediction))

# series_optimal_forecast(GM_dhoust, h, df_spx)

real_time_optimal_forecast(GM_dhoust, h, df_spx)
