library(alfred)
library(dplyr)
library(ggplot2)

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
df = get_alfred_series(
  series_id = "HOUST",
  series_name = "HOUST",
  observation_start = "1959-01-01",
  realtime_start = ymd(today())
) %>% as_tibble()

df
df$value =  c(NA, 100 * diff(log(df$HOUST)))

df_dhoust = df %>% fill_missing_dates(frequency = "month") %>% as_tibble()

# --- Example of use ----
# Estimation
df = df_spx %>% merge(df_dhoust, by = "date")
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
series_optimal_forecast(GM_dhoust,h)

ggplot(data = test) + geom_line(aes(x = horizon, y = optimal_prediction))
ggplot(data = test) + geom_line(aes(x = date, y = optimal_prediction))