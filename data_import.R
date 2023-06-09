library(tseries)
library(dplyr)
library(tidyr)

###-------- S&P500 ---------
# Automatic import of S&P500

spx.raw <- get.hist.quote(instrument = "^GSPC", start=as.Date("1971-01-01"), quote="Close")
spx.ret = 100*diff(log(spx.raw)) #stationary times series

df_spx = fortify.zoo(spx.ret)
df_spx = rename(df_spx, c("date"="Index", "spx" = "Close"))


###---- External variables ------
transform_csv <- function(data){ #transforms the csv file of YahooFinance
  data = select(data, "Close","Date")
  data = rename(data, c("Price" = "Close"))
  data$Price = as.numeric(data$Price)
  data$Date = as.Date(data$Date)
  data = drop_na(data)
  return(data[-1,])
}


### vix
vix.raw = get.hist.quote(instrument = "^VIX", start = as.Date("1990-01-01"), quote="Close")
sum(is.na(vix.raw$Close)) #numbers of NA 1990 to 09/06/2023 : 299

df_vix = fortify.zoo(vix.raw)
df_vix = df_vix  %>% drop_na()
df_vix = mutate(df_vix,vix_dailyscaled = Close/252**(1/2)) # vix which has been transformed at a daily scale
df_vix = df_vix %>% rename(c("date" = "Index", "vix" = "vix_dailyscaled")) %>% select(c("date","vix"))


# RV
#download.file("https://realized.oxford-man.ox.ac.uk/images/oxfordmanrealizedvolatilityindices.zip", destfile = "data-raw/OxfordManRealizedVolatilityIndices.zip")
#system("unzip -o data-raw/OxfordManRealizedVolatilityIndices.zip -d data-raw/")


# library(alfred)
# nfci = get_alfred_series("NFCI")
# rv = get_alfred_series("RV")


