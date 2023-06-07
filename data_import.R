library(tseries)
library(dplyr)
library(tidyr)

###-------- S&P500 ---------
# Automatic import of S&P500

spx.raw <- get.hist.quote(instrument = "^GSPC", start=as.Date("1971-01-01"), quote="Close")
spx.ret = 100*diff(log(spx.raw)) #station

###---- External variables ------
transform_csv <- function(data){ #transforms the csv file of YahooFinance
  data = select(data, "Close","Date")
  data = rename(data, c("Price" = "Close"))
  data$Price = as.numeric(data$Price)
  data$Date = as.Date(data$Date)
  data = drop_na(data)
  return(data[-1,])
}

# Load of the YahooFinance file
vix.raw = read.csv(file = "./data/^VIX.csv")
vix.ret = transform_csv(vix.raw)
vix.ret = mutate(vix.ret, vix_dscaled = Price/252**(1/2)) # we create "vix_dscaled", the vix which has been transformed at a daily scale
vix.ret = select(vix.ret, c("Date","vix_dscaled"))

# RV
#download.file("https://realized.oxford-man.ox.ac.uk/images/oxfordmanrealizedvolatilityindices.zip", destfile = "data-raw/OxfordManRealizedVolatilityIndices.zip")
#system("unzip -o data-raw/OxfordManRealizedVolatilityIndices.zip -d data-raw/")


# library(alfred)
# nfci = get_alfred_series("NFCI")
# rv = get_alfred_series("RV")

t = get.hist.quote(instrument = "^VIX", start = as.Date("1990-01-01"), quote="Close") #a incorporer pour le global, et bien rediviser par racine de 252
plot(t)
