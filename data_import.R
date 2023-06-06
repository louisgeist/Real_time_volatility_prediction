
###-------- S&P500 ---------
# Automatic import of S&P500

s&p.raw <- get.hist.quote(instrument = "^GSPC",start=as.Date("1971-01-01"),end=as.Date("2023-12-31"), quote="Close")
s&p.ret = 100*diff(log(data.raw)) #station

###---- External variables -------
transform_csv <- function(data){ #transforms the csv file of yahoofinance
  data = select(data, "Close","Date")
  data = rename(data, c("Prix"="Close"))
  data$Prix = as.numeric(data$Prix)
  data$Date = as.Date(data$Date)
  data = drop_na(data)
  data = mutate(data, rendement = log(data$Prix/lag(data$Prix)))
  data = mutate(data, rendement2 = rendement**2)
  return(data[-1,])
}

# Load of the YahooFinance file
vix.raw = read.csv(file="./data/vix.csv")

