df = read.csv(file = "./data_nasdaq/NDX_daily_20230705.csv")

df_spx %>% tail()


import_ndx <- function() {
  # Automatic import of NASDAQ-100 from Yahoo finance
  ndx.raw <-
    get.hist.quote(
      instrument = "^NDX",
      start = as.Date("1971-01-01"),
      quote = "Close"
    )
  ndx.ret = 100 * diff(log(ndx.raw)) #stationary times series
  
  df_ndx = fortify.zoo(ndx.ret)
  df_ndx = dplyr::rename(df_ndx, c("date" = "Index", "ndx" = "Close")) %>% as_tibble()
  
  return(df_ndx)
}

df_ndx = import_ndx()
df_ndx


p = ggplot(df_ndx) + geom_line(aes(x = date, y = ndx))

df_ndx %>% as_tibble()

# ---- data import -----
library(readxl)

five_min_ndx = read_excel("./data_nasdaq/ndx_20230705_c.xlsx") %>% dplyr::rename("date" = "Local Date")
five_min_ndx = five_min_ndx  %>%  as_tibble()

x = five_min_ndx

compute_realized_volatility <- function(x){
  
  x$log_return = c(diff(log(x$Close)), NA)
  x$square = x$log_return**2
  
  df = x %>% group_by(date) %>% summarise(RV = mean(square))
  
  return(df)
}

test = compute_realized_volatility(x)

x 