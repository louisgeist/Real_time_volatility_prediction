# -------- tool functions ---------

#For non-daily long term component
#In order to use properly "fit_mfgarch", we need to complete between two non consecutive dates the dataframe with the value of the first date ;
#fill_missing_dates() solves this problem

fill_missing_dates = function(partial_df, frequency = "month", week_start = FALSE) {
  # partial_df must have two columns: "date" & "value"
  # if frequency = "week", then week_start needs an integer between 1 and 7 (Monday based)
  
  list_day <- seq.Date(from = partial_df$date[1], to = Sys.Date(), by = "days")
  list_value <- rep(NA_real_, length(list_day))
  
  i <- 1 # index going through partial_df
  
  for (day_index in seq_along(list_day)) {
    if (frequency == "week") {
      day_floor <- lubridate::floor_date(list_day[day_index], unit = frequency, week_start)
    } else {
      day_floor <- lubridate::floor_date(list_day[day_index], unit = frequency)
    }
    
    if (partial_df$date[i] == day_floor) {
      list_value[day_index] <- partial_df$value[i]
    } else {
      i <- i + 1
      if (i > length(partial_df$value)) {
        break
      }
      if (partial_df$date[i] == day_floor) {
        list_value[day_index] <- partial_df$value[i]
      } else {
        stop("Error in fill_missing_dates: partial_df$date is not ordered or a date is missing.")
      }
    }
  }
  
  df <- data.frame(date = list_day, value = list_value) %>%
    drop_na() %>%
    mutate(date = as.Date(date))
  
  if (frequency == "week") {
    df <- df %>% mutate(year_week = lubridate::floor_date(date, unit = frequency, week_start))
  } else {
    df <- df %>% mutate(year_month = lubridate::floor_date(date, unit = frequency))
  }
  
  return(df)
}

# ------- import functions --------

## ----- daily data ------
import_spx <- function() {
  # Automatic import of S&P500
  spx.raw <-
    get.hist.quote(
      instrument = "^GSPC",
      start = as.Date("1971-01-01"),
      quote = "Close"
    )
  spx.ret = 100 * diff(log(spx.raw)) #stationary times series
  
  df_spx = fortify.zoo(spx.ret)
  df_spx = dplyr::rename(df_spx, c("date" = "Index", "spx" = "Close"))
  
  return(df_spx)
}

import_Rvol22 <- function(){
  spx.raw <-
    get.hist.quote(
      instrument = "^GSPC",
      start = as.Date("1971-01-01"),
      quote = "Close"
    )
  spx.ret = 100 * diff(log(spx.raw)) #stationary times series
  
  Rvol22 = zoo::rollmean(spx.ret**2, 22, align = "right") #the current value and the past 21 values are taken for the mean
  df_Rvol22 = Rvol22 %>% fortify.zoo() %>%  drop_na() %>% dplyr::rename(c("date" = "Index", "value" = "Close"))
  
  return(df_Rvol22)
}

import_vix <- function(){
  vix.raw = get.hist.quote(instrument = "^VIX", start = as.Date("1990-01-01"), quote="Close")
  sum(is.na(vix.raw$Close)) #numbers of NA 1990 to 09/06/2023 : 299
  
  df_vix = fortify.zoo(vix.raw)
  df_vix = df_vix  %>% drop_na()
  df_vix = mutate(df_vix,vix_dailyscaled = Close/252**(1/2)) # vix which has been converted to a daily level
  df_vix = df_vix %>% dplyr::rename(c("date" = "Index", "value" = "vix_dailyscaled")) %>% select(c("date","value"))
  
  return(df_vix)
}

import_vrp <- function(){
  spx.raw = get.hist.quote( instrument = "^GSPC", start = as.Date("1971-01-01"), quote = "Close")
  spx.ret = 100 * diff(log(spx.raw)) #stationary times series
  Rvol22 = zoo::rollmean(spx.ret**2, 22, align = "right") #the current value and the past 21 values are taken for the mean
  
  vix.raw = get.hist.quote(instrument = "^VIX", start = as.Date("1990-01-01"), quote="Close")
  
  vrp = vix.raw/ 252**(1/2) - Rvol22
  df_vrp = vrp %>% fortify.zoo() %>% drop_na() %>% dplyr::rename(c("date" = "Index", "value" = "Close"))
  
  return(df_vrp)
}

## ----- non daily -----
import_houst <- function(){
  df_HOUST = get_alfred_series(
    series_id = "HOUST",
    series_name = "HOUST",
    observation_start = "1959-01-01",
    realtime_start = ymd(today())
  ) %>% as_tibble()
  
  df_HOUST$value =  c(NA, 100 * diff(log(df_HOUST$HOUST)))
  
  df_dhoust = df_HOUST %>% fill_missing_dates(frequency = "month") %>% as_tibble()
  
  return(df_dhoust)
}

import_ip <- function(){
  IP = read.csv("./data/IP_120623.csv")
  partial_df_ip = IP %>% dplyr::rename(c("date" = "DATE", "value" = "IP"))
  df_ip = fill_missing_dates(partial_df_ip)
  
  return(df_ip)
}
