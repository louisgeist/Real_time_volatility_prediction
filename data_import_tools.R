# -------- tool functions ---------

#For non-daily long term component
#In order to use properly "fit_mfgarch", we need to complete between two non consecutive dates the dataframe with the value of the first date ;
#fill_missing_dates() solves this problem

fill_missing_dates = function(partial_df,
                              frequency = "month",
                              week_start = FALSE) {
  # partial_df must have two columns: "date" & "value"
  # if frequency = "week", then week_start needs an integer between 1 and 7 (Monday based)
  
  list_day <-
    seq.Date(from = partial_df$date[1],
             to = Sys.Date(),
             by = "days")
  list_value <- rep(NA_real_, length(list_day))
  
  i <- 1 # index going through partial_df
  
  for (day_index in seq_along(list_day)) {
    if (frequency == "week") {
      day_floor <-
        lubridate::floor_date(list_day[day_index], unit = frequency, week_start)
    } else {
      day_floor <-
        lubridate::floor_date(list_day[day_index], unit = frequency)
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
        stop(
          "Error in fill_missing_dates: partial_df$date is not ordered or a date is missing."
        )
      }
    }
  }
  
  df <- data.frame(date = list_day, value = list_value) %>%
    drop_na() %>%
    mutate(date = as.Date(date))
  
  if (frequency == "week") {
    df <-
      df %>% mutate(year_week = lubridate::floor_date(date, unit = frequency, week_start))
  } else {
    df <-
      df %>% mutate(year_month = lubridate::floor_date(date, unit = frequency))
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
      end = final_date,
      quote = "Close"
    )
  spx.ret = 100 * diff(log(spx.raw)) #stationary times series
  
  df_spx = fortify.zoo(spx.ret)
  df_spx = dplyr::rename(df_spx, c("date" = "Index", "value" = "Close"))
  
  return(df_spx)
}

import_ndx <- function() {
  # Automatic import of NASDAQ-100 from Yahoo finance
  ndx.raw <-
    get.hist.quote(
      instrument = "^NDX",
      start = as.Date("1971-01-01"),
      end = final_date,
      quote = "Close"
    )
  ndx.ret = 100 * diff(log(ndx.raw)) #stationary times series
  
  df_ndx = fortify.zoo(ndx.ret)
  df_ndx = dplyr::rename(df_ndx, c("date" = "Index", "value" = "Close"))
  
  return(df_ndx)
}

import_Rvol22 <- function() {
  if(main_index == "spx"){
    instrument <- "^GSPC"
  } else if(main_index == "ndx"){
    instrument <- "^NDX"
  } else{
    stop("Please enter a correct main_index name (spx or ndx in char)")
  }
  
  spx.raw <-
    get.hist.quote(
      instrument = instrument,
      start = as.Date("1971-01-01"),
      end = final_date,
      quote = "Close"
    )
  spx.ret = 100 * diff(log(spx.raw)) #stationary times series
  
  Rvol22 = zoo::rollmean(spx.ret ** 2, 22, align = "right") #the current value and the past 21 values are taken for the mean
  df_Rvol22 = Rvol22 %>% fortify.zoo() %>%  drop_na() %>% dplyr::rename(c("date" = "Index", "value" = "Close"))
  
  return(df_Rvol22)
}

import_vix <- function() {
  if(main_index == "spx"){
    vix_name <- "^VIX"
  } else if(main_index == "ndx"){
    vix_name <- "^VXN"
  } else{
    stop("Please enter a correct main_index name (spx or ndx in char)")
  }
  
  vix.raw = get.hist.quote(
    instrument = vix_name,
    start = as.Date("1990-01-01"),
    end = final_date,
    quote = "Close"
  )
  sum(is.na(vix.raw$Close)) #numbers of NA 1990 to 09/06/2023 (for the s&p VIX): 299
  
  df_vix = fortify.zoo(vix.raw)
  df_vix = df_vix  %>% drop_na()
  df_vix = mutate(df_vix, vix_dailyscaled = Close / 252 ** (1 / 2)) # vix which has been converted to a daily level
  df_vix = df_vix %>% dplyr::rename(c("date" = "Index", "value" = "vix_dailyscaled")) %>% select(c("date", "value"))
  
  return(df_vix)
}

import_vrp <- function() {
  if(main_index == "spx"){
    instrument <- "^GSPC"
    vix_name <- "^VIX"
  } else if(main_index == "ndx"){
    instrument <- "^NDX"
    vix_name <- "^VXN"
  } else{
    stop("Please enter a correct main_index name (spx or ndx in char)")
  }
  
  spx.raw = get.hist.quote(
    instrument = instrument,
    start = as.Date("1971-01-01"),
    end = final_date,
    quote = "Close"
  )
  spx.ret = 100 * diff(log(spx.raw)) #stationary times series
  Rvol22 = zoo::rollmean(spx.ret ** 2, 22, align = "right") #the current value and the past 21 values are taken for the mean
  
  vix.raw = get.hist.quote(
    instrument = vix_name,
    start = as.Date("1990-01-01"),
    quote = "Close"
  )
  
  vrp = vix.raw / 252 ** (1 / 2) - Rvol22
  df_vrp = vrp %>% fortify.zoo() %>% drop_na() %>% dplyr::rename(c("date" = "Index", "value" = "Close"))
  
  return(df_vrp)
}

## ----- non daily -----
import_houst <- function() {
  df_HOUST = alfred::get_alfred_series(
    series_id = "HOUST",
    series_name = "HOUST",
    observation_start = "1959-01-01",
    realtime_start = final_date,
    realtime_end = final_date,
    api_key = "4f77313cfd688a6d4d70ccf8e650f038"
  )
  
  df_HOUST$value =  c(NA, 100 * diff(log(df_HOUST$HOUST)))
  
  df_dhoust = df_HOUST %>% fill_missing_dates(frequency = "month") %>% as_tibble()
  
  return(df_dhoust)
}

import_ip <- function() {
  df_ip_partial = alfred::get_alfred_series(
    series_id = "INDPRO",
    series_name = "IP",
    observation_start = "1959-01-01",
    realtime_start = final_date,
    realtime_end = final_date,
    api_key = "4f77313cfd688a6d4d70ccf8e650f038"
  ) %>% as_tibble() %>% select(c("date", "IP"))
  
  df_ip_partial$value = c(NA, 100 * diff(log(df_ip_partial$IP)))
  df_ip = df_ip_partial %>% select(c("date", "value")) %>% fill_missing_dates(frequency = "month") %>% as_tibble()
  
  return(df_ip)
}

import_nai <- function() {
  df_nai_partial = alfred::get_alfred_series(
    series_id = "CFNAI",
    series_name = "value",
    observation_start = "1959-01-01",
    realtime_start = final_date,
    realtime_end = final_date,
    api_key = "4f77313cfd688a6d4d70ccf8e650f038"
  ) %>% as_tibble() %>% select(c("date", "value"))
  
  
  df_nai = df_nai_partial %>% fill_missing_dates(frequency = "month") %>% as_tibble()
  
  return(df_ip)
}

import_nfci <- function() {
  df_nfci_partial = alfred::get_alfred_series(
    series_id = "NFCI",
    series_name = "value",
    observation_start = "1959-01-01",
    realtime_start = final_date,
    realtime_end = final_date,
    api_key = "4f77313cfd688a6d4d70ccf8e650f038"
  ) %>% as_tibble() %>% select(c("value", "date")) %>% mutate(date = date - 4) # we do this transformation, because the value of the friday corresponds to the week ending at that friday
    
    df_nfci = fill_missing_dates(df_nfci_partial,
                                 frequency = "week",
                                 week_start = 1)
    
    return(df_nfci)
}

## ----- filter data function -----
filter_data = function(first_date,
                       last_date,
                       df_main_index_raw,
                       df_dhoust_raw,
                       df_ip_raw,
                       df_nai_raw,
                       df_nfci_raw,
                       df_vix_raw,
                       df_vrp_raw,
                       df_Rvol22_raw) {
  # Find the index where the conditions are true
  index_main_index <-
    which(df_main_index_raw$date >= first_date &
            df_main_index_raw$date < last_date)
  index_dhoust <-
    which(
      floor_date(df_dhoust_raw$date, unit = "month") >= floor_date(first_date, unit = "month") &
        floor_date(df_dhoust_raw$date, unit = "month") < floor_date(last_date, unit = "month")
    )
  index_ip <-
    which(
      floor_date(df_ip_raw$date, unit = "month") >= floor_date(first_date, unit = "month") &
        floor_date(df_ip_raw$date, unit = "month") < floor_date(last_date, unit = "month")
    )
  index_nai <-
    which(
      floor_date(df_nai_raw$date, unit = "month") >= floor_date(first_date, unit = "month") &
        floor_date(df_nai_raw$date, unit = "month") < floor_date(last_date, unit = "month")
    )
  index_nfci <-
    which(
      floor_date(df_nfci_raw$date, unit = "week", 5) >= floor_date(first_date, unit = "week", 5) &
        floor_date(df_nfci_raw$date, unit = "week", 5) < floor_date(last_date, unit = "week", 5)
    )
  index_vix <-
    which(df_vix_raw$date >= first_date & df_vix_raw$date < last_date)
  index_vrp <-
    which(df_vrp_raw$date >= first_date & df_vrp_raw$date < last_date)
  index_Rvol22 <-
    which(df_Rvol22_raw$date >= first_date &
            df_Rvol22_raw$date < last_date)
  
  # Subset the DataFrames using the index
  df_main_index <<- df_main_index_raw[index_main_index,]
  df_dhoust <<- df_dhoust_raw[index_dhoust,]
  df_ip <<- df_ip_raw[index_ip,]
  df_nai <<- df_nai_raw[index_nai,]
  df_nfci <<- df_nfci_raw[index_nfci,]
  df_vix <<- df_vix_raw[index_vix,]
  df_vrp <<- df_vrp_raw[index_vrp,]
  df_Rvol22 <<- df_Rvol22_raw[index_Rvol22,]
}