# ----- Forecast with a GARCH-MIDAS ------
# forecast function rely on the "mfGARCH" package
library(timeDate)

## ----- general useful function for forecast -----
next_g_func <- function(alpha, beta, gamma, epsilon, tau, g) {
  return((1 - alpha - gamma / 2 - beta) + (alpha + gamma * as.numeric(epsilon < 0)) *
           (epsilon ** 2) / tau + beta * g)
}

seq_quotation_date = function(initial_date, h) {
  #returns h+1 next quoted days after the initial date
  # initial_date : date
  # h : integer, for the horizon
  
  list_days = seq(initial_date, initial_date + 2 * h + 5, by = "day")
  
  unique_years = lubridate::year(list_days) %>% as.data.frame() %>% dplyr::rename(c("year" = ".")) %>% distinct(year)
  holiday = c()
  for (y in unique_years$year) {
    holiday = c(holiday, ymd(timeDate::holidayNYSE(year = y)))
  }
  
  quoted_days = c()
  for (date in list_days) {
    if ((wday(as_date(date)) %in% seq(2, 6)) &
        (1 - (date %in% holiday))) {
      quoted_days[c(length(quoted_days) + 1)] = date
      
    }
  }
  
  quoted_days = quoted_days %>% as_date()
  return(quoted_days[c(seq(1:(h + 1)))])
}

## ----- bootstrap forecast -----

# Simulation of a GARCH-MIDAS. No hypothesis on the innovation's law : bootstrap
simulate_garchmidas <- function(x, h) {
  # x is a mfGARCH object obtained by fit_mfgarch
  # h is the horizon of simulation
  
  # Build of the boostrap set for the innovations
  df_spx = x$df.fitted %>% select(c("date", "spx"))
  df_epsilon = df_spx %>% mutate(epsilon = spx - x$par[[1]]) %>% select(-c("spx"))
  
  df_resi_g_tau = x$df.fitted %>% select(c("date", "g", "tau", "residuals")) %>% drop_na()
  
  alpha = x$par["alpha"][[1]]
  beta = x$par["beta"][[1]]
  gamma = x$par["gamma"][[1]]
  residuals_bootstrap_set = x$df.fitted$residuals  %>% as.list() %>% na.remove() # bootstrap set
  
  # simulation of the RESIDUALS
  list_residuals = sample(x = residuals_bootstrap_set, size = h)
  
  
  # computation of TAU
  list_tau = rep(x$tau.forecast, times = h)
  
  # (this section of the code is written especially for long-term component that are avaible before the end of period : if tau_t is avaible before the end of the period t)
  # (but I think that this never occurs)
  # list_tau = double(h)
  # low.freq = names(x$df.fitted)[[3]]
  #
  # quoted_days = seq_quotation_date(x$df.fitted$date[c(length(x$df.fitted$date))], h)[-c(1)]
  # current_tau = x$tau[[length(x$tau)]]
  #
  # if(low.freq == "year_month"){
  #   current_lowfreq = x$df.fitted$year_month[[length(x$df.fitted$year_month)]]
  #   for(i in 1:h){
  #     if(floor_date(quoted_days[[i]], unit ="month") == current_lowfreq){
  #       list_tau[[i]] = current_tau
  #     }
  #     else{
  #       list_tau[[i]] = x$tau.forecast
  #     }
  #   }
  # }
  # else{
  #   if(low.freq == "year_week"){
  #     current_lowfreq = x$df.fitted$year_week[[length(x$df.fitted$year_week)]]
  #     for(i in 1:h){
  #       if(floor_date(quoted_days[[i]], unit ="week", week_start = x$week_start) == current_lowfreq){
  #         list_tau[[i]] = current_tau
  #       }
  #       else{
  #         list_tau[[i]] = x$tau.forecast
  #       }
  #     }
  #
  #   }
  #   else{# means that it is a daily long term component
  #     list_tau = rep(x$tau.forecast, times = h)
  #   }
  # }
  
  # computation of EPSILON & G
  list_g = double(h)
  list_epsilon = double(h)
  
  for (i in 1:h) {
    # previous : i-1
    # next (that we want to predict): i
    
    if (i == 1) {
      previous_g = df_resi_g_tau$g[[length(df_resi_g_tau$tau)]]
      previous_epsilon = df_epsilon$epsilon[[length(df_epsilon$epsilon)]]
    }
    else{
      previous_g = list_g[[i - 1]]
      previous_epsilon = list_epsilon[[i - 1]]
    }
    
    list_g[[i]] = next_g_func(alpha,
                            beta,
                            gamma,
                            epsilon = previous_epsilon,
                            tau = list_tau[i],
                            g = previous_g)
    list_epsilon[[i]] = list_residuals[i][[1]] * sqrt(list_g[i][[1]] * list_tau[i][[1]])
    
  }
  
  
  # date = c()
  # for(i in 0:h){
  #   th = as.character(quoted_days[i])
  #   print(th)
  #   date[i] = th
  # }

  df_simulation = as_tibble(as.data.frame(cbind(list_epsilon, list_g, list_tau, list_residuals))) %>% dplyr::rename(
    c(
      "epsilon" = "list_epsilon",
      #date,
      "g" = "list_g",
      "tau" = "list_tau",
      "residuals" = "list_residuals"
    )
  )
  return(df_simulation)
}

# it is faster to use "optimal_forecast" and the results seem to be equivalent
bootstrap_forecast <- function(x, h) {
  n = 1000
  res = 0
  
  for (i in 1:n) {
    # res = res + simulate_garchmidas(x,h)$epsilon[[h]]**2 / simulate_garchmidas(x,h)$residuals[[h]]**2
    res = res + simulate_garchmidas(x, h)$tau[[h]] * simulate_garchmidas(x, h)$g[[h]]
  }

  return(res / n)
}

## ----- optimal forecast ----
### GARCH(1,1)
x = GARCH11

GARCH11_optimal_forecast <-
  function(x,h){
    # returns a list of optimal forecast for horizon 1 to h
    
    omega = x@fit$coef[["omega"]]
    alpha = x@fit$coef[["alpha1"]]
    beta = x@fit$coef[["beta1"]]
    
    n = length(GARCH11@fit$sigma)
    
    first_sigma =  omega + alpha * x@fit$residuals[[n]]**2  + beta * x@fit$sigma[[n]] #GARCH recursive relationship
    
    point_forecast <- function(k){
      return(omega * (1- (alpha+beta)^k)/(1-alpha-beta) + (alpha+beta)^(k-1) * first_sigma)
    }
    
    res = adply( 1:h, .margins = c(1), .fun = point_forecast) %>% dplyr::rename(c("forecast" = "V1")) %>% as_tibble()
    return(res)
  }


### GARCH-MIDAS
point_optimal_forecast <-
  function(x, h) {
    # the optimal forecast is only given for the h^th day ahead.
    # x is a mfGARCH object obtained by fit_mfgarch
    # h is the horizon of forecast
    
    alpha = x$par["alpha"][[1]]
    beta = x$par["beta"][[1]]
    gamma = x$par["gamma"][[1]]
    
    last_g = x$g[c(length(x$g))]
    last_epsilon = x$df.fitted[[main_index]][[length(x$df.fitted[[main_index]])]]
    last_tau = x$tau[[length(x$tau)]]
    

    next_g = next_g_func(alpha, beta, gamma, last_epsilon, last_tau, last_g) # g_1,t+1|t
    
    opt_forecast = x$tau.forecast * (1 + (alpha + gamma / 2 + beta) ** (h - 1) * (next_g - 1))
    
    return(opt_forecast)
  }

series_optimal_forecast <- function(x, h) { # makes the predictions for the next h days, from the last day of estimation data
  # x is a mfGARCH object obtained by fit_mfgarch
  # h is the horizon of forecast
  
  alpha = x$par["alpha"][[1]]
  beta = x$par["beta"][[1]]
  gamma = x$par["gamma"][[1]]
  
  last_g = x$g[[length(x$g)]]
  last_epsilon = x$df.fitted[[main_index]][[length(x$df.fitted[[main_index]])]]
  last_tau = x$tau[[length(x$tau)]]
  

  quoted_days = seq_quotation_date(x$df.fitted$date[[length(x$df.fitted$date)]], h)[-c(1)] # list of days where we want to do a forecast
  
  forecast_list = 1:h

  df = adply(
    forecast_list,
    .margins = c(1),
    .fun = function(h)
      point_optimal_forecast(x, h)
  ) 

  df = df %>% dplyr::rename(c("forecast" = "V1")) %>%
    mutate(horizon = as.numeric(X1)) %>% 
    select(c("horizon","forecast"))
  
  
  
  tib = as_tibble(df)
  tib$date = quoted_days
  
  return(tib)
  
}

real_time_optimal_forecast <- function(x, h, df_epsilon = NULL){ # makes the predictions for the next h days, from last day of quotation availabe
  # x is a mfGARCH object obtained by fit_mfgarch
  # h is the horizon of forecast
  # df_epsilon is the dataframe with all the epsilon available till today
  
  alpha = x$par["alpha"][[1]]
  beta = x$par["beta"][[1]]
  gamma = x$par["gamma"][[1]]
  
  if(is.null(df_epsilon)){
    stop("Please enter the df_epsilon dataframe (that is, the df_spx's last update)")
  }
  
  df_epsilon_new = df_epsilon %>% filter(date > x$df.fitted$date[[length(x$df.fitted$date)]]) %>% as_tibble()


  if(nrow(df_epsilon_new)==0){
    return(series_optimal_forecast(x,h))
  }

  
  last_g = x$g[[length(x$g)]]
  last_epsilon = x$df.fitted[[main_index]][[length(x$g)]]
  df_g_new = double(length(df_epsilon_new$date))
  
  for(i in 1:length(df_epsilon_new$date)){
    df_g_new[[i]] = next_g_func(alpha, beta, gamma, last_epsilon, x$tau.forecast, last_g)
    last_g = df_g_new[[i]]
    last_epsilon = df_epsilon_new[[main_index]][i]
  }
  
  forecast_list = 1:h
  list_opt_forecast = double(h)
  next_g = next_g_func(alpha, beta, gamma, last_epsilon, x$tau.forecast, last_g)
  
  for(i in forecast_list){
    list_opt_forecast[[i]] = x$tau.forecast * (1 + (alpha + gamma / 2 + beta) ** (i - 1) * (next_g - 1))
  }
  
  quoted_days = seq_quotation_date(df_epsilon_new$date[[length(df_epsilon_new$date)]], h)[-c(1)] # list of days where we want to do a forecast
  
  res = as.data.frame(cbind(quoted_days, list_opt_forecast)) %>% as_tibble() %>% dplyr::rename(c("date" = "quoted_days", "forecast" = "list_opt_forecast"))
  #res$date = res$date %>% as_date()
  
  return(res)

}



## ----- use of functions ------

# bug in display with ggplot2, because the columns of my df are not double, but lists of length 1
# library(ggplot2)

# res = simulate_garchmidas(GM_dhoust, h)
# res

# res_tibble = res %>% 
#   as_tibble()
# 
# res_tibble
#  
# res_tibble = res_tibble %>%
#  tibble::rownames_to_column(var = "horizon_car") %>%
#  mutate(horizon = as.numeric(horizon_car)) %>% 
#  mutate(epsilon = list_epsilon[[1]], g = list_g[[1]], tau = list_tau[[1]], residuals = list_residuals[[1]]) %>%
#  select(c("horizon","epsilon", "g", "residuals","tau"))
# 
# res_tibble
# 
# res$list_epsilon
# 
# p = ggplot(res_tibble) + geom_line(aes(x = horizon, y = g))
# p