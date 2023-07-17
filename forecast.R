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

real_time_optimal_forecast <- function(x, h, df_epsilon = NULL, df_long_term1, df_long_term2 = NULL){ # makes the predictions for the next h days, from last day of quotation availabe
  
  # x is a mfGARCH object obtained by fit_mfgarch
  # h is the horizon of forecast
  # df_epsilon is the dataframe with all the epsilon available till today
  
  alpha = x$par["alpha"][[1]]
  beta = x$par["beta"][[1]]
  gamma = x$par["gamma"][[1]]
  
  if(is.null(df_epsilon)){
    stop("Please enter the df_epsilon dataframe (that is, the df_spx's last update)")
  }
  
  df_epsilon_new = df_epsilon %>% filter(date > x$df.fitted$date[[length(x$df.fitted$date)]])
  df_long_term1_new = df_long_term1 %>% filter(date > x$df.fitted$date[[length(x$df.fitted$date)]])
  
  if(nrow(df_epsilon_new)==0){
    return(series_optimal_forecast(x,h))
  } # so there is data available that has not been used in the estimation part,  that we can use now :
  
  
  # TAU computations (that have not been used in the estimation part) & g's
  if(nrow(df_long_term1_new) == 0){ # on a pas de nouvelle période "longue", mais que des petites, donc on calcule toutes les nouvelles valeurs de g avec tau.forecast
    
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
    
  } else{ 
    K = x$K
    
    phi = function(l){ # computation of the weighting scheme
      if(x$weighting.scheme == "beta.resctricted"){
        w1 = 1
        w2 = x$par[["w2"]]
      } else{
        w1 = x$par[["w1"]]
        w2 = x$par[["w2"]]
      }
      
      numerator = (l/(K+1))^(w1-1) * (1- l/(K+1))^(w2-1)
      denominator = adply(1:K, .margins = c(1), .fun = function(j) (j/(K+1))^(w1-1) * (1- j/(K+1))^(w2-1))$V1 %>% sum()
      
      return(numerator/denominator)
    }
    pi = x$par[["theta"]] * aaply(1:K, .margins = c(1), .fun = phi) #weights computation
    
    ### ---- disjunction of cases according to the frequency of the long-term variable ----
    if(length(colnames(df_long_term1))==2){ # case of the long term variable is daily
      
      #create the future df that will store the computed tau, the goal is the have the correct dates here
      df_tau_new = df_long_term1_new %>% 
        subset(!duplicated(date)) %>% 
        select("date")
      
      for(i in seq_along(df_tau_new$date)){
        last_Z = df_long_term1 %>%
          filter(date < df_tau_new$date[[i]] ) %>%
          tail(K)
        
        df_tau_new$value[[i]] = exp(x$par[["m"]] + sum(last_Z$value * pi))
      }
      
      #line = data.frame(year_month = x$df.fitted[["year_month"]][[length(x$df.fitted[["value"]])]], value = x$df.fitted[["tau"]][[length(x$df.fitted[["value"]])]])
      #df_tau_new = df_tau_new %>% bind_rows(line)
      
      # period between estimation and forecast : computation of the g's
      last_g = x$g[[length(x$g)]]
      last_epsilon = x$df.fitted[[main_index]][[length(x$g)]]
      df_g_new = double(length(df_epsilon_new$date))
      
      
      for(i in 1:length(df_epsilon_new$date)){

        
        tau = df_tau_new$value[[length(df_tau_new$value)]] # we hold tau constant for future values of tau
        
        matching_rows = df_tau_new$date == df_epsilon_new$date[[i]]
        if(any(matching_rows)){
          tau = df_tau_new$value[matching_rows][[1]]
        }
        
        
        df_g_new[[i]] = next_g_func(alpha, beta, gamma, last_epsilon, tau, last_g)
        
        last_g = df_g_new[[i]]
        last_epsilon = df_epsilon_new[[main_index]][i]
      }
      
      # forecast :
      forecast_list = 1:h
      list_opt_forecast = double(h)
      next_g = next_g_func(alpha, beta, gamma, last_epsilon, tau, last_g)
      
      for(i in forecast_list){
        list_opt_forecast[[i]] = x$tau.forecast * (1 + (alpha + gamma / 2 + beta) ** (i - 1) * (next_g - 1))
      }
      
      quoted_days = seq_quotation_date(df_epsilon_new$date[[length(df_epsilon_new$date)]], h)[-c(1)] # list of days where we want to do a forecast
      
      res = as.data.frame(cbind(quoted_days, list_opt_forecast)) %>% as_tibble() %>% dplyr::rename(c("date" = "quoted_days", "forecast" = "list_opt_forecast"))
      #res$date = res$date %>% as_date()
      
      print(df_tau_new$value[[1]])
      print(x$tau.forecast)
      
      return(res)
    }else{
      if(colnames(df_long_term1)[[3]]=="year_month"){
        
        #create the future df that will store the computed tau, the goal is the have the correct year_month here
        df_tau_new = df_long_term1_new %>% 
          subset(!duplicated(year_month)) %>% 
          select("year_month")
        df_tau_new$value = NA
        
        #compute of the new values of tau
        for(i in seq_along(df_tau_new$year_month)){
          last_Z = df_long_term1 %>%
            subset(!duplicated(year_month)) %>%
            filter(year_month < df_tau_new$year_month[[i]] ) %>%
            tail(K)
          
          df_tau_new$value[[i]] = exp(x$par[["m"]] + sum(last_Z$value * pi))
        }
        
        line = data.frame(year_month = x$df.fitted[["year_month"]][[length(x$df.fitted[["value"]])]], value = x$df.fitted[["tau"]][[length(x$df.fitted[["value"]])]])
        df_tau_new = df_tau_new %>% bind_rows(line)
        
        # period between estimation and forecast : computation of the g's
        last_g = x$g[[length(x$g)]]
        last_epsilon = x$df.fitted[[main_index]][[length(x$g)]]
        df_g_new = double(length(df_epsilon_new$date))
        
        
        for(i in 1:length(df_epsilon_new$date)){
          
          current_month = floor_date(df_epsilon_new$date[[i]],unit="month")
          
          
          
          tau = df_tau_new$value[[length(df_tau_new$value)]] # we hold tau constant for future values of tau
          
          matching_rows = df_tau_new$year_month == current_month
          if(any(matching_rows)){
            tau = df_tau_new$value[matching_rows][[1]]
          }
          
          
          df_g_new[[i]] = next_g_func(alpha, beta, gamma, last_epsilon, tau, last_g)
          
          last_g = df_g_new[[i]]
          last_epsilon = df_epsilon_new[[main_index]][i]
        }
        
        # forecast :
        forecast_list = 1:h
        list_opt_forecast = double(h)
        next_g = next_g_func(alpha, beta, gamma, last_epsilon, tau, last_g)
        
        for(i in forecast_list){
          list_opt_forecast[[i]] = x$tau.forecast * (1 + (alpha + gamma / 2 + beta) ** (i - 1) * (next_g - 1))
        }
        
        quoted_days = seq_quotation_date(df_epsilon_new$date[[length(df_epsilon_new$date)]], h)[-c(1)] # list of days where we want to do a forecast
        
        res = as.data.frame(cbind(quoted_days, list_opt_forecast)) %>% as_tibble() %>% dplyr::rename(c("date" = "quoted_days", "forecast" = "list_opt_forecast"))
        #res$date = res$date %>% as_date()
        
        print(df_tau_new$value[[1]])
        print(x$tau.forecast)
        
        return(res)
        
      }else{ #year_week case 
        
        
        
        #create the future df that will store the computed tau, the goal is the have the correct year_month here
        df_tau_new = df_long_term1_new %>% 
        subset(!duplicated(year_week)) %>% 
        select("year_week")
        df_tau_new$value = NA
      
        #compute of the new values of tau
        for(i in seq_along(df_tau_new$year_week)){
          last_Z = df_long_term1 %>%
            subset(!duplicated(year_week)) %>%
            filter(year_week < df_tau_new$year_week[[i]] ) %>%
            tail(K)
          
          df_tau_new$value[[i]] = exp(x$par[["m"]] + sum(last_Z$value * pi))
        }
        
        line = data.frame(year_week = x$df.fitted[["year_week"]][[length(x$df.fitted[["value"]])]], value = x$df.fitted[["tau"]][[length(x$df.fitted[["value"]])]])
        df_tau_new = df_tau_new %>% bind_rows(line)
        
        # period between estimation and forecast : computation of the g's
        last_g = x$g[[length(x$g)]]
        last_epsilon = x$df.fitted[[main_index]][[length(x$g)]]
        df_g_new = double(length(df_epsilon_new$date))
        
        
        for(i in 1:length(df_epsilon_new$date)){
          
          current_month = floor_date(df_epsilon_new$date[[i]],unit="month")
          
          
          
          tau = df_tau_new$value[[length(df_tau_new$value)]] # we hold tau constant for future values of tau
          
          matching_rows = df_tau_new$year_week == current_month
          if(any(matching_rows)){
            tau = df_tau_new$value[matching_rows][[1]]
          }
          
          
          df_g_new[[i]] = next_g_func(alpha, beta, gamma, last_epsilon, tau, last_g)
          
          last_g = df_g_new[[i]]
          last_epsilon = df_epsilon_new[[main_index]][i]
        }
        
        # forecast :
        forecast_list = 1:h
        list_opt_forecast = double(h)
        next_g = next_g_func(alpha, beta, gamma, last_epsilon, tau, last_g)
        
        for(i in forecast_list){
          list_opt_forecast[[i]] = x$tau.forecast * (1 + (alpha + gamma / 2 + beta) ** (i - 1) * (next_g - 1))
        }
        
        quoted_days = seq_quotation_date(df_epsilon_new$date[[length(df_epsilon_new$date)]], h)[-c(1)] # list of days where we want to do a forecast
        
        res = as.data.frame(cbind(quoted_days, list_opt_forecast)) %>% as_tibble() %>% dplyr::rename(c("date" = "quoted_days", "forecast" = "list_opt_forecast"))
        res$date = res$date %>% as_date()
        
        print(df_tau_new$value[[1]])
        print(x$tau.forecast)
        
        return(res)
      }
        
    }
     
  }
  
  
}
