# function script
boosted_forecast = function(model_index,
                            h_list,
                            n_forecasts,
                            df_epsilon,
                            df_long_term1,
                            df_long_term2 = NULL) {
  x = get(GM_models_list[[model_index]])
  h_max = max(h_list)
  
  alpha = x$par["alpha"][[1]]
  beta = x$par["beta"][[1]]
  gamma = x$par["gamma"][[1]]
  
  K = x$K
  
  if (is.null(df_epsilon)) {
    stop("Please enter the df_epsilon dataframe (that is, the df_spx's last update)")
  }
  
  estimation_last_date = x$df.fitted$date[[length(x$df.fitted$date)]]
  
  date_list <-
    seq_quotation_date(estimation_last_date, n_forecasts - 1)
  
  # compute of all the tau
  list_tau_t = double(n_forecasts)
  list_tau_t.plus.1 = double(n_forecasts)
  
  if (length(df_long_term1)>2) {
    df_long_term1 <- df_long_term1 %>%
      subset(!duplicated(get(names(df_long_term1)[[3]])))
  }
  
  pi = rev(x$est.weighting) * x$par[["theta"]]
  
  if(is.null(df_long_term2)){
    for (i in seq_along(date_list)) {
      #not really fast with an adply (for n_forecats = 1000, from 45sec to 40sec)
      last_Z = df_long_term1 %>%
        filter(date <= date_list[[i]]) %>%
        tail(K + 1)
      
      list_tau_t[[i]] = exp(x$par[["m"]] + sum((last_Z$value %>% head(K)) * pi))
      list_tau_t.plus.1[[i]] = exp(x$par[["m"]] + sum((last_Z$value %>% tail(K)) * pi))
    }
  }else{ # remark : the second long term variable is necessarly daily, because of mfGARCH implementation
    for (i in seq_along(date_list)) {
      K.two = x$K.two
      pi.two = rev(x$est.weighting.two) * x$par[["theta.two"]]
      
      last_Z = df_long_term1 %>%
        filter(date <= date_list[[i]]) %>%
        tail(K + 1)
      
      last_Z.two = df_long_term2 %>% 
        filter(date <= date_list[[i]]) %>%
        tail(K.two + 1)
      
      list_tau_t[[i]] = exp(x$par[["m"]] + sum((last_Z$value %>% head(K)) * pi) + sum((last_Z.two$value %>% head(K.two)) * pi.two))
      list_tau_t.plus.1[[i]] = exp(x$par[["m"]] + sum((last_Z$value %>% tail(K)) * pi) + sum((last_Z.two$value %>% tail(K.two)) * pi.two))
      
    }
  }
  
  
  # compute of all the g
  delta = alpha + gamma / 2 + beta
  
  df_epsilon_for_g_computation = df_epsilon %>%  filter(date >= date_list[[1]]) %>% head(n_forecasts) # on a les valeurs depuis le 31/12/2014, dernier jour du train set
  
  list_epsilon = df_epsilon_for_g_computation[[main_index]]
  
  list_constant = rep(x = (1 - delta), times = n_forecasts)
  
  list_g_i.plus.1 <-
    list_constant + (alpha + gamma * as.numeric(list_epsilon < 0)) * list_epsilon **
    2 / list_tau_t
  
  list_g_i.plus.1[[1]] <-
    list_g_i.plus.1[[1]] + beta * x$g[[length(x$g)]]
  
  for (i in 2:n_forecasts) {
    list_g_i.plus.1[[i]] <-
      list_g_i.plus.1[[i]] + beta * list_g_i.plus.1[[i - 1]]
  }
  
  # compute of the forecasts
  ## array creation
  forecast_array <- array(0, dim = c(n_forecasts, h_max))
  
  for (date in 1:n_forecasts) {
    forecast_array[date, ] <- double(h_max)
  }
  
  ## add of tau values
  for (date_index in seq_along(date_list)) {
    forecast_array[date_index, ] <-
      rep(list_tau_t.plus.1[[date_index]], h_max)
    
    current_month <- month(date_list[[date_index]])
    
    for (h in 1:h_max) {
      if (current_month == month(date_list[[date_index]] + days(h))) {
        forecast_array[date_index, h_index] <- list_tau_t[[date_index]]
      }
      else{
        break
      }
    }
  }
  
  ## forecast
  for (date_index in seq_along(date_list)) {
    forecast_array[date_index, ] <-
      forecast_array[date_index, ] * (1 + (delta ^(1:h)) * (list_g_i.plus.1[[date_index]] - 1))
  }
  
  return(forecast_array)
}


cumsum_on_forecast_array <- function(forecast_array,h_list){
  for(date_index in seq_along(forecast_array[,1])){
    forecast_array[date_index,] <- forecast_array[date_index,] %>%  cumsum()
  }
  
  return(forecast_array[,h_list])
}
