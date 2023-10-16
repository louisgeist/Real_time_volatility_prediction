# function computes forecast based on Z. In order to compare with results obtained from boosted_forecast.
# This function leads to same results as boosted_forecast!

get_forecast_Z<- function(model_index, 
                            h_list, # forecast horizon
                            B, # Number of bootstrap samples
                            df_long_term1,
                            df_long_term2 = NULL,
                            data_last_date) { #later, data_last_date will be =date_end_training
  x = get(GM_models_list[[model_index]])
  h_max = max(h_list)
  
  alpha = x$par["alpha"][[1]]
  beta = x$par["beta"][[1]]
  gamma = x$par["gamma"][[1]]
  
  K = x$K
  g = x$g[[length(x$g)]] # last value of g = fitted short-term component
  df_residuals<-x$df.fitted$residuals[(K+1):length(x$df.fitted$residuals)]
  
  
  #date_list <- seq_quotation_date(data_last_date, n_forecasts - 1) # vector of size n_forecasts containing
  # origin dates for predictions
  
  # compute of all the tau: we need each tau 2 times, for the upper and the lower quantile
  #list_tau_t = double(2)
  #list_tau_t.plus.1 = double(2)
  
  if (length(df_long_term1)>2) {
    df_long_term1 <- df_long_term1 %>%
      subset(!duplicated(get(names(df_long_term1)[[3]])))
  }
  
  pi = rev(x$est.weighting) * x$par[["theta"]]
  
  if(is.null(df_long_term2)){
    last_Z = df_long_term1 %>%
      filter(date <= data_last_date) %>%
      tail(K + 1)
    
    list_tau_t = rep(x=exp(x$par[["m"]] + sum((last_Z$value %>% head(K)) * pi)), times = 2)
    list_tau_t.plus.1 = rep(x=exp(x$par[["m"]] + sum((last_Z$value %>% tail(K)) * pi)), times = 2)
  }else{ # remark : the second long term variable is necessarly daily, because of mfGARCH implementation
    K.two = x$K.two
    pi.two = rev(x$est.weighting.two) * x$par[["theta.two"]]
    
    last_Z = df_long_term1 %>%
      filter(date <= data_last_date) %>%
      tail(K + 1)
    
    last_Z.two = df_long_term2 %>% 
      filter(date <= data_last_date) %>%
      tail(K.two + 1)
    
    list_tau_t = rep(x = exp(x$par[["m"]] + sum((last_Z$value %>% head(K)) * pi) + sum((last_Z.two$value %>% head(K.two)) * pi.two)), times = 2)
    list_tau_t.plus.1 = rep(x = exp(x$par[["m"]] + sum((last_Z$value %>% tail(K)) * pi) + sum((last_Z.two$value %>% head(K.two)) * pi.two)), times = 2)
    
  }
  
  # compute of all the g: here we only compute g_{i+1}
  delta = alpha + gamma / 2 + beta
  
  Z <- df_residuals[length(df_residuals)]
  
  g_forecasts <- array(data = 0, dim = c(h_max, 1))
  
  constant = (1 - delta)
  
  g_i.plus.1 <-
    constant + (alpha + gamma * as.numeric(Z < 0)) * (Z ** 2) *g + beta * g
  
  for (i in 1:h_max) {
    g_forecasts[i,] <-(1 + (delta ^(i)) * (g_i.plus.1 - 1))
  } 
  
  #g_quantiles <- apply(g_bootstrap, MARGIN = 1, quantile, c(q_lower, q_upper))
  
  # compute of the forecasts
  ## array creation
  forecast_array <- array(0, dim = c(1, h_max))
  
  #date_list_plushdays = seq_quotation_date(date_list[[1]],n_forecasts + h_max) # to test in the loop if the forecast is in the same month/week as the origin
  
  #for (date in 1:n_forecasts) {  forecast_array[date, ] <- double(h_max)}
  
  ## add of tau values
  #for (i in 1:2) {
    forecast_array[1, ] <- rep(list_tau_t.plus.1[[1]], h_max)
    
    
    if(!is.null(df_long_term2) | length(df_long_term1) == 2){# tau is updated daily : nothing has to be done
      
      
    }else if(colnames(df_long_term1)[[3]] == "year_month"){
      origin_month <- month(data_last_date)
      
      for (h in 1:h_max) {
        if (origin_month == month(date_list_plushdays[1 + h])) {
          forecast_array[1, h] <- list_tau_t[[1]]
        }
        else{
          break
        }
      }
      
      
    } else if(colnames(df_long_term1)[[3]] == "year_week"){
      
      origin_week <- week(data_last_date)
      for (h in 1:h_max) {
        if (origin_week == week(date_list_plushdays[1 + h])) {
          forecast_array[1, h] <- list_tau_t[[1]]
        }
        else{
          break
        }
      }
    }else{
      stop("Problem in the long term variables (faced in boosted_forecast).")
    }
    
    
  #}
  
  ## forecast
  #for (i in 1:2) {
    forecast_array[1, ] <- # here we multiply tau with the bootstrap quantiles for g
      forecast_array[1, ] * g_forecasts[,1]
  #}
  
  return(forecast_array)
}