library(readxl)

compute_realized_volatility <- function(x) {
  x$log_return = 100 * c(diff(log(x$Close)), NA)
  x$square = x$log_return ** 2
  
  df = x %>% 
    group_by(lubridate::date(date)) %>% 
    dplyr::summarise(RV = sum(square)) %>% 
    dplyr::rename("date" = "lubridate::date(date)") %>% 
    drop_na()

  df$RV = df$RV
  
  return(df)
}

### example of use
# five_min_data = read_excel("./data_eikon/spx_18_08_23.xlsx") %>% dplyr::rename("date" = "Local Date")
# five_min_data = five_min_data  %>%  as_tibble()
# 
# 
# x = five_min_data %>% mutate(day_date = ymd(date))
# x
# 
# RV_mafonction = compute_realized_volatility(x)
# RV_mafonction

