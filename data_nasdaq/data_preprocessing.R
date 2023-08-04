

df_ndx = import_ndx() #import of daily ndx
df_ndx


# ---- compute of the volatility -----

library(readxl)
library(highfrequency)
library(ggplot2)


#write.csv(realized_library, file = "./realized_library.csv")

five_min_ndx = read_excel("./data_nasdaq/ndx_20230705_c.xlsx") %>% dplyr::rename("date" = "Local Date")
five_min_ndx = five_min_ndx  %>%  as_tibble()

x = five_min_ndx
x_bis = x %>% mutate(day_date = day(date))

compute_realized_volatility <- function(x) {
  x$log_return = 100 * c(diff(log(x$Close)), NA)
  x$square = x$log_return ** 2
  
  df = x %>% 
    group_by(lubridate::date(date)) %>% 
    dplyr::summarise(RV = mean(square)) %>% 
    dplyr::rename("date" = "lubridate::date(date)") %>% 
    drop_na()

  df$RV = df$RV
  
  return(df)
}

RV_mafonction = compute_realized_volatility(x)


RV_highfreq_computation = highfrequency::RV(five_min_ndx$Close, )



# plots
p_rv = ggplot(RV) + geom_point(aes(x = date, y = sqrt(RV)))
p_rv

# plot with highfrequency data
p_rv_hf = ggplot(realized_library) + geom_line(aes(x = date,y = sqrt(rv5)))
p_rv_hf

