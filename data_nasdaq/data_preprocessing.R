
df_ndx = import_ndx() #import of daily ndx
df_ndx


p = ggplot(df_ndx) + geom_line(aes(x = date, y = ndx))
p


# ---- compute of the volatility -----

library(readxl)

five_min_ndx = read_excel("./data_nasdaq/ndx_20230705_c.xlsx") %>% dplyr::rename("date" = "Local Date")
five_min_ndx = five_min_ndx  %>%  as_tibble()

x = five_min_ndx
x_bis = x %>% mutate(day_date = day(date))

compute_realized_volatility <- function(x) {
  x$log_return = c(diff(log(x$Close)), NA)
  x$square = x$log_return ** 2
  
  df = x %>% 
    group_by(lubridate::date(date)) %>% 
    dplyr::summarise(RV = sum(square)) %>% 
    dplyr::rename("date" = "lubridate::date(date)")
  
  return(df)
}

RV = compute_realized_volatility(x)

# plots
p_rv = ggplot(RV) + geom_point(aes(x = date, y = RV))
p_rv


x$log_return = c(diff(log(x$Close)), NA)
x$square = x$log_return ** 2

p = ggplot(x) + geom_line(aes(x = date, y = Close))
p

p = ggplot(x) + geom_line(aes(x = date, y = log_return))
p

p_square = ggplot(x) + geom_line(aes(x = date, y = square))
p_square
