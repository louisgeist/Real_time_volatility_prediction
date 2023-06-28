source(file = "../data_import_tools.R")


# comparison of differienced ip and nai
df_ip = import_ip()
p = ggplot(df_ip)+geom_line(aes(x = df_ip$date, y = df_ip$value))
p



# Industrial production
df_ip_partial = alfred::get_alfred_series(
  series_id = "INDPRO",
  series_name = "IP",
  observation_start = "1959-01-01",
  realtime_start = ymd(today()),
  api_key = "4f77313cfd688a6d4d70ccf8e650f038"
) %>% as_tibble() %>% select(c("date", "IP"))

p = ggplot(df_ip_partial) + geom_line(aes(x = date , y = IP))
p

df_ip_partial$value = c(NA, 100 * diff(log(df_ip_partial$IP)))

p = ggplot(df_ip_partial) + geom_line(aes(x = date , y = value))
p



# CFNAI

df_nai = import_nai()
p = ggplot(df_nai)+geom_line(aes(x = df_nai$date, y = df_nai$value))
p