library("ggplot2")
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

q = ggplot(df_ip_partial) + geom_line(aes(x = date , y = value))
q

p_raw_ip = ggplot(df_ip_partial) + geom_line(aes(x = date, y= IP))
p_raw_ip

# CFNAI

df_nai = import_nai()
p = ggplot(df_nai)+geom_line(aes(x = df_nai$date, y = df_nai$value))
p

# ----- comparison with Rebekka's data -----
tab_ip = read.csv(file = "./data/IP_03072023.csv")
tab_ip = tab_ip %>% select(c("date","IP")) #bad download

tab_nai = read.csv("../data/nai_120623.csv", sep = ";") %>% 
  select(c("Date","CFNAI")) %>%
  mutate(date = ym(Date),value = as.numeric(sub(",", ".", CFNAI, fixed = TRUE))) %>% 
  select(-c("Date",CFNAI))


plot(tab_ip, type = "l")
plot(tab_nai, type = "l")

# mfGARCH data
df_mfgarch = mfGARCH::df_mfgarch

plot(x = df_mfgarch$date, y = df_mfgarch$nai, type = "l")
plot(x = df_mfgarch$date, y = df_mfgarch$dindpro, type = "l")

# -------- data download on chicagofed.org -------
cf_data = read.csv(file = "../data/cfnai_0307_ chicago_fed_download.csv", sep = ";") %>% 
  select(c("Date","CFNAI")) %>%
  mutate(date = ym(Date),value = as.numeric(sub(",", ".", CFNAI, fixed = TRUE))) %>% 
  select(-c("Date",CFNAI))

plot(cf_data, type = "l")

df_ip %>% tail()
df_nai %>% tail()
cf_data %>% tail()
df_ip_partial %>% tail()

# ------ raw from alfred ------
df_ip_partial = alfred::get_alfred_series(
  series_id = "INDPRO",
  series_name = "IP",
  observation_start = "1959-01-01",
  realtime_start = ymd(today()),
  api_key = "4f77313cfd688a6d4d70ccf8e650f038"
) %>% as_tibble() %>% select(c("date", "IP")) %>% mutate(value = c(NA, 100*diff(log(df_ip_partial$IP))))

df_nai_partial = alfred::get_alfred_series(
  series_id = "CFNAI",
  series_name = "nai",
  observation_start = "1959-01-01",
  realtime_start = ymd(today()),
  api_key = "4f77313cfd688a6d4d70ccf8e650f038"
) %>% as_tibble() %>% select(c("date", "nai"))

df = df_nai_partial %>% merge(df_ip_partial, by = "date")


p_nai = ggplot(data = df)+geom_line(aes(x = date, y = nai))
p_ip = ggplot(data = df) + geom_line(aes(x = date, y = value))

p_nai
p_ip
