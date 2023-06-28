library(ggplot2)
library(tidyverse)
library(plotly)
library(RColorBrewer)

# ----- Data loading -------
df_training_data = read.csv(file = "../data_plot/2023-06-26_training_data.csv") %>% mutate(date = ymd(date))
df_forecasts = read.csv(file = "../data_plot/2023-06-26_forecasts.csv") %>% mutate(date = ymd(date))

#----- plot ----- 

