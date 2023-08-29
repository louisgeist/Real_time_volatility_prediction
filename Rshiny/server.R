#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(RColorBrewer)
library(plyr)

list_models = c("GM_Rvol22","GM_vix","GM_vrp","GM_nfci","GM_dhoust", "GM_ip", "GM_nai","GM_vix_dhoust","GM_vix_nai","GM_vix_nfci","GM_vix_ip", "GARCH11") # also in ui.R

source("../qlike.error_analysis.R")
source("../eikon_data_preprocessing.R")
source("../forecast.R") # to import the function seq_quotation_date

five_min_data = read_excel("../data_eikon/spx_29_08_23.xlsx") %>% dplyr::rename("date" = "Local Date")
df_RV = compute_realized_volatility(five_min_data)

quantile_array <- readRDS("../data_daily_forecast/quantile_array.rds")

#-------- server logic --------
function(input, output, session) {
  
  ### reactive dataframes 
  df_training_data = reactive({
    location_training_data = paste0("../data_daily_forecast/training_data.csv")
    
    df = read.csv(file = location_training_data) %>% mutate(date = ymd(date))
    return(df)
  })
  

  df_forecasts = reactive({
    x = readRDS(paste0("../data_daily_forecast/", input$origin_date,"_forecast.RDS"))
    
    forecast_array = x$forecast_array %>% drop() # "drop()" removes the dimension 1 of the array
    df = as.data.frame(forecast_array %>% t())
    
    names(df) <- x$models
    df$date <- seq_quotation_date(input$origin_date, max(x$h_list))[2:(max(x$h_list)+1)] # origin_date -> last date of training

    return(df)
  })
  
  x_forecast = reactive({
    x = readRDS(paste0("../data_daily_forecast/", input$origin_date,"_forecast.RDS"))
    return(x)    
  })
  
  error_array = reactive({
    
    x = readRDS("../data_error_array/error_array1.rds")
    
    return(x)
  })
  
  
  
  ### reactive plots
  output$plot <- renderPlotly({

    
    # adjustement according to the forecast horizon
    df_filtered <- df_forecasts()[1:input$horizon, ]
    
    ### confidence interval - data frame building
    h_max_ci <- dim(quantile_array)[[3]]

    #lower bounds
    df_quantile = as.data.frame(quantile_array[1,,] %>% t())
    df_filtered_lowq <- df_quantile *(df_filtered[1:h_max_ci,1:length(list_models)]) #first slice : because CI are only computable up to h_max_ci / second slice : because last columns is the date
    names(df_filtered_lowq) <- list_models
    df_filtered_lowq <- df_filtered_lowq %>% rename_all(~ paste0("lower_",.))
    #upper bounds
    df_quantile = as.data.frame(quantile_array[2,,] %>% t())
    df_filtered_upq <- df_quantile *(df_filtered[1:h_max_ci,1:length(list_models)]) #first slice : because CI are only computable up to h_max_ci / second slice : because last columns is the date
    names(df_filtered_upq) <- list_models
    df_filtered_upq <- df_filtered_upq %>% rename_all(~ paste0("upper_",.))

    
    df_ic <- df_filtered_lowq %>% cbind(df_filtered_upq)# %>% cbind(df_filtered)
    df_ic$date <- df_filtered$date[1:h_max_ci]
    
    ### PLOT
    main_plot = plot_ly(df_filtered, x = ~date)
    
    for(i in seq_along(input$models)){
      new_model = input$models[[i]]
      
      # point forecast
      main_plot = main_plot %>% add_markers(data = df_filtered, y = as.formula(paste0("~", new_model)), name = new_model) #, line = list(color = color_palette[i])
      # "add_lines"
      
      # confidence interval
      if(input$bool_ic){
        main_plot = main_plot %>% add_ribbons(data = df_ic,
                                              x = ~date,
                                              ymin = as.formula(paste0("~lower_",new_model)),
                                              ymax = as.formula(paste0("~upper_",new_model)))
      }
      
    }
    
    # real volatility
    main_plot = main_plot %>% add_markers(data = df_RV, x = ~date, y = ~RV, line = list(color = "black"), name = "Real volatility")
      # add_lines instead of add_markers to remove the red points

    # gray area
    main_plot = main_plot %>%
      add_ribbons(x = c(df_RV$date[[1]], x_forecast()$origin_date), ymin = 0, ymax = max(df_RV$RV*1.01), data = df_filtered[df_filtered$date <= input$origin_date, ],
                  fillcolor = "rgba(211, 211, 211, 0.3)", line = list(color = "rgba(211, 211, 211, 0.5)"),
                  name = "Grayed Area")
    

    # caption
    main_plot = main_plot %>%
      layout(title = "Volatility point forecast from 1 day to 3 months ahead, with different models",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Volatility"))
    
    main_plot
  })


  output$plot_training_data <- renderPlotly({
    
    p = plot_ly(df_training_data(), x = ~date, y = ~get(input$explanatory_variable), type = 'scatter', mode = 'lines')
    
    p <- p %>% layout(title = "Explanatory variable (should be enough 'smooth')",
                      xaxis = list(title = "Date"),
                      yaxis = list(title = input$explanatory_variable))
    
    p
    
  })
  
  output$spx_plot <- renderPlotly({
     p = plot_ly(df_training_data(), x = ~date, y = ~spx, mode = 'lines') %>%
       layout(title = "S&p500 (at closing)",
              xaxis = list(title = "Date"),
              yaxis = list(title = "Close value"))
     p
  })

  # validation tables
  output$error_array <- renderTable({
    error_array_analysis(error_array()$error_array, error_array()$models, error_array()$h_list)$error_mean
  }, rownames = TRUE)
  
  output$min_array <- renderTable({
    error_array_analysis(error_array()$error_array, error_array()$models, error_array()$h_list)$error_mean_min
  }, rownames = TRUE) #, spacing = "l"
  
  
  output$tab_main_index<- renderUI({
    return(error_array()$main_index)
  })
  output$tab_n_forecasts<- renderUI({
    return(error_array()$n_forecasts)
  })
  output$tab_begin_training<- renderUI({
    return(error_array()$date_begin_training) 
  })
  output$tab_end_training <- renderUI({
    return(error_array()$date_end_training)
  })
  output$tab_cumulative<- renderUI({
    return(error_array()$cum_evaluation)
  })
  
}