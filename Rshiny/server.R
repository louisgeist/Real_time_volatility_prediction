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

source("../qlike.error_analysis.R")
source("../eikon_data_preprocessing.R")

five_min_data = read_excel("../data_eikon/spx_18_08_23.xlsx") %>% dplyr::rename("date" = "Local Date")
df_RV = compute_realized_volatility(five_min_data)

#-------- server logic --------
function(input, output, session) {
  
  ### To show all prediction models at this date
  # But it makes a reset of selected models each time we change the "origin_date"
  # observeEvent(input$origin_date, {
  #    
  #    #location_forecasts = paste0("../data_plot/",input$origin_date,"_forecasts.csv")
  #    #df = read.csv(file = location_forecasts) %>% mutate(date = ymd(date))
  #    
  #    available_models = names(df_forecasts())
  #    
  #    updateCheckboxGroupInput(session, "models", choices = available_models)
  #  })
  
  
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
    df$date <- seq_quotation_date(input$origin_date, max(x$h_list))[1:max(x$h_list)]

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
    
    # plot
    main_plot = plot_ly(df_filtered, x = ~date)
    
    color_palette = brewer.pal(length(input$models), "Set1")
    
    for(i in seq_along(input$models)){
      new_model = input$models[[i]]
      
      main_plot = main_plot %>% add_markers(y = as.formula(paste0("~", new_model)), name = new_model) #, line = list(color = color_palette[i])
      # "add_lines"
    }
    
    # real volatility
    main_plot = main_plot %>% add_markers(data = df_RV, x = ~date, y = ~RV, line = list(color = "black"), name = "Real volatility")
      # add_lines instead of add_markers to remove the red points

    # gray area
    main_plot = main_plot %>%
      add_ribbons(x = c(df_RV$date[[1]], x_forecast()$origin_date - days(1)), ymin = 0, ymax = max(df_RV$RV*1.01), data = df_filtered[df_filtered$date <= input$origin_date, ],
                  fillcolor = "rgba(211, 211, 211, 0.3)", line = list(color = "rgba(211, 211, 211, 0.5)"),
                  name = "Grayed Area")
    

    # caption
    main_plot = main_plot %>%
      layout(title = "Graph",
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

  
  output$error_array <- renderTable({
    error_array_analysis(error_array()$error_array, error_array()$models, error_array()$h_list)$error_mean
  }, rownames = TRUE)
  
  output$min_array <- renderTable({
    error_array_analysis(error_array()$error_array, error_array()$models, error_array()$h_list)$error_mean_min
  }, rownames = TRUE) #, spacing = "l"
  
  output$params<- renderUI({
    params <- error_array()$main_index

    return(params)
  })
}