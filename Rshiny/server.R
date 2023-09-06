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

list_models = c(
  "GM_dhoust",
  "GM_ip",
  "GM_nai",
  "GM_nfci",
  "GM_Rvol22",
  "GM_vix",
  "GM_vrp",
  "GM_vix_dhoust",
  "GM_vix_ip",
  "GM_vix_nai",
  "GM_vix_nfci",
  "GARCH11"
)

source("../qlike.error_analysis.R")
source("../eikon_data_preprocessing.R")
source("../forecast.R") # to import the function seq_quotation_date

quota_days = seq_quotation_date(ymd("2023-05-01"), as.double(today())-as.double(ymd("2023-05-01"))) # function implemented in forecast.R
quota_days = quota_days[quota_days < today()]
all_days = seq(ymd("2023-05-01"),today()-days(1), by = "days")
no_quota_days = all_days[!(all_days %in% quota_days)]

# font
t <- list(
  size = 14 #,
  #color = "blue"
  )

#-------- server logic --------
function(input, output, session) {
  # ---- I. Load of the data according to inputs ----
  
  five_min_data <- reactive({
    if(input$main_index == "spx") {
      
      five_min_data <-read_excel("../data_eikon/spx_29_08_23.xlsx") %>% dplyr::rename("date" = "Local Date")
      
    } else if(input$main_index == "ndx"){
    
      five_min_data <- read_excel("../data_eikon/ndx_31_08_23.xlsx") %>% dplyr::rename("date" = "Local Date")
    }
    return(five_min_data)
  })
  
  df_RV <- reactive({ compute_realized_volatility(five_min_data()) })
  

  quantile_array <- reactive({
    readRDS(paste0("../data_daily_forecast/",input$main_index,"/quantile_array.rds"))
  })

  df_training_data = reactive({
    path_training_data = paste0("../data_daily_forecast/",input$main_index,"/training_data.csv")
    
    df = read.csv(file = path_training_data) %>% mutate(date = ymd(date))
    
    return(df)
  })
  
  GM_active_models = reactive({
    readRDS(paste0("../data_daily_forecast/",input$main_index,"/",input$origin_date,"_GM_models.rds"))
  })
  
  df_forecasts = reactive({
    x = readRDS(paste0("../data_daily_forecast/",input$main_index,"/",input$origin_date,"_forecast.RDS" ))
    
    forecast_array = x$forecast_array %>% drop() # "drop()" removes the dimension 1 of the array
    df = as.data.frame(forecast_array %>% t())
    
    names(df) <- x$models
    df$date <-
      seq_quotation_date(input$origin_date, max(x$h_list))[2:(max(x$h_list) + 1)] # origin_date -> last date of training
    
    return(df)
  })
  
  x_forecast = reactive({
    x_f = readRDS(paste0("../data_daily_forecast/",input$main_index,"/",input$origin_date,"_forecast.RDS" ))
    return(x_f)
  })
  
  error_array = reactive({
    x = readRDS(paste0("../data_error_array/error_array",input$index_data_error_array,".rds"))
    return(x)
  })
  
  # adjustement according to the forecast horizon
  df_filtered <- reactive({
    df_forecasts()[1:input$horizon,]
  })
  
  ### confidence interval - data frame building
  h_max_ci <- reactive({dim(quantile_array())[[2]]})
  
  df_quantile_low = reactive({
    as.data.frame(quantile_array()[ , ,1] %>% t())
  })
  
  df_quantile_up = reactive({
    as.data.frame(quantile_array()[ , ,2] %>% t())
  })
  
  
  df_ic_reactive <- reactive({
    #lower bounds
    
    df_filtered_lowq <- reactive({
      x_df = df_quantile_low() * (df_filtered()[1:h_max_ci(), 1:length(list_models)]) #first slice : because CI are only computable up to h_max_ci / second slice : because last columns is the date
      names(x_df) <- list_models
      x_df <- x_df %>% rename_all( ~ paste0("lower_", .))
      
      return(x_df)
    })
    
    
    #upper bounds
    df_filtered_upq <- reactive({
      x_df = df_quantile_up() * (df_filtered()[1:h_max_ci(), 1:length(list_models)]) #first slice : because CI are only computable up to h_max_ci / second slice : because last columns is the date
      names(x_df) <- list_models
      x_df <- x_df %>% rename_all( ~ paste0("upper_", .))
      
      return(x_df)
    })
    
    
    df_ic <- df_filtered_lowq() %>% cbind(df_filtered_upq())
    df_ic$date <- df_filtered()$date[1:h_max_ci()]
    
    return(df_ic)
  })

  
  # ----- II. REACTIVE PLOTS----
  output$plot <- renderPlotly({
    colors <-
      c(
        "#1f77b4",
        "#ff7f0e",
        "#2ca02c",
        "#d62728",
        "#9467bd",
        "#8c564b",
        "#e377c2",
        "#7f7f7f",
        "#bcbd22",
        "#17becf",
        "#ff9896",
        "#aec7e8"
      )
    

    
    if(input$bool_mult_plot){
      ### MULTIPLE PLOT DISPLAY
      plot_list <- list()
      
      main_plot = plot_ly(x = ~ df_filtered()$date)
      
      for (i in seq_along(input$models)) {
        new_model = input$models[[i]]
        model_color = colors[[i]]
        
        p = plot_ly()
        
        # point forecast
        p = p %>% add_markers(
          data = df_filtered(),
          x = ~ date,
          y = as.formula(paste0("~", new_model)),
          name = new_model,
          marker = list(color = model_color)
        )
        
        # confidence interval
        if (input$bool_ic) {
          p = p %>% add_ribbons(
            data = df_ic_reactive(),
            x = ~ date,
            ymin = as.formula(paste0("~lower_", new_model)),
            ymax = as.formula(paste0("~upper_", new_model)),
            fillcolor = paste0(model_color, "22"),
            #suffix 22 in order to reduce opacity,
            line = list(color = model_color, width = 0.2),
            showlegend = FALSE
          )
        }

        p = p %>% add_markers(
          data = df_RV(),
          x = ~ date,
          y = ~ RV,
          line = list(color = "black", width = 1),
          marker = list(color = "red", width = 1),
          name = "Real volatility",
          showlegend = FALSE
        ) # add_lines instead of add_markers to remove the red points

        # gray area
        p = p %>% add_ribbons(
            data = df_filtered()[df_filtered()$date <= input$origin_date,],
            x = c(df_RV()$date[[1]], x_forecast()$origin_date),
            ymin = 0,
            ymax = max(df_RV()$RV * 1.01),
            fillcolor = "rgba(211, 211, 211, 0.3)",
            line = list(color = "rgba(211, 211, 211, 0.5)"),
            name = "Training period",
            showlegend = FALSE
          )

        plot_list[[i]] <- p

      }
      main_plot <- subplot(plot_list, nrows = length(input$models),shareX = TRUE)
      
      main_plot <- main_plot %>% layout(title = "Volatility forecast from 1 day to 3 months ahead, with different models",
                                        xaxis = list(title = none,
                                          rangebreaks = list(list(bounds = list("sat","mon")),
                                                             list(values = as.character(no_quota_days))
                                          )))
      
    }else{
      ### ONE PLOT DISPLAY
      
      main_plot = plot_ly(x = ~ df_filtered()$date)
      
      for (i in seq_along(input$models)) {
        new_model = input$models[[i]]
        model_color = colors[[i]]
        
        # point forecast
        main_plot = main_plot %>% add_markers(
          data = df_filtered(),
          x = ~ date,
          y = as.formula(paste0("~", new_model)),
          name = new_model,
          marker = list(color = model_color)
        )
        
        # confidence interval
        if (input$bool_ic) {
          main_plot = main_plot %>% add_ribbons(
            data = df_ic_reactive(),
            x = ~ date,
            ymin = as.formula(paste0("~lower_", new_model)),
            ymax = as.formula(paste0("~upper_", new_model)),
            fillcolor = paste0(model_color, "22"),
            #suffix 22 in order to reduce opacity,
            line = list(color = model_color, width = 0.2),
            showlegend = FALSE
          )
        }
        
        
      }
      
      # real volatility
      main_plot = main_plot %>% add_markers(
        data = df_RV(),
        x = ~ date,
        y = ~ RV,
        line = list(color = "black", width = 1),
        marker = list(color = "red", width = 1),
        name = "Real volatility"
      ) # add_lines instead of add_markers to remove the red points
      
      # gray area
      main_plot = main_plot %>%
        add_ribbons(
          x = c(df_RV()$date[[1]], x_forecast()$origin_date),
          ymin = 0,
          ymax = max(df_RV()$RV * 1.01),
          data = df_filtered()[df_filtered()$date <= input$origin_date,],
          fillcolor = "rgba(211, 211, 211, 0.3)",
          line = list(color = "rgba(211, 211, 211, 0.5)"),
          name = "Training period"
        )
      
      
      # caption
      main_plot = main_plot %>%
        layout(
          title = "Volatility point forecast from 1 day to 3 months ahead, with different models",
          xaxis = list(title = none,
                       type = "date",
                       domain = df_filtered()$date,
                       rangebreaks = list(list(bounds = list("sat","mon")),
                                          list(values = as.character(no_quota_days))
                       )),
          yaxis = list(title = none)
        )
      
      
    }
    
    main_plot = main_plot  %>% 
      layout(font = t) %>% 
      config(toImageButtonOptions = list(format = "png", width = 750, height = 500))
    
    main_plot
  })
  
  
  output$plot_training_data <- renderPlotly({

    variables = unlist(strsplit(input$explanatory_variable_model, "_"))[-1]

    GM_chosen_model <- GM_active_models()[[input$explanatory_variable_model]]
    df_tau = GM_chosen_model$df.fitted %>% select(c("date","tau")) %>% drop_na()
    
    p <- plot_ly(
      df_training_data(),
      x = ~ date,
      y = ~ get(variables[1]),
      type = 'scatter',
      mode = 'lines',
      name = variables[1],
      yaxis = "y1"
    )
    
    ay_tau <- list(
      tickfont = list(color = "orange"),
      overlaying = "y",
      side = "right",
      title = "tau"
    )
    
    p <- p %>%  add_trace(
      data = df_tau,
      x = ~date,
      y = ~tau,
      type = 'scatter',
      mode = 'lines',
      name = "tau",
      yaxis = "y2"
    )
    p <- p %>% layout(title = "Explanatory variable and its transformation",
                      yaxis2 = ay_tau,
                      yaxis = list(title = variables[1]),
                      xaxis = list(range = list(ymd("2001-01-01"), today()),
                                   title = none))
    
    
    if(length(variables)==2){
      p <- p %>%  add_trace(
        data = df_training_data(),
        x = ~ date,
        y = ~ get(variables[2]),
        type = 'scatter',
        mode = 'lines',
        name = variables[2]
      )
      
      p <- p %>% layout(yaxis = list(title = paste0(variables[1]," and ", variables[2])),
                        font = t) %>% 
        config(toImageButtonOptions = list(format = "png", width = 750, height = 500))
      
    }
    
    
    
    
    p
    
  })
  
  output$spx_plot <- renderPlotly({
    p = plot_ly(
      df_training_data(),
      x = ~ date,
      y = ~ get(input$main_index),
      mode = 'lines'
    ) %>%
      layout(
        title = "S&p500 (at closing)",
        xaxis = list(xmin = ymd("1990-01-01")),
        yaxis = list(title = "Close value")
      )
    p
  })
  
  # validation tables
  output$error_array <- renderTable({
    error_array_analysis(error_array()$error_array,
                         error_array()$models,
                         error_array()$h_list)$error_mean
  }, rownames = TRUE)
  
  output$min_array <- renderTable({
    x = error_array_analysis(error_array()$error_array,
                         error_array()$models,
                         error_array()$h_list)$error_mean_min
    x = ifelse(x, "True"," ")
    x
  }, rownames = TRUE) #, spacing = "l"
  
  
  output$tab_main_index <- renderUI({
    return(error_array()$main_index)
  })
  output$tab_n_forecasts <- renderUI({
    return(error_array()$n_forecasts)
  })
  output$tab_begin_training <- renderUI({
    return(error_array()$date_begin_training)
  })
  output$tab_end_training <- renderUI({
    return(error_array()$date_end_training)
  })
  output$tab_cumulative <- renderUI({
    return(error_array()$cum_evaluation)
  })
  
}