#
# user interface for real time volatility forecast
#
library(shiny)
library(plotly)
library(lubridate)

source("../forecast.R")
quota_days = seq_quotation_date(ymd("2023-05-01"), as.double(today())-as.double(ymd("2023-05-01"))) # function implemented in forecast.R
quota_days = quota_days[quota_days < today()]
all_days = seq(ymd("2023-05-01"),today()-days(1), by = "days")
no_quota_days = all_days[!(all_days %in% quota_days)]


list_models = c("GM_Rvol22","GM_vix","GM_vrp","GM_nfci","GM_dhoust", "GM_ip", "GM_nai","GM_vix_dhoust","GM_vix_nai","GM_vix_nfci","GM_vix_ip", "GARCH11") # also in server.R


# Define UI for application that draws a histogram
fluidPage(
  titlePanel("Real time volatility forecast"),
  h1("Optimal volatility forecast with GARCH-MIDAS models"),

  
  ##### SIDE BAR LAYOUT
  sidebarLayout(
    sidebarPanel(
      
      wellPanel(
        radioButtons("main_index", label = "Choose the index to be predicted : ",
                     choices = c("S&P 500" = "spx", "NASDAQ-100" = "ndx"),
                     selected = "spx"),
        
        
        dateInput(
          "origin_date",
          "Last date of data :",
          value = quota_days[length(quota_days)], #default origin_date is last monday where we have prediction
          min = "2023-05-01",
          max = today() - days(1),
          format = "yyyy-mm-dd",
          datesdisabled = no_quota_days,
          weekstart = 1
        ),
        
       
        # sliderInput : button to slide
        # numericInput : number to type
        sliderInput(
          "horizon",
          "Forecast horizon (in days):",
          value = 30,
          min = 1,
          max = 80
        ),
        
        checkboxGroupInput("models", "Choose the models used for volatility forecast :", 
                           choices = list_models,
                           selected = c("GM_dhoust", "GM_vix", "GM_vix_dhoust")),
        
        em("'GM' stands for GARCH-MIDAS."),
        em("The name after 'GM' are the explanatory variables."),
        
        wellPanel(
        checkboxInput("bool_ic", "Confidence interval activation", value =  FALSE, width = "4000px"),
        

        radioButtons("ic_level", "Level of the confidence interval :", 
                           choices = c("0.5","0.9"),
                           selected = c("0.9"))
        ),
        
        checkboxInput("bool_mult_plot", "Multiple plot view", value =  FALSE, width = "4000px")
        ),
      wellPanel(
      
      
      selectInput("explanatory_variable_model",
                  "Explanatory variable of model :" ,
                  choices = list_models[1:(length(list_models)-1)],
                  selected = "GM_dhoust")
      )
    ),
    
    ##### MAIN PANEL
    mainPanel(
      plotlyOutput("plot"), 
      plotlyOutput("plot_training_data"),
      plotlyOutput("spx_plot")
      )
  ),
  
  #### PANEL VALIDATION
  h1("Models evaluation"),
  
  numericInput("index_data_error_array", "Select another table number : ", value = 1, min = 1, max = length(list.files("../data_error_array"))),
  
  tags$style("
    .parameter-list {
      list-style-type: none;
      padding-left: 0;
    }
    .parameter-item {
      display: flex;
      align-items: center;
      margin-bottom: 10px;
    }
    .parameter-label {
      font-weight: bold;
      min-width: 200px;
    }
  "),
  
  tags$div(
    p("The following parameters were used:"),
    tags$ul(
      class = "parameter-list",
      tags$li(
        class = "parameter-item",
        tags$span("Main index : ", class = "parameter-label"), 
      uiOutput("tab_main_index")
      ),
      tags$li(
        class = "parameter-item",
        tags$span("Number of forecasts: ", class = "parameter-label"), 
        uiOutput("tab_n_forecasts")
      ),
      tags$li(
        class = "parameter-item",
        tags$span("Training period : ", class = "parameter-label"), 
        uiOutput("tab_begin_training"), 
        "  /  ", 
        uiOutput("tab_end_training")
      ),
      tags$li(
        class = "parameter-item",
        tags$span("Cumulative evaluation : ", class = "parameter-label"), 
        uiOutput("tab_cumulative")
      )
    )
  ),
  
  fluidRow(
    column(width = 6, h3("QLIKE mean error"), tableOutput("error_array")),
    column(width = 6, h3("Minimum mean error"), tableOutput("min_array"))
  )

)
