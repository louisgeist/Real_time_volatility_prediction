#
# user interface for real time volatility forecast
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
fluidPage(
  titlePanel("Real time volatility forecast"),
  h1("Optimal volatility forecast with GARCH-MIDAS models"),
  
  ##### SIDE BAR LAYOUT
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        dateInput(
          "origin_date",
          "Date of data download :",
          value = floor_date(today()-days(1), unit = "week", week_start = 1), #default origin_date is last monday where we have prediction
          min = "2023-05-01",
          max = today() - days(1),
          format = "yyyy-mm-dd",
          daysofweekdisabled = c(0,6),
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
        
        checkboxGroupInput("models", "Choose the displayed models :", 
                           choices = c("GM_Rvol22","GM_vix","GM_vrp","GM_nfci","GM_dhoust", "GM_ip", "GM_nai","GM_vix_dhoust","GM_vix_nai","GM_vix_nfci","GM_vix_ip", "GARCH11"),
                           selected = c("GM_dhoust", "GM_vix", "GM_vix_dhoust"))
        ),
      wellPanel(
      
      
      selectInput("explanatory_variable",
                  "Explanatory variable :",
                  choices = c("Rvol22","vix","vrp","nfci","dhoust","ip","nai"),
                  selected = "dhoust")
      )
    ),
    
    ##### MAIN PANEL
    mainPanel(
      plotlyOutput("plot"), 
      plotlyOutput("plot_training_data"),
      plotlyOutput("spx_plot")
      )
  ),
  
  h1("Models evaluation"),
  
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
    column(width = 6, h1("QLIKE mean error"), tableOutput("error_array")),
    column(width = 6, h1("Minimum mean error"), tableOutput("min_array"))
  )

)
