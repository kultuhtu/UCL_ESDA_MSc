library(ggplot2)
library(gridExtra)
library(shiny)
library(tidyverse)
library(bslib)
library(shinythemes)
library(ggthemes)
library(webr)

# Read the file
source("plot_electricity_consumption_overview.R")
source("plot_temperature_overview.R")
source("plot_household_distribution.R")
source("plot_electricity_consumption.R")
source("plot_confusion_matrix.R")

# Load electricity consumption dataset
df_consumption_daily_2011_2012 <- read_csv(
  "data/household_consumption_2011_2012_daily_sum.csv", 
  show_col_types = FALSE)
df_consumption_daily_2013 <- read_csv(
  "data/household_consumption_2013_daily_sum.csv", 
  show_col_types = FALSE)
df_consumption_daily_2014 <- read_csv(
  "data/household_consumption_2014_daily_sum.csv", 
  show_col_types = FALSE)

# Load grouped electricity consumption
df_joined_daily_avg <- read_csv("data/df_joined_daily_avg.csv", 
                                show_col_types = FALSE)
df_all_year <- read_csv("data/df_all_year.csv", show_col_types = FALSE)

# Load household dataset
df_households <- read_csv("data/household_information.csv", 
                          show_col_types = FALSE)
df_households <- df_households %>% filter(Acorn_grouped %in% c("Affluent", 
                                                               "Comfortable", 
                                                               "Adversity"))
df_households <- df_households %>% rename(Group = Acorn_grouped, 
                                          Tariff = stdorToU)
df_households[df_households$Tariff == "Std", "Tariff"] <- "Standard"
df_households[df_households$Tariff == "ToU", "Tariff"] <- "DToU"

# Load temperature dataset
df_weather_daily <- read_csv("data/weather_daily.csv", show_col_types = FALSE)

# Load confusion matrices dataset
df_cm_all <- read_csv("data/confusion_matrices.csv", show_col_types = FALSE)

# Front-end (UI)
ui <- navbarPage(
  selected='Overview',
  theme = bs_theme(version = 5, bootswatch = "morph"), 
  collapsible = TRUE,
  HTML('<a style="text-decoration:none;cursor:default;
       font-size:24px" class="active" href="#">
       London Smart Meter Analysis and Households Classification</a>'), 
  id="nav",
  windowTitle = "London Smart Meter Analysis and Households Classification",
  
  # Tab 1: Overview panel
  tabPanel(
    "Overview", 
    id = "panel1", 
    sidebarLayout(
      sidebarPanel(
        p("There are three tabs on this website (see top section on the right 
          side of the title) that you can choose: Overview, Data Exploration, 
          and Model Performance. The Data Exploration section gives general 
          information about our datasets, and the Model Performance section 
          comprises our model performance results.", 
          style = "font-size:16px; color:#485785"),
        br(),
        p("Overview", style = "font-size:20px; color:#485785"),
        p("In recent years, climate change has been a critical global issue. 
          One of the initiatives to tackle this is increasing energy efficiency. 
          In the meantime, smart meter captures electricity consumer behaviour. 
          By analysing the historial consumption data, we can find patterns and 
          generate useful insights to manage our energy more efficiently.", 
          style = "font-size:16px; color:#485785"),
        dateRangeInput(
          inputId = "input_date",
          label = HTML('<FONT color="#485785"><FONT size="3px">Select date: 
                       </FONT></FONT>'),
          format = "yyyy-mm-dd",
          start = min(df_joined_daily_avg$date) + 1,
          end = max(df_joined_daily_avg$date) - 1,
          min = min(df_joined_daily_avg$date) + 1,
          max = max(df_joined_daily_avg$date) - 1
        ),
        br(),
        p("We conducted this research to identify typical consumer behaviour in 
          London. Some machine learning models were developed to classify 
          households based on their consumption variation. We also investigated 
          whether weather (temperature) data can help improving our model 
          performance. The top figure on the right side is the average daily 
          electricity consumption for each household's group. 
          The bottom figure is the daily temperature dataset taken 
          from Heathrow Airport station. The band with blue colour shows the 
          observed range (values between its maximum and minimum temperatures 
          at that day). You can choose the date range (see filter box above) 
          to filter the starting and ending date of the graph.", 
          style = "font-size:16px; color:#485785")
      ),
      mainPanel(
        fluidRow(plotOutput("electricity_consumption_overview_plot")),
        fluidRow(plotOutput("temperature_overview_plot"))
      )
    )
  ),
  
  # Tab 2: Data exploration
  tabPanel("Data Exploration", id='panel2',
           sidebarLayout(
             sidebarPanel(
              p("Data Exploration", style = "font-size:20px; color:#485785"),
              p("The top right figure shows the distribution of households by 
                each group and tariff scheme. There are three distinct 
                households' groups (Adversity, Comfortable, and Affluent) and 
                two types of tariff scheme 
                (Standard and Dynamic Time of Use/DToU). 
                The checkbox below is provided to filter the household's group.", 
                style = "font-size:16px; color:#485785"), 
                checkboxGroupInput(inputId = "input_group",
                                   label = HTML('<FONT color="#485785">
                                                <FONT size="3px">Select group: 
                                                </FONT></FONT>'),
                                   choices = 
                                     unique(df_all_year$Acorn_grouped),
                                   selected = 
                                     unique(df_all_year$Acorn_grouped)),
              p("The line chart on the right shows the average daily 
                electricity consumption for each day of month, week, and month.
                As we expected, weekly and monthly charts show an increasing 
                demand of electricity in winter.", 
                style = "font-size:16px; color:#485785"),
              radioButtons(inputId = "input_filter",
                           label = HTML('<FONT color="#485785"><FONT size="3px">
                                        Averaged by: </FONT></FONT>'),
                           choices = c("Day of Month",
                                       "Week",
                                       "Month"),
                           selected = "Week"
                           ),
             ),
             mainPanel(
               fluidRow(plotOutput("household_distribution_plot")), 
               fluidRow(plotOutput("electricity_consumption_plot"))
             )
           )
           ),
    
  
  # Tab 3: Model performance
  tabPanel("Model Performance", id='panel3',
    sidebarLayout(
      sidebarPanel(
        p("Model performance", style = "font-size:20px; color:#485785"),
        p("The aim of building a classification model is to predict 
        households' group (Adversity/Comfortable/Affluent) based on their 
        electricity consumption behaviour.", 
          style = "font-size:16px; color:#485785"),
        br(),
        p("Our research shows that incorporating temperature feature when 
        building the model can improve the model performance (accuracy). 
        On the right side, you can see the confusion matrices comparison 
        between the models for selected model and number of classses.", 
          style = "font-size:16px; color:#485785"),
        br(),
        selectInput(inputId = "model_filter", 
                     label = HTML('<FONT color="#485785"><FONT size="3px">
                                  Select model: </FONT></FONT>'),
                     choices = c("Logistic Regression", 
                                 "Random Forest", 
                                 "XGBoost"),
                     selected = "Random Forest"),
        br(),
        radioButtons(inputId = "classes_filter",
                     label = HTML('<FONT color="#485785"><FONT size="3px">
                                  Select number of classes: </FONT></FONT>'),
                     choices = c(2, 3), 
                     selected = 2),
      ),
      mainPanel(
        br(),
        br(),
        br(),
        br(), 
        br(),
        plotOutput("confusion_matrix_plot"),
        br(),
        br(),
        br(),
        br(),
        br(), 
        )
      )
    ),
  
  # Tags
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  )
)

# R Code (server)
server <- function(input, output, session) {
  # Overview - Electricity consumption plot
  output$electricity_consumption_overview_plot <- renderPlot(
    plot_electricity_consumption_overview(
      df_joined_daily_avg,
      input$input_date
    ),
    bg = "transparent", height = 400
  )
  
  output$temperature_overview_plot <- renderPlot(
    plot_temperature_overview(
      df_joined_daily_avg,
      input$input_date
    ),
    bg = "transparent", height = 325
  )
  
  output$household_distribution_plot <- renderPlot(
    plot_household_distribution(df_households, input$input_group),
    bg = "transparent", height = 385
  )
  
  output$electricity_consumption_plot <- renderPlot(
    plot_electricity_consumption(df_all_year, 
                                 input$input_filter,
                                 input$input_group),
    bg = "transparent", height = 335
  )
  
  output$confusion_matrix_plot <- renderPlot(
    plot_confusion_matrix(df_cm_all, 
                          input$classes_filter, 
                          input$model_filter),
    bg='transparent',
    )
  
  # Observe
  observe({
    if((length(input$input_group)) < 1 | (length(input$input_group)) > 3){
      updateCheckboxGroupInput(session, "input_group", 
                               selected = c("Adversity", "Comfortable", 
                                            "Affluent"))
    }
    if(input$input_date[1] > input$input_date[2]){
      updateDateRangeInput(
        session,
        inputId = "input_date",
        start = min(df_joined_daily_avg$date) + 1,
        end = max(df_joined_daily_avg$date) - 1,
        min = min(df_joined_daily_avg$date) + 1,
        max = max(df_joined_daily_avg$date) - 1
      )
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)