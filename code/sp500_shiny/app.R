# s&p 500

library(tidyverse)
library(ggplot2)
library(shiny)
library(plotly)
library(tidyquant)
library(rsconnect)
library(readr)

finished_data <- read_rds("sp500.rds")

finished_data$Date <- as.Date(finished_data$Date, format = "%Y-%m-%d")  # Ensure the format is "yyyy-mm-dd"

finished_data$Date <- as.Date(finished_data$Date, format = "%Y-%m-%d")  # Ensure the format is "yyyy-mm-dd"

# Grab the input vectors we need
metrics <- c("Open", "High", "Low", "Close", "Adj.Close")
sp500_symbols <- tq_index("SP500")$symbol %>%
  intersect(finished_data$stock_name) %>% 
  sort()

# Extract trading days from the dataset
trading_days <- finished_data %>%
  pull(Date) %>% # Assuming the 'Date' column is already renamed and in Date class
  unique() %>%
  sort()

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Individual Stocks"),
  
  # Dropdowns to enable user input
  sidebarLayout(
    sidebarPanel(
      selectInput("stock_names",
                  "Stock:",
                  choices = sp500_symbols),
      selectInput("metrics",
                  "Metric:",
                  choices = metrics),
      dateRangeInput("date_range",
                     "Date Range:",
                     start = min(finished_data$Date),
                     end = max(finished_data$Date),
                     format = "yyyy-mm-dd",
                     min = min(finished_data$Date),
                     max = max(finished_data$Date),
                     separator = " to ")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("barPlot")
    )
  )
)

# Define server logic required to draw line graph
server <- function(input, output) {
  
  output$barPlot <- renderPlotly({
    label <- str_c(input$metrics, "_cat") # Dynamic column reference
    
    # Filter data based on stock name and date range
    p <- finished_data %>%
      filter(stock_name == input$stock_names,
             Date >= input$date_range[1] & Date <= input$date_range[2]) %>% # Filter by date range
      group_by(!!sym(label)) %>%
      count() %>%
      mutate({{label}} := as.numeric(!!sym(label))/100) %>% # This weird syntax is necessary because R treats the dynamic label as a string, rather than a column name
      ggplot() +
      geom_bar(aes(x = !!sym(label), y = n), stat = "identity") +
      labs(title = "", x = "Cent Price", y = str_c("Recorded ", input$metrics, "s")) + # Dynamic axis labeling
      theme_minimal()
    
    ggplotly(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)