# LIBRARIES ----
library(shiny)
library(shinyWidgets)

library(plotly)
library(tidyverse)

library(rvest)
library(glue)
library(dplyr)

# source(file = "00_scripts/stock_analysis_functions.R")
get_stock_list <- function(stock_index = "DAX") {
  
  # Control for upper and lower case
  index_lower <- str_to_lower(stock_index)
  # Control if user input is valid
  index_valid <- c("dax", "sp500", "dow", "nasdaq")
  if (!index_lower %in% index_valid) {stop(paste0("x must be a character string in the form of a valid exchange.",
                                                  " The following are valid options:\n",
                                                  stringr::str_c(str_to_upper(index_valid), collapse = ", ")))
  }
  
  # Control for different currencies and different column namings in wiki
  vars <- switch(index_lower,
                 dax    = list(wiki     = "DAX", 
                               columns  = c("Ticker", "Company")),
                 sp500  = list(wiki     = "List_of_S%26P_500_companies", 
                               columns  = c("Symbol", "Security")),
                 dow    = list(wiki     = "Dow_Jones_Industrial_Average",
                               columns  = c("Symbol", "Company")),
                 nasdaq = list(wiki     = "NASDAQ-100",
                               columns  = c("Ticker", "Company"))
  )
  
  # Extract stock list depending on user input
  read_html(glue("https://en.wikipedia.org/wiki/{vars$wiki}")) %>% 
    
    # Extract table from wiki
    html_nodes(css = "#constituents") %>% 
    html_table() %>% 
    dplyr::first() %>% 
    as_tibble(.name_repair = "minimal") %>% 
    # Select desired columns (different for each article)
    dplyr::select(vars$columns) %>% 
    # Make naming identical
    set_names(c("symbol", "company")) %>% 
    
    # Clean (just relevant for DOW)
    mutate(symbol = str_remove(symbol, "NYSE\\:[[:space:]]")) %>% 
    
    # Sort
    arrange(symbol) %>%
    # Create the label for the dropdown list (Symbol + company name)
    mutate(label = str_c(symbol, company, sep = ", ")) %>%
    dplyr::select(label)
  
}

stock_list_tbl <- get_stock_list()

# UI ----

ui <- fluidPage(
  title = "Stock Analyzer",
  
  # 1.0 HEADER ----
  div(
    h1("Stock Analyzer"),
    p("This is my second shiny project")
  ),
  
  # 2.0 APPLICATION UI -----
  div(
    column(
      width = 4,
      wellPanel(
        pickerInput(
          inputId = "stock_selection",
          label = "Select a stock",
          choices = stock_list_tbl$label,
          multiple = FALSE,
          options = pickerOptions(
            actionsBox = FALSE,
            liveSearch = TRUE,
            size = 10
          )
        ),
        actionButton(
          inputId = "analyze",
          label = "Analyze",
          icon = icon("download")
        ),
        div(
          id = "analyze_info",
          h4("Analyze Info"),
          textOutput("selected_symbol")
        )
      )
    ),
    column(
      width = 8,
      div(
        id = "plot_div",
        h4(textOutput("plot_header")),  # Placeholder for the selected stock symbol
        plotlyOutput("stock_plot")
      )
    )
  ),
  
  # 3.0 ANALYST COMMENTARY ----
  div(
    h2("Analyst Commentary"),
    p("This is where the commentary will be displayed.")
  ),
  
  # 4.0 STOCK DATA ----
  div(
    h2("Stock Data"),
    verbatimTextOutput("stock_data")
  )
)


# SERVER ----
server <- function(input, output, session) {
  
  # Reactive value to store the selected stock symbol
  selected_stock <- reactiveVal()
  
  # Update the selected stock symbol whenever a stock is selected
  observeEvent(input$stock_selection, {
    print(stock_data_tbl)
    selected_stock(input$stock_selection)
  })
  
  # Generate the stock data based on the selected stock
  stock_data <- reactive({
    stock_symbol <- get_symbol_from_user_input(input$stock_selection)
    get_stock_data(stock_symbol, from = Sys.Date() - 365, to = Sys.Date())
  })
  
  # Render the plot based on the stock data
  output$stock_plot <- renderPlotly({
    plot_stock_data(stock_data())
  })
  
  # Generate the commentary based on the stock data
  output$commentary <- renderText({
    generate_commentary(stock_data(), user_input = input$stock_selection)
  })
  # Stock Symbol ----
  stock_symbol <- eventReactive(input$analyze, {
    input$stock_selection
  })
  # Render the selected symbol in the UI
  output$selected_symbol <- renderText({
    stock_symbol()
  })
  # Render the selected stock symbol in the plot header
  output$plot_header <- renderText({
    selected_stock()
  })
  # Get Stock Data ----
  stock_data_tbl <- reactive({
    stock_symbol() %>% 
      get_stock_data(from = today() - days(180), 
                     to = today(),
                     mavg_short = 20,
                     mavg_long = 50)
  })
  
  output$stock_data <- renderPrint({
    stock_data_tbl()
  })
  
}

# RUN APP ----
shinyApp(ui = ui, server = server)