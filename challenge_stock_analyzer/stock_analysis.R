# Business Analytics with Data Science and Machine Learning ----
# Building Business Data Products ----
# STOCK ANALYZER APP - DATA ANALYSIS -----

# APPLICATION DESCRIPTION ----
# - The user will select 1 stock from the SP 500 stock index
# - The functionality is designed to pull the past 180 days of stock data by default
# - We will implement 2 moving averages - short (fast) and long (slow)
# - We will produce a timeseries visualization
# - We will produce automated commentary based on the moving averages

# LIBRARIES ----
library(tidyverse)
library(fs)
library(glue)

library(rvest)
library(quantmod)

library(plotly)

# 1.0 GET STOCK LIST ----

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
                               columns  = c("Ticker symbol", "Company")),
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


# 2.0 EXTRACT SYMBOL BASED ON USER INPUT ----

library(stringr)
library(purrr)

user_input <- "AAPL, Apple Inc."

split_string <- str_split(user_input, ",\\s*")[[1]]
ticker <- split_string[1]


get_symbol_from_user_input <- function(user_input) {
  split_string <- str_split(user_input, ",\\s*")[[1]]
  ticker <- split_string[1]
}

library(quantmod)
library(timetk)
library(dplyr)
library(stringr)
library(lubridate)
library(zoo)

get_stock_data <- function(stock_symbol, from, to, mavg_short = 20, mavg_long = 50) {
  stock_data <- quantmod::getSymbols(
    stock_symbol,
    src = "yahoo",
    from = from,
    to = to,
    auto.assign = FALSE
  ) %>%
    timetk::tk_tbl(preserve_index = TRUE, silent = TRUE) %>%
    mutate(currency = case_when(
      str_detect(stock_symbol, "\\.DE") ~ "EUR",
      TRUE ~ "USD"
    )) %>%
    na.omit() %>%
    mutate(mavg_short = zoo::rollmeanr(.data[[colnames(.)[2]]], k = mavg_short, fill = NA),
           mavg_long = zoo::rollmeanr(.data[[colnames(.)[2]]], k = mavg_long, fill = NA))
  
  stock_data_tbl <- stock_data %>%
    select(date = index, adjusted = .data[[colnames(.)[2]]], mavg_short, mavg_long, currency)
  
  return(stock_data_tbl)
}

# Example usage:
stock_data_tbl <- get_stock_data("AAPL", from = "2020-06-01", to = "2021-01-12", mavg_short = 5, mavg_long = 50)






# Example usage:
stock_data_tbl <- get_stock_data("AAPL", from = "2020-06-01", to = "2021-01-12", mavg_short = 5, mavg_long = 50)

# 3.0 GET STOCK DATA ----



# 4.0 PLOT STOCK DATA ----

# Add the currency_format function to your script
currency_format <- function(currency) {
  if (currency == "USD") {
    scales::dollar_format(largest_with_cents = 10)
  } else if (currency == "EUR") {
    scales::dollar_format(
      prefix = "",
      suffix = " €",
      big.mark = ".",
      decimal.mark = ",",
      largest_with_cents = 10
    )
  } else {
    identity  # Default to no formatting if currency is not recognized
  }
}

# Modify your plot code
g <- stock_data_tbl %>%
  pivot_longer(
    cols = c(adjusted, mavg_short, mavg_long),
    names_to = "legend",
    values_to = "value",
    names_ptypes = list(legend = factor())
  ) %>%
  ggplot(aes(x = date, y = value, color = legend, linetype = legend, group = legend)) +
  geom_line() +
  labs(y = "Adjusted Share Price", x = "") +
  scale_y_continuous(
    labels = currency_format(stock_data_tbl$currency[[1]])
  )

# Convert the plot to an interactive plotly plot
ggplotly(g)




plot_stock_data <- function(stock_data) {
  g <- stock_data %>%
    pivot_longer(
      cols = c(adjusted, mavg_short, mavg_long),
      names_to = "legend",
      values_to = "value",
      names_ptypes = list(legend = factor())
    ) %>%
    ggplot(aes(x = date, y = value, color = legend, linetype = legend, group = legend)) +
    geom_line() +
    labs(y = "Adjusted Share Price", x = "") +
    scale_y_continuous(
      labels = currency_format(stock_data$currency[[1]])
    )
  
  ggplotly(g)
}

# Example function call
"ADS.DE" %>% 
  get_stock_data(from = "2020-06-01", to = "2021-01-12", mavg_short = 5, mavg_long = 50) %>%
  plot_stock_data()


# 5.0 GENERATE COMMENTARY ----

warning_signal <- stock_data_tbl %>%
  tail(1) %>% # Get last value
  mutate(mavg_warning_flag = mavg_short < mavg_long) %>% # Compare long and short
  pull(mavg_warning_flag)


if (warning_signal) {
  commentary <- "The short-term moving average is less than the long-term moving average, indicating a potentially negative trend."
} else {
  commentary <- "The short-term moving average is greater than the long-term moving average, indicating a potentially positive trend."
}


n_short <- sum(is.na(stock_data_tbl$mavg_short)) + 1
n_long <- sum(is.na(stock_data_tbl$mavg_long)) + 1

if (warning_signal) {
  str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is below the {n_long}-day moving average, indicating negative trends")
} else {
  str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is above the {n_long}-day moving average, indicating positive trends")
}



generate_commentary <- function(data, user_input) {
  warning_signal <- data %>%
    tail(1) %>%
    mutate(mavg_warning_flag = mavg_short < mavg_long) %>%
    pull(mavg_warning_flag)
  
  n_short <- sum(is.na(data$mavg_short)) + 1
  n_long <- sum(is.na(data$mavg_long)) + 1
  
  if (warning_signal) {
    str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is below the {n_long}-day moving average, indicating negative trends.")
  } else {
    str_glue("In reviewing the stock prices of {user_input}, the {n_short}-day moving average is above the {n_long}-day moving average, indicating positive trends.")
  }
}

generate_commentary(stock_data_tbl, user_input = user_input)


# 6.0 TEST WORKFLOW ----

from <- "2022-01-01"
to <- "2022-06-30"


# get_stock_list("DAX")
"ADS.DE, Adidas" %>% 
  get_symbol_from_user_input() %>%
  get_stock_data(from = from, to = to ) %>%
  # plot_stock_data() %>%
  generate_commentary(user_input = "ADS.DE, Adidas")
## In reviewing the stock prices of ADS.DE, Adidas, the 20-day moving average is above the 50-day moving average, indicating positive trends


# 7.0 SAVE SCRIPTS ----


fs::dir_create("00_scripts") #create folder

# write functions to an R file
dump(
  list = c("get_stock_list", "get_symbol_from_user_input", "get_stock_data", "plot_stock_data", "currency_format", "generate_commentary"),
  file = "00_scripts/stock_analysis_functions.R", 
  append = FALSE) # Override existing 


