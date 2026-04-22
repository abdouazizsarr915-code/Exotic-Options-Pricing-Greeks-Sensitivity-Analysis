library(quantmod)
library(tidyquant)
library(tidyverse)
library(lubridate)

if (!dir.exists("data/raw")) dir.create("data/raw", recursive = TRUE)

# Defining the last sample period
t_0 <- "2025-11-01"

# Defining the number of sample periods (months)
n <- 60

# Defining the option maturity (years)
T <- 1

# Stock tickers vector
tickers <- c("NVDA", "AMD")

# Pulling stock price data from Yahoo Finance
prices_daily <- tickers %>%
  tq_get(get = "stock.prices")

# Loading risk free rate data from FRED
rf_monthly <- tq_get("TB3MS", get = "economic.data")

saveRDS(prices_daily, "data/raw/prices_daily.rds")
saveRDS(rf_monthly, "data/raw/rf_monthly.rds")

meta <- list(
  t_0 = t_0,
  n = n,
  T = T,
  tickers = tickers
)
saveRDS(meta, "data/raw/meta.rds")

cat("Raw data saved in data/raw/\n")