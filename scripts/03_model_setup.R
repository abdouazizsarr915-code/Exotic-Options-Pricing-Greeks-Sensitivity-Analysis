library(tidyverse)

if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)

meta <- readRDS("data/raw/meta.rds")
asset_summary <- readRDS("data/processed/asset_summary.rds")
rf <- readRDS("data/processed/rf.rds")

T <- meta$T

# Initializing pseudo RNG seed
set.seed(1)

# Defining simulation parameters
M <- 10000
N_rho <- 20
N_time <- 100
price_range <- 0.6
price_dif <- 0.01
vol_range <- 0.3
vol_dif <- 0.01

# Drawing independent random normal values
Z_1 <- rnorm(M)
Z_2 <- rnorm(M)

# Defining independent Brownian motions
W_1_T <- Z_1 * sqrt(T)
W_2_T <- Z_2 * sqrt(T)

# Defining initial asset prices
NVDA_0 <- asset_summary[[1, 4]]
AMD_0  <- asset_summary[[2, 4]]

# Defining asset volatility
NVDA_vol <- asset_summary[[1, 2]]
AMD_vol  <- asset_summary[[2, 2]]

# Defining strike prices as initial values
basket_strike <- (NVDA_0 + AMD_0) / 2
spread_strike <- AMD_0 - NVDA_0

# Partitioning correlation values
rho_partition <- seq(-1, 1, length.out = N_rho + 1)

# Creating increments of the initial asset prices
price_incremements <- seq(-price_range, price_range, price_dif)

# Partitioning the option duration (time to maturity)
time_partition <- seq(0, 1 + (1 / N_time), 1 / N_time)

# Creating increments of the volatility of returns
volatility_increments <- seq(-vol_range, vol_range, vol_dif)

model_setup <- list(
  T = T,
  M = M,
  N_rho = N_rho,
  N_time = N_time,
  price_range = price_range,
  price_dif = price_dif,
  vol_range = vol_range,
  vol_dif = vol_dif,
  Z_1 = Z_1,
  Z_2 = Z_2,
  W_1_T = W_1_T,
  W_2_T = W_2_T,
  NVDA_0 = NVDA_0,
  AMD_0 = AMD_0,
  NVDA_vol = NVDA_vol,
  AMD_vol = AMD_vol,
  basket_strike = basket_strike,
  spread_strike = spread_strike,
  rho_partition = rho_partition,
  price_incremements = price_incremements,
  time_partition = time_partition,
  volatility_increments = volatility_increments,
  rf = rf
)

saveRDS(model_setup, "data/processed/model_setup.rds")

cat("Model setup saved\n")