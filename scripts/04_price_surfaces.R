library(tidyverse)

if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)

model_setup <- readRDS("data/processed/model_setup.rds")

T <- model_setup$T
W_1_T <- model_setup$W_1_T
W_2_T <- model_setup$W_2_T
NVDA_0 <- model_setup$NVDA_0
AMD_0 <- model_setup$AMD_0
NVDA_vol <- model_setup$NVDA_vol
AMD_vol <- model_setup$AMD_vol
basket_strike <- model_setup$basket_strike
spread_strike <- model_setup$spread_strike
rho_partition <- model_setup$rho_partition
price_incremements <- model_setup$price_incremements
rf <- model_setup$rf

payoff <- function(x) {
  max(x, 0)
}

# BASKET prices
b_call_vec <- c()
b_put_vec  <- c()
b_0_vec    <- c()
b_rho_vec  <- c()

for (rho in rho_partition) {
  W_NVDA_T <- W_1_T
  W_AMD_T  <- rho * W_1_T + sqrt(1 - rho^2) * W_2_T
  
  NVDA_path <- exp((rf - NVDA_vol^2 / 2) * T + NVDA_vol * W_NVDA_T)
  AMD_path  <- exp((rf - AMD_vol^2 / 2) * T + AMD_vol * W_AMD_T)
  
  for (percentage_change in price_incremements) {
    basket_0 <- (NVDA_0 * (1 + percentage_change) + AMD_0 * (1 + percentage_change)) / 2
    
    NVDA_t <- NVDA_0 * (1 + percentage_change) * NVDA_path
    AMD_t  <- AMD_0 * (1 + percentage_change) * AMD_path
    basket_t <- (NVDA_t + AMD_t) / 2
    
    basket_call <- exp(-rf * T) * mean(vapply(basket_t - basket_strike, payoff, FUN.VALUE = numeric(1)))
    basket_put  <- exp(-rf * T) * mean(vapply(basket_strike - basket_t, payoff, FUN.VALUE = numeric(1)))
    
    b_call_vec <- c(b_call_vec, basket_call)
    b_put_vec  <- c(b_put_vec, basket_put)
    b_0_vec    <- c(b_0_vec, basket_0)
    b_rho_vec  <- c(b_rho_vec, rho)
  }
}

df_basket_prices <- data.frame(
  b_call_vec = b_call_vec,
  b_put_vec  = b_put_vec,
  b_0_vec    = b_0_vec,
  b_rho_vec  = b_rho_vec
)

# SPREAD prices
s_call_vec <- c()
s_put_vec  <- c()
s_0_vec    <- c()
s_rho_vec  <- c()

for (rho in rho_partition) {
  W_NVDA_T <- W_1_T
  W_AMD_T  <- rho * W_1_T + sqrt(1 - rho^2) * W_2_T
  
  NVDA_path <- exp((rf - NVDA_vol^2 / 2) * T + NVDA_vol * W_NVDA_T)
  AMD_path  <- exp((rf - AMD_vol^2 / 2) * T + AMD_vol * W_AMD_T)
  
  for (percentage_change in price_incremements) {
    spread_0 <- AMD_0 * (1 + percentage_change) - NVDA_0 * (1 - percentage_change)
    
    NVDA_t <- NVDA_0 * (1 - percentage_change) * NVDA_path
    AMD_t  <- AMD_0 * (1 + percentage_change) * AMD_path
    spread_t <- AMD_t - NVDA_t
    
    spread_call <- exp(-rf * T) * mean(vapply(spread_t - spread_strike, payoff, FUN.VALUE = numeric(1)))
    spread_put  <- exp(-rf * T) * mean(vapply(spread_strike - spread_t, payoff, FUN.VALUE = numeric(1)))
    
    s_call_vec <- c(s_call_vec, spread_call)
    s_put_vec  <- c(s_put_vec, spread_put)
    s_0_vec    <- c(s_0_vec, spread_0)
    s_rho_vec  <- c(s_rho_vec, rho)
  }
}

df_spread_prices <- data.frame(
  s_call_vec = s_call_vec,
  s_put_vec  = s_put_vec,
  s_0_vec    = s_0_vec,
  s_rho_vec  = s_rho_vec
)

saveRDS(df_basket_prices, "data/processed/df_basket_prices.rds")
saveRDS(df_spread_prices, "data/processed/df_spread_prices.rds")

cat("Price surfaces saved\n")