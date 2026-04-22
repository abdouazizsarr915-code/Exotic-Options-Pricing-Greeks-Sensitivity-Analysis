library(tidyverse)

if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)

model_setup <- readRDS("data/processed/model_setup.rds")

Z_1 <- model_setup$Z_1
Z_2 <- model_setup$Z_2
NVDA_0 <- model_setup$NVDA_0
AMD_0 <- model_setup$AMD_0
NVDA_vol <- model_setup$NVDA_vol
AMD_vol <- model_setup$AMD_vol
basket_strike <- model_setup$basket_strike
spread_strike <- model_setup$spread_strike
rho_partition <- model_setup$rho_partition
time_partition <- model_setup$time_partition
rf <- model_setup$rf

payoff <- function(x) {
  max(x, 0)
}

# BASKET theta
b_call_vec <- c()
b_put_vec  <- c()
b_time_vec <- c()
b_rho_vec  <- c()

for (rho in rho_partition) {
  for (t in time_partition) {
    W_1_t <- Z_1 * sqrt(t)
    W_2_t <- Z_2 * sqrt(t)
    
    W_NVDA_t <- W_1_t
    W_AMD_t  <- rho * W_1_t + sqrt(1 - rho^2) * W_2_t
    
    NVDA_path <- exp((rf - NVDA_vol^2 / 2) * t + NVDA_vol * W_NVDA_t)
    AMD_path  <- exp((rf - AMD_vol^2 / 2) * t + AMD_vol * W_AMD_t)
    
    NVDA_t <- NVDA_0 * NVDA_path
    AMD_t  <- AMD_0 * AMD_path
    basket_t <- (NVDA_t + AMD_t) / 2
    
    basket_call <- exp(-rf * t) * mean(vapply(basket_t - basket_strike, payoff, FUN.VALUE = numeric(1)))
    basket_put  <- exp(-rf * t) * mean(vapply(basket_strike - basket_t, payoff, FUN.VALUE = numeric(1)))
    
    b_call_vec <- c(b_call_vec, basket_call)
    b_put_vec  <- c(b_put_vec, basket_put)
    b_time_vec <- c(b_time_vec, t)
    b_rho_vec  <- c(b_rho_vec, rho)
  }
}

df_basket_prices_time <- data.frame(
  b_call_vec = b_call_vec,
  b_put_vec  = b_put_vec,
  b_time_vec = b_time_vec,
  b_rho_vec  = b_rho_vec
)

df_basket_theta <- data.frame(
  b_call_vec = numeric(),
  b_put_vec = numeric(),
  b_time_vec = numeric(),
  b_rho_vec = numeric(),
  b_call_theta = numeric(),
  b_put_theta = numeric()
)

for (rho in rho_partition) {
  options_df_temp <- df_basket_prices_time[df_basket_prices_time$b_rho_vec == rho, ]
  options_df_temp <- options_df_temp %>%
    mutate(
      b_call_theta = (b_call_vec - lead(b_call_vec)) / (lead(b_time_vec) - b_time_vec),
      b_put_theta  = (b_put_vec - lead(b_put_vec)) / (lead(b_time_vec) - b_time_vec)
    )
  options_df_temp <- head(options_df_temp, -1)
  df_basket_theta <- rbind(df_basket_theta, options_df_temp)
}

# SPREAD theta
s_call_vec <- c()
s_put_vec  <- c()
s_time_vec <- c()
s_rho_vec  <- c()

for (rho in rho_partition) {
  for (t in time_partition) {
    W_1_t <- Z_1 * sqrt(t)
    W_2_t <- Z_2 * sqrt(t)
    
    W_NVDA_t <- W_1_t
    W_AMD_t  <- rho * W_1_t + sqrt(1 - rho^2) * W_2_t
    
    NVDA_path <- exp((rf - NVDA_vol^2 / 2) * t + NVDA_vol * W_NVDA_t)
    AMD_path  <- exp((rf - AMD_vol^2 / 2) * t + AMD_vol * W_AMD_t)
    
    NVDA_t <- NVDA_0 * NVDA_path
    AMD_t  <- AMD_0 * AMD_path
    spread_t <- AMD_t - NVDA_t
    
    spread_call <- exp(-rf * t) * mean(vapply(spread_t - spread_strike, payoff, FUN.VALUE = numeric(1)))
    spread_put  <- exp(-rf * t) * mean(vapply(spread_strike - spread_t, payoff, FUN.VALUE = numeric(1)))
    
    s_call_vec <- c(s_call_vec, spread_call)
    s_put_vec  <- c(s_put_vec, spread_put)
    s_time_vec <- c(s_time_vec, t)
    s_rho_vec  <- c(s_rho_vec, rho)
  }
}

df_spread_prices_time <- data.frame(
  s_call_vec = s_call_vec,
  s_put_vec  = s_put_vec,
  s_time_vec = s_time_vec,
  s_rho_vec  = s_rho_vec
)

df_spread_theta <- data.frame(
  s_call_vec = numeric(),
  s_put_vec = numeric(),
  s_time_vec = numeric(),
  s_rho_vec = numeric(),
  s_call_theta = numeric(),
  s_put_theta = numeric()
)

for (rho in rho_partition) {
  options_df_temp <- df_spread_prices_time[df_spread_prices_time$s_rho_vec == rho, ]
  options_df_temp <- options_df_temp %>%
    mutate(
      s_call_theta = (s_call_vec - lead(s_call_vec)) / (lead(s_time_vec) - s_time_vec),
      s_put_theta  = (s_put_vec - lead(s_put_vec)) / (lead(s_time_vec) - s_time_vec)
    )
  options_df_temp <- head(options_df_temp, -1)
  df_spread_theta <- rbind(df_spread_theta, options_df_temp)
}

saveRDS(df_basket_theta, "data/processed/df_basket_theta.rds")
saveRDS(df_spread_theta, "data/processed/df_spread_theta.rds")

cat("Theta surfaces saved\n")