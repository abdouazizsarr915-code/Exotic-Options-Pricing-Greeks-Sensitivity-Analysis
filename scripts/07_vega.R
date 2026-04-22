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
vol_dif <- model_setup$vol_dif
rf <- model_setup$rf

payoff <- function(x) {
  max(x, 0)
}

# =====================
# BASKET VEGA
# =====================

b_call_vec <- c()
b_put_vec  <- c()
b_0_vec    <- c()
b_rho_vec  <- c()
b_vol_vec  <- c()

for (rho in rho_partition) {
  W_NVDA_T <- W_1_T
  W_AMD_T  <- rho * W_1_T + sqrt(1 - rho^2) * W_2_T
  
  NVDA_path <- exp((rf - (NVDA_vol + vol_dif)^2 / 2) * T + (NVDA_vol + vol_dif) * W_NVDA_T)
  AMD_path  <- exp((rf - (AMD_vol + vol_dif)^2 / 2) * T + (AMD_vol + vol_dif) * W_AMD_T)
  
  for (percentage_change in price_incremements) {
    basket_0 <- (NVDA_0 * (1 + percentage_change) + AMD_0 * (1 + percentage_change)) / 2
    
    NVDA_t <- NVDA_0 * (1 + percentage_change) * NVDA_path
    AMD_t  <- AMD_0 * (1 + percentage_change) * AMD_path
    basket_t <- (NVDA_t + AMD_t) / 2
    
    basket_vol <- sd(log(basket_t / basket_0))
    
    basket_call <- exp(-rf * T) * mean(vapply(basket_t - basket_strike, payoff, FUN.VALUE = numeric(1)))
    basket_put  <- exp(-rf * T) * mean(vapply(basket_strike - basket_t, payoff, FUN.VALUE = numeric(1)))
    
    b_call_vec <- c(b_call_vec, basket_call)
    b_put_vec  <- c(b_put_vec, basket_put)
    b_0_vec    <- c(b_0_vec, basket_0)
    b_rho_vec  <- c(b_rho_vec, rho)
    b_vol_vec  <- c(b_vol_vec, basket_vol)
  }
}

df_basket_prices_vol_up <- data.frame(
  b_call_vec = b_call_vec,
  b_put_vec  = b_put_vec,
  b_0_vec    = b_0_vec,
  b_rho_vec  = b_rho_vec,
  b_vol_vec  = b_vol_vec
)

b_call_vec <- c()
b_put_vec  <- c()
b_0_vec    <- c()
b_rho_vec  <- c()
b_vol_vec  <- c()

for (rho in rho_partition) {
  W_NVDA_T <- W_1_T
  W_AMD_T  <- rho * W_1_T + sqrt(1 - rho^2) * W_2_T
  
  NVDA_path <- exp((rf - (NVDA_vol - vol_dif)^2 / 2) * T + (NVDA_vol - vol_dif) * W_NVDA_T)
  AMD_path  <- exp((rf - (AMD_vol - vol_dif)^2 / 2) * T + (AMD_vol - vol_dif) * W_AMD_T)
  
  for (percentage_change in price_incremements) {
    basket_0 <- (NVDA_0 * (1 + percentage_change) + AMD_0 * (1 + percentage_change)) / 2
    
    NVDA_t <- NVDA_0 * (1 + percentage_change) * NVDA_path
    AMD_t  <- AMD_0 * (1 + percentage_change) * AMD_path
    basket_t <- (NVDA_t + AMD_t) / 2
    
    basket_vol <- sd(log(basket_t / basket_0))
    
    basket_call <- exp(-rf * T) * mean(vapply(basket_t - basket_strike, payoff, FUN.VALUE = numeric(1)))
    basket_put  <- exp(-rf * T) * mean(vapply(basket_strike - basket_t, payoff, FUN.VALUE = numeric(1)))
    
    b_call_vec <- c(b_call_vec, basket_call)
    b_put_vec  <- c(b_put_vec, basket_put)
    b_0_vec    <- c(b_0_vec, basket_0)
    b_rho_vec  <- c(b_rho_vec, rho)
    b_vol_vec  <- c(b_vol_vec, basket_vol)
  }
}

df_basket_prices_vol_down <- data.frame(
  b_call_vec = b_call_vec,
  b_put_vec  = b_put_vec,
  b_0_vec    = b_0_vec,
  b_rho_vec  = b_rho_vec,
  b_vol_vec  = b_vol_vec
)

df_basket_prices_vol <- rbind(df_basket_prices_vol_up, df_basket_prices_vol_down)
df_basket_prices_vol <- df_basket_prices_vol[order(
  df_basket_prices_vol$b_rho_vec,
  df_basket_prices_vol$b_0_vec,
  df_basket_prices_vol$b_vol_vec
), ]

df_basket_vega <- data.frame(
  b_call_vec = numeric(),
  b_put_vec = numeric(),
  b_0_vec = numeric(),
  b_rho_vec = numeric(),
  b_vol_vec = numeric(),
  b_call_vega = numeric(),
  b_put_vega = numeric()
)

for (rho in rho_partition) {
  df_options_temp <- df_basket_prices_vol[df_basket_prices_vol$b_rho_vec == rho, ]
  df_options_temp <- df_options_temp %>%
    mutate(
      b_call_vega = (lead(b_call_vec) - b_call_vec) / (lead(b_vol_vec) - b_vol_vec),
      b_put_vega  = (lead(b_put_vec) - b_put_vec) / (lead(b_vol_vec) - b_vol_vec)
    )
  df_basket_vega <- rbind(df_basket_vega, df_options_temp)
}

rownames(df_basket_vega) <- NULL
df_basket_vega <- df_basket_vega[seq(1, nrow(df_basket_vega), 2), ]

# =====================
# SPREAD VEGA
# =====================

s_call_vec <- c()
s_put_vec  <- c()
s_0_vec    <- c()
s_rho_vec  <- c()
s_vol_vec  <- c()

for (rho in rho_partition) {
  W_NVDA_T <- W_1_T
  W_AMD_T  <- rho * W_1_T + sqrt(1 - rho^2) * W_2_T
  
  NVDA_path <- exp((rf - (NVDA_vol + vol_dif)^2 / 2) * T + (NVDA_vol + vol_dif) * W_NVDA_T)
  AMD_path  <- exp((rf - (AMD_vol + vol_dif)^2 / 2) * T + (AMD_vol + vol_dif) * W_AMD_T)
  
  for (percentage_change in price_incremements) {
    spread_0 <- AMD_0 * (1 + percentage_change) - NVDA_0 * (1 - percentage_change)
    
    NVDA_t <- NVDA_0 * (1 - percentage_change) * NVDA_path
    AMD_t  <- AMD_0 * (1 + percentage_change) * AMD_path
    spread_t <- AMD_t - NVDA_t
    
    spread_vol <- sd(spread_t)
    
    spread_call <- exp(-rf * T) * mean(vapply(spread_t - spread_strike, payoff, FUN.VALUE = numeric(1)))
    spread_put  <- exp(-rf * T) * mean(vapply(spread_strike - spread_t, payoff, FUN.VALUE = numeric(1)))
    
    s_call_vec <- c(s_call_vec, spread_call)
    s_put_vec  <- c(s_put_vec, spread_put)
    s_0_vec    <- c(s_0_vec, spread_0)
    s_rho_vec  <- c(s_rho_vec, rho)
    s_vol_vec  <- c(s_vol_vec, spread_vol)
  }
}

df_spread_prices_vol_up <- data.frame(
  s_call_vec = s_call_vec,
  s_put_vec  = s_put_vec,
  s_0_vec    = s_0_vec,
  s_rho_vec  = s_rho_vec,
  s_vol_vec  = s_vol_vec
)

s_call_vec <- c()
s_put_vec  <- c()
s_0_vec    <- c()
s_rho_vec  <- c()
s_vol_vec  <- c()

for (rho in rho_partition) {
  W_NVDA_T <- W_1_T
  W_AMD_T  <- rho * W_1_T + sqrt(1 - rho^2) * W_2_T
  
  NVDA_path <- exp((rf - (NVDA_vol - vol_dif)^2 / 2) * T + (NVDA_vol - vol_dif) * W_NVDA_T)
  AMD_path  <- exp((rf - (AMD_vol - vol_dif)^2 / 2) * T + (AMD_vol - vol_dif) * W_AMD_T)
  
  for (percentage_change in price_incremements) {
    spread_0 <- AMD_0 * (1 + percentage_change) - NVDA_0 * (1 - percentage_change)
    
    NVDA_t <- NVDA_0 * (1 - percentage_change) * NVDA_path
    AMD_t  <- AMD_0 * (1 + percentage_change) * AMD_path
    spread_t <- AMD_t - NVDA_t
    
    spread_vol <- sd(spread_t)
    
    spread_call <- exp(-rf * T) * mean(vapply(spread_t - spread_strike, payoff, FUN.VALUE = numeric(1)))
    spread_put  <- exp(-rf * T) * mean(vapply(spread_strike - spread_t, payoff, FUN.VALUE = numeric(1)))
    
    s_call_vec <- c(s_call_vec, spread_call)
    s_put_vec  <- c(s_put_vec, spread_put)
    s_0_vec    <- c(s_0_vec, spread_0)
    s_rho_vec  <- c(s_rho_vec, rho)
    s_vol_vec  <- c(s_vol_vec, spread_vol)
  }
}

df_spread_prices_vol_down <- data.frame(
  s_call_vec = s_call_vec,
  s_put_vec  = s_put_vec,
  s_0_vec    = s_0_vec,
  s_rho_vec  = s_rho_vec,
  s_vol_vec  = s_vol_vec
)

df_spread_prices_vol <- rbind(df_spread_prices_vol_up, df_spread_prices_vol_down)
df_spread_prices_vol <- df_spread_prices_vol[order(
  df_spread_prices_vol$s_rho_vec,
  df_spread_prices_vol$s_0_vec,
  df_spread_prices_vol$s_vol_vec
), ]

df_spread_vega <- data.frame(
  s_call_vec = numeric(),
  s_put_vec = numeric(),
  s_0_vec = numeric(),
  s_rho_vec = numeric(),
  s_vol_vec = numeric(),
  s_call_vega = numeric(),
  s_put_vega = numeric()
)

for (rho in rho_partition) {
  df_options_temp <- df_spread_prices_vol[df_spread_prices_vol$s_rho_vec == rho, ]
  df_options_temp <- df_options_temp %>%
    mutate(
      s_call_vega = (lead(s_call_vec) - s_call_vec) / (lead(s_vol_vec) - s_vol_vec),
      s_put_vega  = (lead(s_put_vec) - s_put_vec) / (lead(s_vol_vec) - s_vol_vec)
    )
  df_spread_vega <- rbind(df_spread_vega, df_options_temp)
}

rownames(df_spread_vega) <- NULL
df_spread_vega <- df_spread_vega[seq(1, nrow(df_spread_vega), 2), ]

df_spread_vega <- df_spread_vega %>%
  mutate(
    s_call_vega = ifelse(s_call_vega < 0, (lag(s_call_vega) + lead(s_call_vega)) / 2, s_call_vega),
    s_put_vega  = ifelse(s_put_vega < 0, (lag(s_put_vega) + lead(s_put_vega)) / 2, s_put_vega)
  )

saveRDS(df_basket_vega, "data/processed/df_basket_vega.rds")
saveRDS(df_spread_vega, "data/processed/df_spread_vega.rds")

cat("Vega surfaces saved\n")