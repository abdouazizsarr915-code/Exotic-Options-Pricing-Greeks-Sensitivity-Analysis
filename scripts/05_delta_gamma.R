library(tidyverse)

if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)

df_basket_prices <- readRDS("data/processed/df_basket_prices.rds")
df_spread_prices <- readRDS("data/processed/df_spread_prices.rds")
model_setup <- readRDS("data/processed/model_setup.rds")

rho_partition <- model_setup$rho_partition

df_basket_delta_gamma <- data.frame(
  b_call_vec = numeric(),
  b_put_vec = numeric(),
  b_0_vec = numeric(),
  b_rho_vec = numeric(),
  b_call_delta = numeric(),
  b_put_delta = numeric(),
  b_call_gamma = numeric(),
  b_put_gamma = numeric()
)

for (rho in rho_partition) {
  df_options_temp <- df_basket_prices[df_basket_prices$b_rho_vec == rho, ]
  df_options_temp <- df_options_temp %>%
    mutate(
      b_call_delta = (lead(b_call_vec) - lag(b_call_vec)) / (lead(b_0_vec) - lag(b_0_vec)),
      b_put_delta  = (lead(b_put_vec) - lag(b_put_vec)) / (lead(b_0_vec) - lag(b_0_vec)),
      b_call_gamma = (lead(b_call_vec, 10) - 2 * b_call_vec + lag(b_call_vec, 10)) / ((20 * (lead(b_0_vec) - b_0_vec))^2),
      b_put_gamma  = (lead(b_put_vec, 10) - 2 * b_put_vec + lag(b_put_vec, 10)) / ((20 * (lead(b_0_vec) - b_0_vec))^2)
    )
  df_options_temp <- tail(head(df_options_temp, -1), -1)
  df_basket_delta_gamma <- rbind(df_basket_delta_gamma, df_options_temp)
}

df_spread_delta_gamma <- data.frame(
  s_call_vec = numeric(),
  s_put_vec = numeric(),
  s_0_vec = numeric(),
  s_rho_vec = numeric(),
  s_call_delta = numeric(),
  s_put_delta = numeric(),
  s_call_gamma = numeric(),
  s_put_gamma = numeric()
)

for (rho in rho_partition) {
  df_options_temp <- df_spread_prices[df_spread_prices$s_rho_vec == rho, ]
  df_options_temp <- df_options_temp %>%
    mutate(
      s_call_delta = (lead(s_call_vec) - lag(s_call_vec)) / (lead(s_0_vec) - lag(s_0_vec)),
      s_put_delta  = (lead(s_put_vec) - lag(s_put_vec)) / (lead(s_0_vec) - lag(s_0_vec)),
      s_call_gamma = (lead(s_call_vec, 10) - 2 * s_call_vec + lag(s_call_vec, 10)) / ((20 * (lead(s_0_vec) - s_0_vec))^2),
      s_put_gamma  = (lead(s_put_vec, 10) - 2 * s_put_vec + lag(s_put_vec, 10)) / ((20 * (lead(s_0_vec) - s_0_vec))^2)
    )
  df_options_temp <- tail(head(df_options_temp, -1), -1)
  df_spread_delta_gamma <- rbind(df_spread_delta_gamma, df_options_temp)
}

saveRDS(df_basket_delta_gamma, "data/processed/df_basket_delta_gamma.rds")
saveRDS(df_spread_delta_gamma, "data/processed/df_spread_delta_gamma.rds")

cat("Delta and Gamma surfaces saved\n")