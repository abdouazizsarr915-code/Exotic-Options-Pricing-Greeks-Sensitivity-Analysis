# ============================================
# 02_prepare_returns.R
# Prepare returns and risk-free rate
# ============================================

library(tidyverse)
library(tidyquant)
library(lubridate)
library(ggplot2)

if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)
if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)

prices_daily <- readRDS("data/raw/prices_daily.rds")
rf_monthly <- readRDS("data/raw/rf_monthly.rds")
meta <- readRDS("data/raw/meta.rds")

t_0 <- meta$t_0
n <- meta$n
tickers <- meta$tickers

# Transform daily prices to monthly prices
prices_monthly <- prices_daily %>%
  group_by(symbol) %>%
  tq_transmute(select = close, mutate_fun = to.monthly, indexAt = "firstof")

prices_monthly <- data.frame(prices_monthly)
prices_monthly <- reshape(prices_monthly, idvar = "date", timevar = "symbol", direction = "wide")

# Create empty dataframe for monthly returns
returns_monthly <- tail(prices_monthly[1], -1)

# Monthly log returns
for (i in 1:length(tickers)) {
  log_returns <- with(prices_monthly, diff(log(prices_monthly[, i + 1])))
  returns_monthly <- cbind(returns_monthly, log_returns)
}
colnames(returns_monthly) <- c("date", tickers)

# Reverse order
prices_monthly <- prices_monthly[nrow(prices_monthly):1, ]
returns_monthly <- returns_monthly[nrow(returns_monthly):1, ]

# Restrict sample
returns_monthly <- head(returns_monthly[returns_monthly$date <= t_0, ], n)

# Long format
returns_long <- gather(returns_monthly[, -1], factor_key = TRUE)

# Initial prices at final sample date
prices_0 <- t(head(prices_monthly[prices_monthly$date <= t_0, ], 1)[, c(-1)])
colnames(prices_0) <- "price_0"

# Annualized vol and mean return
volatility_summary <- returns_long %>%
  group_by(key) %>%
  summarise(
    annualized_vol = sd(value) * sqrt(12),
    mean_return = mean(value) * sqrt(12),
    .groups = "drop"
  )

asset_summary <- cbind(volatility_summary, prices_0)

returns_correlation <- cor(returns_monthly$NVDA, returns_monthly$AMD)

# Monthly returns plot
returns_long_date <- returns_monthly %>%
  pivot_longer(
    cols = c(NVDA, AMD),
    names_to = "stock",
    values_to = "monthly_return"
  )

stock_colours <- c("AMD" = "#1F968BFF", "NVDA" = "#404788FF")

returns_plot <- ggplot(returns_long_date) +
  aes(x = date, y = monthly_return, group = stock, colour = stock) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "darkgray") +
  geom_line(linewidth = 0.6) +
  labs(
    title = "Monthly Logarithmic Returns",
    x = "Date",
    y = "Monthly Log Return",
    color = "Stock"
  ) +
  scale_color_manual(values = stock_colours)

ggsave("figures/monthly_returns.png", plot = returns_plot, width = 7, height = 4)

# Risk-free rate
rf_monthly$price <- rf_monthly$price / 100
rf <- mean(tail(rf_monthly[rf_monthly$date <= t_0, ], n)$price)

# Save processed objects
saveRDS(prices_monthly, "data/processed/prices_monthly.rds")
saveRDS(returns_monthly, "data/processed/returns_monthly.rds")
saveRDS(asset_summary, "data/processed/asset_summary.rds")
saveRDS(returns_correlation, "data/processed/returns_correlation.rds")
saveRDS(rf, "data/processed/rf.rds")

cat("Prepared data saved in data/processed/\n")
print(asset_summary)
cat("Empirical correlation:", round(returns_correlation, 3), "\n")
cat("Average risk-free rate:", round(rf, 6), "\n")