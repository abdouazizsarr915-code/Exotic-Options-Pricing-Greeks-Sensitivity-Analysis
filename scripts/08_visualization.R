library(tidyverse)
library(plotly)
library(htmlwidgets)
library(webshot)

if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)

# install.packages("webshot")
# webshot::install_phantomjs()

df_basket_prices <- readRDS("data/processed/df_basket_prices.rds")
df_spread_prices <- readRDS("data/processed/df_spread_prices.rds")
df_basket_delta_gamma <- readRDS("data/processed/df_basket_delta_gamma.rds")
df_spread_delta_gamma <- readRDS("data/processed/df_spread_delta_gamma.rds")
df_basket_theta <- readRDS("data/processed/df_basket_theta.rds")
df_spread_theta <- readRDS("data/processed/df_spread_theta.rds")
df_basket_vega <- readRDS("data/processed/df_basket_vega.rds")
df_spread_vega <- readRDS("data/processed/df_spread_vega.rds")
model_setup <- readRDS("data/processed/model_setup.rds")

N_rho <- model_setup$N_rho

surface_3d_plot <- function(df, x, y, z, x_title, y_title, z_title) {
  data_matrix <- df %>%
    select(all_of(c(x, y, z))) %>%
    pivot_wider(names_from = all_of(y), values_from = all_of(z)) %>%
    select(-all_of(x)) %>%
    as.matrix()
  
  x_vals <- unique(df[[x]])
  y_vals <- unique(df[[y]])
  
  p <- plot_ly(
    x = x_vals,
    y = y_vals,
    z = t(data_matrix),
    type = "surface",
    showscale = FALSE,
    opacity = 0.8,
    colorscale = "Viridis"
  )
  
  for (i in seq_along(y_vals)) {
    p <- add_trace(
      p,
      x = x_vals,
      y = rep(y_vals[i], length(x_vals)),
      z = t(data_matrix)[i, ],
      type = "scatter3d",
      mode = "lines",
      line = list(color = "black"),
      showlegend = FALSE
    )
  }
  
  for (j in round(seq(1, length(x_vals), length.out = N_rho))) {
    p <- add_trace(
      p,
      x = rep(x_vals[j], length(y_vals)),
      y = y_vals,
      z = t(data_matrix)[, j],
      type = "scatter3d",
      mode = "lines",
      line = list(color = "black"),
      showlegend = FALSE
    )
  }
  
  p %>%
    layout(
      scene = list(
        xaxis = list(title = x_title),
        yaxis = list(title = y_title),
        zaxis = list(title = z_title)
      )
    )
}

# =========================
# PRICE SURFACES
# =========================

b_call_price_plot <- surface_3d_plot(
  df_basket_prices,
  "b_0_vec", "b_rho_vec", "b_call_vec",
  "Initial Basket Price", "Correlation (ρ)", "Basket Call Price"
) %>%
  layout(scene = list(
    yaxis = list(autorange = "reversed"),
    camera = list(eye = list(x = -1.4, y = -1.9, z = 0.8))
  ))

b_put_price_plot <- surface_3d_plot(
  df_basket_prices,
  "b_0_vec", "b_rho_vec", "b_put_vec",
  "Initial Basket Price", "Correlation (ρ)", "Basket Put Price"
) %>%
  layout(scene = list(
    xaxis = list(autorange = "reversed"),
    yaxis = list(autorange = "reversed"),
    camera = list(eye = list(x = -1.4, y = -1.9, z = 0.8))
  ))

s_call_price_plot <- surface_3d_plot(
  df_spread_prices,
  "s_0_vec", "s_rho_vec", "s_call_vec",
  "Initial Spread Price", "Correlation (ρ)", "Spread Call Price"
) %>%
  layout(scene = list(
    yaxis = list(autorange = "reversed"),
    camera = list(eye = list(x = -1.4, y = -1.9, z = 0.8))
  ))

s_put_price_plot <- surface_3d_plot(
  df_spread_prices,
  "s_0_vec", "s_rho_vec", "s_put_vec",
  "Initial Spread Price", "Correlation (ρ)", "Spread Put Price"
) %>%
  layout(scene = list(
    xaxis = list(autorange = "reversed"),
    yaxis = list(autorange = "reversed"),
    camera = list(eye = list(x = -1.4, y = -1.9, z = 0.8))
  ))

# =========================
# DELTA SURFACES
# =========================

b_call_delta_plot <- surface_3d_plot(
  df_basket_delta_gamma,
  "b_0_vec", "b_rho_vec", "b_call_delta",
  "Initial Basket Price", "Correlation (ρ)", "Basket Call Delta (Δ)"
) %>%
  layout(scene = list(
    yaxis = list(autorange = "reversed"),
    camera = list(eye = list(x = -1.4, y = -1.9, z = 0.8))
  ))

b_put_delta_plot <- surface_3d_plot(
  df_basket_delta_gamma,
  "b_0_vec", "b_rho_vec", "b_put_delta",
  "Initial Basket Price", "Correlation (ρ)", "Basket Put Delta (Δ)"
) %>%
  layout(scene = list(
    yaxis = list(range = c(1, -1.1)),
    camera = list(eye = list(x = -1.4, y = -1.9, z = 0.8))
  ))

s_call_delta_plot <- surface_3d_plot(
  df_spread_delta_gamma,
  "s_0_vec", "s_rho_vec", "s_call_delta",
  "Initial Spread Price", "Correlation (ρ)", "Spread Call Delta (Δ)"
) %>%
  layout(scene = list(
    yaxis = list(autorange = "reversed"),
    camera = list(eye = list(x = -1.4, y = -1.9, z = 0.8))
  ))

s_put_delta_plot <- surface_3d_plot(
  df_spread_delta_gamma,
  "s_0_vec", "s_rho_vec", "s_put_delta",
  "Initial Spread Price", "Correlation (ρ)", "Spread Put Delta (Δ)"
) %>%
  layout(scene = list(
    yaxis = list(autorange = "reversed"),
    camera = list(eye = list(x = -1.4, y = -1.9, z = 0.8))
  ))

# =========================
# GAMMA SURFACES
# =========================

b_call_gamma_plot <- surface_3d_plot(
  df_basket_delta_gamma[!is.na(df_basket_delta_gamma$b_call_gamma), ],
  "b_0_vec", "b_rho_vec", "b_call_gamma",
  "Initial Basket Price", "Correlation (ρ)", "Basket Call Gamma (Γ)"
) %>%
  layout(scene = list(
    yaxis = list(autorange = "reversed"),
    camera = list(eye = list(x = 0.9, y = -2.2, z = 0.5))
  ))

b_put_gamma_plot <- surface_3d_plot(
  df_basket_delta_gamma[!is.na(df_basket_delta_gamma$b_call_gamma), ],
  "b_0_vec", "b_rho_vec", "b_put_gamma",
  "Initial Basket Price", "Correlation (ρ)", "Basket Put Gamma (Γ)"
) %>%
  layout(scene = list(
    xaxis = list(range = c(98, 310)),
    yaxis = list(autorange = "reversed"),
    camera = list(eye = list(x = -0.9, y = 2.2, z = 0.5))
  ))

s_call_gamma_plot <- surface_3d_plot(
  df_spread_delta_gamma[!is.na(df_spread_delta_gamma$s_call_gamma), ],
  "s_0_vec", "s_rho_vec", "s_call_gamma",
  "Initial Spread Price", "Correlation (ρ)", "Spread Call Gamma (Γ)"
) %>%
  layout(scene = list(
    xaxis = list(range = c(-157, 240)),
    yaxis = list(autorange = "reversed"),
    camera = list(eye = list(x = -0.9, y = 2.2, z = 0.5))
  ))

s_put_gamma_plot <- surface_3d_plot(
  df_spread_delta_gamma[!is.na(df_spread_delta_gamma$s_put_gamma), ],
  "s_0_vec", "s_rho_vec", "s_put_gamma",
  "Initial Spread Price", "Correlation (ρ)", "Spread Put Gamma (Γ)"
) %>%
  layout(scene = list(
    xaxis = list(range = c(-165, 240)),
    yaxis = list(autorange = "reversed"),
    camera = list(eye = list(x = 0.9, y = -2.2, z = 0.5))
  ))

# =========================
# THETA SURFACES
# =========================

b_call_price_time_plot <- surface_3d_plot(
  df_basket_theta,
  "b_time_vec", "b_rho_vec", "b_call_vec",
  "Time to Maturity (years)", "Correlation (ρ)", "Basket Call Price"
) %>%
  layout(scene = list(
    xaxis = list(range = c(1.05, 0)),
    camera = list(eye = list(x = 1.2, y = -2.1, z = 0.8))
  ))

b_put_price_time_plot <- surface_3d_plot(
  df_basket_theta,
  "b_time_vec", "b_rho_vec", "b_put_vec",
  "Time to Maturity (years)", "Correlation (ρ)", "Basket Put Price"
) %>%
  layout(scene = list(
    xaxis = list(range = c(1.05, 0)),
    camera = list(eye = list(x = 1.2, y = -2.1, z = 0.8))
  ))

b_call_theta_plot <- surface_3d_plot(
  df_basket_theta,
  "b_time_vec", "b_rho_vec", "b_call_theta",
  "Time to Maturity (years)", "Correlation (ρ)", "Basket Call Theta (Θ)"
) %>%
  layout(scene = list(
    zaxis = list(range = c(-150, -5)),
    camera = list(eye = list(x = -1, y = 2.3, z = 0.5))
  ))

b_put_theta_plot <- surface_3d_plot(
  df_basket_theta,
  "b_time_vec", "b_rho_vec", "b_put_theta",
  "Time to Maturity (years)", "Correlation (ρ)", "Basket Put Theta (Θ)"
) %>%
  layout(scene = list(
    zaxis = list(range = c(-150, -5)),
    camera = list(eye = list(x = -1, y = 2.3, z = 0.5))
  ))

s_call_price_time_plot <- surface_3d_plot(
  df_spread_theta,
  "s_time_vec", "s_rho_vec", "s_call_vec",
  "Time to Maturity (years)", "Correlation (ρ)", "Spread Call Price"
) %>%
  layout(scene = list(
    xaxis = list(range = c(1.05, 0)),
    yaxis = list(autorange = "reversed"),
    camera = list(eye = list(x = 1, y = -2.2, z = 0.6))
  ))

s_put_price_time_plot <- surface_3d_plot(
  df_spread_theta,
  "s_time_vec", "s_rho_vec", "s_put_vec",
  "Time to Maturity (years)", "Correlation (ρ)", "Spread Put Price"
) %>%
  layout(scene = list(
    xaxis = list(range = c(1.05, 0)),
    yaxis = list(autorange = "reversed"),
    camera = list(eye = list(x = 1, y = -2.2, z = 0.6))
  ))

s_call_theta_plot <- surface_3d_plot(
  df_spread_theta,
  "s_time_vec", "s_rho_vec", "s_call_theta",
  "Time to Maturity (years)", "Correlation (ρ)", "Spread Call Theta (Θ)"
) %>%
  layout(scene = list(
    yaxis = list(autorange = "reversed"),
    zaxis = list(range = c(-150, -1)),
    camera = list(eye = list(x = -1, y = 2.3, z = 0.5))
  ))

s_put_theta_plot <- surface_3d_plot(
  df_spread_theta,
  "s_time_vec", "s_rho_vec", "s_put_theta",
  "Time to Maturity (years)", "Correlation (ρ)", "Spread Put Theta (Θ)"
) %>%
  layout(scene = list(
    yaxis = list(autorange = "reversed"),
    zaxis = list(range = c(-150, -1)),
    camera = list(eye = list(x = -1, y = 2.3, z = 0.5))
  ))

# =========================
# VEGA SURFACES
# =========================

b_call_vega_plot <- surface_3d_plot(
  df_basket_vega,
  "b_0_vec", "b_rho_vec", "b_call_vega",
  "Initial Basket Price", "Correlation (ρ)", "Basket Call Vega (ν)"
) %>%
  layout(scene = list(
    xaxis = list(autorange = "reversed"),
    yaxis = list(autorange = "reversed"),
    camera = list(eye = list(x = -0.8, y = 2.2, z = 1))
  ))

b_put_vega_plot <- surface_3d_plot(
  df_basket_vega,
  "b_0_vec", "b_rho_vec", "b_put_vega",
  "Initial Basket Price", "Correlation (ρ)", "Basket Put Vega (ν)"
) %>%
  layout(scene = list(
    xaxis = list(autorange = "reversed"),
    yaxis = list(autorange = "reversed"),
    camera = list(eye = list(x = -0.8, y = 2.2, z = 1))
  ))

s_call_vega_plot <- surface_3d_plot(
  df_spread_vega,
  "s_0_vec", "s_rho_vec", "s_call_vega",
  "Initial Spread Price", "Correlation (ρ)", "Spread Call Vega (ν)"
) %>%
  layout(scene = list(
    xaxis = list(autorange = "reversed"),
    camera = list(eye = list(x = -0.4, y = 2.3, z = 0.7))
  ))

s_put_vega_plot <- surface_3d_plot(
  df_spread_vega,
  "s_0_vec", "s_rho_vec", "s_put_vega",
  "Initial Spread Price", "Correlation (ρ)", "Spread Put Vega (ν)"
) %>%
  layout(scene = list(
    xaxis = list(autorange = "reversed"),
    camera = list(eye = list(x = -0.4, y = 2.3, z = 0.7))
  ))

# =========================
# EXPORT TWO KEY PNGs FOR GITHUB
# =========================

saveWidget(b_call_price_plot, "figures/basket_call_price_surface.html")
webshot(
  "figures/basket_call_price_surface.html",
  "figures/basket_call_price_surface.png",
  vwidth = 1000,
  vheight = 800
)

saveWidget(b_call_gamma_plot, "figures/basket_call_gamma_surface.html")
webshot(
  "figures/basket_call_gamma_surface.html",
  "figures/basket_call_gamma_surface.png",
  vwidth = 1000,
  vheight = 800
)

cat("Visualization completed and key PNG figures exported.\n")

# Print selected interactive plots
b_call_price_plot
s_call_price_plot
b_call_gamma_plot
s_call_gamma_plot
b_call_theta_plot
s_call_theta_plot
b_call_vega_plot
s_call_vega_plot