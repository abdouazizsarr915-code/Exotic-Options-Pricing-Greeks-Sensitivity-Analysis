# Exotic Options Pricing & Greeks Sensitivity Analysis

## Overview

This project studies how correlation between underlying assets affects the pricing and risk sensitivities of exotic options.

The analysis focuses on two multi-asset derivatives:

* **Basket options**
* **Spread options**

Using a Monte Carlo simulation framework, the project evaluates how option prices and Greeks change as correlation, initial synthetic price, and time to maturity vary.

The underlying assets used in the analysis are **NVIDIA (NVDA)** and **AMD**, two technology stocks with strong empirical co-movement. The project estimates option prices and four key Greeks: **Delta, Gamma, Theta, and Vega**. 

---

## Objectives

The goals of this project are to:

* price basket and spread options under correlated asset dynamics
* measure the sensitivity of option values to correlation
* estimate and visualize key Greeks:

  * Delta
  * Gamma
  * Theta
  * Vega
* compare the behavior of basket and spread options under the same market conditions

---

## Methodology

The project is based on a standard quantitative finance framework:

* **Monte Carlo simulation**
* **Risk-neutral valuation**
* **Correlated Geometric Brownian Motion**
* **Finite-difference approximations for Greeks**

Main setup:

* 10,000 simulation paths
* correlation values from -1 to 1
* monthly return data for NVDA and AMD
* risk-free rate from FRED
* option sensitivities evaluated across different initial synthetic prices and maturities

This matches the methodology described in the project report, where terminal prices are simulated under correlated GBM and Greeks are estimated with finite differences. 

---

## Key Findings

Main conclusions from the analysis:

* **Basket option prices generally increase with correlation**
* **Spread option prices generally decrease with correlation**
* **Delta changes smoothly, but its steepness depends on correlation**
* **Gamma becomes highly concentrated near critical regions**
* **Theta reflects how time decay changes with maturity and correlation**
* **Vega shows how volatility sensitivity differs across basket and spread structures**

Overall, the project shows that **correlation is a major driver of both valuation and hedging risk** in multi-asset derivatives. 

---

## Visual Results

### Monthly Returns

![Monthly Returns](figures/monthly_returns.png)

This figure shows the empirical monthly return behavior used to calibrate the model inputs.

---

## Basket Option Prices

### Basket Call Price Surface

![Basket Call Price](figures/basket_call_price_surface.png)

### Basket Put Price Surface

![Basket Put Price](figures/basket_put_price_surface.png)

### Basket Call Price over Time

![Basket Call Price Time](figures/basket_call_price_time.png)

### Basket Put Price over Time

![Basket Put Price Time](figures/basket_put_price_time.png)

---

## Spread Option Prices

### Spread Call Price Surface

![Spread Call Price](figures/spread_call_price_surface.png)

### Spread Put Price Surface

![Spread Put Price](figures/spread_put_price_surface.png)

### Spread Call Price over Time

![Spread Call Price Time](figures/spread_call_price_time.png)

### Spread Put Price over Time

![Spread Put Price Time](figures/spread_put_price_time.png)

---

## Delta Surfaces

### Basket Call Delta

![Basket Call Delta](figures/basket_call_delta_surface.png)

### Basket Put Delta

![Basket Put Delta](figures/basket_put_delta_surface.png)

### Spread Call Delta

![Spread Call Delta](figures/spread_call_delta_surface.png)

### Spread Put Delta

![Spread Put Delta](figures/spread_put_delta_surface.png)

---

## Gamma Surfaces

### Basket Call Gamma

![Basket Call Gamma](figures/basket_call_gamma_surface.png)

### Basket Put Gamma

![Basket Put Gamma](figures/basket_put_gamma_surface.png)

### Spread Call Gamma

![Spread Call Gamma](figures/spread_call_gamma_surface.png)

### Spread Put Gamma

![Spread Put Gamma](figures/spread_put_gamma_surface.png)

---

## Theta Surfaces

### Basket Call Theta

![Basket Call Theta](figures/basket_call_theta_surface.png)

### Basket Put Theta

![Basket Put Theta](figures/basket_put_theta_surface.png)

### Spread Call Theta

![Spread Call Theta](figures/spread_call_theta_surface.png)

### Spread Put Theta

![Spread Put Theta](figures/spread_put_theta_surface.png)

---

## Vega Surfaces

### Basket Call Vega

![Basket Call Vega](figures/basket_call_vega_surface.png)

### Basket Put Vega

![Basket Put Vega](figures/basket_put_vega_surface.png)

### Spread Call Vega

![Spread Call Vega](figures/spread_call_vega_surface.png)

### Spread Put Vega

![Spread Put Vega](figures/spread_put_vega_surface.png)

---

## Project Structure

* `scripts/` – simulation, Greeks estimation, and visualization code
* `data/` – raw and processed data
* `figures/` – generated plots
* `report/` – detailed written analysis
* `README.md` – project summary

---

## How to Run

Run the project from the root folder:

```r
source("scripts/09_run_project.R")
```

---

## Skills Demonstrated

This project demonstrates:

* Monte Carlo pricing
* derivatives modeling
* risk sensitivity analysis
* correlation modeling
* financial data processing in R
* quantitative visualization
* structured research workflow

---

## Author

Abdoul Aziz Sarr

