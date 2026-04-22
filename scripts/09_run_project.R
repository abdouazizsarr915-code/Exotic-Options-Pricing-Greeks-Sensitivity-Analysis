cat("=====================================\n")
cat(" Running Greeks Correlation Project \n")
cat("=====================================\n\n")

# Step 1: Download data
source("scripts/01_download_data.R")

# Step 2: Prepare data
source("scripts/02_prepare_data.R")

# Step 3: Model setup
source("scripts/03_model_setup.R")

# Step 4: Price surfaces
source("scripts/04_price_surfaces.R")

# Step 5: Delta & Gamma
source("scripts/05_delta_gamma.R")

# Step 6: Theta
source("scripts/06_theta.R")

# Step 7: Vega
source("scripts/07_vega.R")

# Step 8: Visualization
source("scripts/08_visualization.R")

cat("\n=====================================\n")
cat(" Project completed successfully\n")
cat("=====================================\n")