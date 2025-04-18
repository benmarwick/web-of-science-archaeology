# Load necessary libraries
library(tidyverse)
library(dplyr)
library(tidyr)

# Define categories and variables
categories <- c("m", "p", "bh", "bs", "s", "h")
variables <- c(
  "N. of authors (ln)", "N. of pages (ln)", "N. of references (sqrt)", "Prop. cited monographs",
  "Price's index", "Shannon div. of sources", "Relative title length (ln)", "First person singular"
)

# Set seed for reproducibility
set.seed(123)

# Number of samples per group
n_per_group <- 500

# --- Simulation Parameters (Approximate Mean and SD for each Var/Cat) ---
# Structure: list(Variable = list(Category = c(mean, sd)))
# These are visual estimates and may need tweaking for a closer match.
params <- list(
  "N. of authors (ln)" = list(
    m = c(0.7, 0.6), p = c(1.4, 0.5), bh = c(1.5, 0.7), bs = c(1.3, 0.6), s = c(0.7, 0.6), h = c(0.1, 0.1)
  ),
  "N. of pages (ln)" = list(
    m = c(2.5, 0.8), p = c(1.8, 0.4), bh = c(2.0, 0.5), bs = c(2.2, 0.6), s = c(2.7, 0.6), h = c(2.5, 0.7)
  ),
  "N. of references (sqrt)" = list(
    m = c(4.5, 1.5), p = c(5.0, 1.8), bh = c(6.0, 2.0), bs = c(6.5, 2.5), s = c(6.0, 2.5), h = c(5.0, 2.5)
  ),
  "Prop. cited monographs" = list( # Bounded 0-1
    m = c(0.05, 0.1), p = c(0.04, 0.08), bh = c(0.05, 0.1), bs = c(0.15, 0.2), s = c(0.2, 0.25), h = c(0.6, 0.2)
  ),
  "Price's index" = list( # Bounded 0-1
    m = c(0.25, 0.15), p = c(0.35, 0.18), bh = c(0.38, 0.18), bs = c(0.28, 0.15), s = c(0.30, 0.2), h = c(0.18, 0.15)
  ),
  "Shannon div. of sources" = list(
    m = c(2.7, 0.6), p = c(2.8, 0.6), bh = c(3.1, 0.5), bs = c(3.2, 0.5), s = c(3.3, 0.6), h = c(3.4, 0.7)
  ),
  "Relative title length (ln)" = list(
    m = c(-0.2, 0.8), p = c(0.5, 0.7), bh = c(0.4, 0.8), bs = c(0.3, 0.8), s = c(0.0, 0.9), h = c(-0.2, 0.7)
  ),
  "First person singular" = list( # Bounded >= 0, very skewed
    # Use rnorm with small mean/sd and take absolute value, or add specific outliers
    # Let's try rnorm near 0 and cap at 0, maybe add a few larger values manually
    m = c(0.005, 0.01), p = c(0.005, 0.01), bh = c(0.005, 0.01), bs = c(0.005, 0.01), s = c(0.01, 0.02), h = c(0.01, 0.015)
  )
)

# --- Generate Simulated Data ---
sim_data_list <- list()

for (var in variables) {
  for (cat in categories) {
    mean_val <- params[[var]][[cat]][1]
    sd_val <- params[[var]][[cat]][2]
    
    # Simulate base data
    sim_values <- rnorm(n_per_group, mean = mean_val, sd = sd_val)
    
    # Apply constraints / adjustments based on variable type
    if (var == "Prop. cited monographs" || var == "Price's index") {
      sim_values <- pmax(0, pmin(1, sim_values)) # Cap between 0 and 1
    } else if (var == "First person singular") {
      sim_values <- pmax(0, sim_values) # Ensure non-negative
      # Add some extra outliers to better match the plot
      n_outliers <- round(n_per_group * 0.05) # Add 5% outliers
      extra_outliers <- runif(n_outliers, min=0.03, max= if(cat %in% c('s', 'h')) 0.1 else 0.08)
      # Randomly replace some values with outliers
      indices_to_replace <- sample(1:n_per_group, n_outliers)
      sim_values[indices_to_replace] <- extra_outliers
      # Add one very large outlier for 'm' as seen in plot
      if(cat == 'm'){
        sim_values[sample(1:n_per_group, 1)] <- runif(1, 0.18, 0.22)
      }
    } else if (var == "N. of authors (ln)" && cat == 'h') {
      # Special case for 'h' in authors - very low values
      sim_values <- rnorm(n_per_group, mean = 0.05, sd = 0.05)
      sim_values <- pmax(0, sim_values) # Ensure non-negative
    } else if (var == "N. of references (sqrt)") {
      sim_values <- pmax(0, sim_values) # Ensure non-negative (sqrt can't be < 0)
    }
    
    
    sim_data_list[[length(sim_data_list) + 1]] <- data.frame(
      Category = cat,
      Variable = var,
      Value = sim_values
    )
  }
}

# Combine into a single data frame
sim_data <- bind_rows(sim_data_list)

# Convert Category and Variable to factors with specified order
sim_data$Category <- factor(sim_data$Category, levels = categories)
sim_data$Variable <- factor(sim_data$Variable, levels = variables)

# --- Create the Plot ---
ggplot(sim_data, aes(x = Category, y = Value)) +
  geom_boxplot(outlier.shape = 1, outlier.size = 1.5) + # Use circles for outliers
  facet_wrap(~ Variable, scales = "free_y", ncol = 4) + # Create grid, free y-axis
  theme_bw() + # Black and white theme similar to original
  labs(x = NULL, y = NULL) + # Remove axis titles
  theme(
    strip.background = element_blank(), # Remove background from facet titles
    strip.text = element_text(size = 10, face = "bold"), # Adjust facet title appearance
    axis.text.x = element_text(size = 10, face = "bold"), # Make x-axis labels bold like original
    panel.spacing = unit(1, "lines") # Add some space between panels
  )
