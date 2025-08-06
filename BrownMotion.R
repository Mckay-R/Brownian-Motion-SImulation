# Simulating Brownian Motion in R

# Load necessary libraries
library(quantmod)
library(ggplot2)
library(dplyr)
library(scales)
library(gridExtra)
library(tidyr)

#### Single Path GBM Simulation

set.seed(12)
#function to simulate single GBM path
gbm_sim <- function(n, S0, mu, sigma, T = 1) {
  dt <- T / n                      # the size of each time step: time difference (independent increment)
  time_grid <- seq(0, T, length.out = n + 1)  # Time vector: 0/n, 1/n, 3/n, 4/n, ...
  Bt <- sqrt(dt) * cumsum(rnorm(n + 1, mean = 0, sd = 1))  # Brownian motion
  St <- S0 * exp((mu - 0.5 * sigma^2) * time_grid + sigma * Bt)  # GBM formula
  data.frame(Time = time_grid, Price = St)
}
#Generate simulations
gbm_df1 <- gbm_sim(n = 252, S0 = 285, mu = 0.1, sigma = 0.3) #S0=TSLA closing price as at last Friday

# Plot
ggplot(gbm_df1, aes(x = Time, y = Price)) +
  geom_line(color = "black", size = 0.5) +
  labs(title = "Simulated Geometric Brownian Motion Path",
       x = "Time", y = "Simulated Price") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))




## Multiple path Simulation

set.seed(3)
# Function to simulate multiple GBM paths
gbm_sim_multi <- function(n, S0, mu, sigma, T = 1, n_paths) {
  dt <- T / n
  time_grid <- seq(0, T, length.out = n + 1)
  
  sim_list <- lapply(1:n_paths, function(i) {
    Bt <- cumsum(rnorm(n + 1, mean = 0, sd = sqrt(dt)))
    St <- S0 * exp((mu - 0.5 * sigma^2) * time_grid + sigma * Bt)
    data.frame(Time = time_grid, Price = St, Path = as.factor(i))
  })
  
  do.call(rbind, sim_list)
}

# Parameters
n_steps <- 252 #number of time steps
S0 <- 285 #initial stock price:
mu <- 0.1 #drift 
sigma <- 0.3 #volatility
n_paths <- 100 #number of paths to sim

# Generate simulations
gbm_df2 <- gbm_sim_multi(n = n_steps, S0 = S0, mu = mu, sigma = sigma, n_paths = n_paths)

# Plot
ggplot(gbm_df2, aes(x = Time, y = Price, group = Path, color = Path)) +
  geom_line(size = 0.4) +
  labs(title = "Realization of 100 Simulated Geometric Brownian Motion Paths\nwith 10% Drift and 30% Volatility",
       x = "Time", y = "Simulated Price") +
  theme_minimal() +
  theme(legend.position = "none",  # Hide legend to avoid clutter
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_color_manual(values = colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(n_paths))