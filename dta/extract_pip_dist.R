

# Load packages
library(pipr)
library(dplyr)

# Define poverty line vector
pov_lines_dist <- seq(0, 25, by = 0.5)  # $0 to $25, step of 0.5

dta_pip_ctry_dist <- get_stats(
  country = "all", 
  year = "all", 
  reporting_level = "national", 
  povline = pov_lines_dist,
  nowcast = TRUE
)

dta_pip_ctry_dist <- dta_pip_ctry_dist %>%
  mutate(poverty_line = formatC(poverty_line, format = "f", digits = 2))
