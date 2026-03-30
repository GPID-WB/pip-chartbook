
# 
# # Load packages
# library(pipr)
# library(dplyr)
# 
# # Define poverty line vector
# pov_lines_dist <- seq(0, 25, by = 0.5)  # $0 to $25, step of 0.5
# 
# dta_pip_ctry_dist <- get_stats(
#   country = "all", 
#   year = "all", 
#   reporting_level = "national", 
#   povline = pov_lines_dist,
#   nowcast = TRUE
# )
# 
# dta_pip_ctry_dist <- dta_pip_ctry_dist %>%
#   mutate(poverty_line = formatC(poverty_line, format = "f", digits = 2))


library(pipr)
library(dplyr)
library(purrr)  # for map_dfr

# Define poverty line vector
pov_lines_dist <- seq(0, 25, by = 0.5)  # $0 to $25, step of 0.5

# Loop over each poverty line and bind results
dta_pip_ctry_dist <- map_dfr(pov_lines_dist, function(pl) {
  get_stats(
    country = "all", 
    year = "all", 
    reporting_level = "national", 
    povline = pl,
    nowcast = TRUE
  )
})