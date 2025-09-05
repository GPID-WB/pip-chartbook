

## *****************************************************
## Title: PIP Chartbook Automatic Update Pipelines   ###
## Author: Jing Xie, Martha Celmira Viveros Mendoza  ###
## Latest Updates: Sept 4th, 2025                    ### 
## *****************************************************

## Objective of this workbook is to create a live csv file that automatically 
## update data needed for Flourish charts using latest PIP data

## Clean Environment
rm(list = ls())


# ********************
# ---- Set up ----
# ********************

# ---- 1. Load Packages ----

library(haven)
library(dplyr)
library(readr)
library(pipr)
library(Hmisc)
library(tidyr)
library(stringr)
library(ggplot2)


# ---- 2. Define Various Inputs ----

pov_lines <- c(3.0, 4.2, 8.3)

line3pct <- 0 

millions3pct2030 <- 256

year_start <- 1990

# ---- 3. Load Data     ----

## 1) PIP Complete Data

dta_pip <- get_wb(
  year = "all",
  povline = pov_lines,
  version = NULL,
  ppp_version = 2021,
  release_version = NULL,
  api_version = "v1",
  format = c("rds", "json", "csv"),
  simplify = TRUE,
  server = NULL
)

## 2) Class data
dta_class <- read_dta("https://raw.githubusercontent.com/GPID-WB/Class/6e6123c1e5f1eea1636dd99f387aa98517d1ac7f/OutputData/CLASS.dta")

## 3) 2026-2030 Projection 
dta_proj <- read_dta("https://raw.githubusercontent.com/GPID-WB/pip-chartbook/main/dta/Global_FGT_2026_2030_20250401_2021_01_02_PROD.dta") %>%
  rename(poverty_line = povertyline,
         pop_in_poverty = poorpop, 
         headcount = fgt0, 
         poverty_gap = fgt1, 
         poverty_severity = fgt2) %>%
  mutate(headcount = headcount/100, 
         poverty_gap = poverty_gap/100, 
         poverty_severity = poverty_severity/100,
         pop_in_poverty = pop_in_poverty * 1000000) # make sure units are consistent with PIP

# ********************
# ---- Figures ----
# ********************

# ---- 1. Figure 1a. Progress in Reducing Poverty ----

## Progress in reducing extreme poverty has come to a halt 
## Data needed: 
## World Poverty rate (1990~latest)
## Millions of Poor at each poverty line

pov_lines <- format(pov_lines, nsmall = 1, trim = TRUE)

## Combine projection data with PIP data 
dta_fig_1a <- bind_rows(dta_proj, dta_pip) %>%
  select(region_name, year, poverty_line, headcount, pop_in_poverty, estimate_type) %>%
  mutate(estimate_type = if_else(is.na(estimate_type), "nowcast", estimate_type))

dta_fig_1a <- dta_fig_1a %>%
  filter(region_name == "World") %>%
  mutate(estimate_type = if_else(estimate_type == "projection", "actual", estimate_type)) %>%
  mutate(pop_in_poverty = pop_in_poverty / 1000000) %>% # change population unit to "Million" 
  select(-region_name)


## Reshape the dataset to Flourish format 

# 1) Separate forecast and actual value 

dta_fig_1a <- dta_fig_1a %>%
  mutate(estimate_type = str_to_lower(estimate_type)) 

dta_fig_1a_wide <- dta_fig_1a %>%
  pivot_wider(
    id_cols = c(year, poverty_line), 
    names_from = estimate_type, 
    values_from = c(headcount, pop_in_poverty), 
    names_glue = "{.value}_{estimate_type}"
  ) %>%
  arrange(year, poverty_line) 

# 2) Separate each poverty line as a separate column 
dta_fig_1a_wide <- dta_fig_1a_wide %>%
  mutate(poverty_line = format(as.numeric(poverty_line), nsmall = 1)) %>%
  pivot_wider(
    id_cols = year, 
    names_from = poverty_line, 
    values_from = c(headcount_actual, headcount_nowcast, 
                    pop_in_poverty_actual, pop_in_poverty_nowcast), 
    names_glue = "{.value}_pl_{poverty_line}"
  ) 
  
# 3) Add additional columns 
dta_fig_1a_wide <- dta_fig_1a_wide %>%
  mutate(line3pct = line3pct, 
         millions3pct2030 = millions3pct2030) 

# 4) Re-order columns 
# Generate column names using pov_lines

cols_to_select <- c("year",
                    paste0("headcount_actual_pl_", pov_lines[1]),
                    paste0("pop_in_poverty_actual_pl_", pov_lines[1]),
                    
                    paste0("headcount_actual_pl_", pov_lines[2]),
                    paste0("pop_in_poverty_actual_pl_", pov_lines[2]),
                    
                    paste0("headcount_actual_pl_", pov_lines[3]),
                    paste0("pop_in_poverty_actual_pl_", pov_lines[3]),
                    
                    paste0("headcount_nowcast_pl_", pov_lines[1]), 
                    paste0("headcount_nowcast_pl_", pov_lines[2]), 
                    paste0("headcount_nowcast_pl_", pov_lines[3]), 
                    
                    paste0("pop_in_poverty_nowcast_pl_", pov_lines[1]), 
                    paste0("pop_in_poverty_nowcast_pl_", pov_lines[2]), 
                    paste0("pop_in_poverty_nowcast_pl_", pov_lines[3]), 
                    
                    "line3pct", "millions3pct2030")

# Select the columns
dta_fig_1a_wide <- dta_fig_1a_wide %>%
  select(all_of(cols_to_select))


# 6) Copy values for the year right before the start of nowcast (as actual)
# Identify last actual year 

# 1- Identify columns
actual_cols  <- names(dta_fig_1a_wide) %>% str_subset("^(headcount|pop_in_poverty)_actual_pl_")
nowcast_cols <- sub("actual", "nowcast", actual_cols)

# 2- Find the last year with actual data (use the first actual column as reference)
ref_actual_col <- actual_cols[1]
last_actual_year <- dta_fig_1a_wide %>%
  filter(!is.na(.data[[ref_actual_col]])) %>%
  summarise(max_year = max(year, na.rm = TRUE)) %>%
  pull(max_year)

# 3- Add value 
dta_fig_1a_wide <- dta_fig_1a_wide %>%
  mutate(headcount_nowcast_pl_3.0 = if_else(year == last_actual_year, headcount_actual_pl_3.0, headcount_nowcast_pl_3.0),
         headcount_nowcast_pl_4.2 = if_else(year == last_actual_year, headcount_actual_pl_4.2, headcount_nowcast_pl_4.2),
         headcount_nowcast_pl_8.3 = if_else(year == last_actual_year, headcount_actual_pl_8.3, headcount_nowcast_pl_8.3),
         pop_in_poverty_nowcast_pl_3.0 = if_else(year == last_actual_year, pop_in_poverty_actual_pl_3.0, pop_in_poverty_nowcast_pl_3.0),
         pop_in_poverty_nowcast_pl_4.2 = if_else(year == last_actual_year, pop_in_poverty_actual_pl_4.2, pop_in_poverty_nowcast_pl_4.2),
         pop_in_poverty_nowcast_pl_8.3 = if_else(year == last_actual_year, pop_in_poverty_actual_pl_8.3, pop_in_poverty_nowcast_pl_8.3)
         )


# 5) Other necessary adjustments 
dta_fig_1a_wide <- dta_fig_1a_wide %>%
  filter(year >= year_start) %>% # ensure correct starting year 
  mutate(across(everything(), ~ suppressWarnings(as.numeric(.)))) %>% # ensure all columns are numeric
  mutate(across(where(is.numeric), ~ {
    if (startsWith(cur_column(), "headcount_")) round(., 5) 
    else round(., 0)
  })) %>% # Ensure consistent rounding as in Flourish
  mutate(across(everything(), ~ifelse(is.na(.), "", .))) # ensure NA expresses as empty 


# 7) Adding Labels 
dta_fig_1a_final <- dta_fig_1a_wide %>%
  rename_with(~ paste0("Poverty rate at $", str_extract(.x, "\\d+\\.\\d+")),
              starts_with("headcount_actual_pl_")) %>%
  rename_with(~ paste0("Millions of poor at $", str_extract(.x, "\\d+\\.\\d+")),
              starts_with("pop_in_poverty_actual_pl_")) %>%
  rename_with(~ paste0("Poverty rate at $", str_extract(.x, "\\d+\\.\\d+"), " (forecast)"),
              starts_with("headcount_nowcast_pl_")) %>%
  rename_with(~ paste0("pop_in_poverty", gsub("\\.", "", str_extract(.x, "\\d+\\.\\d+")), "_forecast"),
              starts_with("pop_in_poverty_nowcast_pl_")) 


# 8) Export csv file 
write_csv(dta_fig_1a_final, "csv/chartbook_fig_1a.csv")

