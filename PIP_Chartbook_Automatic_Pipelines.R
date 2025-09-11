

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
library(glue)
library(rlang)



# ---- 2. Define Various Inputs ----

pov_lines <- c(3.0, 4.2, 8.3)

# Figure 1
# *********
line3pct <- 0 
millions3pct2030 <- 256
year_start_fig1 <- 1990

# Figure 2
# *********
year_start_fig2 <- 2010

# Figure 3 
# *********
year_start_fig3 <- 2019
year_end_fig3 <- 2024


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

dta_pip_ctry <- get_stats(
  country = "all", 
  year = "all", 
  reporting_level = "national", 
  povline = pov_lines,
  nowcast = TRUE
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

pl_str <- format(pov_lines, nsmall = 1, trim = TRUE)  # "3.0", "4.2", "8.3"

for (pl in pl_str) {
  dta_fig_1a_wide <- dta_fig_1a_wide %>%
    mutate(
      !!glue("headcount_nowcast_pl_{pl}") := if_else(
        year == last_actual_year,
        .data[[glue("headcount_actual_pl_{pl}")]],
        .data[[glue("headcount_nowcast_pl_{pl}")]]
      ),
      !!glue("pop_in_poverty_nowcast_pl_{pl}") := if_else(
        year == last_actual_year,
        .data[[glue("pop_in_poverty_actual_pl_{pl}")]],
        .data[[glue("pop_in_poverty_nowcast_pl_{pl}")]]
      )
    )
}

# dta_fig_1a_wide <- dta_fig_1a_wide %>%
#   mutate(headcount_nowcast_pl_3.0 = if_else(year == last_actual_year, headcount_actual_pl_3.0, headcount_nowcast_pl_3.0),
#          headcount_nowcast_pl_4.2 = if_else(year == last_actual_year, headcount_actual_pl_4.2, headcount_nowcast_pl_4.2),
#          headcount_nowcast_pl_8.3 = if_else(year == last_actual_year, headcount_actual_pl_8.3, headcount_nowcast_pl_8.3),
#          pop_in_poverty_nowcast_pl_3.0 = if_else(year == last_actual_year, pop_in_poverty_actual_pl_3.0, pop_in_poverty_nowcast_pl_3.0),
#          pop_in_poverty_nowcast_pl_4.2 = if_else(year == last_actual_year, pop_in_poverty_actual_pl_4.2, pop_in_poverty_nowcast_pl_4.2),
#          pop_in_poverty_nowcast_pl_8.3 = if_else(year == last_actual_year, pop_in_poverty_actual_pl_8.3, pop_in_poverty_nowcast_pl_8.3)
#          )


# 5) Other necessary adjustments 
dta_fig_1a_wide <- dta_fig_1a_wide %>%
  filter(year >= year_start_fig1) %>% # ensure correct starting year 
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


# ---- 2. Figure 1b. Number of Poor (millions) ----

# 1) Copy data file from figure 1 
dta_fig_1b_final <- dta_fig_1a_final 

# 2) Only minor changes is variable names for millions of poor forecatsed 
pl_suffix <- gsub("\\.", "", format(pov_lines, nsmall = 1, trim = TRUE))
pl_label  <- format(pov_lines, nsmall = 1, trim = TRUE)

# Create a named vector for renaming
rename_vec <- setNames(
  glue("pop_in_poverty{pl_suffix}_forecast"),
  glue("Millions of poor at ${pl_label} (forecast)")
  
)

# Apply renaming
dta_fig_1b_final <- dta_fig_1b_final %>%
  rename(!!!rename_vec)

# 3) Export csv file 
write_csv(dta_fig_1b_final, "csv/chartbook_fig_1b.csv")


# ---- (WIP) 3. Figure 2a. Projections of Poverty until 2050 under different scenarios ($3.00 Line)----

# 1) Add new rows up to 2050
new_years <- data.frame(year = (2031:2050))

# 2) Create data frame 
dta_fig_2a <- bind_rows(dta_fig_1b_final, new_years)

dta_fig_2a <- dta_fig_2a %>%
  select(year, `Poverty rate at $3.0`) %>%
  filter(year >= year_start_fig2) %>%
  rename(Observed = `Poverty rate at $3.0`) %>%
  mutate(Observed = 100*as.numeric(Observed)) # persent as Percentage Point 



# ---- (WIP) 4. Figure 2b. Projections of Poverty until 2050 under different scenarios ($3.00 Line)----




# ---- 5. Figure 3. Poverty is still above pre-pandemic levels ------

# 1) Combine pip data with income group class 
# Only use income group
dta_class_inc <- dta_class %>%
  select(code, incgroup_current) %>%
  distinct() %>%
  rename(country_code = code, 
         inc_grp = incgroup_current)

# 2) Combine with pip data 
dta_fig_3 <- left_join(dta_pip_ctry, dta_class_inc, 
                        by = "country_code") %>%
  select(country_code, year, inc_grp, pop, poverty_line, headcount, estimate_type) %>%
  filter(year >= year_start_fig3 &
           year <= year_end_fig3)

# 3) Split different poverty line (only first two)
dta_fig_3a <- dta_fig_3 %>%
  filter(poverty_line == 3.0)

dta_fig_3b <- dta_fig_3 %>%
  filter(poverty_line == 8.3)

# Helper: keep only the last k non-NA entries for every "()" column
.keep_last_k_paren <- function(df, k = 2) {
  for (col in names(df)) {
    if (grepl("\\(\\)", col)) {
      non_na_idx <- which(!is.na(df[[col]]))
      if (length(non_na_idx) > k) {
        drop_idx <- head(non_na_idx, length(non_na_idx) - k)
        df[[col]][drop_idx] <- NA
      }
    }
  }
  df
}

# Main: build the Fig 3 table
build_fig3 <- function(data, year_start_fig3, keep_last_k = 2) {
  
  # 1) Aggregate to weighted headcount by year / inc_grp / estimate_type
  out <- data %>%
    group_by(year, inc_grp, estimate_type) %>%
    summarise(
      weighted_value = sum(headcount * pop, na.rm = TRUE) / sum(pop, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Standardize group labels to match desired output
    mutate(
      inc_grp = recode(inc_grp,
                       "Low income" = "Low-income",
                       "Lower middle income" = "Lower-middle-income",
                       "Upper middle income" = "Upper-middle-income",
                       "High income" = "High-income"),
      variable = paste(inc_grp, estimate_type, sep = " - ")
    ) %>%
    select(year, variable, weighted_value) %>%
    pivot_wider(names_from = variable, values_from = weighted_value)
  
  # 2) Ensure columns exist even if missing after pivot (avoid mutate errors)
  needed <- c(
    "Low-income - actual", "Low-income - projection", "Low-income - nowcast",
    "Lower-middle-income - actual", "Lower-middle-income - nowcast",
    "Upper-middle-income - actual", "Upper-middle-income - nowcast"
  )
  for (nm in needed) if (!nm %in% names(out)) out[[nm]] <- NA_real_
  
  # 3) Merge actual / projection / nowcast as in your pipeline
  # Low-income: prefer actual, then projection, then nowcast
  out <- out %>%
    mutate(
      `Low-income` = if_else(!is.na(`Low-income - actual`), `Low-income - actual`, `Low-income - projection`),
      `Low-income` = if_else(!is.na(`Low-income`), `Low-income`, `Low-income - nowcast`)
    ) %>%
    # For middle-income groups, ensure () (nowcast) exists; if missing, backfill from actual
    mutate(
      `Lower-middle-income - nowcast` = if_else(!is.na(`Lower-middle-income - nowcast`), `Lower-middle-income - nowcast`, `Lower-middle-income - actual`),
      `Upper-middle-income - nowcast` = if_else(!is.na(`Upper-middle-income - nowcast`), `Upper-middle-income - nowcast`, `Upper-middle-income - actual`)
    ) %>%
    # Keep, then rename to final columns
    select(
      year,
      `Low-income`,
      `Lower-middle-income - actual`, `Lower-middle-income - nowcast`,
      `Upper-middle-income - actual`, `Upper-middle-income - nowcast`
    ) %>%
    rename(
      `Lower-middle-income`   = `Lower-middle-income - actual`,
      `Lower-middle-income ()`= `Lower-middle-income - nowcast`,
      `Upper-middle-income`   = `Upper-middle-income - actual`,
      `Upper-middle-income ()`= `Upper-middle-income - nowcast`
    )

  # 4) Relative to base year, rounded to 2 decimals
  out <- out %>%
    mutate(across(-year, ~ .x / .x[year == year_start_fig3])) %>%
    mutate(across(-year, ~ round(.x, 2)))

  # 5) Keep only the last 'keep_last_k' non-NA values in each () column
  out <- .keep_last_k_paren(out, k = keep_last_k)

  # 6) Replace NA with "" for visual clarity
  out <- out %>%
    mutate(across(-year, ~ ifelse(is.na(.x), "", .x)))

  out
}

dta_fig_3a_final <- build_fig3(dta_fig_3a, year_start_fig3 = year_start_fig3, keep_last_k = 2)
dta_fig_3b_final <- build_fig3(dta_fig_3b, year_start_fig3 = year_start_fig3, keep_last_k = 2)

# Export csv file 
write_csv(dta_fig_3a_final, "csv/chartbook_fig_3a.csv")
write_csv(dta_fig_3b_final, "csv/chartbook_fig_3b.csv")