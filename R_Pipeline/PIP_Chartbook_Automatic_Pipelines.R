

## ***************************************************
## Title: PIP Chartbook Automatic Update Pipelines ###
## Author: Martha Celmira Viveros Mendoza,         ###
##         Kayley Ashlynn Watson, Jing Xie         ###
## Latest Updates: Oct 1st, 2025                   ### 
## ***************************************************

## Objective of this workbook is to create a live 
## csv file that automatically update data needed 
## for Flourish charts using latest PIP data

## Clean Environment
rm(list = ls())


# *****************************
# ---- Section #1. Set up ----
# *****************************

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
library(WDI)
library(purrr)
library(forcats)



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
year_end_fig2 <- 2050
  
# Figure 3 
# *********
year_start_fig3 <- 2019
year_end_fig3 <- 2024

# Figure 4
# *********
year_start_fig4a <- 1990 
year_fig4b <- 2025 

# Figure 7
# *********
year_fig7a <- 1990 
year_fig7b <- 2024 

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

# Income distribution from $0 to $25 per capita per day 


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


## 4) 2026 - 2050 Scenario Projection 
dta_proj_scen <- read_dta("https://raw.githubusercontent.com/GPID-WB/pip-chartbook/main/dta/Country_FGT_VariousScenarios_2026_2050_20250401_2021_01_02_PROD.dta") %>%
  select(code, year, scenario, povertyline, pop, fgt0)


## 5) Inequality Gini data 
WDI_Gini <- WDI(indicator = "SI.POV.GINI", extra = TRUE)


## 6) pop_reporting level 
pop_reportinglevel <- read_dta("../dta/pop_reportinglevel.dta")

# ---- 4. Function     ----

source("PIP_Chartbook_functions.R")


# *****************************
# ---- Section #2. Figures ----
# *****************************

# ---- 1. Figure 1. Progress in Reducing Poverty ----

res <- build_fig1(
  dta_proj = dta_proj,
  dta_pip  = dta_pip,
  pov_lines = pov_lines,
  year_start_fig1 = year_start_fig1,
  line3pct = line3pct,
  millions3pct2030 = millions3pct2030
)

dta_fig_1a_final <- res$fig1a
dta_fig_1b_final <- res$fig1b

readr::write_csv(dta_fig_1a_final, "../csv/chartbook_fig_1a.csv")
readr::write_csv(dta_fig_1b_final, "../csv/chartbook_fig_1b.csv")

# ---- 2. Figure 2. Projections of Poverty until 2050 under different scenarios ($3.00 Line)----

# 1) Change structure for dta_proj_scen 

dta_proj_scen <- dta_proj_scen %>%
  group_by(year, scenario, povertyline) %>%
  summarise(
    fgt0_wavg          = wavg(fgt0, pop),   # pop-weighted average of fgt0
    .groups = "drop"
  ) %>%
  arrange(povertyline, scenario, year)

dta_proj_scen_wide <- dta_proj_scen %>%
  select(year, povertyline, scenario, fgt0_wavg) %>%
  pivot_wider(names_from = scenario, values_from = fgt0_wavg) %>%
  select(-`6pct growth`, -`8pct growth`) %>%
  rename(`2% growth` = `2pct growth`,
         `2% growth + Gini reduction 1%` = `2pct growth + 1% Gini reduction`,
         `2% growth + Gini reduction 2%` = `2pct growth + 2% Gini reduction`,
         `4% growth` = `4pct growth`,
         )

# Fig 2a (povline 3.0)
dta_fig_2a_final <- build_fig2(
  povline = 3.0,
  year_start_fig2 = year_start_fig2,
  year_end_fig2   = year_end_fig2,
  dta_proj_scen_wide = dta_proj_scen_wide,
  dta_proj           = dta_proj
)

# Fig 2b (povline 8.3)
dta_fig_2b_final <- build_fig2(
  povline = 8.3,
  year_start_fig2 = year_start_fig2,
  year_end_fig2   = year_end_fig2,
  dta_proj_scen_wide = dta_proj_scen_wide,
  dta_proj           = dta_proj
)

# Export csv file 
write_csv(dta_fig_2a_final, "../csv/chartbook_fig_2a.csv")
write_csv(dta_fig_2b_final, "../csv/chartbook_fig_2b.csv")


# ---- 3. Figure 3. Poverty is still above pre-pandemic levels ------

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

dta_fig_3a_final <- build_fig3(dta_fig_3a, year_start_fig3 = year_start_fig3, keep_last_k = 2)
dta_fig_3b_final <- build_fig3(dta_fig_3b, year_start_fig3 = year_start_fig3, keep_last_k = 2)

# Export csv file 
write_csv(dta_fig_3a_final, "../csv/chartbook_fig_3a.csv")
write_csv(dta_fig_3b_final, "../csv/chartbook_fig_3b.csv")


# ---- 4. Figure 4. Stalled progress in Global Prosperity Gap Reduction ------
# 4a. Progress in reducing the Global Prosperity Gap

dta_fig_4a <- dta_pip %>%
  filter(region_name == "World",
         poverty_line == 3.0, 
         year >= year_start_fig4a) %>%
  select(year, pg, estimate_type) %>%
  mutate(estimate_type = case_when(
    estimate_type == "projection" ~ "actual",
    estimate_type == "nowcast"    ~ "forecast",
    TRUE                          ~ estimate_type
  ))

dta_fig_4a_final <- build_fig4(
  df = dta_fig_4a,
  label = "Global Prosperity Gap",
  digits = 2,
  keep_last_k = 2
)

write_csv(dta_fig_4a_final, "../csv/chartbook_fig_4a.csv")

# 4b. Contribution to the Global Prosperity Gap by region 

region_keep <- c("Other High Income Countries", "Sub-Saharan Africa", "South Asia",
                 "East Asia & Pacific", "Latin America & Caribbean",
                 "Middle East, North Africa, Afghanistan & Pakistan", 
                 "Europe & Central Asia")

dta_fig_4b <- dta_pip %>%
  filter(poverty_line == 3.0, 
         year >= year_fig4b, 
         region_name %in% region_keep) %>%
  select(region_name, pop, pg) %>%
  mutate(pop_share = round(100*(pop/sum(pop, na.rm = TRUE)),2),
         pg_weighted = pop * pg, 
         pg_share = round(100*(pg_weighted / sum(pg_weighted, na.rm = TRUE)),2)) %>%
  select(region_name, pop_share, pg_share)

# dta_fig_4b has: region_name, pop_share, pg_share (already in percent or proportion)
dta_fig_4b_final <- build_fig4b(dta_fig_4b, digits = 2)
readr::write_csv(dta_fig_4b_final, "../csv/chartbook_fig_4b.csv")


# ---- 5. Figure 5. Limited gains in the global prosperity gap ------

dta_fig_5 <- dta_pip_ctry %>%
  select(country_code, year, mean, gini, pop, pg)

dta_fig_5_final <- build_fig5(dta_fig_5) %>%
  select(-check_sum)

readr::write_csv(dta_fig_5_final, "../csv/chartbook_fig_5.csv")


# ---- 6. Figure 6. Limited gains in the global prosperity gap ------

countrycodes_current <- dta_class %>%
  select(code, economy, region, region_pip, ida_current, region_SSA, 
         incgroup_current, fcv_current)

dta_fig_6 <- build_fig6(WDI_Gini, countrycodes_current)

dta_fig_6a_final <- dta_fig_6 %>%
  select(name, `Low inequality`, `Moderate inequality`, `High inequality`) %>%
  filter(name != "FCS", 
         name != "Non-FCS") %>%
  rename(Group = name) %>%
  mutate(Group = gsub(" ","-", Group)) %>%
  mutate(Group = factor(Group, levels = c(
    "Low-income",
    "Lower-middle-income",
    "Upper-middle-income",
    "High-income"
  ))) %>%
  arrange(Group)

dta_fig_6b_final <- dta_fig_6 %>%
  select(name, `Low inequality`, `Moderate inequality`, `High inequality`) %>%
  filter(name %in% c("FCS", "Non-FCS")) %>%
  rename(Group = name) 

readr::write_csv(dta_fig_6a_final, "../csv/chartbook_fig_6a.csv")
readr::write_csv(dta_fig_6b_final, "../csv/chartbook_fig_6b.csv")



# ---- 7. Figure 7. Income Levels in the world by poverty line ------


# dta_fig_7 <- build_fig_7(country_income_distribution,
#                          pop_reportinglevel,
#                          countrycodes_current)
# 
# # Make dta_fig_7 match the display in Picture 2
# dta_fig_7 <- dta_fig_7 %>%
#   arrange(year, poverty_line) %>%
#   mutate(`poverty line in 2017 PPP US$ (per capita per day)` = round(poverty_line, 1)) %>%
#   select(
#     year,
#     `Low-income`, `Lower-middle-income`, `Upper-middle-income`, `High-income`,
#     `poverty line in 2017 PPP US$ (per capita per day)`
#   ) %>%
#   mutate(dplyr::across(
#     c(`Low-income`, `Lower-middle-income`, `Upper-middle-income`, `High-income`),
#     ~{
#       v <- round(.x, 6)
#       out <- sprintf("%.6f", v)
#       out[is.na(v)] <- NA_character_   # keep NAs blank; change to "-" if you prefer
#       out[v == 0]  <- "-"              # replace 0.000000 with "-"
#       out
#     }
#   ))
# 
# 
# dta_fig_7a_final <- dta_fig_7 %>%
#   filter(year == year_fig7a) 
# 
# dta_fig_7b_final <- dta_fig_7 %>%
#   filter(year == year_fig7b) 
# 
# readr::write_csv(dta_fig_7a_final, "../csv/chartbook_fig_7a.csv")
# readr::write_csv(dta_fig_7b_final, "../csv/chartbook_fig_7b.csv")
