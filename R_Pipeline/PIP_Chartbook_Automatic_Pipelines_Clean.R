

## ***************************************************
## Title: PIP Chartbook Automatic Update Pipelines ###
## Author: Martha Celmira Viveros Mendoza,         ###
##         Kayley Ashlynn Watson, Jing Xie,        ###
##         Christoph Lakner, Nishant Yonzan        ###
## Latest Updates: Oct 16th, 2025                  ### 
## ***************************************************

## Objective of this workbook is to create a live 
## csv file that automatically update data needed 
## for Flourish charts using latest PIP data

## Clean Environment
rm(list = ls())


# ************************************
# ---- Section #1. Load Packages ----
# ************************************

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
library(tsibble)
library(scales)
library(data.table)

# *****************************
# ---- Section #2. Inputs ----
# *****************************

pov_lines <- c(3.0, 4.2, 8.3)

# If not specify, we will use general start and end year
general_year_start <- 1990 
general_year_end <- 2025 

# F1 & 2 - Poverty Rate and Millions of Poor forecasted 2030
# *********
line3pct <- 0 
millions3pct2030 <- 256

# F3 - Poverty rates by region
# *********
year_bridge_fig3 <- 2024
  
# F4 & 5 - Projections of poverty until 2050 under different scenarios
# *********
year_start_fig4_5 <- 2010
year_end_fig4_5 <- 2050
  
# F6 - Poverty is still above pre-pandemic levels ($3.0 & $8.3)
# *********
year_start_fig6 <- 2019

# F16 - Increased concentration of extreme poverty in Sub-Saharan Africa
# *********
year_start_fig16 <- 2000

# F18 - Global Growth Incidence Curve (GIC) using 100-bin percentiles
# *********
target_years <- c(1990, 2000, 2010, 2019, 2025)

# ********************************
# ---- Section #3. Load Data ----
# ********************************

## 1) PIP Complete Data
# ***********************
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

dta_pip_ctry_v2 <- get_stats(
  country = "all", 
  year = "all", 
  reporting_level = "national", 
  povline = "3.0"
)

# Fetch all national + subnational data first
dta_pip_ctry_v2 <- get_stats(
  country = "all",
  year = "all",
  reporting_level = "all",   # include all levels
  povline = pov_lines
) %>%
  dplyr::filter(
    # keep national for all except Argentina
    (reporting_level == "national" & country_code != "ARG") |
      # for Argentina, keep non-national (urban/rural)
      (country_code == "ARG" & reporting_level != "national")
  )


## 2) Class data
# ***********************
# dta_class <- read_dta("https://raw.githubusercontent.com/GPID-WB/Class/6e6123c1e5f1eea1636dd99f387aa98517d1ac7f/OutputData/CLASS.dta")
dta_class <- read_dta("https://raw.githubusercontent.com/GPID-WB/Class/master/OutputData/CLASS.dta") %>%
  rename(region_pip = region_code,
         region_SSA = regionssa,
         incgroup_current = incgroup,
         ida_current = ida)

## 3) 2026-2030 Projection 
# ***********************
dta_proj <- read_dta("https://raw.githubusercontent.com/GPID-WB/pip-chartbook/main/dta/Global_FGT_2026_2030_20250930_2021_01_02_PROD.dta") %>%
  rename(poverty_line = povertyline,
         pop_in_poverty = poorpop, 
         headcount = fgt0, 
         poverty_gap = fgt1, 
         poverty_severity = fgt2) %>%
  mutate(headcount = headcount/100, 
         poverty_gap = poverty_gap/100, 
         poverty_severity = poverty_severity/100,
         pop_in_poverty = pop_in_poverty * 1000000) # make sure units are consistent with PIP

dta_proj_v2 <- read_dta("https://raw.githubusercontent.com/GPID-WB/pip-chartbook/main/dta/Global_FGT_1981_2050_20250930_2021_01_02_PROD.dta") %>%
  rename(poverty_line = povertyline,
         pop_in_poverty = poorpop, 
         headcount = fgt0, 
         poverty_gap = fgt1, 
         poverty_severity = fgt2) %>%
  mutate(headcount = headcount/100, 
         poverty_gap = poverty_gap/100, 
         poverty_severity = poverty_severity/100,
         pop_in_poverty = pop_in_poverty * 1000000) # make sure units are consistent with PIP

dta_proj_ctry <- read_dta("https://raw.githubusercontent.com/GPID-WB/pip-chartbook/main/dta/Country_FGT_2026_2030_20250930_2021_01_02_PROD.dta") %>%
  rename(poverty_line = povertyline,
         pop_in_poverty = poorpop, 
         headcount = fgt0, 
         poverty_gap = fgt1, 
         poverty_severity = fgt2) %>%
  select(code, year, poverty_line, pop, headcount)

## 4) 2026 - 2050 Scenario Projection 
# ***********************
dta_proj_scen <- read_dta("https://raw.githubusercontent.com/GPID-WB/pip-chartbook/main/dta/Country_FGT_VariousScenarios_2026_2050_20250930_2021_01_02_PROD.dta") %>%
  select(code, year, scenario, povertyline, pop, fgt0)


## 5) Inequality Gini data 
# ***********************
WDI_Gini <- WDI(indicator = "SI.POV.GINI", extra = TRUE)


## 6) One thousand bin data
# ***********************
dta_inc_dist <- read_dta("dta/country_income_distribution.dta") %>%
  rename(
    country_code = code, 
    population = pop,
    poverty_line = povertyline, 
    headcount = pov
  )

dta_1000_bins <- read_dta("dta/GlobalDist1000bins_1990_20250930_2021_01_02_PROD.dta")

# ************************************
# ---- Section #4. Load Functions ----
# ************************************

source("R_Pipeline/PIP_Chartbook_Functions_Clean.R")


# *****************************
# ---- Section #5. Figures ----
# *****************************

# ---- F1 - Poverty Rate forecasted 2030 ----

dta_fig_1_2 <- build_fig1_2(
  dta_proj = dta_proj,
  dta_pip  = dta_pip,
  pov_lines = pov_lines,
  general_year_start = general_year_start,
  line3pct = line3pct,
  millions3pct2030 = millions3pct2030
)

dta_fig_1_final <- dta_fig_1_2$fig1a %>%
  rename("Poverty rate at $3.00" = "Poverty rate at $3.0", 
         "Poverty rate at $4.20" = "Poverty rate at $4.2",
         "Poverty rate at $8.30" = "Poverty rate at $8.3")

write_csv(dta_fig_1_final, "csv/chartbook_F1.csv")

# ---- F2 - Number of Poor forecasted 2030 ----

dta_fig_2_final <- dta_fig_1_2$fig1b %>%
  rename("Millions of poor at $3.00" = "Millions of poor at $3.0", 
         "Millions of poor at $4.20" = "Millions of poor at $4.2",
         "Millions of poor at $8.30" = "Millions of poor at $8.3")

write_csv(dta_fig_2_final, "csv/chartbook_F2.csv")

# ---- F3 - Poverty rates by region ----

dta_fig_3 <- dta_pip %>%
  select(poverty_line, region_code, region_name, year, headcount, estimate_type)

dta_fig_3_final <- build_fig3(
  dta_fig_3          = dta_fig_3,
  general_year_start    = general_year_start,   
  bridge_year        = year_bridge_fig3   
)

write_csv(dta_fig_3_final, "csv/chartbook_F3.csv")


# ---- F4 - Projections of poverty until 2050 under different scenarios ($3.0)----

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

# Add Current + Historical growth forecast
dta_proj_cur <- dta_proj_v2 %>%
  filter(region_code == "WLD", 
         year >= 2026) %>%
  mutate(`Current forecast + historical growth` = headcount * 100) %>%
  rename(povertyline = poverty_line) %>%
  select(year, povertyline, `Current forecast + historical growth`)

# Combine 
dta_proj_scen_wide_final <- dta_proj_scen_wide %>%
  left_join(dta_proj_cur, by = c("year","povertyline"))
  
# (povline 3.0)
dta_fig_4_final <- build_fig4_5(
  povline = 3.0,
  year_start_fig4_5 = year_start_fig4_5,
  year_end_fig4_5   = year_end_fig4_5,
  dta_proj_scen_wide = dta_proj_scen_wide_final,
  dta_proj           = dta_proj
)

# Export csv file 
write_csv(dta_fig_4_final, "csv/chartbook_F4.csv")


# ---- F5 - Projections of poverty until 2050 under different scenarios ($8.3)----

# (povline 8.3)
dta_fig_5_final <- build_fig4_5(
  povline = 8.3,
  year_start_fig4_5 = year_start_fig4_5,
  year_end_fig4_5   = year_end_fig4_5,
  dta_proj_scen_wide = dta_proj_scen_wide_final,
  dta_proj           = dta_proj
)


write_csv(dta_fig_5_final, "csv/chartbook_F5.csv")


# ---- F6 - Poverty is still above pre-pandemic levels ($3.0 & $8.3) ------

# Combine pip data with income group class (only use income group)
dta_class_inc <- dta_class %>%
  filter(year_data == max(year_data)) %>%
  select(code, incgroup_current) %>%
  distinct() %>%
  rename(country_code = code,
         inc_grp = incgroup_current)

dta_fig_6_final <- build_fig6(
  dta_class      = dta_class,
  dta_pip_ctry   = dta_pip_ctry,
  year_start_fig6 = year_start_fig6,   
  general_year_end   = general_year_end     
)

# Export csv file 

write_csv(dta_fig_6_final, "csv/chartbook_F6.csv")


# ---- F7 - Millions of Poor by region ------

dta_fig_7_final <- build_fig7(
  dta_pip_ctry = dta_pip_ctry,
  target_year  = general_year_end
)

write_csv(dta_fig_7_final, "csv/chartbook_F7.csv")


# ---- F8 - Stalled progress in Global Prosperity Gap Reduction (Global Prosperity Gap) ------

dta_fig_8 <- dta_pip %>%
  filter(region_name == "World",
         poverty_line == 3.0, 
         year >= general_year_start ) %>%
  select(year, pg, estimate_type) %>%
  mutate(estimate_type = case_when(
    estimate_type == "projection" ~ "actual",
    estimate_type == "nowcast"    ~ "forecast",
    TRUE                          ~ estimate_type
  ))

dta_fig_8_final <- build_fig8(
  df = dta_fig_8,
  label = "Global Prosperity Gap",
  digits = 2,
  keep_last_k = 2
) 

dta_fig_8_final <- dta_fig_8_final %>%
  mutate(across(
    c(`Global Prosperity Gap`, `Global Prosperity Gap (forecast)`),
    ~ as.numeric(gsub(",", "", as.character(.)))
  ))

write_csv(dta_fig_8_final, "csv/chartbook_F8.csv", na = "")

# ---- F9 - Stalled progress in Global Prosperity Gap Reduction (Regional Shares) ------

region_keep <- c("North America", "Sub-Saharan Africa", "South Asia",
                 "East Asia & Pacific", "Latin America & Caribbean",
                 "Middle East, North Africa, Afghanistan & Pakistan", 
                 "Europe & Central Asia")

dta_fig_9 <- dta_pip %>%
  filter(poverty_line == 3.0, 
         year >= general_year_end , 
         region_name %in% region_keep) %>%
  select(region_name, pop, pg) %>%
  mutate(pop_share = round(100*(pop/sum(pop, na.rm = TRUE)),2),
         pg_weighted = pop * pg, 
         pg_share = round(100*(pg_weighted / sum(pg_weighted, na.rm = TRUE)),2)) %>%
  select(region_name, pop_share, pg_share)

# dta_fig_9 has: region_name, pop_share, pg_share (already in percent or proportion)
dta_fig_9_final <- build_fig9(dta_fig_9, digits = 2)
write_csv(dta_fig_9_final, "csv/chartbook_F9.csv")


# ---- F10 - Limited Gains in the Global Prosperity Gap  ------

dta_fig_10 <- dta_pip_ctry %>%
  select(country_code, year, mean, gini, pop, pg)

dta_fig_10_final <- build_fig10(dta_fig_10,
                                last_label_override = "2019–2025 (projected)") %>%
  select(-check_sum)

write_csv(dta_fig_10_final, "csv/chartbook_F10.csv")


# ---- F11 - Within-country inequality map  ------

dta_fig_11_latest <- build_fig11(dta_pip_ctry_v2 = dta_pip_ctry_v2)

write_csv(dta_fig_11_latest, "csv/chartbook_F11.csv")


# ---- F12 - Poorer and conflict-affected economies (Income Level) ------

countrycodes_current <- dta_class %>%
  select(code, economy, region, region_pip, ida_current, region_SSA, 
         incgroup_current, fcv)

dta_fig_12_13 <- build_fig12_13(WDI_Gini, countrycodes_current)

dta_fig_12_final <- dta_fig_12_13 %>%
  select(name, `Low inequality`, `Moderate inequality`, `High inequality`) %>%
  filter(name != "Yes", 
         name != "No") %>%
  rename(Group = name) %>%
  mutate(Group = gsub(" ","-", Group)) %>%
  mutate(Group = factor(Group, levels = c(
    "Low-income",
    "Lower-middle-income",
    "Upper-middle-income",
    "High-income"
  ))) %>%
  arrange(Group) %>%
  filter(!is.na(Group))

write_csv(dta_fig_12_final, "csv/chartbook_F12.csv")


# ---- F13 - Poorer and conflict-affected economies (FCS status) ------

dta_fig_13_final <- dta_fig_12_13 %>%
  select(name, `Low inequality`, `Moderate inequality`, `High inequality`) %>%
  filter(name %in% c("Yes", "No")) %>%
  mutate(name = case_when(
    name == "Yes" ~ "FCS",
    name == "No"  ~ "Non-FCS",
    TRUE ~ name
  )) %>%
  rename(Group = name) 

write_csv(dta_fig_13_final, "csv/chartbook_F13.csv")


# ---- F14 - Income levels in the world have grown between 1990 ------

dta_fig_14_final_v2 <- build_fig14_15(
  dta_inc_dist   = dta_inc_dist,
  dta_class_inc  = dta_class_inc,
  target_year    = general_year_start   # e.g., 1990
)

write_csv(dta_fig_14_final_v2, "csv/chartbook_F14.csv")


# ---- F15 - Income levels in the world have grown 2024 ------

dta_fig_15_final_v2 <- build_fig14_15(
  dta_inc_dist   = dta_inc_dist,
  dta_class_inc  = dta_class_inc,
  target_year    = general_year_end   # e.g., 2024
)

write_csv(dta_fig_15_final_v2, "csv/chartbook_F15.csv")


# ---- F16 - Increased concentration of extreme poverty in Sub-Saharan Africa ------

dta_fig_16_final_v2 <- build_fig16(
  dta_class       = dta_class,
  dta_proj_ctry   = dta_proj_ctry,
  dta_pip         = dta_pip,
  dta_proj        = dta_proj,
  dta_pip_ctry    = dta_pip_ctry,
  year_start_fig16 = year_start_fig16
)


write_csv(dta_fig_16_final_v2, "csv/chartbook_F16.csv")


# ---- F17 - Millions of poor lived below the $3.00 per day ------
regions = c("AFE","AFW","EAS","ECS","LCN","MEA","NAC","SAS","SSF","WLD")

dta_fig_17 <- build_fig17(dta_pip = dta_pip, regions = regions)

write_csv(dta_fig_17, "csv/chartbook_F17.csv")


# ---- F18 - Global Growth Incidence Curve (GIC) using 100-bin percentiles ----

welfare_floor <- 0.25   # Stata: global welf_fl 0.25

# --- 1) Keep target years & apply welfare floor
df <- dta_1000_bins %>%
  filter(year %in% target_years) %>%
  mutate(welf_adj = pmax(welf, welfare_floor))

out <- build_fig18(df, target_years = target_years, n = 100)
dta_fig_18_final <- out$dta_fig_18_final

write_csv(dta_fig_18_final, "csv/chartbook_F18.csv")