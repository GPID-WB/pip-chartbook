

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

# *****************************
# ---- Section #2. Inputs ----
# *****************************

pov_lines <- c(3.0, 4.2, 8.3)

general_year_start <- 1990 
general_year_end <- 2025 

# F1 - Poverty Rate forecasted 2030
# *********
line3pct <- 0 
millions3pct2030 <- 256
year_start_fig1 <- general_year_start

# F3 - Poverty rates by region
# *********
year_start_fig3 <- year_start_fig1
year_bridge_fig3 <- 2024
  
# F4 & 5 - Projections of poverty until 2050 under different scenarios
# *********
year_start_fig4_5 <- 2010
year_end_fig4_5 <- 2050
  
# F6 - Poverty is still above pre-pandemic levels ($3.0 & $8.3)
# *********
year_start_fig6 <- 2019
year_end_fig6 <- general_year_end

# F7 - Millions of Poor by region
# *********
year_end_fig7 <- general_year_end

# F8 - Stalled progress in Global Prosperity Gap Reduction (Global Prosperity Gap)
# *********
year_start_fig8 <- general_year_start 

# F9 - Stalled progress in Global Prosperity Gap Reduction (Regional Shares)
# *********
year_end_fig9 <- general_year_end 

# F14 - Income levels in the world have grown between 1990
# *********
year_fig14 <- general_year_start

# F15 - Income levels in the world have grown between 2025
# *********
year_fig15 <- general_year_end

# F16 - Increased concentration of extreme poverty in Sub-Saharan Africa
# *********
year_start_fig16 <- 2000


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
  year_start_fig1 = year_start_fig1,
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
  year_start_fig3    = year_start_fig3,   
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
  year_end_fig6   = year_end_fig6     
)

# Export csv file 

write_csv(dta_fig_6_final, "csv/chartbook_F6.csv")


# ---- F7 - Millions of Poor by region ------

dta_fig_7_final <- build_fig7(
  dta_pip_ctry = dta_pip_ctry,
  target_year  = year_end_fig7
)

write_csv(dta_fig_7_final, "csv/chartbook_F7.csv")


# ---- F8 - Stalled progress in Global Prosperity Gap Reduction (Global Prosperity Gap) ------

dta_fig_8 <- dta_pip %>%
  filter(region_name == "World",
         poverty_line == 3.0, 
         year >= year_start_fig8) %>%
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
         year >= year_end_fig9, 
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

dta_fig_10_final <- build_fig10(dta_fig_10) %>%
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
  target_year    = year_fig14   # e.g., 1990
)

write_csv(dta_fig_14_final_v2, "csv/chartbook_F14.csv")


# ---- F15 - Income levels in the world have grown 2024 ------

dta_fig_15_final_v2 <- build_fig14_15(
  dta_inc_dist   = dta_inc_dist,
  dta_class_inc  = dta_class_inc,
  target_year    = year_fig15   # e.g., 2024
)

write_csv(dta_fig_15_final_v2, "csv/chartbook_F15.csv")


# ---- F16 - Increased concentration of extreme poverty in Sub-Saharan Africa ------

# dta_fcs <- dta_class %>%
#   select(code, year_data, fcv) 
# 
# dta_proj_ctry_v2 <- dta_proj_ctry %>%
#   filter(poverty_line == 3.0) %>%
#   rename(country_code = code) %>%
#   select(-poverty_line) 
# 
# # Extract world population
# dta_pop_wld <- dta_pip %>%
#   filter(region_code == "WLD",
#          poverty_line == 3.0) %>%
#   bind_rows(dta_proj) %>%
#   select(region_code, year, pop_in_poverty)
# 
# dta_fig_16 <- dta_pip_ctry %>%
#   filter(poverty_line == 3.0,
#          year >= year_start_fig16) %>%
#   select(region_code, country_code, year, headcount, pop) %>%
#   left_join(dta_fcs, by = c("country_code" = "code", "year" = "year_data")) %>%
#   bind_rows(dta_proj_ctry_v2)
# 
# # Extract latest region and fcv definitions for projection years
# latest_region <- get_latest_value(dta_fig_16, region_code)
# latest_fcv    <- get_latest_value(dta_fig_16, fcv)
# 
# # Combine it back to original dataset
# 
# dta_fig_16_final <- dta_fig_16 %>%
#   left_join(latest_region, by = "country_code") %>%
#   left_join(latest_fcv,    by = "country_code") %>%
#   mutate(
#     region_code = coalesce(region_code, region_code_latest),
#     fcv = coalesce(fcv, fcv_latest)
#   ) %>%
#   select(-ends_with("_latest")) %>%
#   filter(!is.na(fcv) & !is.na(region_code)) %>%
#   mutate(pop_in_poverty = headcount * pop)
# 
# # Calculate share in poverty by group
# dta_fig_16_grouped <- dta_fig_16_final %>%
#   mutate(group = case_when(
#     region_code == "SSF" & fcv == "Yes"  ~ "FCS in SSA",
#     region_code == "SSF" & fcv == "No"   ~ "Non-FCS in SSA",
#     region_code != "SSF" & fcv == "Yes"  ~ "FCS outside SSA",
#     region_code != "SSF" & fcv == "No"   ~ "Rest of the world"
#   )) %>%
#   group_by(year, group) %>%
#   summarise(pop_in_poverty = sum(pop_in_poverty, na.rm = TRUE), .groups = "drop") %>%
#   ungroup()
# 
# 
# dta_fig_16_wld <- dta_fig_16_final %>%
#   mutate(group = case_when(
#     region_code == "SSF" & fcv == "Yes"  ~ "FCS in SSA",
#     region_code == "SSF" & fcv == "No"   ~ "Non-FCS in SSA",
#     region_code != "SSF" & fcv == "Yes"  ~ "FCS outside SSA",
#     region_code != "SSF" & fcv == "No"   ~ "Rest of the world"
#   )) %>%
#   group_by(year) %>%
#   summarise(pop_in_poverty_wld = sum(pop_in_poverty, na.rm = TRUE), .groups = "drop") %>%
#   ungroup()
# 
# # Combine World Population
# dta_fig_16_final_v2 <- dta_fig_16_grouped %>%
#   left_join(dta_fig_16_wld, by = "year") %>%
#   mutate(pop_in_poverty_share = 100* (pop_in_poverty / pop_in_poverty_wld)) %>%
#   select(year, group, pop_in_poverty_share) %>%           # change to your dataset name
#   pivot_wider(
#     names_from = group,                   # each group becomes a new column
#     values_from = pop_in_poverty_share    # column values to fill
#   ) %>%
#   arrange(year) %>%
#   mutate(across(-year, ~ round(.x, 1))) %>%     # round to 1 decimal
#   rename(Year = year) %>%
#   select(Year, "Non-FCS in SSA", "FCS in SSA",
#          "FCS outside SSA", "Rest of the world")

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

dta_fig_17 <- dta_pip %>%
  filter(region_code %in% regions) %>%
  select(year, region_name, poverty_line, pop_in_poverty) %>%
  pivot_wider(
    names_from = region_name, 
    values_from = pop_in_poverty
  ) %>%
  select(poverty_line, year, "East Asia & Pacific", "South Asia", "Sub-Saharan Africa", 
          "Latin America & Caribbean", "Middle East, North Africa, Afghanistan & Pakistan", 
         "Europe & Central Asia", "North America") %>%
  rename("East Asia Pacific" = "East Asia & Pacific",
         "Europe and Central Asia" = "Europe & Central Asia",
         "Latin America and Caribbean" = "Latin America & Caribbean",
         `Poverty line` = poverty_line) %>%
  mutate(
    `Poverty line` = case_when(
      round(as.numeric(`Poverty line`), 1) == 3.0 ~ "$3.00 (2021 PPP)",
      round(as.numeric(`Poverty line`), 1) == 4.2 ~ "$4.20 (2021 PPP)",
      round(as.numeric(`Poverty line`), 1) == 8.3 ~ "$8.30 (2021 PPP)",
      TRUE ~ as.character(`Poverty line`)
    )
  ) 

write_csv(dta_fig_17, "csv/chartbook_F17.csv")
