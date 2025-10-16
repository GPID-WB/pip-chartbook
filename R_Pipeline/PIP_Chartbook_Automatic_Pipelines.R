

## ***************************************************
## Title: PIP Chartbook Automatic Update Pipelines ###
## Author: Martha Celmira Viveros Mendoza,         ###
##         Kayley Ashlynn Watson, Jing Xie         ###
## Latest Updates: Oct 9th, 2025                   ### 
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
library(tsibble)


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
year_end_fig3 <- 2025

# Figure 4
# *********
year_start_fig4a <- 1990 
year_fig4b <- 2025 

# Figure 7
# *********
year_fig7a <- 1990 
year_fig7b <- 2025

# Figure 13
# *********
year_start_fig13 <- 2000

# Figure 17 
# *********
year_fig17 <- 2021

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
# dta_class <- read_dta("https://raw.githubusercontent.com/GPID-WB/Class/6e6123c1e5f1eea1636dd99f387aa98517d1ac7f/OutputData/CLASS.dta")
dta_class <- read_dta("https://raw.githubusercontent.com/GPID-WB/Class/master/OutputData/CLASS.dta") %>%
  rename(region_pip = region_code,
         region_SSA = regionssa,
         incgroup_current = incgroup,
         ida_current = ida)

## 3) 2026-2030 Projection 
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
dta_proj_scen <- read_dta("https://raw.githubusercontent.com/GPID-WB/pip-chartbook/main/dta/Country_FGT_VariousScenarios_2026_2050_20250930_2021_01_02_PROD.dta") %>%
  select(code, year, scenario, povertyline, pop, fgt0)


## 5) Inequality Gini data 
WDI_Gini <- WDI(indicator = "SI.POV.GINI", extra = TRUE)


## 6) One thousand bin data
dta_inc_dist <- read_dta("dta/country_income_distribution.dta") %>%
  rename(
    country_code = code, 
    population = pop,
    poverty_line = povertyline, 
    headcount = pov
  )


# ---- 4. Function     ----

source("R_Pipeline/PIP_Chartbook_functions.R")


# *****************************
# ---- Section #2. Figures ----
# *****************************

# ---- F1 - Poverty Rate forecasted 2030 ----

res <- build_fig1(
  dta_proj = dta_proj,
  dta_pip  = dta_pip,
  pov_lines = pov_lines,
  year_start_fig1 = year_start_fig1,
  line3pct = line3pct,
  millions3pct2030 = millions3pct2030
)

dta_fig_1_final <- res$fig1a %>%
  rename("Poverty rate at $3.00" = "Poverty rate at $3.0", 
         "Poverty rate at $4.20" = "Poverty rate at $4.2",
         "Poverty rate at $8.30" = "Poverty rate at $8.3")

write_csv(dta_fig_1_final, "csv/chartbook_F1.csv")

# ---- F2 - Number of Poor forecasted 2030 ----

dta_fig_2_final <- res$fig1b %>%
  rename("Millions of poor at $3.00" = "Millions of poor at $3.0", 
         "Millions of poor at $4.20" = "Millions of poor at $4.2",
         "Millions of poor at $8.30" = "Millions of poor at $8.3")

write_csv(dta_fig_2_final, "csv/chartbook_F2.csv")

# ---- F3 - Poverty rates by region ----

dta_fig_3 <- dta_pip %>%
  select(poverty_line, region_code, region_name, year, headcount, estimate_type)

# STEP 1. Keep target regions and years
regions <- c("AFE","AFW","EAS","ECS","LCN","MEA","NAC","SAS","SSF","WLD")

regions_name <- c(
  "Eastern and Southern Africa",
  "Western and Central Africa",
  "East Asia & Pacific",
  "Europe & Central Asia",
  "Latin America & Caribbean",
  "Middle East, North Africa, Afghanistan & Pakistan",
  "North America",
  "South Asia",
  "Sub-Saharan Africa",
  "World"
)

dta_fig_3 <- dta_fig_3 %>%
  filter(region_code %in% regions, year >= 1990) %>%          # keep regions and years
  mutate(
    hc = headcount * 100,                                     # headcount to percent
    estimate_type = case_when(                                # Stata replace logic
      estimate_type == "actual" ~ "obs",
      estimate_type %in% c("projection","nowcast") ~ "proj",
      TRUE ~ estimate_type
    ),
    year = as.character(year)                                 # Stata tostring
  ) %>%
  select(poverty_line, year, region_name, estimate_type, hc)

# STEP 2. Reshape wide like Stata's two-stage reshape
# First by region, then by estimate_type, ending with columns like WLD_obs, WLD_proj, etc.
dta_fig_3_wide <- dta_fig_3 %>%
  pivot_wider(
    names_from  = c(region_name, estimate_type),
    values_from = hc,
    names_glue  = "{region_name}_{estimate_type}"
  ) %>%
  arrange(poverty_line, as.numeric(year))

# STEP 4. Fill gaps so dotted lines connect for 2024
# Stata: replace r_proj = r_obs if r_proj==. & year=="2024"
for (r in regions_name) {
  proj <- paste0(r, "_proj"); obs <- paste0(r, "_obs")
  dta_fig_3_wide[[proj]] <- ifelse(dta_fig_3_wide$year == "2024" & is.na(dta_fig_3_wide[[proj]]),
                           dta_fig_3_wide[[obs]], dta_fig_3_wide[[proj]])
}

regions_name <- c(
  "Eastern and Southern Africa",
  "Western and Central Africa",
  "East Asia & Pacific",
  "Europe & Central Asia",
  "Latin America & Caribbean",
  "Middle East, North Africa, Afghanistan & Pakistan",
  "North America",
  "South Asia",
  "Sub-Saharan Africa",
  "World"
)

# STEP 5. Smooth specific transitions so observed and projected meet cleanly
# Ensure ordering
dta_fig_3_wide$year <- as.integer(dta_fig_3_wide$year)
dta_fig_3_wide <- dplyr::arrange(dta_fig_3_wide, poverty_line, year)

# All projection columns
proj_cols <- grep("_proj$", names(dta_fig_3_wide), value = TRUE)

# Indices by poverty line
grp_idx <- split(seq_len(nrow(dta_fig_3_wide)), dta_fig_3_wide$poverty_line)

for (col in proj_cols) {
  obs_col <- sub("_proj$", "_obs", col)
  if (!obs_col %in% names(dta_fig_3_wide)) next
  
  for (rows in grp_idx) {
    # within-group slices
    yr   <- dta_fig_3_wide$year[rows]
    proj <- dta_fig_3_wide[rows, col][[1]]
    obs  <- dta_fig_3_wide[rows, obs_col][[1]]
    
    # If no projections in this group, skip
    proj_idx <- which(!is.na(proj))
    if (length(proj_idx) == 0L) next
    
    # Find contiguous projection blocks (by year continuity)
    # e.g., 2001,2002,2003 is one block; 2010,2011 another
    cut_points <- c(1L, which(diff(proj_idx) > 1L) + 1L)
    block_starts <- proj_idx[cut_points]
    block_ends   <- proj_idx[c(cut_points[-1] - 1L, length(proj_idx))]
    
    # For each projection block, set proj to equal obs at the year
    # immediately before and after the block, if those rows exist
    for (b in seq_along(block_starts)) {
      s <- block_starts[b]
      e <- block_ends[b]
      
      # one year BEFORE the block
      if (s > 1L) {
        before_idx <- s - 1L
        # only set if obs is available
        if (!is.na(obs[before_idx])) proj[before_idx] <- obs[before_idx]
      }
      
      # one year AFTER the block
      if (e < length(proj)) {
        after_idx <- e + 1L
        if (!is.na(obs[after_idx])) proj[after_idx] <- obs[after_idx]
      }
    }
    
    # Optional: keep your original “bridge” rule so proj(t) copies obs(t)
    # when proj(t+1) exists but proj(t) is NA (helps one-sided edges)
    next_has_proj <- c(!is.na(proj[-1]) & yr[-1] == yr[-length(yr)] + 1L, FALSE)
    fill_here <- is.na(proj) & next_has_proj
    if (any(fill_here)) proj[fill_here] <- obs[fill_here]
    
    # write back
    dta_fig_3_wide[rows, col] <- proj
  }
}


# Rename and reorder 

# Desired final column order and names
new_col_order <- c(
  "poverty_line",
  "year",
  "World_obs",
  "World_proj",
  "East Asia & Pacific_obs",
  "East Asia & Pacific_proj",
  "Europe & Central Asia_obs",
  "Europe & Central Asia_proj",
  "Latin America & Caribbean_obs",
  "Latin America & Caribbean_proj",
  "Middle East, North Africa, Afghanistan & Pakistan_obs",
  "Middle East, North Africa, Afghanistan & Pakistan_proj",
  "North America_obs",
  "North America_proj",
  "South Asia_obs",
  "South Asia_proj",
  "Sub-Saharan Africa_obs",
  "Sub-Saharan Africa_proj",
  "Eastern and Southern Africa_obs",
  "Eastern and Southern Africa_proj",
  "Western and Central Africa_obs",
  "Western and Central Africa_proj"
)

# Rename to use parentheses for observed series (for Excel display)
new_names <- c(
  "Poverty line",
  "Year",
  "World",
  "World (Projection)",
  "East Asia & Pacific",
  "East Asia & Pacific (Projection)",
  "Europe & Central Asia",
  "Europe & Central Asia (Projection)",
  "Latin America & Caribbean",
  "Latin America & Caribbean (Projection)",
  "Middle East, North Africa, Afghanistan & Pakistan",
  "Middle East, North Africa, Afghanistan & Pakistan (Projection)",
  "North America",
  "North America (Projection)",
  "South Asia",
  "South Asia (Projection)",
  "Sub-Saharan Africa",
  "Sub-Saharan Africa (Projection)",
  "Eastern & Southern Africa",
  "Eastern & Southern Africa (Projection)",
  "Western & Central Africa",
  "Western & Central Africa (Projection)"
)

# Apply reorder and rename
dta_fig_3_wide <- dta_fig_3_wide %>%
  dplyr::select(any_of(new_col_order))

colnames(dta_fig_3_wide) <- new_names

# Rename poverty line 
dta_fig_3_wide <- dta_fig_3_wide %>%
  mutate(
    `Poverty line` = case_when(
      round(as.numeric(`Poverty line`), 1) == 3.0 ~ "$3.00 (2021 PPP)",
      round(as.numeric(`Poverty line`), 1) == 4.2 ~ "$4.20 (2021 PPP)",
      round(as.numeric(`Poverty line`), 1) == 8.3 ~ "$8.30 (2021 PPP)",
      TRUE ~ as.character(`Poverty line`)
    )
  ) 

dta_fig_3_final <- dta_fig_3_wide %>%
  mutate(
    across(
      -c(Year, `Poverty line`),          # exclude these two columns
      ~ round(clean_to_numeric(.), 1)     # coerce then round to 1 decimal
    )
  ) %>% 
  mutate(across(everything(), ~ifelse(is.na(.), "", .)))

  
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
dta_fig_4_final <- build_fig2(
  povline = 3.0,
  year_start_fig2 = year_start_fig2,
  year_end_fig2   = year_end_fig2,
  dta_proj_scen_wide = dta_proj_scen_wide_final,
  dta_proj           = dta_proj
)

# Export csv file 
write_csv(dta_fig_4_final, "csv/chartbook_F4.csv")

# ---- F5 - Projections of poverty until 2050 under different scenarios ($8.3)----

# (povline 8.3)
dta_fig_5_final <- build_fig2(
  povline = 8.3,
  year_start_fig2 = year_start_fig2,
  year_end_fig2   = year_end_fig2,
  dta_proj_scen_wide = dta_proj_scen_wide_final,
  dta_proj           = dta_proj
)


write_csv(dta_fig_5_final, "csv/chartbook_F5.csv")


# ---- F6 - Poverty is still above pre-pandemic levels ($3.0 & $8.3) ------

# 1) Combine pip data with income group class 
# Only use income group
dta_class_inc <- dta_class %>%
  filter(year_data == max(year_data)) %>%
  select(code, incgroup_current) %>%
  distinct() %>%
  rename(country_code = code, 
         inc_grp = incgroup_current)

# 2) Combine with pip data 
dta_fig_6 <- left_join(dta_pip_ctry, dta_class_inc, 
                        by = "country_code") %>%
  select(country_code, year, inc_grp, pop, poverty_line, headcount, estimate_type) %>%
  filter(year >= year_start_fig3 &
           year <= year_end_fig3) 

# 3) Split different poverty line (only first two)
dta_fig_6a <- dta_fig_6 %>%
  filter(poverty_line == 3.0)

# 4) Transformation

dta_fig_6a_final <- dta_fig_6a %>%
  group_by(inc_grp, year) %>%
  summarise(
    headcount_group = weighted.mean(headcount, pop, na.rm = TRUE)
  ) %>%
  group_by(inc_grp) %>%
  mutate(
    headcount_ref = headcount_group[year == 2019],
    headcount_diff = headcount_group/headcount_ref
  ) %>%
  # Standardize group labels to match desired output
  mutate(
    inc_grp = recode(inc_grp,
                     "Low income" = "Low-income",
                     "Lower middle income" = "Lower-middle-income",
                     "Upper middle income" = "Upper-middle-income",
                     "High income" = "High-income")
  ) %>%
  select(year, inc_grp, headcount_diff) %>%
  pivot_wider(names_from = inc_grp, values_from = headcount_diff) %>%
  mutate("Poverty Line" = "$3.00 (2021PPP)") %>%
  select("Poverty Line", year, "Low-income", "Lower-middle-income", "Upper-middle-income") %>%
  mutate(across(-c(year, "Poverty Line"), ~ round(.x, 2)))

# $8.3 poverty line

dta_fig_6b <- dta_fig_6 %>%
  filter(poverty_line == 8.3)

dta_fig_6b_final <- dta_fig_6b %>%
  group_by(inc_grp, year) %>%  
  summarise(
    headcount_group = weighted.mean(headcount, pop, na.rm = TRUE)
  ) %>%
  group_by(inc_grp) %>%
  mutate(
    headcount_ref = headcount_group[year == 2019],      
    headcount_diff = headcount_group/headcount_ref                
  ) %>%
  # Standardize group labels to match desired output
  mutate(
    inc_grp = recode(inc_grp,
                     "Low income" = "Low-income",
                     "Lower middle income" = "Lower-middle-income",
                     "Upper middle income" = "Upper-middle-income",
                     "High income" = "High-income")
  ) %>%
  select(year, inc_grp, headcount_diff) %>%
  pivot_wider(names_from = inc_grp, values_from = headcount_diff) %>%
  mutate("Poverty Line" = "$8.30 (2021PPP)") %>%
  select("Poverty Line", year, "Low-income", "Lower-middle-income", "Upper-middle-income") %>%
  mutate(across(-c(year, "Poverty Line"), ~ round(.x, 2)))

# Combine 6a and 6b 
dta_fig_6_final <- bind_rows(dta_fig_6a_final, dta_fig_6b_final)

# Export csv file 

write_csv(dta_fig_6_final, "csv/chartbook_F6.csv")


# ---- F7 - Millions of Poor by region ------

dta_fig_7 <- dta_pip_ctry %>%
  select(year, region_name, country_name, headcount, pop, poverty_line) %>%
  mutate(pop_in_poverty = as.integer((pop * headcount)/1000000)) %>%
  group_by(country_name) %>%
  filter(year == 2025) %>%
  select(-year) %>%
  mutate(
    poverty_line = case_when(
      round(as.numeric(poverty_line), 1) == 3.0 ~ "$3.00 (2021 PPP)",
      round(as.numeric(poverty_line), 1) == 4.2 ~ "$4.20 (2021 PPP)",
      round(as.numeric(poverty_line), 1) == 8.3 ~ "$8.30 (2021 PPP)",
      TRUE ~ as.character(poverty_line)
    )
  ) %>%
  select(region_name, country_name, pop_in_poverty, poverty_line) %>%
  rename(
    "Region" = region_name, 
    "Country Name" = country_name, 
    "Millions of poor" = pop_in_poverty, 
    "Poverty Line" = poverty_line
  )

write_csv(dta_fig_7, "csv/chartbook_F7.csv")


# ---- F8 - Stalled progress in Global Prosperity Gap Reduction (Global Prosperity Gap) ------

dta_fig_8 <- dta_pip %>%
  filter(region_name == "World",
         poverty_line == 3.0, 
         year >= year_start_fig4a) %>%
  select(year, pg, estimate_type) %>%
  mutate(estimate_type = case_when(
    estimate_type == "projection" ~ "actual",
    estimate_type == "nowcast"    ~ "forecast",
    TRUE                          ~ estimate_type
  ))

dta_fig_8_final <- build_fig4(
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
         year >= year_fig4b, 
         region_name %in% region_keep) %>%
  select(region_name, pop, pg) %>%
  mutate(pop_share = round(100*(pop/sum(pop, na.rm = TRUE)),2),
         pg_weighted = pop * pg, 
         pg_share = round(100*(pg_weighted / sum(pg_weighted, na.rm = TRUE)),2)) %>%
  select(region_name, pop_share, pg_share)

# dta_fig_9 has: region_name, pop_share, pg_share (already in percent or proportion)
dta_fig_9_final <- build_fig4b(dta_fig_9, digits = 2)
write_csv(dta_fig_9_final, "csv/chartbook_F9.csv")


# ---- F10 - Limited Gains in the Global Prosperity Gap  ------

dta_fig_10 <- dta_pip_ctry %>%
  select(country_code, year, mean, gini, pop, pg)

dta_fig_10_final <- build_fig5(dta_fig_10) %>%
  select(-check_sum)

write_csv(dta_fig_10_final, "csv/chartbook_F10.csv")


# ---- F11 - Within-country inequality map  ------

dta_fig_11 <- dta_pip_ctry_v2 %>%
  mutate(gini = gini * 100, 
         Classification = case_when(
           gini > 40              ~ "High inequality",
           gini >= 30 & gini <= 40 ~ "Moderate inequality",
           gini < 30               ~ "Low inequality",
           TRUE                    ~ NA_character_  # for missing or undefined values
         )) %>%
  rename(`Country Name` = country_name, 
         `Region` = region_name,
         `Survey year` = welfare_time, 
         `Gini index` = gini, 
         `Welfare type` = welfare_type) %>%
  mutate(country_name_flourish = recode(
    `Country Name`,
    # Only names that differ from original
    "Slovak Republic"              = "Slovakia",
    "Czechia"                      = "Czech Republic",
    "Kyrgyz Republic"              = "Kyrgyzstan",
    "Syrian Arab Republic"         = "Syria",
    "Egypt, Arab Rep."             = "Egypt",
    "Korea, Rep."                  = "South Korea",
    "Iran, Islamic Rep."           = "Iran",
    "Russian Federation"           = "Russia",
    "Cote d'Ivoire"                = "Ivory Coast",
    "Viet Nam"                     = "Vietnam",
    "Yemen, Rep."                  = "Yemen",
    "West Bank and Gaza"           = "Palestine",
    "North Macedonia"              = "Republic of Macedonia",
    "Turkiye"                      = "Turkey",
    "Micronesia, Fed. Sts."        = "Federated States of Micronesia",
    "Gambia, The"                  = "The Gambia",
    "Lao PDR"                      = "Laos",
    "Cabo Verde"                   = "Cape Verde",
    "Sao Tome and Principe"        = "São Tomé and Príncipe",
    "Congo, Dem. Rep."             = "Democratic Republic of the Congo",
    "Congo, Rep."                  = "Republic of the Congo",
    "United States"                = "United States of America",
    .default = `Country Name`
  )) %>%
  select(country_code, `Country Name`, country_name_flourish, Region, 
         `Survey year`, `Gini index`, `Welfare type`, Classification) 

dta_fig_11_latest <- dta_fig_11 %>%
  group_by(country_code) %>%
  filter(`Survey year` == max(`Survey year`, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct()

write_csv(dta_fig_11_latest, "csv/chartbook_F11.csv")


# ---- F12 - Poorer and conflict-affected economies (Income Level) ------

countrycodes_current <- dta_class %>%
  select(code, economy, region, region_pip, ida_current, region_SSA, 
         incgroup_current, fcv)

dta_fig_12_13 <- build_fig6(WDI_Gini, countrycodes_current)

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

# 1. Merge with income group data
dta_fig_14 <- dta_inc_dist %>%
  left_join(dta_class_inc, by = "country_code")  %>%
  filter(year == 1990) %>%
  rename(code = country_code, 
         pop = population, 
         povertyline = poverty_line, 
         pov = headcount, 
         incgroup = inc_grp) %>%
  select(code, year, pop, povertyline, pov, incgroup)

# 2. Compute total poor and collapse by group
povincgroup <- dta_fig_14 %>%
  mutate(poor = pov * pop) %>%
  summarise(
    poor = sum(poor, na.rm = TRUE),
    pop = sum(pop, na.rm = TRUE),
    .by = c(incgroup, year, povertyline)
  )

# 3. Compute incremental poor
povincgroup <- povincgroup %>%
  group_by(year, incgroup) %>%
  arrange(povertyline, .by_group = TRUE) %>%
  mutate(
    poorincremental = poor - lag(poor, default = 0),
    poorincremental = if_else(is.na(poorincremental) | poorincremental < 0, poor, poorincremental)
  ) %>%
  ungroup()

# 4. Compute global population and poor shares
povincgroup <- povincgroup %>%
  group_by(year, povertyline) %>%
  mutate(pop_global = sum(pop, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(poorshare = (poorincremental / pop_global))

# 5. Keep relevant variables and reshape wide
dta_fig_14_final <- povincgroup %>%
  select(year, povertyline, incgroup, poorshare) %>%
  pivot_wider(
    names_from = incgroup,
    values_from = poorshare
  ) 

# 6. Create cumulative variables for stacked chart
dta_fig_14_final_v2 <- dta_fig_14_final %>%
  mutate(
    `Low-income` = `Low income`,
    `Lower-middle-income` = `Low-income` + `Lower middle income`,
    `Upper-middle-income` = `Lower-middle-income` + `Upper middle income`,
    `High-income` = `Upper-middle-income` + `High income`
  ) %>%
  rename("poverty line in 2021 PPP US$ (per capita per day)" = povertyline) %>%
  select(year, `Low-income`, `Lower-middle-income`, `Upper-middle-income`, `High-income`, 
         "poverty line in 2021 PPP US$ (per capita per day)")

write_csv(dta_fig_14_final_v2, "csv/chartbook_F14.csv")


# ---- F15 - Income levels in the world have grown 2024 ------

# 1. Merge with income group data
dta_fig_15 <- dta_inc_dist %>%
  left_join(dta_class_inc, by = "country_code")  %>%
  filter(year == 2025) %>%
  rename(code = country_code, 
         pop = population, 
         povertyline = poverty_line, 
         pov = headcount, 
         incgroup = inc_grp) %>%
  select(code, year, pop, povertyline, pov, incgroup)

# 2. Compute total poor and collapse by group
povincgroup <- dta_fig_15 %>%
  mutate(poor = pov * pop) %>%
  summarise(
    poor = sum(poor, na.rm = TRUE),
    pop = sum(pop, na.rm = TRUE),
    .by = c(incgroup, year, povertyline)
  )

# 3. Compute incremental poor
povincgroup <- povincgroup %>%
  group_by(year, incgroup) %>%
  arrange(povertyline, .by_group = TRUE) %>%
  mutate(
    poorincremental = poor - lag(poor, default = 0),
    poorincremental = if_else(is.na(poorincremental) | poorincremental < 0, poor, poorincremental)
  ) %>%
  ungroup()

# 4. Compute global population and poor shares
povincgroup <- povincgroup %>%
  group_by(year, povertyline) %>%
  mutate(pop_global = sum(pop, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(poorshare = (poorincremental / pop_global))

# 5. Keep relevant variables and reshape wide
dta_fig_15_final <- povincgroup %>%
  select(year, povertyline, incgroup, poorshare) %>%
  pivot_wider(
    names_from = incgroup,
    values_from = poorshare
  ) 

# 6. Create cumulative variables for stacked chart
dta_fig_15_final_v2 <- dta_fig_15_final %>%
  mutate(
    `Low-income` = `Low income`,
    `Lower-middle-income` = `Low-income` + `Lower middle income`,
    `Upper-middle-income` = `Lower-middle-income` + `Upper middle income`,
    `High-income` = `Upper-middle-income` + `High income`
  ) %>%
  rename("poverty line in 2021 PPP US$ (per capita per day)" = povertyline) %>%
  select(year, `Low-income`, `Lower-middle-income`, `Upper-middle-income`, `High-income`, 
         "poverty line in 2021 PPP US$ (per capita per day)")

write_csv(dta_fig_15_final_v2, "csv/chartbook_F15.csv")


# ---- F16 - Increased concentratrion of extreme poverty in Sub-Saharan Africa ------

dta_fcs <- dta_class %>%
  select(code, year_data, fcv) 

dta_proj_ctry_v2 <- dta_proj_ctry %>%
  filter(poverty_line == 3.0) %>%
  rename(country_code = code) %>%
  select(-poverty_line) 

# Extract world population 
dta_pop_wld <- dta_pip %>%
  filter(region_code == "WLD",
         poverty_line == 3.0) %>%
  bind_rows(dta_proj) %>%
  select(region_code, year, pop_in_poverty)

dta_fig_16 <- dta_pip_ctry %>%
  filter(poverty_line == 3.0, 
         year >= year_start_fig13) %>%
  select(region_code, country_code, year, headcount, pop) %>%
  left_join(dta_fcs, by = c("country_code" = "code", "year" = "year_data")) %>%
  bind_rows(dta_proj_ctry_v2)

# Extract latest region and fcv definitions for projection years
latest_region <- get_latest_value(dta_fig_16, region_code)
latest_fcv    <- get_latest_value(dta_fig_16, fcv)

# Combine it back to original dataset 

dta_fig_16_final <- dta_fig_16 %>%
  left_join(latest_region, by = "country_code") %>%
  left_join(latest_fcv,    by = "country_code") %>%
  mutate(
    region_code = coalesce(region_code, region_code_latest),
    fcv = coalesce(fcv, fcv_latest)
  ) %>%
  select(-ends_with("_latest")) %>%
  filter(!is.na(fcv) & !is.na(region_code)) %>%
  mutate(pop_in_poverty = headcount * pop)
  
# Calculate share in poverty by group
dta_fig_16_grouped <- dta_fig_16_final %>%
  mutate(group = case_when(
    region_code == "SSF" & fcv == "Yes"  ~ "FCS in SSA",
    region_code == "SSF" & fcv == "No"   ~ "Non-FCS in SSA",
    region_code != "SSF" & fcv == "Yes"  ~ "FCS outside SSA",
    region_code != "SSF" & fcv == "No"   ~ "Rest of the world"
  )) %>%
  group_by(year, group) %>%
  summarise(pop_in_poverty = sum(pop_in_poverty, na.rm = TRUE), .groups = "drop") %>%
  ungroup() 


dta_fig_16_wld <- dta_fig_16_final %>%
  mutate(group = case_when(
    region_code == "SSF" & fcv == "Yes"  ~ "FCS in SSA",
    region_code == "SSF" & fcv == "No"   ~ "Non-FCS in SSA",
    region_code != "SSF" & fcv == "Yes"  ~ "FCS outside SSA",
    region_code != "SSF" & fcv == "No"   ~ "Rest of the world"
  )) %>%
  group_by(year) %>%
  summarise(pop_in_poverty_wld = sum(pop_in_poverty, na.rm = TRUE), .groups = "drop") %>%
  ungroup() 

# Combine World Population
dta_fig_16_final_v2 <- dta_fig_16_grouped %>%
  left_join(dta_fig_16_wld, by = "year") %>%
  mutate(pop_in_poverty_share = 100* (pop_in_poverty / pop_in_poverty_wld)) %>%
  select(year, group, pop_in_poverty_share) %>%           # change to your dataset name
  pivot_wider(
    names_from = group,                   # each group becomes a new column
    values_from = pop_in_poverty_share    # column values to fill
  ) %>%
  arrange(year) %>%
  mutate(across(-year, ~ round(.x, 1))) %>%     # round to 1 decimal
  rename(Year = year) %>%
  select(Year, "Non-FCS in SSA", "FCS in SSA", 
         "FCS outside SSA", "Rest of the world")

write_csv(dta_fig_16_final_v2, "csv/chartbook_F16.csv")


# ---- F17 - Millions of poor lived below the $3.00 per day ------

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
