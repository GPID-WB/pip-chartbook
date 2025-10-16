
# ------- Section #1. General Functions ---------

# identify the last actual year from wide table 

.last_actual_year <- function(df_wide) {
  actual_cols <- names(df_wide) %>% stringr::str_subset("^(headcount|pop_in_poverty)_actual_pl_")
  if (length(actual_cols) == 0) return(NA_integer_)
  ref <- actual_cols[1]
  df_wide %>%
    dplyr::filter(!is.na(.data[[ref]])) %>%
    dplyr::summarise(max_year = max(year, na.rm = TRUE)) %>%
    dplyr::pull(max_year)
}

# Copy actual → nowcast for ONLY the last actual year (all pov lines)

.copy_last_actual_into_nowcast <- function(df_wide, pov_lines_fmt) {
  last_year <- .last_actual_year(df_wide)
  if (is.na(last_year)) return(df_wide)
  
  for (pl in pov_lines_fmt) {
    hc_a <- glue("headcount_actual_pl_{pl}")
    hc_n <- glue("headcount_nowcast_pl_{pl}")
    po_a <- glue("pop_in_poverty_actual_pl_{pl}")
    po_n <- glue("pop_in_poverty_nowcast_pl_{pl}")
    
    if (hc_a %in% names(df_wide) && hc_n %in% names(df_wide)) {
      df_wide[[hc_n]] <- ifelse(df_wide$year == last_year,
                                df_wide[[hc_a]], df_wide[[hc_n]])
    }
    if (po_a %in% names(df_wide) && po_n %in% names(df_wide)) {
      df_wide[[po_n]] <- ifelse(df_wide$year == last_year,
                                df_wide[[po_a]], df_wide[[po_n]])
    }
  }
  df_wide
}


# Safe weighted mean 

wavg <- function(x, w) {
  den <- sum(w, na.rm = TRUE)
  ifelse(den > 0, sum(x * w, na.rm = TRUE) / den, NA_real_)
}

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

# Extend historical growth

extend_with_hist_growth <- function(df, end_year) {
  df <- df %>% arrange(year)
  
  # Base: last non-NA in "Current forecast + historical growth"
  base_year <- max(df$year[!is.na(df$`Current forecast + historical growth`)], na.rm = TRUE)
  base_val  <- df$`Current forecast + historical growth`[df$year == base_year][1]
  
  # Arithmetic mean of YoY growth from Observed (robust, simple)
  g <- df %>%
    arrange(year) %>%
    transmute(gr = Observed / dplyr::lag(Observed) - 1) %>%
    summarise(g = mean(gr, na.rm = TRUE)) %>%
    pull(g)
  
  future_years <- seq(base_year + 1, end_year)
  if (length(future_years) == 0 || is.na(base_val) || !is.finite(g)) return(df)
  
  future_vals <- as.numeric(base_val) * cumprod(rep(1 + g, length(future_years)))
  
  future_df <- tibble(
    year = future_years,
    `Current forecast + historical growth` = future_vals
  )
  
  df %>%
    left_join(future_df, by = "year", suffix = c("", ".new")) %>%
    mutate(`Current forecast + historical growth` =
             dplyr::coalesce(`Current forecast + historical growth`,
                             `Current forecast + historical growth.new`)) %>%
    select(-`Current forecast + historical growth.new`)
}


# Fill Last Observed Year 

fill_last_observed_year <- function(df, observed_col, year_col = "year", digits = 2) {
  last_year <- max(df[[year_col]][!is.na(df[[observed_col]])], na.rm = TRUE)
  last_val  <- round(df[[observed_col]][df[[year_col]] == last_year][1], digits)
  
  fill_cols <- setdiff(names(df), c(year_col, observed_col))
  num_cols  <- fill_cols[sapply(df[fill_cols], is.numeric)]
  
  df %>%
    mutate(across(
      all_of(num_cols),
      ~ ifelse(.data[[year_col]] == last_year & is.na(.x), last_val, .x)
    ))
}

# Get latest value 

get_latest_value <- function(df, var) {
  var <- rlang::ensym(var)
  var_name <- rlang::as_name(var)
  new_col  <- paste0(var_name, "_latest")
  
  df %>%
    filter(!is.na(!!var)) %>%
    group_by(country_code) %>%
    slice_max(order_by = year, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(country_code, !!new_col := !!var)
}

# Numeric transformation

# helper to coerce numeric-like text (e.g., "1,234.56", "12.3%") to numeric
clean_to_numeric <- function(x) {
  x <- gsub(",", "", x)
  x <- sub("%$", "", x)
  suppressWarnings(as.numeric(x))
}

# Keep only the last k non-NA entries for any column name matching a regex pattern
.keep_last_k_by_pattern <- function(df, pattern = "\\(forecast\\)", k = 2) {
  for (col in names(df)) {
    if (grepl(pattern, col)) {
      non_na_idx <- which(!is.na(df[[col]]))
      if (length(non_na_idx) > k) {
        drop_idx <- head(non_na_idx, length(non_na_idx) - k)
        df[[col]][drop_idx] <- NA
      }
    }
  }
  df
}

# ------- Section #2. Figure-Specific Helpers ---------

# Builder for Figure 1 & 2

# Inputs:
#   dta_proj, dta_pip: long frames with region_name, year, poverty_line, headcount, pop_in_poverty, estimate_type
#   pov_lines: numeric vector, e.g. c(3.0, 4.2, 8.3)
#   year_start_fig1: first year to keep in output
#   line3pct, millions3pct2030: scalars to append as columns
build_fig1_2 <- function(dta_proj, dta_pip, pov_lines, year_start_fig1, line3pct, millions3pct2030) {
  
  pov_lines_fmt <- format(pov_lines, nsmall = 1, trim = TRUE)  # "3.0" "4.2" "8.3"
  
  # 1) Combine, keep World, fix estimate_type, convert pop to millions
  dta <- dplyr::bind_rows(dta_proj, dta_pip) %>%
    dplyr::select(region_name, year, poverty_line, headcount, pop_in_poverty, estimate_type) %>%
    dplyr::mutate(
      estimate_type = dplyr::if_else(is.na(estimate_type), "nowcast", estimate_type),
      estimate_type = dplyr::if_else(estimate_type == "projection", "actual", estimate_type),
      estimate_type = stringr::str_to_lower(estimate_type)
    ) %>%
    dplyr::filter(region_name == "World") %>%
    dplyr::mutate(pop_in_poverty = pop_in_poverty / 1e6) %>%  # millions
    dplyr::select(-region_name)
  
  # 2) Long → wide (split by estimate_type), then split by poverty_line
  wide <- dta %>%
    tidyr::pivot_wider(
      id_cols = c(year, poverty_line),
      names_from = estimate_type,
      values_from = c(headcount, pop_in_poverty),
      names_glue = "{.value}_{estimate_type}"
    ) %>%
    dplyr::arrange(year, poverty_line) %>%
    dplyr::mutate(poverty_line = format(as.numeric(poverty_line), nsmall = 1)) %>%
    tidyr::pivot_wider(
      id_cols = year,
      names_from = poverty_line,
      values_from = c(headcount_actual, headcount_nowcast,
                      pop_in_poverty_actual, pop_in_poverty_nowcast),
      names_glue = "{.value}_pl_{poverty_line}"
    )
  
  # 3) Add extra series
  wide <- wide %>%
    dplyr::mutate(line3pct = !!line3pct,
                  millions3pct2030 = !!millions3pct2030)
  
  # 4) Order columns programmatically
  cols_order <- c(
    "year",
    as.vector(rbind(
      glue("headcount_actual_pl_{pov_lines_fmt}"),
      glue("pop_in_poverty_actual_pl_{pov_lines_fmt}")
    )),
    glue("headcount_nowcast_pl_{pov_lines_fmt}"),
    glue("pop_in_poverty_nowcast_pl_{pov_lines_fmt}"),
    c("line3pct", "millions3pct2030")
  ) %>% unique()
  
  wide <- wide %>% dplyr::select(dplyr::any_of(cols_order))
  
  # 5) Copy actual to nowcast ONLY for the last actual year
  wide <- .copy_last_actual_into_nowcast(wide, pov_lines_fmt)
  
  # 6) Filter start year, enforce numeric, apply rounding, NA → ""
  wide <- wide %>%
    dplyr::filter(year >= year_start_fig1) %>%
    dplyr::mutate(dplyr::across(everything(), ~ suppressWarnings(as.numeric(.)))) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ {
      if (startsWith(cur_column(), "headcount_")) round(., 5) else round(., 0)
    })) %>%
    dplyr::mutate(dplyr::across(everything(), ~ ifelse(is.na(.), "", .)))
  
  # 7) Labels for Figure 1a
  fig1a <- wide %>%
    dplyr::rename_with(~ paste0("Poverty rate at $", stringr::str_extract(.x, "\\d+\\.\\d+")),
                       dplyr::starts_with("headcount_actual_pl_")) %>%
    dplyr::rename_with(~ paste0("Millions of poor at $", stringr::str_extract(.x, "\\d+\\.\\d+")),
                       dplyr::starts_with("pop_in_poverty_actual_pl_")) %>%
    dplyr::rename_with(~ paste0("Poverty rate at $", stringr::str_extract(.x, "\\d+\\.\\d+"), " (forecast)"),
                       dplyr::starts_with("headcount_nowcast_pl_")) %>%
    dplyr::rename_with(~ paste0("pop_in_poverty", gsub("\\.", "", stringr::str_extract(.x, "\\d+\\.\\d+")), "_forecast"),
                       dplyr::starts_with("pop_in_poverty_nowcast_pl_"))
  
  # 8) Figure 1b: rename the forecast millions to display labels
  pl_suffix <- gsub("\\.", "", pov_lines_fmt)
  pl_label  <- pov_lines_fmt
  rename_vec <- stats::setNames(
    glue("pop_in_poverty{pl_suffix}_forecast"),
    glue("Millions of poor at ${pl_label} (forecast)")
  )
  fig1b <- fig1a %>% dplyr::rename(!!!rename_vec)
  
  list(fig1a = fig1a, fig1b = fig1b)
}

# Builder for Figure 3

build_fig3 <- function(dta_fig_3,
                       year_start_fig3 = 1990,
                       bridge_year = 2024,
                       regions = c("AFE","AFW","EAS","ECS","LCN","MEA","NAC","SAS","SSF","WLD"),
                       regions_name = c(
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
                       )) {
  
  # deps
  require(dplyr)
  require(tidyr)
  require(stringr)
  # small helper (safe numeric)
  clean_to_numeric <- function(x) suppressWarnings(readr::parse_number(as.character(x)))
  
  # ---- STEP 1. Filter, recode, prepare
  dta_long <- dta_fig_3 %>%
    dplyr::filter(region_code %in% regions, year >= year_start_fig3) %>%
    dplyr::mutate(
      hc = headcount * 100,
      estimate_type = dplyr::case_when(
        estimate_type == "actual" ~ "obs",
        estimate_type %in% c("projection", "nowcast") ~ "proj",
        TRUE ~ estimate_type
      ),
      year = as.character(year)
    ) %>%
    dplyr::select(poverty_line, year, region_name, estimate_type, hc)
  
  # ---- STEP 2. Wide reshape: Region x estimate_type
  dta_wide <- dta_long %>%
    tidyr::pivot_wider(
      names_from  = c(region_name, estimate_type),
      values_from = hc,
      names_glue  = "{region_name}_{estimate_type}"
    ) %>%
    dplyr::arrange(poverty_line, as.numeric(year))
  
  # ---- STEP 3. Bridge missing projections in a specific year
  for (r in regions_name) {
    proj <- paste0(r, "_proj"); obs <- paste0(r, "_obs")
    if (proj %in% names(dta_wide) && obs %in% names(dta_wide)) {
      dta_wide[[proj]] <- ifelse(dta_wide$year == as.character(bridge_year) &
                                   is.na(dta_wide[[proj]]),
                                 dta_wide[[obs]], dta_wide[[proj]])
    }
  }
  
  # ---- STEP 4. Smooth transitions around projection blocks
  dta_wide$year <- as.integer(dta_wide$year)
  dta_wide <- dplyr::arrange(dta_wide, poverty_line, year)
  
  proj_cols <- grep("_proj$", names(dta_wide), value = TRUE)
  grp_idx   <- split(seq_len(nrow(dta_wide)), dta_wide$poverty_line)
  
  for (col in proj_cols) {
    obs_col <- sub("_proj$", "_obs", col)
    if (!obs_col %in% names(dta_wide)) next
    
    for (rows in grp_idx) {
      yr   <- dta_wide$year[rows]
      proj <- dta_wide[rows, col][[1]]
      obs  <- dta_wide[rows, obs_col][[1]]
      
      proj_idx <- which(!is.na(proj))
      if (length(proj_idx) == 0L) next
      
      cut_points  <- c(1L, which(diff(proj_idx) > 1L) + 1L)
      block_start <- proj_idx[cut_points]
      block_end   <- proj_idx[c(cut_points[-1] - 1L, length(proj_idx))]
      
      for (b in seq_along(block_start)) {
        s <- block_start[b]; e <- block_end[b]
        if (s > 1L) {
          before_idx <- s - 1L
          if (!is.na(obs[before_idx])) proj[before_idx] <- obs[before_idx]
        }
        if (e < length(proj)) {
          after_idx <- e + 1L
          if (!is.na(obs[after_idx])) proj[after_idx] <- obs[after_idx]
        }
      }
      
      next_has_proj <- c(!is.na(proj[-1]) & yr[-1] == yr[-length(yr)] + 1L, FALSE)
      fill_here <- is.na(proj) & next_has_proj
      if (any(fill_here)) proj[fill_here] <- obs[fill_here]
      
      dta_wide[rows, col] <- proj
    }
  }
  
  # ---- STEP 5. Column order + display names
  new_col_order <- c(
    "poverty_line","year",
    "World_obs","World_proj",
    "East Asia & Pacific_obs","East Asia & Pacific_proj",
    "Europe & Central Asia_obs","Europe & Central Asia_proj",
    "Latin America & Caribbean_obs","Latin America & Caribbean_proj",
    "Middle East, North Africa, Afghanistan & Pakistan_obs",
    "Middle East, North Africa, Afghanistan & Pakistan_proj",
    "North America_obs","North America_proj",
    "South Asia_obs","South Asia_proj",
    "Sub-Saharan Africa_obs","Sub-Saharan Africa_proj",
    "Eastern and Southern Africa_obs","Eastern and Southern Africa_proj",
    "Western and Central Africa_obs","Western and Central Africa_proj"
  )
  
  # silently keep only those that exist (some projects may miss some regions)
  keep_cols <- intersect(new_col_order, names(dta_wide))
  dta_wide  <- dplyr::select(dta_wide, dplyr::any_of(keep_cols))
  
  new_names <- c(
    "Poverty line",
    "Year",
    "World","World (Projection)",
    "East Asia & Pacific","East Asia & Pacific (Projection)",
    "Europe & Central Asia","Europe & Central Asia (Projection)",
    "Latin America & Caribbean","Latin America & Caribbean (Projection)",
    "Middle East, North Africa, Afghanistan & Pakistan",
    "Middle East, North Africa, Afghanistan & Pakistan (Projection)",
    "North America","North America (Projection)",
    "South Asia","South Asia (Projection)",
    "Sub-Saharan Africa","Sub-Saharan Africa (Projection)",
    "Eastern & Southern Africa","Eastern & Southern Africa (Projection)",
    "Western & Central Africa","Western & Central Africa (Projection)"
  )[seq_along(keep_cols)]
  
  colnames(dta_wide) <- new_names
  
  # ---- STEP 6. Poverty line labels, rounding, blanks
  out <- dta_wide %>%
    dplyr::mutate(
      `Poverty line` = dplyr::case_when(
        round(as.numeric(`Poverty line`), 1) == 3.0 ~ "$3.00 (2021 PPP)",
        round(as.numeric(`Poverty line`), 1) == 4.2 ~ "$4.20 (2021 PPP)",
        round(as.numeric(`Poverty line`), 1) == 8.3 ~ "$8.30 (2021 PPP)",
        TRUE ~ as.character(`Poverty line`)
      )
    ) %>%
    dplyr::mutate(
      dplyr::across(
        -c(Year, `Poverty line`),
        ~ round(clean_to_numeric(.), 1)
      )
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(is.na(.), "", .)))
  
  return(out)
}

# Builder for Figure 4 & 5

build_fig4_5 <- function(povline,
                       year_start_fig4_5,
                       year_end_fig4_5,
                       dta_proj_scen_wide_final,
                       dta_proj,
                       round_digits = 2) {
  
  # 1) Observed (World) base
  dta_fig_2 <- get_wb(povline = povline) %>%
    select(region_name, year, headcount) %>%
    filter(region_name == "World", year >= year_start_fig4_5) %>%
    mutate(
      `Observed` = headcount * 100,
      `Current forecast + historical growth` = NA_real_,
      `2% growth` = NA_real_,
      `2% growth + Gini reduction 1%` = NA_real_,
      `2% growth + Gini reduction 2%` = NA_real_,
      `4% growth` = NA_real_
    ) %>%
    select(
      year, `Observed`, `Current forecast + historical growth`,
      `2% growth`, `2% growth + Gini reduction 1%`,
      `2% growth + Gini reduction 2%`, `4% growth`
    ) %>%
    complete(year = full_seq(c(min(year), year_end_fig4_5), 1))
  
  # 2) Scenario rows for this povline  --------- CHANGED: use *_final
  dta_proj_scen_p <- dta_proj_scen_wide_final %>%
    dplyr::filter(povertyline == povline)

  # 4) Merge base + scenarios; no extra join for FC column  --------- CHANGED
  out <- dplyr::bind_rows(
    dta_fig_2 %>% dplyr::anti_join(dta_proj_scen_p, by = "year"),
    dta_proj_scen_p
  ) %>%
    dplyr::select(-povertyline)
  
  # 6) Fill ONLY the last Observed year across other numeric columns if NA
  out <- fill_last_observed_year(out, observed_col = "Observed", year_col = "year", digits = round_digits)
  
  # 7) Round all numeric columns except year
  out <- out %>% mutate(across(c(where(is.numeric), -year), ~ round(.x, round_digits))) %>%
    dplyr::mutate(across(-year, ~ ifelse(is.na(.x), "", .x)))
  
  out
}

# Builder for Figure 6 

build_fig6 <- function(dta_class,
                       dta_pip_ctry,
                       year_start_fig6,
                       year_end_fig6) {

  # ---- 2) Combine with pip data
  dta_fig_6 <- left_join(dta_pip_ctry, dta_class_inc,
                         by = "country_code") %>%
    select(country_code, year, inc_grp, pop, poverty_line, headcount, estimate_type) %>%
    filter(year >= year_start_fig6 &
             year <= year_end_fig6)
  
  # ---- 3) Split different poverty line (only first two)
  dta_fig_6a <- dta_fig_6 %>%
    filter(poverty_line == 3.0)
  
  # ---- 4) Transformation
  dta_fig_6a_final <- dta_fig_6a %>%
    group_by(inc_grp, year) %>%
    summarise(
      headcount_group = weighted.mean(headcount, pop, na.rm = TRUE)
    ) %>%
    group_by(inc_grp) %>%
    mutate(
      headcount_ref = headcount_group[year == year_start_fig6],
      headcount_diff = headcount_group / headcount_ref
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
  
  # ---- $8.3 poverty line
  dta_fig_6b <- dta_fig_6 %>%
    filter(poverty_line == 8.3)
  
  dta_fig_6b_final <- dta_fig_6b %>%
    group_by(inc_grp, year) %>%
    summarise(
      headcount_group = weighted.mean(headcount, pop, na.rm = TRUE)
    ) %>%
    group_by(inc_grp) %>%
    mutate(
      headcount_ref = headcount_group[year == year_start_fig6],
      headcount_diff = headcount_group / headcount_ref
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
  
  # ---- Combine 6a and 6b
  dta_fig_6_final <- bind_rows(dta_fig_6a_final, dta_fig_6b_final)
  
  return(dta_fig_6_final)
}


build_fig7 <- function(dta_pip_ctry, target_year = 2025) {
  
  # ---- 1) Select and compute
  dta_fig_7_final <- dta_pip_ctry %>%
    select(year, region_name, country_name, headcount, pop, poverty_line) %>%
    mutate(pop_in_poverty = as.integer((pop * headcount) / 1000000)) %>%
    group_by(country_name) %>%
    filter(year == target_year) %>%
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
      "Region"           = region_name,
      "Country Name"     = country_name,
      "Millions of poor" = pop_in_poverty,
      "Poverty Line"     = poverty_line
    )
  
  # ✅ Return only final output
  return(dta_fig_7_final)
}


# Builder for Figure 8

build_fig8 <- function(df,
                       label = "Global Prosperity Gap",
                       digits = 2,
                       keep_last_k = 2) {
  
  # 1) Normalize estimate_type and aggregate
  df <- df %>%
    dplyr::mutate(estimate_type = dplyr::case_when(
      estimate_type == "projection" ~ "actual",
      estimate_type == "nowcast"    ~ "forecast",
      TRUE                          ~ estimate_type
    )) %>%
    dplyr::group_by(year, estimate_type) %>%
    dplyr::summarise(pg = mean(pg, na.rm = TRUE), .groups = "drop")
  
  # 2) Wide: year | actual | forecast
  df_wide <- df %>%
    tidyr::pivot_wider(names_from = estimate_type, values_from = pg) %>%
    dplyr::arrange(year)
  
  # 3) Make overlapping year match: forecast(year*) = actual(year*)
  last_actual_year <- max(df_wide$year[!is.na(df_wide$actual)], na.rm = TRUE)
  if (is.finite(last_actual_year)) {
    df_wide <- df_wide %>%
      dplyr::mutate(
        forecast = dplyr::if_else(year == last_actual_year, actual, forecast)
      )
  }
  
  # 4) Round
  df_wide <- df_wide %>%
    dplyr::mutate(
      actual   = round(actual,   digits),
      forecast = round(forecast, digits)
    )
  
  # 5) Keep only the last k forecast values, but always keep the overlap year too
  if (is.finite(keep_last_k) && keep_last_k > 0) {
    non_na_idx <- which(!is.na(df_wide$forecast))
    if (length(non_na_idx) > keep_last_k) {
      keep_idx <- tail(non_na_idx, keep_last_k)
      if (is.finite(last_actual_year)) {
        overlap_idx <- which(df_wide$year == last_actual_year)
        keep_idx <- sort(unique(c(keep_idx, overlap_idx)))
      }
      drop_idx <- setdiff(non_na_idx, keep_idx)
      df_wide$forecast[drop_idx] <- NA_real_
    }
  }
  
  # 6) Final labels
  col_actual   <- label
  col_forecast <- glue::glue("{label} (forecast)")
  
  out <- df_wide %>%
    dplyr::transmute(
      year,
      !!col_actual   := actual,
      !!col_forecast := forecast
    ) %>%
    dplyr::mutate(dplyr::across(-year, ~ ifelse(is.na(.x), "", .x)))
  
  out
}

# Builder for Figure 9

build_fig9 <- function(df, digits = 2) {
  df1 <- df %>%
    dplyr::mutate(
      region = dplyr::recode(
        region_name,
        "East Asia & Pacific"         = "East Asia and Pacific",
        "Latin America & Caribbean"   = "Latin America and the Caribbean",
        "Middle East, North Africa, Afghanistan & Pakistan"  = "Middle East, North Africa, Afghanistan and Pakistan",
        "North America" = "North America",
        "Europe & Central Asia"       = "Europe and Central Asia",
        .default = region_name
      )
    )%>%
    dplyr::select(region, pop_share, pg_share) %>%
    # make sure shares are numeric (remove % if any)
    dplyr::mutate(
      pop_share = as.numeric(gsub("%", "", pop_share)),
      pg_share  = as.numeric(gsub("%", "", pg_share))
    )
  
  col_order <- c(
    "Sub-Saharan Africa",
    "South Asia",
    "East Asia and Pacific",
    "Latin America and the Caribbean",
    "Middle East, North Africa, Afghanistan and Pakistan",
    "North America",
    "Europe and Central Asia"
  )
  
  # Row 1: Prosperity Gap share (aggregate to unique region)
  pg_row <- df1 %>%
    dplyr::select(region, value = pg_share) %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = region, values_from = value) %>%
    # ensure all expected columns exist
    { for (nm in col_order) if (!nm %in% names(.)) .[[nm]] <- NA_real_; . } %>%
    dplyr::relocate(dplyr::all_of(col_order)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(as.numeric(.x), digits)))
  
  # Row 2: Population share (aggregate to unique region)
  pop_row <- df1 %>%
    dplyr::select(region, value = pop_share) %>%
    dplyr::group_by(region) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = region, values_from = value) %>%
    { for (nm in col_order) if (!nm %in% names(.)) .[[nm]] <- NA_real_; . } %>%
    dplyr::relocate(dplyr::all_of(col_order)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(as.numeric(.x), digits)))
  
  out <- dplyr::bind_rows(pg_row, pop_row)
  out <- dplyr::bind_cols(
    tibble::tibble(Country = c("Prosperity Gap share", "Population share")),
    out
  )
  
  out %>%
    dplyr::mutate(dplyr::across(-Country, ~ ifelse(is.na(.x), "", .x)))
}

# Builder for Figure 10

# Input  : country-year data with cols: country_code, year, mean, gini, pop, pg
# Output : tibble with the four components + a check column
# Notes  : Implements the same logic as the Stata code you shared

build_fig10 <- function(dta_fig_5,
                         z = 25,
                         keep_years = c(1990, 2000, 2010, 2019, 2025),
                         last_label_override = "2019–2025 (projected)") {
  
  stopifnot(all(c("year","pop","mean","pg") %in% names(dta_fig_5)))
  
  suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
    library(tibble)
    library(stringr)
  })
  
  # 1) Country-year inequality term (if not provided in data)
  df <- dta_fig_5 %>%
    mutate(i = (pg * mean) / z)
  
  # 2) Build within-country inequality aggregator per year
  #    weight_cty = (pop/mean) / sum(pop/mean)  -> I_g = weight_cty * i
  #    w_I_year   = sum(I_g)
  df_wI <- df %>%
    mutate(w_i = pop / mean) %>%
    group_by(year) %>%
    mutate(weight_cty = w_i / sum(w_i, na.rm = TRUE),
           I_g        = weight_cty * i) %>%
    summarise(w_I = sum(I_g, na.rm = TRUE), .groups = "drop")
  
  # 3) Collapse to global series (pop-weighted pg and mean), join w_I
  df_year <- df %>%
    group_by(year) %>%
    summarise(
      pg   = weighted.mean(pg,   w = pop, na.rm = TRUE),
      mean = weighted.mean(mean, w = pop, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(df_wI, by = "year") %>%
    mutate(
      i_total = (pg * mean) / z,     # total inequality implied by identity
      b_I     = i_total / w_I        # between-country factor so i_total = b_I * w_I
    ) %>%
    arrange(year) %>%
    filter(year %in% keep_years) %>%
    arrange(year)
  
  # 4) Annualized log growth between consecutive kept years
  grow <- function(x, yrs) 100 * diff(log(x)) / diff(yrs)
  
  yrs   <- df_year$year
  g_pg  <- grow(df_year$pg,     yrs)
  g_i   <- grow(df_year$i_total,yrs)
  g_wI  <- grow(df_year$w_I,    yrs)
  g_mu  <- grow(df_year$mean,   yrs)
  
  # Contributions (match Stata’s signs):
  # gpg = g(PG)
  # gi  = g(i_total) = gb_I + gw_I
  # gw_I = g(w_I)
  # gb_I = gi - gw_I
  # mean contribution enters with negative sign (higher mean lowers PG)
  out <- tibble(
    year = paste(yrs[-length(yrs)], yrs[-1], sep = "\u2013"),
    `Global Prosperity Gap growth rate`         = g_pg,
    `Contribution of within-country inequality` = g_wI,
    `Contribution of between-country inequality`= g_i - g_wI,
    `Contribution of mean growth`               = -g_mu
  ) %>%
    mutate(
      check_sum = `Global Prosperity Gap growth rate` -
        (`Contribution of within-country inequality` +
           `Contribution of between-country inequality` +
           `Contribution of mean growth`)
    )
  
  # Optional pretty label for the last interval (e.g., projected)
  last_iv <- paste(2019, 2025, sep = "\u2013")
  out$year[out$year == last_iv] <- last_label_override
  
  # Round to 2 decimals (World Bank table style); keep check_sum unrounded for QC
  out %>%
    mutate(across(-c(year, check_sum), ~ round(.x, 2)))
}

# Builder for Figure 11

build_fig11 <- function(dta_pip_ctry_v2) {
  
  # ---- 1) Prepare and classify inequality levels
  dta_fig_11 <- dta_pip_ctry_v2 %>%
    mutate(
      gini = gini * 100,
      Classification = case_when(
        gini > 40               ~ "High inequality",
        gini >= 30 & gini <= 40 ~ "Moderate inequality",
        gini < 30               ~ "Low inequality",
        TRUE                    ~ NA_character_
      )
    ) %>%
    rename(
      `Country Name` = country_name,
      `Region`       = region_name,
      `Survey year`  = welfare_time,
      `Gini index`   = gini,
      `Welfare type` = welfare_type
    ) %>%
    mutate(
      country_name_flourish = recode(
        `Country Name`,
        "Slovak Republic"       = "Slovakia",
        "Czechia"               = "Czech Republic",
        "Kyrgyz Republic"       = "Kyrgyzstan",
        "Syrian Arab Republic"  = "Syria",
        "Egypt, Arab Rep."      = "Egypt",
        "Korea, Rep."           = "South Korea",
        "Iran, Islamic Rep."    = "Iran",
        "Russian Federation"    = "Russia",
        "Cote d'Ivoire"         = "Ivory Coast",
        "Viet Nam"              = "Vietnam",
        "Yemen, Rep."           = "Yemen",
        "West Bank and Gaza"    = "Palestine",
        "North Macedonia"       = "Republic of Macedonia",
        "Turkiye"               = "Turkey",
        "Micronesia, Fed. Sts." = "Federated States of Micronesia",
        "Gambia, The"           = "The Gambia",
        "Lao PDR"               = "Laos",
        "Cabo Verde"            = "Cape Verde",
        "Sao Tome and Principe" = "São Tomé and Príncipe",
        "Congo, Dem. Rep."      = "Democratic Republic of the Congo",
        "Congo, Rep."           = "Republic of the Congo",
        "United States"         = "United States of America",
        .default                = `Country Name`
      )
    ) %>%
    select(country_code, `Country Name`, country_name_flourish,
           Region, `Survey year`, `Gini index`, `Welfare type`, Classification)
  
  # ---- 2) Keep latest survey year per country
  dta_fig_11_latest <- dta_fig_11 %>%
    group_by(country_code) %>%
    filter(`Survey year` == max(`Survey year`, na.rm = TRUE)) %>%
    ungroup() %>%
    distinct()
  
  # ✅ Return only the final cleaned dataset
  return(dta_fig_11_latest)
}


# Builder for Figure 12 & 13

# Inputs expected:
# - WDI_Gini: columns country, iso3c, year, SI.POV.GINI, ...
# - countrycodes_current: columns code, incgroup_current, fcv, ...
build_fig12_13 <- function(WDI_Gini, countrycodes_current) {
  
  # 1) keep latest non-missing Gini by country (proxy for *_latest.dta)
  latest_gini <- WDI_Gini %>%
    dplyr::filter(!is.na(SI.POV.GINI)) %>%
    dplyr::arrange(iso3c, dplyr::desc(year)) %>%
    dplyr::group_by(iso3c) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(code = iso3c)                # Stata: ren country_code code
  
  # 2) merge 1:1 on code, keep matched only (Stata: keep(3))
  merged <- latest_gini %>%
    dplyr::inner_join(
      countrycodes_current %>%
        dplyr::select(code, incgroup_current, fcv),
      by = "code"
    )
  
  # 3) inequality groups (ginigroup): 0=Low (<30 or missing), 1=30–40, 2=>40
  merged <- merged %>%
    dplyr::mutate(
      ginigroup = dplyr::case_when(
        SI.POV.GINI > 40                      ~ 2L,
        SI.POV.GINI >= 30 & SI.POV.GINI <= 40 ~ 1L,
        TRUE                                  ~ 0L
      ),
      count = 1L
    )
  
  # 4) collapse counts by incgroup_current × ginigroup (Income panel)
  inc_panel <- merged %>%
    dplyr::count(incgroup_current, ginigroup, name = "count") %>%
    dplyr::transmute(name = incgroup_current, group = "Income group FY24",
                     ginigroup, count)
  
  # 5) collapse counts by fcv × ginigroup (FCV panel)
  fcv_panel <- merged %>%
    dplyr::count(fcv, ginigroup, name = "count") %>%
    dplyr::transmute(name = fcv, group = "FCV group FY24",
                     ginigroup, count)
  
  # 6) stack, compute shares within (group,name), reshape wide
  stacked <- dplyr::bind_rows(fcv_panel, inc_panel) %>%
    dplyr::group_by(group, name) %>%
    dplyr::mutate(total = sum(count), share = 100 * count / total) %>%
    dplyr::ungroup()
  
  wide_tbl <- stacked %>%
    dplyr::select(group, name, ginigroup, share) %>%
    dplyr::mutate(ginigroup = paste0("share", ginigroup)) %>%
    tidyr::pivot_wider(names_from = ginigroup, values_from = share, values_fill = 0)
  
  # 7) sort order BEFORE renaming Yes/No (replace recode() with factor ordering)
  sort_levels <- c("Yes", "No",
                   "Low-income", "Lower-middle-income",
                   "Upper-middle-income", "High-income")
  
  wide_sorted <- wide_tbl %>%
    dplyr::mutate(
      sort = as.integer(factor(name, levels = sort_levels)),
      sort = ifelse(is.na(sort), 999L, sort)
    ) %>%
    dplyr::arrange(sort)
  
  # 8) rename Yes/No to FCS/Non-FCS (after sorting), select/format columns
  out <- wide_sorted %>%
    dplyr::mutate(
      name = dplyr::if_else(group == "FCV group FY26" & name == "Yes", "FCS", name),
      name = dplyr::if_else(group == "FCV group FY26" & name == "No",  "Non-FCS", name)
    ) %>%
    dplyr::select(group, name,
                  `Low inequality`      = share0,
                  `Moderate inequality` = share1,
                  `High inequality`     = share2) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 2)))
  
  out
}


# Builder for Figure 14 & 15

build_fig14_15 <- function(dta_inc_dist, dta_class_inc, target_year) {
  
  # 1) Merge with income group data and keep target year
  dta_base <- dta_inc_dist %>%
    left_join(dta_class_inc, by = "country_code") %>%
    filter(year == target_year) %>%
    rename(code = country_code,
           pop = population,
           povertyline = poverty_line,
           pov = headcount,
           incgroup = inc_grp) %>%
    select(code, year, pop, povertyline, pov, incgroup)
  
  # 2) Total poor and collapse by group-povertyline-year
  povincgroup <- dta_base %>%
    mutate(poor = pov * pop) %>%
    summarise(
      poor = sum(poor, na.rm = TRUE),
      pop  = sum(pop,  na.rm = TRUE),
      .by  = c(incgroup, year, povertyline)
    )
  
  # 3) Incremental poor (marginal between adjacent poverty lines)
  povincgroup <- povincgroup %>%
    group_by(year, incgroup) %>%
    arrange(povertyline, .by_group = TRUE) %>%
    mutate(
      poorincremental = poor - lag(poor, default = 0),
      poorincremental = if_else(is.na(poorincremental) | poorincremental < 0,
                                poor, poorincremental)
    ) %>%
    ungroup()
  
  # 4) Global population by poverty line and shares
  povincgroup <- povincgroup %>%
    group_by(year, povertyline) %>%
    mutate(pop_global = sum(pop, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(poorshare = poorincremental / pop_global)
  
  # 5) Wide: columns are income groups
  dta_final <- povincgroup %>%
    select(year, povertyline, incgroup, poorshare) %>%
    pivot_wider(names_from = incgroup, values_from = poorshare)
  
  # 6) Cumulative series for stacked area chart
  dta_final_v2 <- dta_final %>%
    mutate(
      `Low-income`           = `Low income`,
      `Lower-middle-income`  = `Low-income` + `Lower middle income`,
      `Upper-middle-income`  = `Lower-middle-income` + `Upper middle income`,
      `High-income`          = `Upper-middle-income` + `High income`
    ) %>%
    rename("poverty line in 2021 PPP US$ (per capita per day)" = povertyline) %>%
    select(year,
           `Low-income`, `Lower-middle-income`,
           `Upper-middle-income`, `High-income`,
           "poverty line in 2021 PPP US$ (per capita per day)")
  
  # Return only the final table
  return(dta_final_v2)
}


# Builder for Figure 16

build_fig16 <- function(dta_class,
                        dta_proj_ctry,
                        dta_pip,
                        dta_proj,
                        dta_pip_ctry,
                        year_start_fig16) {
  
  # deps
  require(dplyr)
  require(tidyr)
  require(rlang)
  
  # ---- Helper: latest non-missing value per country (by year desc)
  get_latest_value <- function(df, var) {
    var <- enquo(var)
    df %>%
      arrange(country_code, desc(year)) %>%
      group_by(country_code) %>%
      summarise(!!paste0(as_name(var), "_latest") := dplyr::first(na.omit(!!var)),
                .groups = "drop")
  }
  
  # dta_fcs
  dta_fcs <- dta_class %>%
    select(code, year_data, fcv)
  
  # dta_proj_ctry_v2
  dta_proj_ctry_v2 <- dta_proj_ctry %>%
    filter(poverty_line == 3.0) %>%
    rename(country_code = code) %>%
    select(-poverty_line)
  
  # Extract world population (kept exactly as in your script)
  dta_pop_wld <- dta_pip %>%
    filter(region_code == "WLD",
           poverty_line == 3.0) %>%
    bind_rows(dta_proj) %>%
    select(region_code, year, pop_in_poverty)
  
  # Base: country-level, $3.00, from start year; join FCS; append projections
  dta_fig_16 <- dta_pip_ctry %>%
    filter(poverty_line == 3.0,
           year >= year_start_fig16) %>%
    select(region_code, country_code, year, headcount, pop) %>%
    left_join(dta_fcs, by = c("country_code" = "code", "year" = "year_data")) %>%
    bind_rows(dta_proj_ctry_v2)
  
  # Latest region and fcv definitions for projection years
  latest_region <- get_latest_value(dta_fig_16, region_code)
  latest_fcv    <- get_latest_value(dta_fig_16, fcv)
  
  # Combine it back to original dataset
  dta_fig_16_final <- dta_fig_16 %>%
    left_join(latest_region, by = "country_code") %>%
    left_join(latest_fcv,    by = "country_code") %>%
    mutate(
      region_code = coalesce(region_code, region_code_latest),
      fcv         = coalesce(fcv, fcv_latest)
    ) %>%
    select(-ends_with("_latest")) %>%
    filter(!is.na(fcv) & !is.na(region_code)) %>%
    mutate(pop_in_poverty = headcount * pop)
  
  # Calculate share in poverty by group
  dta_fig_16_grouped <- dta_fig_16_final %>%
    mutate(group = case_when(
      region_code == "SSF" & fcv == "Yes" ~ "FCS in SSA",
      region_code == "SSF" & fcv == "No"  ~ "Non-FCS in SSA",
      region_code != "SSF" & fcv == "Yes" ~ "FCS outside SSA",
      region_code != "SSF" & fcv == "No"  ~ "Rest of the world"
    )) %>%
    group_by(year, group) %>%
    summarise(pop_in_poverty = sum(pop_in_poverty, na.rm = TRUE), .groups = "drop") %>%
    ungroup()
  
  dta_fig_16_wld <- dta_fig_16_final %>%
    mutate(group = case_when(
      region_code == "SSF" & fcv == "Yes" ~ "FCS in SSA",
      region_code == "SSF" & fcv == "No"  ~ "Non-FCS in SSA",
      region_code != "SSF" & fcv == "Yes" ~ "FCS outside SSA",
      region_code != "SSF" & fcv == "No"  ~ "Rest of the world"
    )) %>%
    group_by(year) %>%
    summarise(pop_in_poverty_wld = sum(pop_in_poverty, na.rm = TRUE), .groups = "drop") %>%
    ungroup()
  
  # Combine World Population -> shares -> wide -> order and rounding
  dta_fig_16_final_v2 <- dta_fig_16_grouped %>%
    left_join(dta_fig_16_wld, by = "year") %>%
    mutate(pop_in_poverty_share = 100 * (pop_in_poverty / pop_in_poverty_wld)) %>%
    select(year, group, pop_in_poverty_share) %>%
    pivot_wider(
      names_from  = group,
      values_from = pop_in_poverty_share
    ) %>%
    arrange(year) %>%
    mutate(across(-year, ~ round(.x, 1))) %>%
    rename(Year = year) %>%
    select(Year, "Non-FCS in SSA", "FCS in SSA",
           "FCS outside SSA", "Rest of the world")
  
  # Return only the final table (exact output)
  return(dta_fig_16_final_v2)
}

