
# ------- identify the last actual year from wide table ---------

.last_actual_year <- function(df_wide) {
  actual_cols <- names(df_wide) %>% stringr::str_subset("^(headcount|pop_in_poverty)_actual_pl_")
  if (length(actual_cols) == 0) return(NA_integer_)
  ref <- actual_cols[1]
  df_wide %>%
    dplyr::filter(!is.na(.data[[ref]])) %>%
    dplyr::summarise(max_year = max(year, na.rm = TRUE)) %>%
    dplyr::pull(max_year)
}

# ------- Copy actual → nowcast for ONLY the last actual year (all pov lines) ---------

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


# ------- Safe weighted mean ---------

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

# ------- Extend historical growth ---------

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


# ------- Fill Last Observed Year ---------

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


# ------- Builder for Figure 1 ---------

# Inputs:
#   dta_proj, dta_pip: long frames with region_name, year, poverty_line, headcount, pop_in_poverty, estimate_type
#   pov_lines: numeric vector, e.g. c(3.0, 4.2, 8.3)
#   year_start_fig1: first year to keep in output
#   line3pct, millions3pct2030: scalars to append as columns
build_fig1 <- function(dta_proj, dta_pip, pov_lines, year_start_fig1, line3pct, millions3pct2030) {
  
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


# ------- Builder for Figure 2 ---------

build_fig2 <- function(povline,
                       year_start_fig2,
                       year_end_fig2,
                       dta_proj_scen_wide,
                       dta_proj,
                       round_digits = 2) {
  
  # 1) Observed (World) base
  dta_fig_2 <- get_wb(povline = povline) %>%
    select(region_name, year, headcount) %>%
    filter(region_name == "World", year >= year_start_fig2) %>%
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
    complete(year = full_seq(c(min(year), year_end_fig2), 1))
  
  # 2) Scenario rows for this povline
  dta_proj_scen_p <- dta_proj_scen_wide %>% filter(povertyline == povline)
  
  # 3) Current forecast + historical growth from dta_proj (same povline)
  dta_proj_fc <- dta_proj %>%
    filter(poverty_line == povline) %>%
    mutate(`Current forecast + historical growth` = headcount * 100) %>%
    select(year, `Current forecast + historical growth`)
  
  # 4) Merge base + scenarios (avoid duplicate years), then bring in the FC column
  out <- bind_rows(
    dta_fig_2 %>% anti_join(dta_proj_scen_p, by = "year"),
    dta_proj_scen_p
  ) %>%
    select(-povertyline) %>%
    left_join(
      dta_proj_fc %>% transmute(year, add_fc = as.numeric(`Current forecast + historical growth`)),
      by = "year"
    ) %>%
    mutate(`Current forecast + historical growth` =
             dplyr::coalesce(`Current forecast + historical growth`, add_fc)) %>%
    select(-add_fc)
  
  # 5) Extend "Current forecast + historical growth" to end year using historical growth from Observed
  out <- extend_with_hist_growth(out, end_year = year_end_fig2)
  
  # 6) Fill ONLY the last Observed year across other numeric columns if NA
  out <- fill_last_observed_year(out, observed_col = "Observed", year_col = "year", digits = round_digits)
  
  # 7) Round all numeric columns except year
  out <- out %>% mutate(across(c(where(is.numeric), -year), ~ round(.x, round_digits))) %>%
    dplyr::mutate(across(-year, ~ ifelse(is.na(.x), "", .x)))
  
  out
}

# ------- Builder for F3 ---------

# ---- Fig 3 builder ----
build_fig3_table <- function(dta_fig_3,
                             min_year   = 1990,
                             fill_year  = 2024) {
  
  # target regions and their long names (for columns)
  regions_code <- c("AFE","AFW","EAS","ECS","LCN","MEA","NAC","SAS","SSF","WLD")
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
  
  # 1. filter, transform, and standardize estimate_type
  d_long <- dta_fig_3 %>%
    filter(region_code %in% regions_code, year >= min_year) %>%
    mutate(
      hc = headcount * 100,
      estimate_type = case_when(
        estimate_type == "actual" ~ "obs",
        estimate_type %in% c("projection","nowcast") ~ "proj",
        TRUE ~ estimate_type
      ),
      year = as.character(year)
    ) %>%
    select(poverty_line, year, region_name, estimate_type, hc)
  
  # 2. reshape wide: Region_obs, Region_proj
  d_wide <- d_long %>%
    pivot_wider(
      names_from  = c(region_name, estimate_type),
      values_from = hc,
      names_glue  = "{region_name}_{estimate_type}"
    ) %>%
    arrange(poverty_line, as.numeric(year))
  
  # 3. connect dotted lines at fill_year by copying obs -> proj when proj missing
  for (r in regions_name) {
    proj <- paste0(r, "_proj"); obs <- paste0(r, "_obs")
    if (!proj %in% names(d_wide)) d_wide[[proj]] <- NA_real_
    if (!obs  %in% names(d_wide)) d_wide[[obs]]  <- NA_real_
    d_wide[[proj]] <- ifelse(d_wide$year == as.character(fill_year) & is.na(d_wide[[proj]]),
                             d_wide[[obs]], d_wide[[proj]])
  }
  
  # 4. auto smooth: if proj exists at year t, fill proj at year t-1 with obs(t-1)
  d_wide$year <- as.integer(d_wide$year)
  d_wide <- d_wide[order(d_wide$poverty_line, d_wide$year), ]
  proj_cols <- grep("_proj$", names(d_wide), value = TRUE)
  grp_idx   <- split(seq_len(nrow(d_wide)), d_wide$poverty_line)
  
  for (col in proj_cols) {
    obs_col <- sub("_proj$", "_obs", col)
    if (!obs_col %in% names(d_wide)) next
    for (rows in grp_idx) {
      yr   <- d_wide$year[rows]
      proj <- d_wide[rows, col][[1]]
      obs  <- d_wide[rows, obs_col][[1]]
      next_has_proj <- c(!is.na(proj[-1]) & yr[-1] == yr[-length(yr)] + 1, FALSE)
      fill_here     <- is.na(proj) & next_has_proj
      if (any(fill_here)) d_wide[rows[fill_here], col] <- obs[fill_here]
    }
  }
  
  # 5. reorder and rename headers to match your Excel Flourish layout
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
  
  nice_names <- c(
    "Poverty line","Year",
    "World (Observed)","World",
    "East Asia & Pacific (Observed)","East Asia & Pacific",
    "Europe & Central Asia (Observed)","Europe & Central Asia",
    "Latin America & Caribbean (Observed)","Latin America & Caribbean",
    "Middle East, North Africa, Afghanistan & Pakistan (Observed)",
    "Middle East, North Africa, Afghanistan & Pakistan",
    "North America (Observed)","North America",
    "South Asia (Observed)","South Asia",
    "Sub-Saharan Africa (Observed)","Sub-Saharan Africa",
    "Eastern & Southern Africa (Observed)","Eastern & Southern Africa",
    "Western & Central Africa (Observed)","Western & Central Africa"
  )
  
  # ensure all columns exist before selecting
  for (nm in new_col_order) if (!nm %in% names(d_wide)) d_wide[[nm]] <- NA_real_
  
  d_out <- d_wide %>%
    select(any_of(new_col_order))
  
  names(d_out) <- nice_names
  
  # 6. format poverty line labels
  d_out <- d_out %>%
    mutate(
      `Poverty line` = case_when(
        round(as.numeric(`Poverty line`), 1) == 3.0 ~ "$3.00 (2021 PPP)",
        round(as.numeric(`Poverty line`), 1) == 4.2 ~ "$4.20 (2021 PPP)",
        round(as.numeric(`Poverty line`), 1) == 8.3 ~ "$8.30 (2021 PPP)",
        TRUE ~ as.character(`Poverty line`)
      ),
      Year = as.character(Year)
    )
  
  d_out
}

# ---- Main wrapper you can call from your pipeline ----
build_and_export_fig3 <- function(dta_fig_3, xlsx_path = NULL, ...) {
  out <- build_fig3_table(dta_fig_3, ...)
  if (!is.null(xlsx_path)) {
    # write to Excel if a path is provided
    if (!requireNamespace("writexl", quietly = TRUE)) {
      stop("Please install writexl to export: install.packages('writexl')")
    }
    writexl::write_xlsx(list(Flourish = out), path = xlsx_path)
  }
  out
}


# ------- Builder for Figure 3 ---------

# Main: build the Fig 3 table
build_fig3 <- function(data, year_start_fig3, keep_last_k = 2) {
  
  # 1) Aggregate to weighted headcount by year / inc_grp / estimate_type
  out <- data %>%
    group_by(year, inc_grp, estimate_type) %>%
    summarise(
      weighted_value = weighted.mean(headcount, pop, na.rm = TRUE),
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



build_fig3_from_dataset <- function(df,
                                    poverty_lines = c(2.15, 6.85),
                                    base_year = 2019) {
  library(dplyr)
  library(tidyr)
  library(stringr)
  # helper to standardize income group names similar to Stata labels
  std_inc <- function(x) dplyr::recode(x,
                                       "Low income"          = "Low-income",
                                       "Lower middle income" = "Lower-middle-income",
                                       "Upper middle income" = "Upper-middle-income",
                                       "High income"         = "High-income",
                                       .default = x
  )
  # inner worker: replicate Stata for one poverty line
  one_line <- function(dd, pl) {
    dd %>%
      filter(year > 2018,
             # handle both 2.15 or 0.0215 representations safely
             abs(poverty_line - pl) < 1e-6 | abs(poverty_line - pl/100) < 1e-9) %>%
      mutate(inc_grp = std_inc(inc_grp)) %>%
      group_by(inc_grp, year) %>%
      summarise(weighted_value = weighted.mean(headcount, pop, na.rm = TRUE),
                .groups = "drop") %>%
      group_by(inc_grp) %>%
      mutate(headcount_growth = headcount / headcount[year == base_year]) %>%
      ungroup() %>%
      select(year, inc_grp, headcount_growth) %>%
      pivot_wider(names_from = inc_grp,
                  values_from = headcount_growth,
                  names_prefix = "headcount_growth_") %>%
      { # create _forecast cols (year > 2021) and blank main for year > 2022
        main_cols <- setdiff(names(.), "year")
        fc_cols <- paste0(main_cols, "_forecast")
        for (i in seq_along(main_cols)) {
          .[[fc_cols[i]]] <- ifelse(.$year > 2021, .[[main_cols[i]]], NA_real_)
          .[[main_cols[i]]] <- ifelse(.$year > 2022, NA_real_, .[[main_cols[i]]])
        }
        .
      } %>%
      { # force Low-income dotted: move all Low-income to forecast, blank main
        li_main <- grep("^headcount_growth_Low-income$", names(.), value = TRUE)
        if (length(li_main) == 1) {
          li_fc <- paste0(li_main, "_forecast")
          .[[li_fc]] <- ifelse(is.na(.[[li_fc]]), .[[li_main]], .[[li_fc]])
          .[[li_main]] <- NA_real_
        }
        .
      } %>%
      { # order like Stata: LIC, LMC, UMC, HIC, then forecasts
        ord_main <- c("year",
                      "headcount_growth_Low-income",
                      "headcount_growth_Lower-middle-income",
                      "headcount_growth_Upper-middle-income",
                      "headcount_growth_High-income")
        ord_fc <- paste0(setdiff(ord_main, "year"), "_forecast")
        ord <- c(ord_main, ord_fc)
        keep <- intersect(ord, names(.))
        select(., all_of(keep))
      } %>%
      mutate(poverty_line = pl, .before = 1)
  }
  # run for all lines; return a named list like Stata’s separate sheets
  out_list <- lapply(poverty_lines, function(pl) one_line(df, pl))
  names(out_list) <- paste0("fig3_", poverty_lines)
  out_list
}

# ------- Builder for Figure 4 ---------

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

build_fig4 <- function(df,
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

# Figure 4b
.recode_regions_fig4b <- function(df) {
  df %>%
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
    )
}

build_fig4b <- function(df, digits = 2) {
  df1 <- .recode_regions_fig4b(df) %>%
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



# ------- Builder for Figure 5 ---------

# ---- Figure 5 helper: PG growth decomposition (global) ----
# Input  : country-year data with cols: country_code, year, mean, gini, pop, pg
# Output : tibble with the four components + a check column
# Notes  : Implements the same logic as the Stata code you shared

build_fig5 <- function(dta_fig_5,
                         z = 25,
                         keep_years = c(1990, 2000, 2010, 2019, 2024),
                         last_label_override = "2019–2024 (projected)") {
  
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
  last_iv <- paste(2019, 2024, sep = "\u2013")
  out$year[out$year == last_iv] <- last_label_override
  
  # Round to 2 decimals (World Bank table style); keep check_sum unrounded for QC
  out %>%
    mutate(across(-c(year, check_sum), ~ round(.x, 2)))
}


# ------- Builder for Figure 6 ---------

# ---- Figure 6: Share of countries by income and FCV group (latest survey)

# Inputs expected:
# - WDI_Gini: columns country, iso3c, year, SI.POV.GINI, ...
# - countrycodes_current: columns code, incgroup_current, fcv_current, ...
build_fig6 <- function(WDI_Gini, countrycodes_current) {
  
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
        dplyr::select(code, incgroup_current, fcv_current),
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
  
  # 5) collapse counts by fcv_current × ginigroup (FCV panel)
  fcv_panel <- merged %>%
    dplyr::count(fcv_current, ginigroup, name = "count") %>%
    dplyr::transmute(name = fcv_current, group = "FCV group FY24",
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


# ------- Get latest value ---------

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

# ------- Numeric transformation ---------

# helper to coerce numeric-like text (e.g., "1,234.56", "12.3%") to numeric
clean_to_numeric <- function(x) {
  x <- gsub(",", "", x)
  x <- sub("%$", "", x)
  suppressWarnings(as.numeric(x))
}

