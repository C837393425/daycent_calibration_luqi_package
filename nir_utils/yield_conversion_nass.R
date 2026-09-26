#!/usr/bin/env Rscript

#' @title NASS yield unit conversion to DayCent grain carbon (g C/m2)
#' @description Compute Yld2Cgrn and Cgrn2Yld factors for crops with non-bu/ac NASS units.
#'   Formulas per S. Spencer (7 May 2026).
#' @details
#'   CF: carbon fraction (g C / g dry matter)
#'   MC: moisture content (fraction moisture, as-harvested)
#'   CW: crop weight per native yield unit (lb per bu, cwt, ton, or lb as appropriate)
#'
#'   Constants:
#'     g/lb  = 453.59237
#'     m2/ac = 4046.8564224
#'     g C/m2 -> lb C/ac: 8.92179 (= m2/ac / g/lb)
#'
#'   Cgrn2Yld (g C/m2 -> native yield unit, e.g. bu/ac):
#'     yld = cgrain_gm2 * (1/CF) * (1/(1-MC)) * 8.92179 / CW_lb
#'
#'   Yld2Cgrn (native yield unit -> g C/m2); inverse of Cgrn2Yld:
#'     cgrain_gm2 = yld * CW_lb * (1-MC) * CF * 453.59237 / 4046.8564224
#'
#'   Corn (bu/ac) example: CF=0.435, MC=0.155, CW=56 lb/bu
#'     Yld2Cgrn = 2.30718,  Cgrn2Yld = 0.433429
#'
#' @author Luqi Jiao Emanuele (conversion formulas: S. Spencer)

.libPaths("/data/rubelscratch/rubelogle/daycent_calibration/rlib")
library(RMySQL)
library(dplyr)
library(readr)

G_PER_LB  <- 453.59237
M2_PER_AC <- 4046.8564224
GC_M2_TO_LB_C_AC <- M2_PER_AC / G_PER_LB  # 8.92179: g C/m2 -> lb C/ac

crop_nass    <- c("cotton", "peanuts", "sugarbeets", "potato", "rice", "sweet_potato", "lentil", "hay")
crop_nass_st <- c("tobacco", "sunflower", "drybeans", "onions", "peas", "tomato")
all_crops    <- c(crop_nass, crop_nass_st)

carb_wat_path <- "/data/rubelscratch/rubelogle/daycent_calibration/nir_utils/prep_data/carbon_frac_water_cont.csv"
out_csv_path  <- "/data/rubelscratch/rubelogle/daycent_calibration/nir_utils/prep_data/yield_conversion_factors.csv"

carb_wat <- read.csv(carb_wat_path, stringsAsFactors = FALSE) %>%
  filter(crop %in% all_crops) %>%
  mutate(
    bushel_lb = ifelse(is.na(bushel_lb) | bushel_lb == "", NA_real_, as.numeric(bushel_lb))
  )

# Expected NASS units per crop (used when DB is unavailable).
default_yld_unit <- carb_wat %>% select(crop, yld_unit)

#' Map NASS DB unit strings to canonical form used in conversion formulas.
normalize_yld_unit <- function(yld_unit) {
  u <- tolower(trimws(gsub("\\s+", "", yld_unit)))
  dplyr::case_when(
    u %in% c("bu/ac", "bushel/acre", "bushels/acre", "bushel/ac") ~ "bu/ac",
    u %in% c("lb/ac", "lbs/ac", "pounds/acre", "pound/acre") ~ "lb/ac",
    u %in% c("cwt/ac", "cwt/acre", "hundredweight/acre") ~ "cwt/ac",
    u %in% c("tons/ac", "ton/ac", "tons/acre", "ton/acre") ~ "tons/ac",
    TRUE ~ yld_unit
  )
}

#' YieldConversion.unit label (native unit for Cgrn2Yld; Yld2Cgrn is always g C/m2).
native_conv_unit <- function(yld_unit) {
  dplyr::case_when(
    yld_unit == "bu/ac" ~ "bushel/acre",
    yld_unit == "lb/ac" ~ "lb/ac",
    yld_unit == "cwt/ac" ~ "cwt/ac",
    yld_unit == "tons/ac" ~ "tons/ac",
    TRUE ~ yld_unit
  )
}

#' Format fraction as percent label (e.g. 0.435 -> "43.5% C").
format_pct_label <- function(frac, suffix) {
  pct <- frac * 100
  if (abs(pct - round(pct)) < 0.05) {
    return(paste0(sprintf("%.0f", pct), "%", suffix))
  }
  paste0(sprintf("%.1f", pct), "%", suffix)
}

#' Human-readable parameter note (Spencer / YieldConversion style).
format_conv_notes <- function(carbon_frac, moisture, yld_unit, crop_weight_lb) {
  parts <- c(
    format_pct_label(carbon_frac, " C"),
    format_pct_label(moisture, " M")
  )
  if (yld_unit %in% c("bu/ac", "bushel/acre")) {
    bu_lb <- crop_weight_lb
    bu_note <- paste0(sprintf("%.0f", bu_lb), " lb/bu")
    if (bu_lb %in% c(56, 60)) {
      bu_note <- paste0(bu_note, sprintf(" (%.1f kg/bu)", bu_lb / 2.20462))
    }
    parts <- c(parts, bu_note)
  }
  paste(parts, collapse = ", ")
}

#' Pounds of as-harvested product per one native NASS yield unit.
unit_to_crop_weight_lb <- function(yld_unit, bushel_lb = NA_real_) {
  if (yld_unit %in% c("bu/ac", "bushel/acre")) {
    if (is.na(bushel_lb)) {
      stop("bushel_lb is required when yld_unit is bu/ac or bushel/acre", call. = FALSE)
    }
    return(bushel_lb)
  }
  switch(
    yld_unit,
    "lb/ac"   = 1,
    "cwt/ac"  = 100,
    "tons/ac" = 2000,
    stop("Unsupported yld_unit: ", yld_unit, call. = FALSE)
  )
}

#' Yld2Cgrn: multiply NASS yield (native unit/ac) to get g C/m2.
yld2cgrn_factor <- function(carbon_frac, moisture, crop_weight_lb) {
  crop_weight_lb * (1 - moisture) * carbon_frac * G_PER_LB / M2_PER_AC
}

#' Cgrn2Yld: multiply DayCent cgrain (g C/m2) to get NASS native yield unit/ac.
cgrn2yld_factor <- function(carbon_frac, moisture, crop_weight_lb) {
  GC_M2_TO_LB_C_AC / (carbon_frac * (1 - moisture) * crop_weight_lb)
}

compute_yield_conversion <- function(crop, yld_unit, carbon_frac, moisture,
                                     bushel_lb = NA_real_) {
  if (yld_unit %in% c("bu/ac", "bushel/acre") && is.na(bushel_lb)) {
    stop(
      "bushel_lb is required for crop '", crop, "' (yld_unit = ", yld_unit, ")",
      call. = FALSE
    )
  }
  cw_lb <- unit_to_crop_weight_lb(yld_unit, bushel_lb)
  y2c <- yld2cgrn_factor(carbon_frac, moisture, cw_lb)
  c2y <- cgrn2yld_factor(carbon_frac, moisture, cw_lb)
  dplyr::bind_rows(
    tibble::tibble(
      crop = crop, yld_unit = yld_unit, ConvType = "Yld2Cgrn",
      conv_unit = "g C/m2",
      ConvXFact = y2c, carbon_frac = carbon_frac, moisture = moisture,
      crop_weight_lb = cw_lb
    ),
    tibble::tibble(
      crop = crop, yld_unit = yld_unit, ConvType = "Cgrn2Yld",
      conv_unit = native_conv_unit(yld_unit),
      ConvXFact = c2y, carbon_frac = carbon_frac, moisture = moisture,
      crop_weight_lb = cw_lb
    )
  )
}

#----------------------------------------------------------------------------------------------------------------------------
# Optional DB: distinct NASS yield units + existing YieldConversion rows
#----------------------------------------------------------------------------------------------------------------------------

nass_units <- default_yld_unit
existing_conversion <- NULL
conn_write <- NULL

cred_file <- "~/.dblogin"
if (file.exists(cred_file)) {
  cred <- readLines(cred_file, warn = FALSE)
  tryCatch({
    conn_write <- dbConnect(
      MySQL(),
      host = "trillium.nrel.colostate.edu",
      user = cred[1],
      password = cred[2],
      dbname = "nass_data"
    )
    nass_data_tbl <- "NASS_County_Yld_CropYear"
    nass_data_state_tbl <- "NASS_State_Yld_CropYear"
    yield_conversion_tbl <- "YieldConversion"

    crop_sql <- paste0("'", paste(all_crops, collapse = "', '"), "'")
    nass_data_county <- dbGetQuery(conn_write, paste0(
      "SELECT DISTINCT crop, yld_unit FROM ", nass_data_tbl,
      " WHERE crop IN (", crop_sql, ");"
    ))
    nass_data_state <- dbGetQuery(conn_write, paste0(
      "SELECT DISTINCT crop, yld_unit FROM ", nass_data_state_tbl,
      " WHERE crop IN (", crop_sql, ");"
    ))
    nass_units <- bind_rows(nass_data_county, nass_data_state) %>%
      mutate(yld_unit = normalize_yld_unit(yld_unit)) %>%
      distinct(crop, yld_unit)

    existing_conversion <- dbGetQuery(conn_write, paste0(
      "SELECT crop, ConvType, ConvXFact FROM ", yield_conversion_tbl,
      " WHERE crop IN (", crop_sql, ");"
    ))
  }, error = function(e) {
    message("Database unavailable; using default_yld_unit. ", conditionMessage(e))
    if (!is.null(conn_write)) dbDisconnect(conn_write)
    conn_write <<- NULL
  })
}

if (is.null(nass_units) || nrow(nass_units) == 0) {
  nass_units <- default_yld_unit
}

# Union DB units with expected units from carb_wat; keep every distinct crop + unit pair.
nass_units <- bind_rows(
  nass_units %>% mutate(yld_unit = normalize_yld_unit(yld_unit)),
  default_yld_unit
) %>%
  distinct(crop, yld_unit) %>%
  filter(crop %in% all_crops)

if (exists("nass_data_county") && exists("nass_data_state")) {
  multi_unit <- bind_rows(nass_data_county, nass_data_state) %>%
    filter(crop %in% all_crops) %>%
    group_by(crop) %>%
    summarise(
      n_units = n_distinct(yld_unit),
      units = paste(unique(yld_unit), collapse = ", "),
      .groups = "drop"
    ) %>%
    filter(n_units > 1)
} else {
  multi_unit <- tibble::tibble()
}
if (nrow(multi_unit) > 0) {
  warning(
    "Multiple yld_unit values for: ",
    paste(multi_unit$crop, " (", multi_unit$units, ")", sep = "", collapse = "; ")
  )
}

#----------------------------------------------------------------------------------------------------------------------------
# Build conversion table (YieldConversion format: crop, ConvType, unit, ConvXFact)
#----------------------------------------------------------------------------------------------------------------------------

conversion_long <- nass_units %>%
  inner_join(
    carb_wat %>% select(crop, carbon_frac, moisture, bushel_lb),
    by = "crop"
  ) %>%
  rowwise() %>%
  group_map(~ {
    compute_yield_conversion(
      crop = .x$crop,
      yld_unit = .x$yld_unit,
      carbon_frac = .x$carbon_frac,
      moisture = .x$moisture,
      bushel_lb = .x$bushel_lb
    )
  }) %>%
  bind_rows()

# Same layout as YieldConversion: crop | ConvType | unit | ConvXFact
# (all Cgrn2Yld rows, then all Yld2Cgrn rows; sorted by crop within each block).
conversion_table <- conversion_long %>%
  mutate(ConvXFact = round(ConvXFact, 4)) %>%
  transmute(crop, ConvType, unit = conv_unit, ConvXFact) %>%
  arrange(factor(ConvType, levels = c("Cgrn2Yld", "Yld2Cgrn")), crop)

crop_notes <- conversion_long %>%
  distinct(crop, yld_unit, carbon_frac, moisture, crop_weight_lb) %>%
  mutate(
    Notes = mapply(
      format_conv_notes,
      carbon_frac, moisture, yld_unit, crop_weight_lb,
      USE.NAMES = FALSE
    )
  ) %>%
  select(crop, Notes)

conversion_wide_complete <- conversion_table %>%
  left_join(crop_notes, by = "crop") %>%
  select(crop, ConvType, FinalUnits = unit, ConvXFact, Notes)



write.csv(conversion_wide_complete, out_csv_path, row.names = FALSE)
message("Wrote ", nrow(conversion_wide_complete), " rows to ", out_csv_path)
print(conversion_wide_complete, n = Inf)

if (!is.null(existing_conversion) && nrow(existing_conversion) > 0) {
  cmp <- conversion_long %>%
    filter(ConvType == "Yld2Cgrn") %>%
    inner_join(
      existing_conversion %>% filter(ConvType == "Yld2Cgrn"),
      by = c("crop", "ConvType"),
      suffix = c("_calc", "_db")
    ) %>%
    mutate(rel_diff = abs(ConvXFact_calc - ConvXFact_db) / ConvXFact_db)
  if (nrow(cmp) > 0) {
    message("Comparison to YieldConversion.Yld2Cgrn in database:")
    print(cmp %>% select(crop, ConvXFact_calc, ConvXFact_db, rel_diff))
  }
}

#----------------------------------------------------------------------------------------------------------------------------
# Optional: write Yld2Cgrn / Cgrn2Yld rows to YieldConversion (set WRITE_DB=1)
#----------------------------------------------------------------------------------------------------------------------------

if (!is.null(conn_write) && identical(Sys.getenv("WRITE_DB"), "1")) {
  yield_conversion_tbl <- "YieldConversion"
  conversion_wide_complete <- read.csv("/data/rubelscratch/rubelogle/daycent_calibration/nir_utils/prep_data/yield_conversion_factors.csv", stringsAsFactors = FALSE)
  for (i in seq_len(nrow(conversion_wide_complete))) {
    row <- conversion_wide_complete[i, ]
    dbExecute(conn_write, paste0(
      "INSERT INTO ", yield_conversion_tbl,
      " (crop, ConvType, FinalUnits, ConvXFact, Notes) VALUES ('",
      row$crop, "', '", row$ConvType, "', '", row$FinalUnits, "', ", row$ConvXFact, ", '", row$Notes, "');"
    ))
  }
  message("Updated ", nrow(conversion_wide_complete), " rows in ", yield_conversion_tbl)
}

yield_conversion_path <- "/data/rubelscratch/rubelogle/daycent_calibration/nir_utils/prep_data/crop_yield.csv"
yield_conversion <- read.csv(yield_conversion_path, stringsAsFactors = FALSE)
for (i in seq_len(nrow(yield_conversion))) {
  row <- yield_conversion[i, ] 
  dbExecute(conn_write, paste0(
    "INSERT INTO ", yield_conversion_tbl,
    " (Crop, ConvType, FinalUnits, ConvXFact, Notes) VALUES ('",
    row$Crop, "', '", row$ConvType, "', '", row$FinalUnits, "', ", row$ConvXFact, ", '", row$Notes, "');"
  ))
}
message("Updated ", nrow(yield_conversion), " rows in ", yield_conversion_tbl)

if (!is.null(conn_write)) {
  dbDisconnect(conn_write)
}
