#!/usr/bin/env Rscript

#' @title Sampling County for Crop 
#' @description Script to extract data from existing database tables and sampling county for crop maturity group
#' @author Luqi Jiao Emanuele
#' @date March 2026

# Set up environment
.libPaths("/data/rubelscratch/rubelogle/daycent_calibration/rlib")
library(RMySQL)
library(dplyr)
library(ggplot2)
library(purrr)
library(sf)
library(rlang)
library(readr)
library(tigris)
library(TeachingDemos)
library(bayesiancalibr)


# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
    stop("missing arguments")
}

chist_id <- args[1]
crop <- args[2]
crop_nass <- args[3]
state_level <- args[4]
crop_mg <- if (length(args) >= 5) args[5] else NA_character_

parse_logical_arg <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(trimws(as.character(x)))) {
    return(default)
  }
  val <- tolower(trimws(as.character(x)))
  val %in% c("true", "t", "1", "yes")
}

has_crop_mg <- function(x) {
  !is.null(x) && length(x) == 1 && !is.na(x) && nzchar(trimws(as.character(x)))
}

state_level <- parse_logical_arg(state_level)

cred_file <- path.expand("~/.dblogin")
if (!file.exists(cred_file)) {
  stop("Credential file not found: ", cred_file)
}

cred <- readLines(cred_file, warn = FALSE)
cred <- trimws(cred)
cred <- cred[nzchar(cred)]
if (length(cred) < 2) {
  stop("Credential file must contain at least two non-empty lines (user, password): ", cred_file)
}

user     <- cred[1]
password <- cred[2]


host       = "trillium.nrel.colostate.edu"
db_nri     = "nri2017"
db_nass    = "nass_data"
db_output  = "inv2024_output"
db_schfile = "inv2024_schlfiles"
db_calib   = "inv2024_calib"

crop_yield_conv <- switch(
  crop,
  sorg = "sorghum",
  soyb = "soybean",
  swpo = "sweet_potato",
  pnut = "peanuts",
  sugb = "sugarbeets",
  pota = "potato",
  barl = "barley",
  toba = "tobacco",
  sunf = "sunflower",
  drbe = "drybeans",
  onio = "onions",
  toma = "tomato",
  cott = "cotton",
  crop
)

dc_results_tbl = "ZZ_INV24_OUT_LAIRice_LAIMode_4_NRI_Yearly_Iter1_081726"
chist_tbl = "CHIST_INV2024_FINAL_1979_2023_vert_01142026"
sorg_matur_tbl = "EVI_FIPS_Sorg_Regions"
matur_tbl = "FIPS_lookup_crop_regions"
infost_tbl = "INV_lookup_point_site_info" # tier_2023
area_tbl  = "LandRep_xcls_xfact_INV2024" #xfact_adj_2024 
crop_schedule_tbl = "Inv24_CROP_intermediate_1"
nass_data_tbl = "NASS_County_Yld_CropYear"
yield_conversion_tbl = "YieldConversion"
aggre_level = "fips"
st_nass_crop <- c("toba", "sunf", "drbe", "onio", "peas", "toma", "cott")
if(crop %in% st_nass_crop) {
  nass_data_tbl = "NASS_State_Yld_CropYear"
  aggre_level = "st" 
}


# read in data
conn_write = dbConnect(MySQL(), host = host, user = user, password = password, dbname = db_schfile)

crop_intermediate <- dbGetQuery(
  conn_write,
  paste0("SELECT recordid2017, year, fips, id_chist, chist_landuse, winter_grain, daycent_crop FROM ", crop_schedule_tbl, " WHERE id_chist = ", chist_id, " ;")
)

# Close database connection
dbDisconnect(conn_write)

recordid2017 <- unique(crop_intermediate$recordid2017)
if (length(recordid2017) == 0) {
  stop("No recordid2017 values found for id_chist = ", chist_id)
}

# read in dc results
conn_write = dbConnect(MySQL(), host = host, user = user, password = password, dbname = db_output)

dc_results <- dbGetQuery(
  conn_write,
  paste0("SELECT recordid2017, year, cgrain FROM ", dc_results_tbl, " WHERE recordid2017 in (", paste0(recordid2017, collapse = ", "), ") ;")
)

# Close database connection
dbDisconnect(conn_write)

# read in data in nri2017
conn_write = dbConnect(MySQL(), host = host, user = user, password = password, dbname = db_nri)

# load in crop table
if(crop == "hay") {
  crop_tbl <- dbGetQuery(
    conn_write,
    paste0("SELECT recordid2017, year, id_chist, winter_grain FROM ", chist_tbl, " WHERE recordid2017 in (", paste0(recordid2017, collapse = ", "), ");") 
  )
} else {
  crop_tbl <- dbGetQuery(
    conn_write,
    paste0("SELECT recordid2017, year, id_chist, winter_grain FROM ", chist_tbl, " WHERE recordid2017 in (", paste0(recordid2017, collapse = ", "), ") ;") 
  )
}


if (crop != "sorg" && crop != "cott") {
  matur_grp <- NULL
  if (has_crop_mg(crop_mg)) {
    matur_grp <- dbGetQuery(
      conn_write,
      paste0("SELECT fips, ", crop_mg, " FROM ", matur_tbl, " ;")
    )
  }
}

infoSite <- dbGetQuery(
  conn_write,
  paste0("SELECT recordid2017, fips, fips_st AS st, state_abbr FROM ", infost_tbl, "  WHERE recordid2017 in (", paste0(recordid2017, collapse = ", "), ")  ;")
)

xfact_rid <- dbGetQuery(
  conn_write,
  paste0("SELECT recordid2017, year, xfact_adj_2024 AS xfact_adj FROM ", area_tbl, " WHERE recordid2017 in (", 
         paste0(recordid2017, collapse = ", "), ") ;")
)

# Close database connection
dbDisconnect(conn_write)

# NASS data
conn_write = dbConnect(MySQL(), host = host, user = user, password = password, dbname = db_nass)
if(crop == "wheat") {
  crop_nass_all = c("swht", "wwht")
  queue = paste0("SELECT * FROM ", nass_data_tbl, 
         " WHERE crop in ('", paste0(crop_nass_all, collapse = "', '"), "') ;")
} else if(state_level == TRUE) {
  queue = paste0("SELECT * FROM ", nass_data_tbl, 
         " WHERE crop = '", crop_nass, "' ;")
} else if(crop == "hay") {
  queue = paste0("SELECT * FROM ", nass_data_tbl, 
         " WHERE crop = '", crop_nass, "' ;")
} else {
  queue = paste0("SELECT * FROM ", nass_data_tbl, 
         " WHERE crop = '", crop_nass, "' ;")
}
nass_data <- dbGetQuery(
  conn_write,
  queue)

if (aggre_level == "st" && "state_fips" %in% names(nass_data) && !"st" %in% names(nass_data)) {
  nass_data$st <- nass_data$state_fips
}

yield_conversion <- dbGetQuery(conn_write,
                                paste0("SELECT ConvXFact FROM ", yield_conversion_tbl,
                                    " WHERE crop = '", crop_yield_conv, "' AND ConvType = 'Yld2Cgrn' ;"))
yield_conversion <- yield_conversion$ConvXFact

# Close database connection
dbDisconnect(conn_write)


#calib
if (crop == "sorg") {
  conn_write = dbConnect(MySQL(), host = host, user = user, password = password, dbname = db_calib)
  matur_grp <- NULL
  if (has_crop_mg(crop_mg)) {
    matur_grp <- dbGetQuery(
      conn_write,
      paste0("SELECT fips, ", crop_mg, " FROM ", sorg_matur_tbl, " ;")
    )
  }
  dbDisconnect(conn_write)
}

#----------------------------------------------------------------------------------------------------------------------------------------
# NASS yield conversion
if (crop == "wheat") {
  df_obser <- nass_data %>%
    filter(crop %in% c("swht", "wwht")) %>%
    mutate(meas_cgrain_gm2 = tot_yld * yield_conversion) %>%
    group_by(.data[[aggre_level]], year) %>%
    summarise(meas_cgrain_gm2 = mean(meas_cgrain_gm2, na.rm = TRUE), .groups = "drop")
} else {
  df_obser <- nass_data %>%
    filter(crop == crop_nass) %>%
    mutate(meas_cgrain_gm2 = tot_yld * yield_conversion) %>%
    reframe(
      !!aggre_level := .data[[aggre_level]],
      year = year,
      meas_cgrain_gm2
    )
}
    
 
#----------------------------------------------------------------------------------------------------------------------------------------
# combine the tables
combine <- merge(crop_intermediate, crop_tbl, by = c("recordid2017", "year", "id_chist", "winter_grain"))
combine <- merge(combine, dc_results, by = c("recordid2017", "year"))
combine <- merge(combine, infoSite[, c("recordid2017", "fips", "st"), drop = FALSE], by = "recordid2017")
combine_all <- merge(combine, xfact_rid, by = c("recordid2017", "year"))


#----------------------------------------------------------------------------------------------------------------------------------------
# aggregate
site_annual_data <- combine_all %>%
  mutate(
    site_name = paste0(recordid2017, "_", year),
    SampleID = 1L
  ) %>%
  select(site_name, year, SampleID, cgrain)

aggregation_metadata <- combine_all %>%
  distinct(recordid2017, year, .keep_all = TRUE) %>%
  mutate(
    site_name = paste0(recordid2017, "_", year),
    aggregation_level = .data[[aggre_level]],
    aggregation_weight = xfact_adj
  ) %>%
  select(site_name, aggregation_level, aggregation_weight)

weighted_cgrain <- aggregate_sites_to_county(
  site_annual_data = site_annual_data,
  aggregation_metadata = aggregation_metadata,
  variable_name = "cgrain"
)

aggre_obser <- df_obser %>%
  distinct(.data[[aggre_level]], year, meas_cgrain_gm2) %>%
  rename(aggregation_level = !!sym(aggre_level))

weighted_compare <- weighted_cgrain %>%
  rename(mod_cgrain = weighted_value) %>%
  left_join(aggre_obser, by = c("aggregation_level", "year")) %>%
  rename(!!sym(aggre_level) := aggregation_level) %>%
  select(all_of(aggre_level), year, mod_cgrain, meas_cgrain_gm2, n_sites, total_weight)

if (!is.null(matur_grp)) {
  final_tbl <- merge(weighted_compare, matur_grp, by = aggre_level)
} else {
  final_tbl <- weighted_compare
}

output_dir <- file.path("results", "crop_validation")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_file <- file.path(
  output_dir,
  paste0(crop, "_chist", chist_id, "_", aggre_level, "_agg.csv")
)
write_csv(final_tbl, output_file)

cat("Saved validation aggregation table:", normalizePath(output_file, winslash = "/"), "\n")
cat("Rows:", nrow(final_tbl), "\n")
