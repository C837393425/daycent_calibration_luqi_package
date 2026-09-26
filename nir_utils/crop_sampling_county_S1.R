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

# change the set up
{
  Input.arg  = commandArgs(trailingOnly = TRUE)
  id_chist     = as.numeric(Input.arg[1])  

  crop_mg <- NULL
  state_level <- FALSE
  crop_type <- "crop"
  if (id_chist == 25) {                   ## COUNTY NASS DATA 
    crop = "corn"
    crop_nass = "corn_grain" 
    crop_mg = "corn_maturity_region"
  } else if (id_chist == 13) {
    crop = "soyb"
    crop_nass = "soybean" 
    crop_mg = "soyb_maturity_region"
  } else if (id_chist == 27) {
    crop = "sorg"
    crop_nass = "sorghum_grain" 
    crop_mg = "sorg_region"
  } else if (id_chist == 111) {             
    crop = "wheat"
    crop_nass = c("swht", "wwht")
    crop_mg = "wheat_type"
  } else if (id_chist == 15) {
    crop = "pnut"
    crop_nass = "peanuts" 
    crop_mg = NULL
  } else if (id_chist == 17) {
    crop = "sugb"
    crop_nass = "sugarbeets" 
    crop_mg = NULL
  } else if (id_chist == 114) {
    crop = "barl"
    crop_nass = "barley" 
    crop_mg = NULL
  } else if (id_chist == 112) {
    crop = "oats"
    crop_nass = "oats" 
    crop_mg = NULL 
  } else if (id_chist == 18) {
    crop = "pota"
    crop_nass = "potato" 
    crop_mg = NULL
  } else if (id_chist == 113) {
    crop = "rice"
    crop_nass = "rice" 
    crop_mg = "rice_type"
  } else if (id_chist == 990046) {
    crop = "swpo"
    crop_nass = "sweet_potato" 
    crop_mg = NULL
  } else if (id_chist == 990052) {
    crop = "lent"
    crop_nass = "lentil" 
    crop_mg = NULL
  } else if (id_chist == 14) {         ## STATE NASS DATA 
    crop = "cott"
    crop_nass = "cotton" 
    crop_mg = "cott_region"
    state_level = TRUE
  } else if (id_chist == 16) {           
    crop = "toba"
    crop_nass = "tobacco" 
    crop_mg = NULL
    state_level = TRUE
  } else if (id_chist == 21) {             
    crop = "sunf"
    crop_nass = "sunflower" 
    crop_mg = NULL
    state_level = TRUE
  } else if (id_chist == 990042) {             
    crop = "drbe"
    crop_nass = "drybeans" 
    crop_mg = NULL
    state_level = TRUE
  } else if (id_chist == 990049) {             
    crop = "onio"
    crop_nass = "onions" 
    crop_mg = NULL
    state_level = TRUE
  } else if (id_chist == 990053) {             
    crop = "peas"
    crop_nass = "peas" 
    crop_mg = NULL
    state_level = TRUE
  } else if (id_chist == 990054) {             
    crop = "toma"
    crop_nass = "tomato" 
    crop_mg = NULL
    state_level = TRUE
  }         

  cat("Crop: ", id_chist, "\n")
  folder_address = "/data/rubelscratch/rubelogle/daycent_calibration/data/"
}


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
db_calib   = "inv2024_calib"
db_schfile = "inv2024_schlfiles"

chist_tbl = "CHIST_INV2024_FINAL_1979_2023_vert_01142026"
sorg_matur_tbl = "EVI_FIPS_Sorg_Regions"
matur_tbl = "FIPS_lookup_maturity_groups"
infost_tbl = "INV_lookup_point_site_info" # tier_2023
area_tbl  = "LandRep_xcls_xfact_INV2024" #xfact_adj_2024
climate_tbl = "INV_IPCC_climate_soil_assignment"
crop_schedule_tbl = "Inv24_CROP_intermediate_1"
nass_data_tbl = "NASS_County_Yld_CropYear"
aggre_level = "fips"
st_nass_crop <- c("toba", "sunf", "drbe", "onio", "peas", "toma", "cott")
if(crop %in% st_nass_crop) {
  nass_data_tbl = "NASS_State_Yld_CropYear"
  aggre_level = "st" 
}

conn_write = dbConnect(MySQL(), host = host, user = user, password = password, dbname = db_nri)

# load in crop table
if(crop_type == "hay") {
  crop_tbl <- dbGetQuery(
    conn_write,
    paste0("SELECT recordid2017, year, id_chist, winter_grain FROM ", chist_tbl, " Where chist_landuse LIKE 'Hay/Legume %' AND year BETWEEN 2011 AND 2023 ;") 
  )
} else {
  crop_tbl <- dbGetQuery(
    conn_write,
    paste0("SELECT recordid2017, year, id_chist, winter_grain FROM ", chist_tbl, " Where id_chist = ", id_chist, " AND year BETWEEN 2011 AND 2023 ;") 
  )
}


if(crop != "sorg" & crop != "cott") { 
  matur_grp <- NULL
  if (!is.null(crop_mg)) {
    matur_grp <- dbGetQuery(
      conn_write,
      paste0("SELECT fips, ", crop_mg, " FROM ", matur_tbl, " ;")
    )
  }
}

infoSite <- dbGetQuery(
  conn_write,
  paste0("SELECT recordid2017, fips, fips_st AS st, state_abbr, MLRA2022 FROM ", infost_tbl, " WHERE tier_2023 = 3;")
)

unique_rid = unique(crop_tbl$recordid2017)

xfact_rid <- dbGetQuery(
  conn_write,
  paste0("SELECT recordid2017, year, xfact_adj_2024 AS xfact_adj FROM ", area_tbl, " WHERE recordid2017 in (", 
         paste0(unique_rid, collapse = ", "), ") AND year BETWEEN 2011 AND 2023 ;")
)

climate_rid <- dbGetQuery(
  conn_write,
  paste0("SELECT recordid2017, climate_zone, climate_desc, soil_code, soil_class FROM ", climate_tbl, " WHERE recordid2017 in (", 
         paste0(unique_rid, collapse = ", "), ") ;")
)
# Close database connection
dbDisconnect(conn_write)

# NASS data
conn_write = dbConnect(MySQL(), host = host, user = user, password = password, dbname = db_nass)
if(crop == "wheat") {
  crop_nass_all = c("swht", "wwht")
  queue = paste0("SELECT distinct ", aggre_level, ", crop FROM ", nass_data_tbl, 
         " WHERE year BETWEEN 2011 AND 2023",
         " AND crop in ('", paste0(crop_nass_all, collapse = "', '"), "') ;")
} else if(state_level == TRUE) {
  queue = paste0("SELECT distinct state_fips AS st FROM ", nass_data_tbl, 
         " WHERE year BETWEEN 2011 AND 2023",
         " AND crop = '", crop_nass, "' ;")
} else if(crop_type == "hay") {
  queue = paste0("SELECT ", aggre_level, ",year, irr_yld, dry_yld FROM ", nass_data_tbl, 
         " WHERE year BETWEEN 2011 AND 2023",
         " AND crop = '", crop_nass, "' ;")
} else {
  queue = paste0("SELECT distinct ", aggre_level, " AS fips FROM ", nass_data_tbl, 
         " WHERE year BETWEEN 2011 AND 2023",
         " AND crop = '", crop_nass, "' ;")
}
nass_data <- dbGetQuery(
  conn_write,
  queue)
# Close database connection
dbDisconnect(conn_write)


#calib
if(crop == "sorg") { 
  conn_write = dbConnect(MySQL(), host = host, user = user, password = password, dbname = db_calib)
  matur_grp <- NULL
  if (!is.null(crop_mg)) {
    matur_grp <- dbGetQuery(
      conn_write,
      paste0("SELECT fips, ", crop_mg, " FROM ", sorg_matur_tbl, " ;")
    )
  }
  # Close database connection
  dbDisconnect(conn_write)
}


#----------------------------------------------------------------------------------------------------------------------------------------
# combine the tables
if(crop != "corn" && crop != "soyb" && crop != "sorg") {
  combine <- merge(infoSite, nass_data, by = aggre_level)
  combine <- merge(combine, xfact_rid, by = "recordid2017")
  combine <- merge(combine, climate_rid, by = "recordid2017")
  combine_all <- merge(combine, crop_tbl, by = c("recordid2017", "year") )
} else {
  combine <- merge(infoSite, matur_grp, by = aggre_level)
  combine <- merge(combine, nass_data, by = aggre_level)
  combine <- merge(combine, xfact_rid, by = "recordid2017")
  combine <- merge(combine, climate_rid, by = "recordid2017")
  combine_all <- merge(combine, crop_tbl, by = c("recordid2017", "year") )
}


# Rice type :
## 1: RICA
## 2: RICL
## 3: RICM/RICR
if(crop == "rice") {
  rice_type_perct <- read.csv("/data/rubelscratch/rubelogle/daycent_calibration/nir_utils/prep_data/rice_type_perct.csv")
  combine_all <- combine_all %>%
    left_join(rice_type_perct, by = c("fips", "state_abbr")) %>%
    mutate(
    rice_type = case_when(
      ratoon_frac == 0 ~ "2",
      ratoon_frac > 0.95 ~ "3",
      st == 6 ~ "1",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(rice_type))
}

# Short summary of assignment logic:
# - W3SR (soft red winter wheat) is assigned to all W3 records in states that are
#   NOT in HRW (W3HR) states and NOT in no-LAI states.
# - W3SR is also explicitly assigned in MLRA exceptions for:
#   KS (MLRA 112), OK (MLRA 112), and TX (MLRA 86, 133B).
# - W3HR (hard red winter wheat) is assigned for HRW (W3HR) states:
#   TX, OK, KS, NE, SD, WY, CO, NM.
# - Within HRW (W3HR) states, these are excluded from W3HR assignment:
#   SD MLRAs 53B, 54, 55B, 56, 58D, 102A (no-LAI exceptions),
#   and SRW exception MLRAs in KS (112), OK (112), TX (86, 133B)

# State-by-state assignment details:
# - W3HR states (default hard red assignment): TX, OK, KS, NE, SD, WY, CO, NM
# - W3SR states from the broad rule: all states except
#   (a) HRW (W3HR) states TX, OK, KS, NE, SD, WY, CO, NM and
#   (b) no-LAI states WA, OR, ID, CA, NV, AZ, UT, ND, MT
# - W3SR MLRA exceptions inside HRW (W3HR) states:
#   KS -> MLRA 112
#   OK -> MLRA 112
#   TX -> MLRAs 86, 133B
# - SD no-LAI exception MLRAs (53B, 54, 55B, 56, 58D, 102A) are excluded from W3HR
#   by this code and remain base W3.

if (crop == "wheat") {
  hrw_states <- c("TX", "OK", "KS", "NE", "SD", "WY", "CO", "NM")

  no_lai_states <- c("WA", "OR", "ID", "CA", "NV", "AZ", "UT", "ND", "MT")
  sd_no_lai_mlra <- c("53B", "54", "55B", "56", "58D", "102A")
  ks_ok_srw_mlra <- c("112")
  tx_srw_mlra <- c("86", "133B")

  combine_all <- combine_all %>%
    mutate(
      mlra_chr = as.character(MLRA2022),
      wheat_type = dplyr::case_when(
        winter_grain == 0 & crop == "swht" ~ "SW3",
        winter_grain == 1 & !(state_abbr %in% hrw_states) & !(state_abbr %in% no_lai_states) & crop == "wwht" ~ "W3SR",
        winter_grain == 1 & state_abbr == "KS" & mlra_chr %in% ks_ok_srw_mlra & crop == "wwht" ~ "W3SR",
        winter_grain == 1 & state_abbr == "OK" & mlra_chr %in% ks_ok_srw_mlra & crop == "wwht" ~ "W3SR",
        winter_grain == 1 & state_abbr == "TX" & mlra_chr %in% tx_srw_mlra & crop == "wwht" ~ "W3SR",
        winter_grain == 1 & state_abbr %in% hrw_states & crop == "wwht" &
          !(state_abbr == "SD" & mlra_chr %in% sd_no_lai_mlra) &
          !(state_abbr == "KS" & mlra_chr %in% ks_ok_srw_mlra) &
          !(state_abbr == "OK" & mlra_chr %in% ks_ok_srw_mlra) &
          !(state_abbr == "TX" & mlra_chr %in% tx_srw_mlra) ~ "W3HR",
        winter_grain == 1 & crop == "wwht" ~ "W3",
        TRUE ~ NA_character_
      )
    ) %>%
    select(-mlra_chr)

}

# ---------------------------------------------------------------------------------------------------------------------------------------
## Define Cotton Region
if(crop == "cott") {
  st_region1 <- c(6, 4, 35)
  st_region2 <- c(20, 40, 48)
  st_region3 <- c(29, 5, 22, 47, 28)
  st_region4 <- c(51, 37, 45, 13, 1, 12)

  combine_all <- combine_all %>%
    mutate(
      cott_region = case_when(
        st %in% st_region1 ~ "1",
        st %in% st_region2 ~ "2",
        st %in% st_region3 ~ "3",
        st %in% st_region4 ~ "4",
        TRUE ~ NA_character_
      )
    )
}

mean_xfact_rid <- combine_all %>%
  group_by(recordid2017) %>%
  mutate(mean_xfact = mean(xfact_adj)) %>%
  ungroup() %>%
  reframe(recordid2017, mean_xfact) %>%
  distinct()


rm(crop_tbl, matur_grp, infoSite, nass_data, combine)

# Set up the folder in data for crop 
if(crop == "corn" || crop == "soyb" || crop == "sorg" || crop == "rice" || crop == "cott"){
  unique_maturity <- unique(combine_all[[crop_mg]][combine_all[[crop_mg]] >= 0])
  
  for (maturity in unique_maturity) {
    folder_name <- paste0(folder_address,"crop_yield_", crop, "_m", maturity)
    if (!dir.exists(folder_name)) dir.create(folder_name)
  }
} else if(crop == "wheat") {
  unique_wheat_type <- unique(combine_all$wheat_type[combine_all$wheat_type != "NA"])
  for (wheat_type in unique_wheat_type) {
    folder_name <- paste0(folder_address,"crop_yield_wheat_", wheat_type)
    if (!dir.exists(folder_name)) dir.create(folder_name)
  }
}

# set up the rule
## year from 2010 to 2023
## Corn, Cotton:
### count year in each rid, set > 5
### count rid in each fips, set > 30
## Soybean, Peanuts, Sugarbeets, Barley:
### count year in each rid, set > 1
### count rid in each fips, set > 5 

if(crop == "corn" || crop == "cott"){
  nri_yr = 5
  nri_rid = 30
} else if(crop == "lent") {
  nri_yr = 1
  nri_rid = 2
} else if(crop == "swpo") {
  nri_yr = 1
  nri_rid = 1
} else if(crop == "sorg" || crop == "toma") {
  nri_yr = 3
  nri_rid = 5
} else {
  nri_yr = 1
  nri_rid = 5
}

if(crop_mg == "wheat") {
  df_crop <- combine_all %>%
    filter((!is.na(wheat_type) & wheat_type != "NA")) %>%
    #group_by(fips) %>%
    #filter( n_distinct(wheat_type) == 1) %>%
    #ungroup() %>%
    group_by(recordid2017) %>%
    mutate(n_rid_yr = n()) %>%     # number of years per site
    ungroup() %>%
    group_by(fips) %>%
    mutate(n_rid = n_distinct(recordid2017)) %>%  # number of sites per county
    ungroup() %>% 
    filter(n_rid_yr > nri_yr, n_rid > nri_rid)
} else if(state_level == TRUE) {
  df_crop <- combine_all %>% 
    group_by(recordid2017) %>%
    mutate(n_rid_yr = n()) %>%     # number of years per site
    ungroup() %>%
    group_by(fips) %>%
    mutate(n_rid = n_distinct(recordid2017)) %>%  # number of sites per county
    ungroup()
} else {
  df_crop <- combine_all %>% 
    group_by(recordid2017) %>%
    mutate(n_rid_yr = n()) %>%     # number of years per site
    ungroup() %>%
    group_by(fips) %>%
    mutate(n_rid = n_distinct(recordid2017)) %>%  # number of sites per county
    ungroup() %>% 
    filter(n_rid_yr > nri_yr, n_rid > nri_rid)
}

dim(df_crop)


if(crop != "corn" && crop != "soyb" && crop != "wheat" && crop != "sorg" && crop != "rice"){
  eligible_counties <- df_crop %>%
    mutate(n_fips = n_distinct(fips)) %>%  
    distinct(fips, st, n_rid, n_fips)
} else if(crop == "wheat") {
  eligible_counties <- df_crop %>%
    group_by(wheat_type) %>%
    mutate(n_fips = n_distinct(fips)) %>%  
    ungroup() %>% 
    distinct(wheat_type, fips, st, n_rid, n_fips)
} else {
  eligible_counties <- df_crop %>%
    group_by(!!sym(crop_mg)) %>%
    mutate(n_fips = n_distinct(fips)) %>%  # number of county per maturity group
    ungroup() %>% 
    distinct(fips, st, n_rid, n_fips, !!sym(crop_mg))
}


#----------------------------------------------------------------------------------------------------------------------------------------
if(state_level == FALSE) {
  if(crop == "corn" || crop == "soyb" || crop == "sorg" || crop == "rice" || crop == "cott"){
    cat("Exploring: ", crop_mg, "\n")
    
    seed <- char2seed(crop_mg, set = TRUE)
    
    eligible_counties <- df_crop %>%
      filter(!!sym(crop_mg) >= 0) %>%        # drop negative values in the crop_mg variable
      group_by(!!sym(crop_mg)) %>%
      mutate(n_fips = n_distinct(fips)) %>%  # number of counties per maturity group
      ungroup() %>% 
      distinct(fips, st, n_rid, n_fips, !!sym(crop_mg))
    
    # Explore
    eligible_counties %>%
      group_by(!!sym(crop_mg)) %>%
      summarise(n_fips    = n_distinct(fips),
                min_n_rid = min(n_rid, na.rm = TRUE),
                max_n_rid = max(n_rid, na.rm = TRUE),
                .groups = "drop") %>%
      print()
    
    # Total distinct fips
    total_fips <- eligible_counties %>%
      summarise(total = n_distinct(fips)) %>%
      pull(total)
    cat("Total unique fips: ", total_fips, "\n")
    
  } else if(crop == "wheat") {
    cat("Exploring: ", crop_mg, "\n")
    
    seed <- char2seed(crop_mg, set = TRUE)
    
    eligible_counties <- df_crop %>% 
      group_by(wheat_type) %>%
      mutate(n_fips = n_distinct(fips)) %>%  # number of counties per maturity group
      ungroup() %>% 
      distinct(fips, st, n_rid, n_fips, wheat_type)
    
    # Explore
    eligible_counties %>%
      group_by(wheat_type) %>%
      summarise(n_fips    = n_distinct(fips),
                min_n_rid = min(n_rid, na.rm = TRUE),
                max_n_rid = max(n_rid, na.rm = TRUE),
                .groups = "drop") %>%
      print()
    
    # Total distinct fips
    total_fips <- eligible_counties %>%
      summarise(total = n_distinct(fips)) %>%
      pull(total)
    cat("Total unique fips: ", total_fips, "\n")

  } else {
    
    cat("Exploring: ", crop, "\n")
    
    seed <- char2seed(crop, set = TRUE)
    
    eligible_counties <- df_crop %>%
      mutate(n_fips = n_distinct(fips)) %>% 
      distinct(fips, st, n_rid, n_fips)
    
    # Explore
    eligible_counties %>%
      summarise(n_fips    = n_distinct(fips),
                min_n_rid = min(n_rid, na.rm = TRUE),
                max_n_rid = max(n_rid, na.rm = TRUE),
                .groups = "drop") %>%
      print()
    
    # Total distinct fips
    total_fips <- eligible_counties %>%
      summarise(total = n_distinct(fips)) %>%
      pull(total)
    cat("Total unique fips: ", total_fips, "\n")
    
  }


  # Standardize fips
  eligible_counties <- eligible_counties %>%
    mutate(fips = sprintf("%05d", fips))

  # Load shapefiles
  counties <- counties(cb = TRUE, year = 2023)
  states   <- states(cb = TRUE, year = 2023)

  states_main   <- states %>%
    filter(!STUSPS %in% c("HI", "AK", "PR", "GU", "MP", "RI", "DC", "AS", "VI"))  # "HI", "AK"
  counties_main <- counties %>%
    filter(STATEFP %in% states_main$STATEFP)

  # Join county shapes with your data
  map_data <- counties_main %>%
    inner_join(eligible_counties, by = c("GEOID" = "fips"))

  # Add longitude info for splitting
  map_data <- map_data %>%
    mutate(long_center = st_coordinates(st_centroid(geometry))[,1])

  # Random sampling: 2 counties per region in east/west
  set.seed(seed)
  if (crop == "soyb" || crop == "sorg" || crop == "rice" || crop == "cott") {
    
    # just sample up to 8 counties in this maturity region
    sampled_counties <- map_data %>%
      group_by(!!sym(crop_mg)) %>%
      group_split() %>%
      lapply(function(df_region) {
        slice_sample(df_region, n = min(4, nrow(df_region)))
      }) %>%
      bind_rows() %>%
      rename(fips = GEOID) %>%
      select(fips, !!sym(crop_mg))
    
  } else if (crop == "corn"){
    
    sampled_counties <- map_data %>%
      group_by(!!sym(crop_mg)) %>%
      group_split() %>%
      lapply(function(df_region) {
        
        if (all(df_region$long_center <= -100)) {
          sampled <- df_region %>% slice_sample(n = min(4, nrow(.)))
          
        } else if (all(df_region$long_center > -100)) {
          sampled <- df_region %>% slice_sample(n = min(4, nrow(.)))
          
        } else {
          west <- df_region %>% 
            filter(long_center <= -100) %>% 
            slice_sample(n = min(4, nrow(.)))
          east <- df_region %>% 
            filter(long_center > -100) %>% 
            slice_sample(n = min(4, nrow(.)))
          sampled <- bind_rows(west, east)
        }
        
        sampled
      }) %>%
      bind_rows() %>%
      rename(fips = GEOID) %>%
      select(fips, st, !!sym(crop_mg))
  } else if (crop == "wheat"){
    set.seed(seed)
    sampled_counties_lai <- map_data %>%
      filter(wheat_type %in% c("W3SR", "W3HR")) %>%
      group_by(wheat_type) %>%
      group_split() %>%
      lapply(function(df_region) {
        slice_sample(df_region, n = min(8, nrow(df_region)))
      }) %>%
      bind_rows() %>%
      rename(fips = GEOID) %>%
      select(fips, wheat_type)
    
    ## non-LAI wheat (SW3 and W3)
    {
      {  # SW3
        seed <- char2seed("spring_wheat", set = TRUE)
        set.seed(seed)
        candidate_counties_sw3 <- map_data %>%
          filter(wheat_type == "SW3") %>%
          rename(fips = GEOID) %>%
          distinct(fips, st, wheat_type) %>%
          arrange(fips)

        n_target <- min(8, nrow(candidate_counties_sw3))

        k <- nrow(candidate_counties_sw3) / n_target
        start <- runif(1, min = 0, max = k)
        sample_idx <- ((floor(start + (0:(n_target - 1)) * k)) %% nrow(candidate_counties_sw3)) + 1

        sampled_counties_sw3 <- candidate_counties_sw3 %>%
          slice(sample_idx) %>%
          select(fips, st, wheat_type)
      }
      {  # W3
        seed <- char2seed("winter_wheat", set = TRUE)
        set.seed(seed)
        candidate_counties_w3 <- map_data %>%
          filter(wheat_type == "W3") %>%
          rename(fips = GEOID) %>%
          distinct(fips, st, wheat_type) %>%
          arrange(fips)

        n_target <- min(8, nrow(candidate_counties_w3))

        k <- nrow(candidate_counties_w3) / n_target
        start <- runif(1, min = 0, max = k)
        sample_idx <- ((floor(start + (0:(n_target - 1)) * k)) %% nrow(candidate_counties_w3)) + 1

        sampled_counties_w3 <- candidate_counties_w3 %>%
          slice(sample_idx) %>%
          select(fips, st, wheat_type)
      }
    }

    # combine
    sampled_counties_non_lai <- bind_rows(sampled_counties_sw3, sampled_counties_w3)
    sampled_counties <- bind_rows(sampled_counties_lai, sampled_counties_non_lai)

  } else if (crop != "corn" && crop != "soyb" && crop != "wheat") {

    # Systematic sampling with random start for crops without maturity groups.
    # Counties are ordered by FIPS, then sampled at a fixed interval.
    candidate_counties <- map_data %>%
      rename(fips = GEOID) %>%
      distinct(fips, st) %>%
      arrange(fips)

    n_target <- min(8, nrow(candidate_counties))

    if (n_target == 0) {
      sampled_counties <- candidate_counties
    } else {
      k <- nrow(candidate_counties) / n_target
      start <- runif(1, min = 0, max = k)
      sample_idx <- ((floor(start + (0:(n_target - 1)) * k)) %% nrow(candidate_counties)) + 1

      sampled_counties <- candidate_counties %>%
        slice(sample_idx) %>%
        select(fips, st)
    }
  }


  # Make sampled counties sf
  sampled_counties_sf <- counties_main %>%
    inner_join(sampled_counties %>% as.data.frame(),
              by = c("GEOID" = "fips"))

  # ------------------------------------------------------------------
  # Diagnostics: how representative are the calibration counties?
  # Compare eligible vs sampled by maturity region and east/west
  # ------------------------------------------------------------------

  if (crop == "corn" || crop == "soyb" || crop == "sorg" || crop == "rice" || crop == "cott") {
    # Add east/west label to all eligible counties
    map_data_diag <- map_data %>%
      mutate(region_side = if_else(long_center <= -100, "west", "east"))
    
    cat("\n=== Eligible counties by maturity region ===\n")
    map_data_diag %>%
      st_drop_geometry() %>%
      count(!!sym(crop_mg), name = "n_eligible") %>%
      arrange(!!sym(crop_mg)) %>%
      print()
    
    # For corn, also check east vs west within each maturity region
    if (crop == "corn") {
      cat("\n=== Eligible counties by maturity region and east/west ===\n")
      map_data_diag %>%
        st_drop_geometry() %>%
        count(!!sym(crop_mg), region_side, name = "n_eligible") %>%
        arrange(!!sym(crop_mg), region_side) %>%
        print()
    }
    
    # Now restrict to the sampled calibration counties
    sampled_map_diag <- map_data_diag %>%
      semi_join(sampled_counties_sf %>% st_drop_geometry(), by = "GEOID")
    
    cat("\n=== Sampled calibration counties by maturity region ===\n")
    sampled_map_diag %>%
      st_drop_geometry() %>%
      count(!!sym(crop_mg), name = "n_sampled") %>%
      arrange(!!sym(crop_mg)) %>%
      print()
    
    if (crop == "corn") {
      cat("\n=== Sampled calibration counties by maturity region and east/west ===\n")
      sampled_map_diag %>%
        st_drop_geometry() %>%
        count(!!sym(crop_mg), region_side, name = "n_sampled") %>%
        arrange(!!sym(crop_mg), region_side) %>%
        print()
    }
  }

  # Vertical line at 100W
  vline_sf <- st_sfc(st_linestring(matrix(c(-100, 25,
                                            -100, 50),
                                          ncol = 2, byrow = TRUE)), crs = 4326) %>%
    st_sf()

  # Plot
  if(crop == "corn"){
    p1 = ggplot() +
      geom_sf(data = map_data, aes(fill = as.factor(!!sym(crop_mg))), color = NA) +
      geom_sf(data = states_main, fill = NA, color = "black", size = 0.3) +
      geom_sf(data = sampled_counties_sf, fill = NA, color = "red", size = 0.7) +
      #geom_sf(data = vline_sf, color = "blue", linetype = "dashed", size = 0.8) +
      theme_minimal() +
      labs(title = paste0(crop_mg, " Maturity Regions by County"),
          fill = "Maturity Region")
    p1 
    
    ggsave(
      filename = paste0("/data/rubelscratch/rubelogle/daycent_calibration/nir_utils/figures/", crop_mg, "_map.pdf"),
      plot = p1,
      width = 10,
      height = 8,
      device = cairo_pdf   # ensures high-quality vector PDF output
    )
  } else if(crop == "soyb") {
    p1 = ggplot() +
      geom_sf(data = map_data, aes(fill = as.factor(!!sym(crop_mg))), color = NA) +
      geom_sf(data = states_main, fill = NA, color = "black", size = 0.3) +
      geom_sf(data = sampled_counties_sf, fill = NA, color = "red", size = 0.7) +
      #geom_sf(data = vline_sf, color = "blue", linetype = "dashed", size = 0.8) +
      theme_minimal() +
      labs(title = paste0(crop_mg, " Maturity Regions by County"),
          fill = "Maturity Region")
    p1 
    
    ggsave(
      filename = paste0("/data/rubelscratch/rubelogle/daycent_calibration/nir_utils/figures/", crop_mg, "_map.pdf"),
      plot = p1,
      width = 10,
      height = 8,
      device = cairo_pdf   # ensures high-quality vector PDF output
    )
  } else if(crop == "sorg" || crop == "rice" || crop == "cott") {
    p1 = ggplot() +
      geom_sf(data = map_data, aes(fill = as.factor(!!sym(crop_mg))), color = NA) +
      geom_sf(data = states_main, fill = NA, color = "black", size = 0.3) +
      geom_sf(data = sampled_counties_sf, fill = NA, color = "red", size = 0.7) +
      #geom_sf(data = vline_sf, color = "blue", linetype = "dashed", size = 0.8) +
      theme_minimal() +
      labs(title = paste0(crop_mg, " Maturity Regions by County"),
          fill = "Maturity Region")
    p1 
    
    ggsave(
      filename = paste0("/data/rubelscratch/rubelogle/daycent_calibration/nir_utils/figures/", crop_mg, "_map.pdf"),
      plot = p1,
      width = 10,
      height = 8,
      device = cairo_pdf   # ensures high-quality vector PDF output
    )
  } else if (crop == "wheat") {
    # eligible_counties can repeat GEOID when both swht and wwht types occur in one
    # county; overlapping geom_sf fills hide the multiplicity — one row per county.
    map_data_plot <- map_data %>%
      group_by(GEOID) %>%
      summarise(
        wheat_type_label = paste(sort(unique(wheat_type)), collapse = " + "),
        geometry = dplyr::first(geometry),
        .groups = "drop"
      )
    map_data_plot <- map_data_plot %>%
      mutate(
        wheat_type_label = factor(
          wheat_type_label,
          levels = sort(unique(as.character(wheat_type_label)))
        )
      )

    p1 <- ggplot() +
      geom_sf(data = map_data_plot, aes(fill = wheat_type_label), color = NA) +
      geom_sf(data = states_main, fill = NA, color = "black", size = 0.3) +
      geom_sf(data = sampled_counties_sf, fill = NA, aes(color = wheat_type), size = 0.7) +
      #geom_sf(data = vline_sf, color = "blue", linetype = "dashed", size = 0.8) +
      theme_minimal() +
      labs(
        title = paste0(crop, " types by county (counties with multiple types combined)"),
        fill = "Wheat type(s)"
      )
    p1

    ggsave(
      filename = paste0("/data/rubelscratch/rubelogle/daycent_calibration/nir_utils/figures/", crop, "_wheat_type_map.pdf"),
      plot = p1,
      width = 10,
      height = 8,
      device = cairo_pdf   # ensures high-quality vector PDF output
    )

  pdf(paste0("/data/rubelscratch/rubelogle/daycent_calibration/nir_utils/figures/", crop, "_wheat_type_all_map.pdf"), width = 10, height = 8)

    for (wheat_type_i in unique(sampled_counties_sf$wheat_type)) {
      # Labels are combined with " + " (e.g. SW3 + W3HR); keep any county where
      # this calibration type is one of the components, not only exact matches.
      map_data_plot_df <- map_data_plot %>%
        filter(purrr::map_lgl(wheat_type_label, function(lab) {
          types_in_county <- strsplit(as.character(lab), " + ", fixed = TRUE)[[1]]
          wheat_type_i %in% types_in_county
        }))
      sampled_counties_sf_df <- sampled_counties_sf %>%
        filter(wheat_type == wheat_type_i)

      p2 <- ggplot() +
        geom_sf(data = map_data_plot_df, aes(fill = wheat_type_label), color = NA) +
        geom_sf(data = states_main, fill = NA, color = "black", size = 0.3) +
        geom_sf(data = sampled_counties_sf_df, fill = NA, color = "red", size = 0.7) +
        theme_minimal() +
        labs(title = paste0(crop, " ", wheat_type_i, " by county"),
            fill = "Wheat type(s)")

      print(p2)

    }

    dev.off()


  } else {
    p1 <- ggplot() +
      geom_sf(data = map_data, fill = "lightblue", color = NA) +   # one uniform color
      geom_sf(data = states_main, fill = NA, color = "black", size = 0.3) +
      geom_sf(data = sampled_counties_sf, fill = NA, color = "red", size = 0.7) +
      theme_minimal() +
      labs(title = paste0(crop, " by County"),
          fill = NULL)
    
    p1
    
    ggsave(
      filename = paste0("/data/rubelscratch/rubelogle/daycent_calibration/nir_utils/figures/", crop, "_map.pdf"),
      plot = p1,
      width = 10,
      height = 8,
      device = cairo_pdf   # ensures high-quality vector PDF output
    )
  }
} else if(state_level == TRUE) {
  {
    
    cat("Exploring: ", crop, "\n")
    
    seed <- char2seed(crop, set = TRUE)
        
    if(crop == "cott") {
      eligible_state <- df_crop %>% 
        group_by(!!sym(crop_mg)) %>%
        mutate(n_st = n_distinct(st)) %>% 
        distinct(!!sym(crop_mg), st, n_rid, n_st)
      
      # Explore
      eligible_state %>%
        group_by(!!sym(crop_mg)) %>%
        summarise(n_st    = n_distinct(st),
                  min_n_rid = min(n_rid, na.rm = TRUE),
                  max_n_rid = max(n_rid, na.rm = TRUE),
                  .groups = "drop") %>%
        print()
      
      # Total distinct fips
      total_st <- eligible_state %>%
        group_by(!!sym(crop_mg)) %>%
        summarise(total = n_distinct(st)) %>%
        pull(total)
      cat("Total unique states: ", total_st, "\n")
    } else {
      eligible_state <- df_crop %>%  
        mutate(n_st = n_distinct(st)) %>% 
        distinct(!!sym(crop_mg), st, n_rid, n_st)
      
      # Explore
      eligible_state %>% 
        summarise(n_st    = n_distinct(st),
                  min_n_rid = min(n_rid, na.rm = TRUE),
                  max_n_rid = max(n_rid, na.rm = TRUE),
                  .groups = "drop") %>%
        print()
      
      # Total distinct fips
      total_st <- eligible_state %>% 
        summarise(total = n_distinct(st)) %>%
        pull(total)
      cat("Total unique states: ", total_st, "\n")
    }
    
  }

  # Load shapefiles 
  states   <- states(cb = TRUE, year = 2023)

  states_main   <- states %>%
    filter(!STUSPS %in% c("HI", "AK", "PR", "GU", "MP", "RI", "DC", "AS", "VI"))  # "HI", "AK"

# Join state shapes with eligible data (STATEFP is character; DB st is integer FIPS)
  map_data <- states_main %>%
    mutate(st = as.integer(STATEFP)) %>%
    inner_join(eligible_state, by = "st")

    set.seed(seed)
    if(crop == "cott") {
      candidate_states <- map_data %>%
        distinct(crop_mg, STATEFP) %>%
        arrange(crop_mg, STATEFP)
      
      sampled_states <- candidate_states         

    } else {
      candidate_states <- map_data %>%
        distinct(STATEFP) %>%
        arrange(STATEFP)

      n_target <- min(4, nrow(candidate_states))

      if (n_target == 0) {
        sampled_states <- candidate_states
      } else {
        k <- nrow(candidate_states) / n_target
        start <- runif(1, min = 0, max = k)
        sample_idx <- ((floor(start + (0:(n_target - 1)) * k)) %% nrow(candidate_states)) + 1

        sampled_states <- candidate_states %>%
          slice(sample_idx) %>%
          select(STATEFP)
      }
    }
 
  # Make sampled states sf (STATEFP is character; DB st is integer FIPS)
  {
    sampled_states_sf <- states_main %>%
      inner_join(sampled_states %>% as.data.frame(),
                by = c("STATEFP" = "STATEFP"))
  }

  # ------------------------------------------------------------------
  # Diagnostics: how representative are the calibration counties?
  # Compare eligible vs sampled by maturity region and east/west
  # ------------------------------------------------------------------

  # Plot
  {
    if(crop == "cott") {
      p1 <- ggplot() + 
        geom_sf(data = map_data, aes(fill = as.factor(!!sym(crop_mg))), color = NA) +   # one uniform color
        geom_sf(data = states_main, fill = NA, color = "black", size = 0.3) +
        #geom_sf(data = sampled_states_sf, fill = "light blue", color = "red", size = 0.7) +
        theme_minimal() +
        labs(title = paste0(crop_nass, " by State"),
            fill = NULL)
    } else {
      p1 <- ggplot() + 
        geom_sf(data = map_data, fill = "lightblue", color = NA) +   # one uniform color
        geom_sf(data = states_main, fill = NA, color = "black", size = 0.3) +
        geom_sf(data = sampled_states_sf, fill = "light blue", color = "red", size = 0.7) +
        theme_minimal() +
        labs(title = paste0(crop_nass, " by State"),
            fill = NULL)
    }
    
    
    p1
    
    ggsave(
      filename = paste0("/data/rubelscratch/rubelogle/daycent_calibration/nir_utils/figures/", crop_nass, "_map.pdf"),
      plot = p1,
      width = 10,
      height = 8,
      device = cairo_pdf   # ensures high-quality vector PDF output
    )
  }
}



# combine to merge the final table
if ((!is.null(crop_mg) && (crop_mg == "soyb_maturity_region" || crop_mg == "sorg_region" || crop_mg == "rice_type"))) {
  df <- sampled_counties %>%
    mutate(fips = as.integer(fips)) %>%
    inner_join(combine_all %>% mutate(fips = as.integer(fips)),
               by = c("fips", setNames(crop_mg, crop_mg))) %>%
    st_drop_geometry() %>%   
    select(recordid2017, fips, st, year, id_chist, climate_desc, climate_zone, soil_class, all_of(crop_mg), xfact_adj)
} else if (!is.null(crop_mg) && crop_mg == "corn_maturity_region") {
  df <- sampled_counties %>%
    mutate(fips = as.integer(fips)) %>%
    inner_join(combine_all %>% mutate(fips = as.integer(fips)),
               by = c("fips", "st", setNames(crop_mg, crop_mg))) %>%
    st_drop_geometry() %>%   
    select(recordid2017, fips, st, year, id_chist, climate_desc, climate_zone, soil_class, all_of(crop_mg), xfact_adj)
} else if (!is.null(crop_mg) && crop_mg == "wheat_type") {
  df <- sampled_counties %>%
    mutate(fips = as.integer(fips)) %>%
    inner_join(df_crop %>% mutate(fips = as.integer(fips)),
               by = c("fips", setNames(crop_mg, crop_mg))) %>%
    st_drop_geometry() %>%   
    select(recordid2017, fips, year, id_chist, climate_desc, climate_zone, soil_class, all_of(crop_mg), xfact_adj)
} else if(state_level == TRUE) {
  if(crop == "cott") {
    df <- combine_all %>% 
      filter(st %in% unique(as.integer(sampled_states_sf$STATEFP))) %>%   
      select(recordid2017, st, year, all_of(crop_mg), id_chist, climate_desc, climate_zone, soil_class, xfact_adj)
  } else {
    df <- combine_all %>% 
      filter(st %in% unique(as.integer(sampled_states_sf$STATEFP))) %>%   
      select(recordid2017, st, year, id_chist, climate_desc, climate_zone, soil_class, xfact_adj)
  }
} else {
  df <- sampled_counties %>%
    mutate(fips = as.integer(fips)) %>%
    inner_join(combine_all %>% mutate(fips = as.integer(fips)),
               by = c("fips", "st")) %>%
    st_drop_geometry() %>%   
    select(recordid2017, fips, st, year, id_chist, climate_desc, climate_zone, soil_class, xfact_adj)
}


# QC
if(state_level == FALSE) {
  fips_sampled <- unique(as.integer(sampled_counties$fips))
  fips_df      <- unique(df$fips)
  
  qc_all_match <- setequal(fips_sampled, fips_df)
  
  if (qc_all_match) {
    message("QC Passed: Unique FIPS matches")
  } else {
    message("QC Failed: Some FIPS do not match between sampled_counties and df.")
    
    fips_only_in_sampled <- setdiff(fips_sampled, fips_df)
    fips_only_in_df      <- setdiff(fips_df, fips_sampled)
    
    if (length(fips_only_in_sampled) > 0) {
      message("FIPS present in sampled_counties but missing in df: ", 
              paste(fips_only_in_sampled, collapse = ", "))
    }
    
    if (length(fips_only_in_df) > 0) {
      message("FIPS present in df but missing in sampled_counties: ", 
              paste(fips_only_in_df, collapse = ", "))
    }
  }
} else if(state_level == TRUE) {
  fips_sampled <- unique(as.integer(sampled_states_sf$STATEFP))
  fips_df      <- unique(df$st)
  
  qc_all_match <- setequal(fips_sampled, fips_df)
  
  if (qc_all_match) {
    message("QC Passed: Unique STATE matches")
  }
}

# Summary Table 
if(state_level == FALSE) {
  df %>%
    group_by(fips) %>%
    summarise(n_rid    = n_distinct(recordid2017),
              min_yr = min(year, na.rm = TRUE),
              max_yr = max(year, na.rm = TRUE),
              .groups = "drop") %>%
    print()
} else if(state_level == TRUE) {
  df %>%
    group_by(st) %>%
    summarise(n_rid    = n_distinct(recordid2017),
              min_yr = min(year, na.rm = TRUE),
              max_yr = max(year, na.rm = TRUE),
              .groups = "drop") %>%
    print()
}

# Save table
if(crop == "corn" || crop == "soyb" || crop == "sorg" || crop == "rice" || crop == "cott") {
  
  {
    crop_i <- crop
    df_list <- df %>%
      group_split(!!sym(crop_mg))
    
    # Iterate and save each table
    for (i in seq_along(df_list)) {
      region_val <- unique(df_list[[i]][[crop_mg]])
      
      recorids = unique(df_list[[i]]$recordid2017)
      
      mean_xfact_rid_mg <- mean_xfact_rid %>%
        filter(recordid2017 %in% recorids)
      
      # Define folder name
      folder_name <- paste0(folder_address,"crop_yield_", crop_i, "_m", region_val)
      
      # Create folder if it doesn't exist
      if (!dir.exists(folder_name)) dir.create(folder_name)
      
      # Define file name inside the folder
      file_name <- file.path(folder_name, paste0(crop_i, "_m", region_val, "_rid.csv"))
      file_name_area <- file.path(folder_name, paste0(crop_i, "_m", region_val, "mean_area_rid.csv"))
      
      # Write CSV
      write_csv(df_list[[i]], file_name)
      write_csv(mean_xfact_rid_mg, file_name_area)
      
      cat("Saved region ", region_val, " to ", file_name, "\n")
      cat("Saved region ", region_val, " to ", file_name_area, "\n")
    }
  }

} else if(crop == "wheat") {
  
  {
    crop_i <- crop
    df_list <- df %>%
      group_split(!!sym(crop_mg))
    
    # Iterate and save each table
    for (i in seq_along(df_list)) {
      region_val <- unique(df_list[[i]][[crop_mg]])
      
      recorids = unique(df_list[[i]]$recordid2017)
      
      mean_xfact_rid_mg <- mean_xfact_rid %>%
        filter(recordid2017 %in% recorids)
      
      # Define folder name
      folder_name <- paste0(folder_address,"crop_yield_", crop_i, "_", region_val)
      
      # Create folder if it doesn't exist
      if (!dir.exists(folder_name)) dir.create(folder_name)
      
      # Define file name inside the folder
      file_name <- file.path(folder_name, paste0(crop_i, "_", region_val, "_rid.csv"))
      file_name_area <- file.path(folder_name, paste0(crop_i, "_", region_val, "mean_area_rid.csv"))
      
      # Write CSV
      write_csv(df_list[[i]], file_name)
      write_csv(mean_xfact_rid_mg, file_name_area)
      
      cat("Saved region ", region_val, " to ", file_name, "\n")
      cat("Saved region ", region_val, " to ", file_name_area, "\n")
    }
  }

} else {

  {
    crop_i = crop
    
    # Iterate and save each table
    folder_name <- paste0(folder_address,"crop_yield_", crop_i)
      
      # Create folder if it doesn't exist
      if (!dir.exists(folder_name)) dir.create(folder_name)
      
      # Define file name inside the folder
      file_name <- file.path(folder_name, paste0(crop_i, "_rid.csv"))
      file_name_area <- file.path(folder_name, paste0(crop_i, "_mean_area_rid.csv"))
      
      # Write CSV
      write_csv(df, file_name)
      write_csv(mean_xfact_rid, file_name_area)
      
      cat("Saved to ", file_name, "\n")
      cat("Saved to ", file_name_area, "\n")
  }

}

 
