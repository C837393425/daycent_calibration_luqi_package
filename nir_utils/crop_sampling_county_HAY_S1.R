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
  job_id     = as.numeric(Input.arg[1])  

  crop_mg <- NULL
  state_level <- FALSE
  crop_type <- "hay"
  if (job_id == 1) {                   ## COUNTY NASS DATA 
    crop = "alf"
    crop_nass = "hay_alf" 
    crop_mg = "daycent_crop" 
    crop_plot = "Alfalfa"
  } else if (job_id == 2) {  
    crop = "hay"
    crop_nass = "hay_excl_alf" 
    crop_mg = "daycent_crop"
    crop_plot = "Hay"
  } 

  cat("Crop: ", job_id, "\n")
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
 
infost_tbl = "INV_lookup_point_site_info" # tier_2023  #####
area_tbl  = "LandRep_xcls_xfact_INV2024" #xfact_adj_2024 
crop_schedule_tbl = "Inv24_CROP_intermediate_1"
nass_data_tbl = "NASS_County_Yld_CropYear"
aggre_level = "fips"


# NRI tables --------------------------------------------------------
conn_write = dbConnect(MySQL(), host = host, user = user, password = password, dbname = db_nri)
 
infoSite <- dbGetQuery(
  conn_write,
  paste0("SELECT recordid2017, fips, fips_st AS st, state_abbr FROM ", infost_tbl, " WHERE tier_2023 = 3;")
)

unique_rid = unique(infoSite$recordid2017)

xfact_rid <- dbGetQuery(
  conn_write,
  paste0("SELECT recordid2017, year, xfact_adj_2024 AS xfact_adj, xcls_final_2024 AS landuse FROM ", area_tbl, " WHERE recordid2017 in (", 
         paste0(unique_rid, collapse = ", "), ") AND year BETWEEN 2011 AND 2023 AND (xcls_final_2024 = 'CRC' OR xcls_final_2024 LIKE '_CC') ;")
)
 
# Close database connection
dbDisconnect(conn_write)

## schedule intermediate table -----------------------------------
conn_write = dbConnect(MySQL(), host = host, user = user, password = password, dbname = db_schfile)
if(crop == "alf") {
  queue = paste0("SELECT * FROM ", crop_schedule_tbl, " WHERE chist_landuse LIKE 'Hay/Legume %' AND year BETWEEN 2011 AND 2023 ;")
} else if(crop == "hay") {
  queue = paste0("SELECT * FROM ", crop_schedule_tbl, " WHERE chist_landuse LIKE 'Hay/%' AND year BETWEEN 2011 AND 2023 ;")
}
crop_inte_tbl <- dbGetQuery(
  conn_write,
  queue)

dbDisconnect(conn_write)

# NASS data -------------------------------------------------------
conn_write = dbConnect(MySQL(), host = host, user = user, password = password, dbname = db_nass)

queue = paste0("SELECT ", aggre_level, ", year, irr_yld, dry_yld, tot_yld FROM ", nass_data_tbl, 
              " WHERE crop = '", crop_nass, "'
              AND year BETWEEN 2011 AND 2023 ;")

nass_data <- dbGetQuery(
  conn_write,
  queue)
# Close database connection
dbDisconnect(conn_write)



#----------------------------------------------------------------------------------------------------------------------------------------
# combine the tables
{ 
  combine <- merge(xfact_rid, crop_inte_tbl, by = c("recordid2017", "year")) 
  combine <- merge(combine, infoSite, by = "recordid2017")  
  combine_all_i <- merge(combine, nass_data, by = c("fips", "year")) 
}


#----------------------------------------------------------------------------------------------------------------------------------------
if(crop == "alf") {   # define the hay alfalfa type
  combine_all_df <- combine_all_i %>%
    group_by(fips) %>%
    mutate(tot_area = sum(xfact_adj)) %>%
    ungroup() %>%
    group_by(daycent_crop, fips) %>%
    mutate(area_frac = sum(xfact_adj) / tot_area ) %>%
    ungroup() %>%
    filter(area_frac > 0.95) %>%
    select(fips, daycent_crop, area_frac) %>%
    distinct()

  combine_all <- NULL
  for(i in unique(combine_all_i$daycent_crop)) {
    cat("Daycent crop: ", i, "\n")
    combine_all_df_i <- combine_all_i %>%
      filter(daycent_crop == i) %>%
      semi_join(combine_all_df, by = c("fips", "daycent_crop"))
    cat("States: ", unique(combine_all_df_i$state_abbr), "\n")
    combine_all <- rbind(combine_all, combine_all_df_i)
  }

} else if(crop == "hay") {   # define the hay non-alfalfa type

  combine_all_df <- combine_all_i %>%
    filter(!daycent_crop %in% c("ALF", "ALF2")) %>%
    group_by(fips) %>%
    mutate(tot_area = sum(xfact_adj)) %>%
    ungroup() %>%
    group_by(daycent_crop, fips) %>%
    mutate(area_frac = sum(xfact_adj) / tot_area ) %>%
    ungroup() %>%
    filter(daycent_crop %in% c("G5", "G4", "G3", "GI3", "WC3", "G3CPI"),
           area_frac > 0.95) %>%
    select(fips, daycent_crop, area_frac) %>%
    distinct()

  test <- combine_all_i %>%
    filter(!daycent_crop %in% c("ALF", "ALF2")) %>%
    group_by(fips) %>%
    mutate(tot_area = sum(xfact_adj)) %>%
    ungroup() %>%
    group_by(daycent_crop, fips) %>%
    mutate(area_frac = sum(xfact_adj) / tot_area ) %>%
    ungroup() %>%
    filter(area_frac > 0.95) %>%
    select(fips, daycent_crop, area_frac) %>%
    group_by(daycent_crop) %>%
    summarise(n_fips = n_distinct(fips)) %>%
    distinct()
  print(test)

  combine_all <- NULL
  for(i in unique(combine_all_df$daycent_crop)) {
    cat("Daycent crop: ", i, "\n")
    combine_all_df_i <- combine_all_i %>%
      filter(daycent_crop == i) %>%
      semi_join(combine_all_df, by = c("fips", "daycent_crop"))
    cat("States: ", unique(combine_all_df_i$state_abbr), "\n")
    combine_all <- rbind(combine_all, combine_all_df_i)
  }

}


mean_xfact_rid <- combine_all %>%
  group_by(recordid2017) %>%
  mutate(mean_xfact = mean(xfact_adj)) %>%
  ungroup() %>%
  reframe(recordid2017, mean_xfact) %>%
  distinct()


rm(crop_tbl, matur_grp, infoSite, nass_data, combine)

# Set up the folder in data for crop 
if(crop == "alf" | crop == "hay"){
  unique_maturity <- unique(combine_all[[crop_mg]][combine_all[[crop_mg]] >= 0])
  
  for (maturity in unique_maturity) {
    folder_name <- paste0(folder_address,"crop_yield_", crop, "_m", maturity)
    if (!dir.exists(folder_name)) dir.create(folder_name)
  }
}

# set up the rule
if(crop == "hay") {
  df_crop = NULL
  for(i in unique(combine_all[[crop_mg]])) {
    if(i == "G3CPI" | i == "WC3" | i == "G5"){
      nri_yr = 0
      nri_rid = 0
    } else if(i == "ALF" | i == "ALF2") {
      nri_yr = 3
      nri_rid = 5
    } else {
      nri_yr = 5
      nri_rid = 10
    }

    df_crop_i <- combine_all %>%
      filter(!!sym(crop_mg) == i) %>%
      group_by(recordid2017) %>%
      mutate(n_rid_yr = n()) %>%     # number of years per site
      ungroup() %>%
      group_by(fips) %>%
      mutate(n_rid = n_distinct(recordid2017)) %>%  # number of sites per county
      ungroup() %>% 
      filter(n_rid_yr > nri_yr, n_rid > nri_rid)
    df_crop <- rbind(df_crop, df_crop_i)
  }
} else {
  df_crop = NULL
  for(i in unique(combine_all[[crop_mg]])) {
    if(i == "WC3"){
      df_crop_i <- combine_all %>% 
      filter(!!sym(crop_mg) == i) %>%
      group_by(recordid2017) %>%
      mutate(n_rid_yr = n()) %>%     # number of years per site
      ungroup() %>%
      group_by(fips) %>%
      mutate(n_rid = n_distinct(recordid2017)) %>%  # number of sites per county
      ungroup() 

    } else {
      nri_yr = 5
      nri_rid = 10

      df_crop_i <- combine_all %>% 
      filter(!!sym(crop_mg) == i) %>%
      group_by(recordid2017) %>%
      mutate(n_rid_yr = n()) %>%     # number of years per site
      ungroup() %>%
      group_by(fips) %>%
      mutate(n_rid = n_distinct(recordid2017)) %>%  # number of sites per county
      ungroup() %>% 
      filter(n_rid_yr > nri_yr, n_rid > nri_rid)
    }
 
    df_crop <- rbind(df_crop, df_crop_i)
  }
}

dim(df_crop)

{
  eligible_counties <- df_crop %>%
    group_by(!!sym(crop_mg)) %>%
    mutate(n_fips = n_distinct(fips)) %>%  # number of county per maturity group
    ungroup() %>% 
    distinct(fips, st, n_rid, n_fips, !!sym(crop_mg))
}


#----------------------------------------------------------------------------------------------------------------------------------------
  if(crop == "alf" | crop == "hay"){
    cat("Exploring: ", crop_mg, "\n")
        
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
  
  if (crop == "alf" | crop == "hay") {
    
    # just sample up to 8 counties in this maturity region 
    sampled_counties <- NULL
    for(i in unique(map_data[[crop_mg]])) {
      may_data_i <- map_data %>%
        filter(.data[[crop_mg]] == i)

      seed_i <- combine_all %>%
        filter(.data[[crop_mg]] == i) %>%
        distinct(daycent_crop)
      seed <- char2seed(seed_i, set = TRUE)
      set.seed(seed)
 
      if(i == "ALF" | i == "ALF2") {
        n_sample = 100
      } else {
        n_sample = 20
      }
      sampled_counties_i <- may_data_i %>%
        group_by(!!sym(crop_mg)) %>%
        group_split() %>%
        lapply(function(df_region) {
          slice_sample(df_region, n = min(n_sample, nrow(df_region)))
        }) %>%
        bind_rows() %>%
        rename(fips = GEOID) %>%
        select(fips, !!sym(crop_mg))
      
      sampled_counties <- rbind(sampled_counties, sampled_counties_i)
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

  if (crop == "alf" | crop == "hay") {
    # Add east/west label to all eligible counties
    map_data_diag <- map_data %>%
      mutate(region_side = if_else(long_center <= -100, "west", "east"))
    
    cat("\n=== Eligible counties by maturity region ===\n")
    map_data_diag %>%
      st_drop_geometry() %>%
      count(!!sym(crop_mg), name = "n_eligible") %>%
      arrange(!!sym(crop_mg)) %>%
      print()
     
    
    # Now restrict to the sampled calibration counties
    sampled_map_diag <- map_data_diag %>%
      semi_join(sampled_counties_sf %>% st_drop_geometry(), by = "GEOID")
    
    cat("\n=== Sampled calibration counties by maturity region ===\n")
    sampled_map_diag %>%
      st_drop_geometry() %>%
      count(!!sym(crop_mg), name = "n_sampled") %>%
      arrange(!!sym(crop_mg)) %>%
      print()
     
  }

  # Vertical line at 100W
  vline_sf <- st_sfc(st_linestring(matrix(c(-100, 25,
                                            -100, 50),
                                          ncol = 2, byrow = TRUE)), crs = 4326) %>%
    st_sf()

  # Plot
  if(crop == "alf" | crop == "hay") {
    for(i in unique(sampled_counties[[crop_mg]])) {
      map_data_i <- map_data %>%
        filter(!!sym(crop_mg) == i)
      sampled_counties_sf_i <- sampled_counties_sf %>%
        filter(!!sym(crop_mg) == i)
      p1 = ggplot() +
        geom_sf(data = map_data_i, aes(fill = as.factor(!!sym(crop_mg))), color = NA) +
        geom_sf(data = states_main, fill = NA, color = "black", size = 0.3) +
        geom_sf(data = sampled_counties_sf_i, fill = NA, color = "red", size = 0.7) + 
        theme_minimal() +
        labs(title = paste0(crop_plot, " Maturity Regions by County"),
            fill = "Maturity Region")
      p1 
      
      ggsave(
        filename = paste0("/data/rubelscratch/rubelogle/daycent_calibration/nir_utils/figures/", crop, "_", i, "_map.pdf"),
        plot = p1,
        width = 10,
        height = 8,
        device = cairo_pdf   # ensures high-quality vector PDF output
      )
    }
  } 




# combine to merge the final table
if (crop == "alf" | crop == "hay") {
  df <- sampled_counties %>%
    mutate(fips = as.integer(fips)) %>%
    inner_join(combine_all %>% mutate(fips = as.integer(fips)),
               by = c("fips", setNames(crop_mg, crop_mg))) %>%
    st_drop_geometry() %>%   
    select(recordid2017, fips, st, year, !!sym(crop_mg), xfact_adj)
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
{
  df %>%
    group_by(fips) %>%
    summarise(n_rid    = n_distinct(recordid2017),
              min_yr = min(year, na.rm = TRUE),
              max_yr = max(year, na.rm = TRUE),
              .groups = "drop") %>%
    print()
}

# Save table
if(crop == "alf" | crop == "hay") {
  
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

} 

 
