#!/usr/bin/env Rscript

#' @title Extract NASS Data as Observation Data for Crop
#' @description Extract NASS data for each crop and maturity group based on the sampling county
#' @author Luqi Jiao Emanuele
#' @date September 2025

# Set up environment
.libPaths("/data/rubelscratch/rubelogle/daycent_calibration/rlib")
library(RMySQL)
library(dplyr)
library(readr)

# change the set up
{
  crop = "corn"
  crop_nass = "corn_grain"
  crop_yield_conv = "corn"
  crop_mg = c(2, 3, 4, 5, 6)
  
  folder_address = "/data/rubelscratch/rubelogle/daycent_calibration/data"
}

cred_file <- "~/.dblogin"
cred <- readLines(cred_file)

user     <- cred[1]
password <- cred[2]

host       = "trillium.nrel.colostate.edu"
db_nri     = "nass_data"
conn_write = dbConnect(MySQL(), host = host, user = user, password = password, dbname = db_nri)

nass_data_tbl = "NASS_County_Yld_CropYear"
yield_conversion_tbl = "YieldConversion"

# read in nass data from db
{
  nass_data <- dbGetQuery(conn_write,
                          paste0("SELECT fips, year, tot_yld FROM ", nass_data_tbl, 
                                 " WHERE year BETWEEN 2011 AND 2020",
                                 " AND crop = '", crop_nass, "' ;"))
  
  yield_conversion <- dbGetQuery(conn_write,
                                 paste0("SELECT ConvXFact FROM ", yield_conversion_tbl,
                                        " WHERE crop = '", crop_yield_conv, "' AND ConvType = 'Yld2Cgrn' ;"))
  yield_conversion <- yield_conversion$ConvXFact
}  

#----------------------------------------------------------------------------------------------------------------------------
# individual maturity group
for(crop_mg_i in crop_mg){

  # read in county table
  county_address = paste0(folder_address, "/crop_yield_", crop, "_m", crop_mg_i, "/", crop, "_m", crop_mg_i, "_rid_evaluate.csv")
  crop_fips <- read.csv(county_address)
  
  unique_fips <- unique(crop_fips$fips)
  
  # select fips from nass table
  ObservData_crop <- nass_data %>%
    filter(fips %in% unique_fips)
  
  df_obser <- ObservData_crop %>%
    mutate(cgrain_gm2 = tot_yld * yield_conversion) %>%
    reframe(siteID = fips, 
            path = NA, 
            equil_schedule = NA, 
            base_schedule = NA, 
            treatment_schedule = NA, 
            meas_year = year, 
            cgrain_gm2, 
            elev_m = NA, 
            group = NA, 
            experiment = NA)
  
  # QC 
  {
    fips_in_obser <- unique(df_obser$siteID)
    missing_in_obser <- setdiff(unique_fips, fips_in_obser)
    extra_in_obser   <- setdiff(fips_in_obser, unique_fips)

    if (length(missing_in_obser) == 0 & length(extra_in_obser) == 0) {
      message("✅ QC passed: fips match for maturity group ", crop_mg_i)
    } else {
      warning("⚠️ QC failed for maturity group ", crop_mg_i,
              "\nMissing in df_obser: ", paste(missing_in_obser, collapse = ", "),
              "\nExtra in df_obser: ", paste(extra_in_obser, collapse = ", "))
    }
  }

  # define folder and ensure it exists
  obs_folder <- file.path(folder_address, paste0("crop_yield_", crop, "_m", crop_mg_i, "/Observation_Data"))
  dir.create(obs_folder, recursive = TRUE, showWarnings = FALSE)
  
  # save nass table
  nass_tbl_address <- file.path(obs_folder, "ObservData_crop_evaluate.csv")
  write_csv(df_obser, nass_tbl_address)

  folder_name <- paste0(folder_address,"/crop_yield_", crop, "_m", crop_mg_i, "/DayCent_Files")
    if (!dir.exists(folder_name)) dir.create(folder_name)
  folder_name <- paste0(folder_address,"/crop_yield_", crop, "_m", crop_mg_i, "/Daycent_ScheduleFiles")
    if (!dir.exists(folder_name)) dir.create(folder_name)
}

#----------------------------------------------------------------------------------------------------------------------------
# all maturity groups
df_obser_all = NULL
for(crop_mg_i in crop_mg){

  # read in county table
  county_address = paste0(folder_address, "/crop_yield_", crop, "_m", crop_mg_i, "/", crop, "_m", crop_mg_i, "_rid_evaluate.csv")
  crop_fips <- read.csv(county_address)
  
  unique_fips <- unique(crop_fips$fips)
  
  # select fips from nass table
  ObservData_crop <- nass_data %>%
    filter(fips %in% unique_fips)
  
  df_obser <- ObservData_crop %>%
    mutate(cgrain_gm2 = tot_yld * yield_conversion) %>%
    reframe(siteID = fips, 
            path = NA, 
            equil_schedule = NA, 
            base_schedule = NA, 
            treatment_schedule = NA, 
            meas_year = year, 
            cgrain_gm2, 
            elev_m = NA, 
            group = NA, 
            experiment = NA)
  
  # QC 
  {
    fips_in_obser <- unique(df_obser$siteID)
    missing_in_obser <- setdiff(unique_fips, fips_in_obser)
    extra_in_obser   <- setdiff(fips_in_obser, unique_fips)

    if (length(missing_in_obser) == 0 & length(extra_in_obser) == 0) {
      message("✅ QC passed: fips match for maturity group ", crop_mg_i)
    } else {
      warning("⚠️ QC failed for maturity group ", crop_mg_i,
              "\nMissing in df_obser: ", paste(missing_in_obser, collapse = ", "),
              "\nExtra in df_obser: ", paste(extra_in_obser, collapse = ", "))
    }
  }

  df_obser_all = rbind(df_obser_all, df_obser)
}


  # define folder and ensure it exists
  obs_folder <- file.path(folder_address, paste0("crop_yield_", crop, "_m", crop_mg, "/Observation_Data"))
  dir.create(obs_folder, recursive = TRUE, showWarnings = FALSE)

  # save nass table
    nass_tbl_address <- file.path(obs_folder, "ObservData_crop_evaluate.csv")
    write_csv(df_obser_all, nass_tbl_address)

