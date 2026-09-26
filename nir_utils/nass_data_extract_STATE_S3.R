#!/usr/bin/env Rscript

#' @title Extract NASS Data as Observation Data for Crop
#' @description Extract NASS data for each crop and maturity group based on the sampling county
#' @author Luqi Jiao Emanuele
#' @date September 
#' @future update: change xfact_adj to area_ha and unit conversion to g/m2

# Set up environment
.libPaths("/data/rubelscratch/rubelogle/daycent_calibration/rlib")
library(RMySQL)
library(dplyr)
library(readr)

# change the set up
{
  Input.arg  = commandArgs(trailingOnly = TRUE)
  job_id     = as.numeric(Input.arg[1])

  if(job_id == 1){   # State county level
    id_chist <- 16
    crop = "toba"
    crop_nass = "tobacco" 
    crop_yield_conv = "tobacco" 
    begin_year = 2011
    end_year = 2023
  } else if(job_id == 2){  
    id_chist <- 21
    crop = "sunf"
    crop_nass = "sunflower" 
    crop_yield_conv = "sunflower" 
    begin_year = 2011
    end_year = 2023
  } else if(job_id == 3){  
    id_chist <- 990042
    crop = "drbe"
    crop_nass = "drybeans" 
    crop_yield_conv = "drybeans" 
    begin_year = 2011
    end_year = 2023
  } else if(job_id == 4){  
    id_chist <- 990049
    crop = "onio"
    crop_nass = "onions" 
    crop_yield_conv = "onions" 
    begin_year = 2011
    end_year = 2023
  } else if(job_id == 5){   
    id_chist <- 990053
    crop = "peas"
    crop_nass = "peas" 
    crop_yield_conv = "peas" 
    begin_year = 2011
    end_year = 2023
  } else if(job_id == 6){  
    id_chist <- 990054
    crop = "toma"
    crop_nass = "tomato" 
    crop_yield_conv = "tomato" 
    begin_year = 2011
    end_year = 2023
  } else if(job_id == 7){     
    id_chist <- 14
    crop = "cott"
    crop_nass = "cotton" 
    crop_yield_conv = "cotton"
    crop_mg = c(1, 2, 3, 4)
    begin_year = 2011
    end_year = 2023
  }  
  
  cat("Crop: ", crop, "\n")
  folder_address = "/data/rubelscratch/rubelogle/daycent_calibration/data"
}

cred_file <- "~/.dblogin"
cred <- readLines(cred_file)

user     <- cred[1]
password <- cred[2]

host       = "trillium.nrel.colostate.edu"
db_nri     = "nass_data"
conn_write = dbConnect(MySQL(), host = host, user = user, password = password, dbname = db_nri)

nass_data_tbl = "NASS_State_Yld_CropYear"
yield_conversion_tbl = "YieldConversion"

# read in nass data from db
{
  nass_data <- dbGetQuery(conn_write,
                          paste0("SELECT * FROM ", nass_data_tbl, 
                                 " WHERE year BETWEEN ", begin_year, " AND ", end_year,
                                 " AND crop = '", crop_nass, "' ;"))
  
  yield_conversion <- dbGetQuery(conn_write,
                                 paste0("SELECT ConvXFact FROM ", yield_conversion_tbl,
                                        " WHERE crop = '", crop_yield_conv, "' AND ConvType = 'Yld2Cgrn' ;"))
  yield_conversion <- yield_conversion$ConvXFact

}  

# Close database connection
dbDisconnect(conn_write)

# conversion equation:
# G C/m2 = Yield (bu/ac) * W_kg/bu * (1 - moisture) * fc * 1000/4046.86
## W_kg/bu: weight of bu in kg
## fc: carbon fraction
## 1000: kg - g
## 4046.86: acres - m2

#----------------------------------------------------------------------------------------------------------------------------
# individual maturity group
{
  if(crop == "hay") {
    # read in county table
    state_address = paste0(folder_address, "/crop_yield_", crop, "/", crop, "_rid.csv")
    state_fips <- read.csv(state_address)
    
    unique_states <- unique(state_fips$state)
    
    # select fips from nass table
    ObservData_crop <- nass_data %>%
      filter(state %in% unique_states)
      df_obser <- ObservData_crop %>%
      mutate(cgrain_gm2 = tot_yld * yield_conversion) %>%
      reframe(siteID = state_fips, 
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
    if(crop == "hay") {
      fips_in_obser <- unique(df_obser$siteID)
      missing_in_obser <- setdiff(unique_states, fips_in_obser)
      extra_in_obser   <- setdiff(fips_in_obser, unique_states)

      if (length(missing_in_obser) == 0 & length(extra_in_obser) == 0) {
        message("✅ QC passed: states match for ", crop)
      } else {
      warning("⚠️ QC failed for ", crop,
              "\nMissing in df_obser: ", paste(missing_in_obser, collapse = ", "),
              "\nExtra in df_obser: ", paste(extra_in_obser, collapse = ", "))
      }
    }

    # define folder and ensure it exists
    obs_folder <- file.path(folder_address, paste0("crop_yield_", crop, "/Observation_Data"))
    dir.create(obs_folder, recursive = TRUE, showWarnings = FALSE)
    
    # save nass table
    nass_tbl_address <- file.path(obs_folder, "ObservData_crop.csv")
    write_csv(df_obser, nass_tbl_address)

    folder_name <- paste0(folder_address,"/crop_yield_", crop, "/DayCent_Files")
      if (!dir.exists(folder_name)) dir.create(folder_name)
    folder_name <- paste0(folder_address,"/crop_yield_", crop, "/Daycent_ScheduleFiles")
      if (!dir.exists(folder_name)) dir.create(folder_name)

  } if(crop == "cott") { 
    for(crop_mg_i in crop_mg){
      state_address = paste0(folder_address, "/crop_yield_", crop, "_m", crop_mg_i, "/", crop, "_m", crop_mg_i, "_rid.csv")
      state_fips <- read.csv(state_address)
      
      unique_states <- unique(state_fips$st)
      
      # select fips from nass table
      ObservData_crop <- nass_data %>%
        filter(state_fips %in% unique_states)
      
      df_obser <- ObservData_crop %>%
      mutate(cgrain_gm2 = tot_yld * yield_conversion * 2.6) %>%
      reframe(siteID = state_fips, 
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
      if(crop == "cott") {
        fips_in_obser <- unique(df_obser$siteID)
        missing_in_obser <- setdiff(unique_states, fips_in_obser)
        extra_in_obser   <- setdiff(fips_in_obser, unique_states)

        if (length(missing_in_obser) == 0 & length(extra_in_obser) == 0) {
          message("✅ QC passed: states match for ", crop)
        } else {
        warning("⚠️ QC failed for ", crop,
                "\nMissing in df_obser: ", paste(missing_in_obser, collapse = ", "),
                "\nExtra in df_obser: ", paste(extra_in_obser, collapse = ", "))
        }
      }

      # define folder and ensure it exists
      obs_folder <- file.path(folder_address, paste0("crop_yield_", crop, "_m", crop_mg_i, "/Observation_Data"))
      dir.create(obs_folder, recursive = TRUE, showWarnings = FALSE)
      
      # save nass table
      nass_tbl_address <- file.path(obs_folder, "ObservData_crop.csv")
      write_csv(df_obser, nass_tbl_address)

      folder_name <- paste0(folder_address,"/crop_yield_", crop, "/DayCent_Files")
        if (!dir.exists(folder_name)) dir.create(folder_name)
      folder_name <- paste0(folder_address,"/crop_yield_", crop, "/Daycent_ScheduleFiles")
        if (!dir.exists(folder_name)) dir.create(folder_name)
    }

  } else {
    state_address = paste0(folder_address, "/crop_yield_", crop, "/", crop, "_rid.csv")
    state_fips <- read.csv(state_address)
    
    unique_states <- unique(state_fips$st)
    
    # select fips from nass table
    ObservData_crop <- nass_data %>%
      filter(state_fips %in% unique_states)

    df_obser <- ObservData_crop %>%
      mutate(cgrain_gm2 = tot_yld * yield_conversion) %>%
      reframe(siteID = state_fips, 
              path = NA, 
              equil_schedule = NA, 
              base_schedule = NA, 
              treatment_schedule = NA, 
              meas_year = year, 
              cgrain_gm2, 
              elev_m = NA, 
              group = NA, 
              experiment = NA)
  }
  
  
  
}


  
