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
  node       = Sys.info()["nodename"]

  all_period <- FALSE 
  if(job_id == 1) { 
    id_chist = 25
    crop = "corn"
    crop_nass = "corn_grain" #"corn_grain"
    crop_yield_conv = "corn"
    crop_mg = c(2, 3, 4, 5, 6)   
    all_period = TRUE
  } else if(job_id == 2) { 
    id_chist <- 13
    crop = "soyb"
    crop_nass = "soybean" 
    crop_yield_conv = "soybean"
    crop_mg = c(0, 1, 2, 3, 4, 5, 6) 
    all_period = TRUE
  } else if(job_id == 3) {
    id_chist <- 27
    crop = "sorg"
    crop_nass = "sorghum_grain"  
    crop_yield_conv = "sorghum"
    crop_mg = c(1, 2, 3, 4, 5)
  } else if(job_id == 4) { 
    id_chist <- 111
    crop = "wheat"
    crop_nass = "wwht"
    crop_yield_conv = "wheat"
    crop_mg = c("W3SR", "W3HR") 
  } else if(job_id == 5) {
    id_chist <- 111
    crop = "wheat_SW3"
    crop_nass = "swht"
    crop_yield_conv = "wheat"
    crop_mg = "SW3" 
  } else if(job_id == 6) {
    id_chist <- 111
    crop = "wheat_W3"
    crop_nass = "wwht"
    crop_yield_conv = "wheat"
    crop_mg = "W3" 
  } else if(job_id == 7){   # NASS county level
    id_chist <- 114
    crop = "barl"
    crop_nass = "barley" 
    crop_yield_conv = "barley"
    crop_mg = NULL 
  } else if(job_id == 8){ 
    id_chist <- 112
    crop = "oats"
    crop_nass = "oats" 
    crop_yield_conv = "oats"
    crop_mg = NULL
  } else if(job_id == 10){ 
    id_chist <- 15
    crop = "pnut"
    crop_nass = "peanuts" 
    crop_yield_conv = "peanuts"
    crop_mg = NULL
  } else if(job_id == 11){ 
    id_chist <- 17
    crop = "sugb"
    crop_nass = "sugarbeets" 
    crop_yield_conv = "sugarbeets"
    crop_mg = NULL
  } else if(job_id == 12){ 
    id_chist <- 18
    crop = "pota"
    crop_nass = "potato" 
    crop_yield_conv = "potato"
    crop_mg = NULL
  } else if(job_id == 13){ 
    id_chist <- 113
    crop = "rice"
    crop_nass = "rice" 
    crop_yield_conv = "rice"
    crop_mg = c(1, 2, 3)
  } else if(job_id == 14){ 
    id_chist <- 990046
    crop = "swpo"
    crop_nass = "sweet_potato" 
    crop_yield_conv = "sweet_potato"
    crop_mg = NULL
  } else if(job_id == 15){ 
    id_chist <- 990052
    crop = "lent"
    crop_nass = "lentil" 
    crop_yield_conv = "lentil"
    crop_mg = NULL
  } else if(job_id == 16){   # State county level
    id_chist <- 16
    crop = "toba"
    crop_nass = "tobacco" 
    crop_yield_conv = "tobacco"
    crop_mg = NULL
  } else if(job_id == 17){  
    id_chist <- 21
    crop = "sunf"
    crop_nass = "sunflower" 
    crop_yield_conv = "sunflower"
    crop_mg = NULL
  } else if(job_id == 18){  
    id_chist <- 990042
    crop = "drbe"
    crop_nass = "drybeans" 
    crop_yield_conv = "drybeans"
    crop_mg = NULL
  } else if(job_id == 19){  
    id_chist <- 990049
    crop = "onio"
    crop_nass = "onions" 
    crop_yield_conv = "onions"
    crop_mg = NULL
  } else if(job_id == 20){   
    id_chist <- 990053
    crop = "peas"
    crop_nass = "peas" 
    crop_yield_conv = "peas"
    crop_mg = NULL
  } else if(job_id == 21){  
    id_chist <- 990054
    crop = "toma"
    crop_nass = "tomato" 
    crop_yield_conv = "tomato"
    crop_mg = NULL
  } else if(job_id == 22){   # NASS county level - HAY
    id_chist <- 146
    crop = "hay"
    crop_nass = "hay_alf" 
    crop_yield_conv = "hay"
    crop_mg = c("ALF", "ALF2") 
  } else if(job_id == 23){    
    id_chist <- 147
    crop = "hay"
    crop_nass = "hay_excl_alf" 
    crop_yield_conv = "hay"
    crop_mg = c("G5", "G4", "G3", "GI3", "WC3", "G3CPI") 
  }    
  
  folder_address = "/data/rubelscratch/rubelogle/daycent_calibration/data"
  if(node == "WCNR-EL-EMANUEL") {
    folder_address = "C:/Users/C837404338/Rscript/daycent_calibration/data"
  }
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
  if(all_period == TRUE) {
    queue = paste0("SELECT * FROM ", nass_data_tbl, 
                                 " WHERE crop = '", crop_nass, "' ;")
  } else {
    queue = paste0("SELECT * FROM ", nass_data_tbl, 
                                 " WHERE year BETWEEN 2011 AND 2023",
                                 " AND crop = '", crop_nass, "' ;")
  }

  nass_data <- dbGetQuery(conn_write, queue)
  
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
if(crop == "corn" | crop == "soyb" | crop == "sorg" | crop == "rice" | crop == "hay" | crop == "cott") {
  for(crop_mg_i in crop_mg){

    # read in county table
    if(crop == "hay") {
      county_address = paste0(folder_address, "/crop_yield_", crop, "_", crop_mg_i, "/", crop, "_", crop_mg_i, "_rid.csv")
    } else {
      county_address = paste0(folder_address, "/crop_yield_", crop, "_m", crop_mg_i, "/", crop, "_m", crop_mg_i, "_rid.csv")
    }
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
    if(crop == "hay") {
      obs_folder <- file.path(folder_address, paste0("crop_yield_", crop, "_", crop_mg_i, "/Observation_Data"))
    } else {
      obs_folder <- file.path(folder_address, paste0("crop_yield_", crop, "_m", crop_mg_i, "/Observation_Data"))
    }
    dir.create(obs_folder, recursive = TRUE, showWarnings = FALSE)
    
    # save nass table
    if(all_period == TRUE) {
      nass_tbl_address <- file.path(obs_folder, "ObservData_crop_all_period.csv")
    } else {
      nass_tbl_address <- file.path(obs_folder, "ObservData_crop.csv")
    }
    write_csv(df_obser, nass_tbl_address)

    if(crop == "hay") {
      folder_name <- paste0(folder_address,"/crop_yield_", crop, "_", crop_mg_i, "/DayCent_Files")
    } else {
      folder_name <- paste0(folder_address,"/crop_yield_", crop, "_m", crop_mg_i, "/DayCent_Files")
    }
      if (!dir.exists(folder_name)) dir.create(folder_name)
    if(crop == "hay") {
      folder_name <- paste0(folder_address,"/crop_yield_", crop, "_", crop_mg_i, "/Daycent_ScheduleFiles")
    } else {
      folder_name <- paste0(folder_address,"/crop_yield_", crop, "_m", crop_mg_i, "/Daycent_ScheduleFiles")
    }
      if (!dir.exists(folder_name)) dir.create(folder_name)
  }

  #----------------------------------------------------------------------------------------------------------------------------
# all maturity groups
df_obser_all = NULL
for(crop_mg_i in crop_mg){

  # read in county table
  if(crop == "hay") {
    county_address = paste0(folder_address, "/crop_yield_", crop, "_", crop_mg_i, "/", crop, "_", crop_mg_i, "_rid.csv")
  } else {
    county_address = paste0(folder_address, "/crop_yield_", crop, "_m", crop_mg_i, "/", crop, "_m", crop_mg_i, "_rid.csv")
  }
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
  if(crop == "corn" | crop == "soyb" | crop == "sorg" | crop == "rice" | crop == "alf" | crop == "hay" | crop == "cott") {
    obs_folder <- file.path(folder_address, paste0("crop_yield_", crop, "_all/Observation_Data"))
  } else {
    obs_folder <- file.path(folder_address, paste0("crop_yield_", crop, "/Observation_Data"))
  }
  dir.create(obs_folder, recursive = TRUE, showWarnings = FALSE)

  # save nass table
  nass_tbl_address <- file.path(obs_folder, "ObservData_crop.csv")
  write_csv(df_obser_all, nass_tbl_address)

} else if(job_id == 9) {
  for(wheat_type_i in crop_mg){

    # read in county table
    county_address = paste0(folder_address, "/crop_yield_", crop, "_", wheat_type_i, "/", crop, "_", wheat_type_i, "_rid.csv")
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
        message("✅ QC passed: fips match for maturity group ", wheat_type_i)
      } else {
        warning("⚠️ QC failed for maturity group ", wheat_type_i,
                "\nMissing in df_obser: ", paste(missing_in_obser, collapse = ", "),
                "\nExtra in df_obser: ", paste(extra_in_obser, collapse = ", "))
      }
    }

    # define folder and ensure it exists
    obs_folder <- file.path(folder_address, paste0("crop_yield_", crop, "_", wheat_type_i, "/Observation_Data"))
    dir.create(obs_folder, recursive = TRUE, showWarnings = FALSE)
    
    # save nass table
    nass_tbl_address <- file.path(obs_folder, "ObservData_crop.csv")
    write_csv(df_obser, nass_tbl_address)

    folder_name <- paste0(folder_address,"/crop_yield_", crop, "_", wheat_type_i, "/DayCent_Files")
      if (!dir.exists(folder_name)) dir.create(folder_name)
    folder_name <- paste0(folder_address,"/crop_yield_", crop, "_", wheat_type_i, "/Daycent_ScheduleFiles")
      if (!dir.exists(folder_name)) dir.create(folder_name)
  }

  #----------------------------------------------------------------------------------------------------------------------------
# all maturity groups for wheat
df_obser_all = NULL
for(wheat_type_i in crop_mg){

  # read in county table
  county_address = paste0(folder_address, "/crop_yield_", crop, "_", wheat_type_i, "/", crop, "_", wheat_type_i, "_rid.csv")
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
      message("✅ QC passed: fips match for maturity group ", wheat_type_i)
    } else {
      warning("⚠️ QC failed for maturity group ", wheat_type_i,
              "\nMissing in df_obser: ", paste(missing_in_obser, collapse = ", "),
              "\nExtra in df_obser: ", paste(extra_in_obser, collapse = ", "))
    }
  }

  df_obser_all = rbind(df_obser_all, df_obser)
 }


  # define folder and ensure it exists
  if(job_id == 9) {
    obs_folder <- file.path(folder_address, paste0("crop_yield_", crop, "LAI_all/Observation_Data"))
  } else {
    obs_folder <- file.path(folder_address, paste0("crop_yield_", crop, "/Observation_Data"))
  }
  dir.create(obs_folder, recursive = TRUE, showWarnings = FALSE)

  # save nass table
  nass_tbl_address <- file.path(obs_folder, "ObservData_crop.csv")
  write_csv(df_obser_all, nass_tbl_address)

} else {
  # read in county table
  county_address = paste0(folder_address, "/crop_yield_", crop, "/", crop, "_rid.csv")
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
      message("✅ QC passed: fips match for ", crop)
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
}



  
