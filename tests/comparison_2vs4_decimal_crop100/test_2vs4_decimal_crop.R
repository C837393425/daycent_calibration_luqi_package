library(ggplot2)
library(openxlsx)
library(data.table)
library(tidyverse)
library(RMySQL)
library(DBI)
# *** Defining your working directory ****
#
# Define folder path for DayCent simulation exercise:
#  - This is the top level folder where you will save files from
#    the DayCent downloads and site files 
#

project_dir <- paste0("C:/Users/C837404338/Rscript/daycent_calibration/")

# Define DayCent executable location:
#  - This is the full path for DayCent executable (dc_exe) and 
#    DayCent 100 utility (dc_L100).
#
dc_exe =  file.path(project_dir, "daycent_build/bin/DDcentEVI_dev_LAI_Rice_23Sep2025.exe")
dc_L100 = file.path(project_dir, "daycent_build/bin/DDlist100_dev_LAI_Rice_23Sep2025.exe")
# dc_exe =  file.path(project_dir, "DayCent/DD15centEVI.exe")
# dc_L100 = file.path(project_dir, "DayCent/DD15list100.exe")

# Define DayCent parameter files (i.d. *.100 and ) location:
#  - This is the full path for DayCent executable (dc_exe) and 
#    DayCent 100 utility (dc_L100).
# 
dot100s <- file.path(project_dir, "tests/comparison_2vs4_decimal_crop100/crop_yield_corn_m2/DayCent_Files/dot100Files")

# Define folder path for site files for simulation:
#  - This is the full path where site files for DayCent simulation are saved.
#  - We will also run DayCent from this folder so all output is saved here.
#  
site_dir <- file.path(project_dir, "tests/comparison_2vs4_decimal_crop100/crop_yield_corn_m2")


setwd(site_dir)

# Accessing DayCent help menu 
system(command = dc_exe)


cred <- readLines("~/.dblogin")
con <- dbConnect(
  MySQL(),
  host = "trillium.nrel.colostate.edu",
  dbname = "inv2024_calib",
  username = cred[1],
  password = cred[2]
)

schedule_files_sample <- dbGetQuery(
  con,
  "SELECT * FROM schedule_files_corn_m2 LIMIT 15"
)
siteID <- unique(schedule_files_sample$site_name)

site_files_sample <- dbGetQuery(
  con,
  paste0(
    "SELECT * FROM site_files_corn_m2 WHERE site_name IN ('",
    paste(siteID, collapse = "', '"), "')"
  )
)

run_order_sample <- dbGetQuery(
  con,
  paste0(
    "SELECT site_name, treatment_name, weather_code ",
    "FROM run_order_corn_m2 ",
    "WHERE site_name IN ('", paste(siteID, collapse = "', '"), "')"
  )
)

dbDisconnect(con)

# Load package helpers from source (local rlib does not have bayesiancalibr/yaml)
`%||%` <- function(x, y) if (is.null(x)) y else x
source(file.path(project_dir, "bayesiancalibr/R/daycent.R"))
source(file.path(project_dir, "bayesiancalibr/R/os_tools.R"))
source(file.path(project_dir, "bayesiancalibr/R/database.R"))

# Local weather files for this test (flat: {weather_code}.wth)
weather_base_dir <- file.path(site_dir, "weather")

sim_root <- file.path(site_dir, "simulateion4")
dir.create(sim_root, recursive = TRUE, showWarnings = FALSE)

for (i in seq_along(siteID)) {
  this_site <- siteID[i]
  cat("\n========== Site", i, "of", length(siteID), ":", this_site, "==========\n")

  site_sim_dir <- file.path(sim_root, this_site)
  if (dir.exists(site_sim_dir)) {
    unlink(site_sim_dir, recursive = TRUE)
  }
  dir.create(site_sim_dir, recursive = TRUE)

  # Schedule file(s) for this site
  sch_rows <- schedule_files_sample[schedule_files_sample$site_name == this_site, , drop = FALSE]
  if (nrow(sch_rows) == 0) {
    cat("No schedule rows for", this_site, "- skipping\n")
    next
  }

  weather_code <- run_order_sample$weather_code[
    run_order_sample$site_name == this_site
  ][1]
  new_weather_file <- if (!is.na(weather_code) && nzchar(weather_code)) {
    paste0(weather_code, ".wth")
  } else {
    NA_character_
  }

  treatment_name <- NULL
  for (r in seq_len(nrow(sch_rows))) {
    trt <- sch_rows$treatment_name[r]
    if (is.na(trt) || !nzchar(as.character(trt))) {
      sch_name <- paste0(this_site, ".sch")
    } else {
      sch_name <- paste0(this_site, "_", trt, ".sch")
      treatment_name <- as.character(trt)
    }

    sched_data <- sch_rows$schedule_file_data[r]
    if (!is.na(new_weather_file) && !is.na(sched_data) && nzchar(sched_data)) {
      # Replace any existing *.wth reference with run_order weather_code
      sched_data <- gsub("[A-Za-z0-9_.-]+\\.wth", new_weather_file, sched_data, perl = TRUE)
      cat("  Schedule weather set to:", new_weather_file, "\n")
    }

    writeLines(sched_data, file.path(site_sim_dir, sch_name))
  }

  trt_sch_file <- if (is.null(treatment_name)) {
    paste0(this_site, ".sch")
  } else if (file.exists(file.path(site_sim_dir, paste0(this_site, ".sch")))) {
    paste0(this_site, ".sch")
  } else {
    paste0(this_site, "_", treatment_name, ".sch")
  }

  # Site.100 from database (soil/site state)
  site_rows <- site_files_sample[site_files_sample$site_name == this_site, , drop = FALSE]
  if (nrow(site_rows) == 0) {
    cat("No site.100 for", this_site, "- skipping\n")
    next
  }
  site100_file <- paste0(this_site, ".100")
  writeLines(site_rows$site_file_data[1], file.path(site_sim_dir, site100_file))

  # Use .100 parameter files as-is from dot100Files (no updates)
  file.copy(
    from = list.files(dot100s, full.names = TRUE),
    to = site_sim_dir,
    overwrite = TRUE,
    recursive = TRUE
  )

  # Weather file from local weather/ folder
  if (!is.na(weather_code) && nzchar(weather_code)) {
    weather_path <- file.path(weather_base_dir, paste0(weather_code, ".wth"))
    if (file.exists(weather_path)) {
      file.copy(weather_path, file.path(site_sim_dir, basename(weather_path)), overwrite = TRUE)
      cat("Copied weather:", basename(weather_path), "\n")
    } else {
      cat("WARNING: weather file not found for", this_site,
          "weather_code =", weather_code, "\n")
      cat("  Expected:", weather_path, "\n")
    }
  } else {
    cat("WARNING: no weather_code in run_order for", this_site, "\n")
  }

  # Run DayCent with schedule + site.100; parameter .100s come from dot100Files
  old_wd <- getwd()
  setwd(site_sim_dir)
  daycent_status <- run_DayCent(
    filepath_exe = dc_exe,
    sch_file = trt_sch_file,
    ext_site100_2read = site100_file,
    ext_site100_2write = NULL
  )
  setwd(old_wd)

  # DDList100 creates the .lis file from the .bin (use absolute paths)
  if (daycent_status == 0) {
    sch_base <- sub("\\.sch$", "", trt_sch_file)
    bin_file <- file.path(site_sim_dir, paste0(sch_base, ".bin"))
    lis_file <- file.path(site_sim_dir, paste0(sch_base, ".lis"))
    outvars_file <- file.path(site_sim_dir, "outvars.txt")

    if (file.exists(bin_file) && file.exists(outvars_file)) {
      # DDList100 expects to run in the directory containing the .bin
      setwd(site_sim_dir)
      ddlist_result <- run_ddlist100(
        ddlist_exe = dc_L100,
        bin_file = basename(bin_file),
        lis_file = basename(lis_file),
        outvars_file = basename(outvars_file)
      )
      setwd(old_wd)

      if (isTRUE(ddlist_result$success)) {
        cat("DDList100 SUCCESS:", basename(lis_file), "\n")
      } else {
        cat("DDList100 FAILED:", ddlist_result$error_message, "\n")
      }
    } else {
      cat("WARNING: missing", bin_file, "or", outvars_file, "; skipped DDList100\n")
    }
  }

  if (daycent_status == 0) {
    cat("DayCent SUCCESS for", this_site, "\n")
  } else {
    cat("DayCent FAILED for", this_site, "\n")
  }
}

