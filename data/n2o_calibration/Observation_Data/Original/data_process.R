
#The objective of this code is preparing annual measured avg
#First loaded daily data
#Then did annual avg


###############################################################################
# LIBRARIES
###############################################################################
library(readxl)
library(tidyverse)
library(lubridate)
library(gtools)  # for mixedsort()


# LOAD MEASUREMENT PERIOD DEFINITIONS
runfile <- read_csv("N:/Research/Ogle/LandCraft/measurement_cleanup/measured_data_grouping/calibartion/meas_periods_cal.csv") %>%
  mutate(
    Meas_StartDate = as.Date(Meas_StartDate, format = "%m/%d/%Y"),
    Meas_EndDate   = as.Date(Meas_EndDate,   format = "%m/%d/%Y"),
    GS_StartDate   = as.Date(GS_StartDate,   format = "%m/%d/%Y"),
    GS_EndDate     = as.Date(GS_EndDate,     format = "%m/%d/%Y")
  ) %>%
  mutate(runfile_id = paste0("RF", row_number())) %>%
  select(SiteID, TreatmentID, runfile_id, Meas_StartDate,  Meas_EndDate) 


# Load annual grouped obseved data
daily_df <- readRDS("N:/Research/Ogle/LandCraft/landcraft_model_calibration_new_obs/N2O_Measurements/df_mp_seasonal.rds") %>%
  mutate(runfile_id = as.character(runfile_id))

# Join Meas_StartDate and Meas_EndDate from runfile into daily_df
# by matching on runfile_id
daily_df <- daily_df %>%
  left_join(
    runfile %>% select(runfile_id, Meas_StartDate, Meas_EndDate),  # only pull the columns we need
    by = "runfile_id"
  )

# Aggregate: calculate average measN2O_gN_ha_day for each combination of
# site, treatment, and measurement start/end date
annual_avg <- daily_df %>%
  group_by(siteID_dc, treatment_schedule, Meas_StartDate, Meas_EndDate) %>%  # define grouping columns
  summarise(
    N2O_gN_ha_day = mean(measN2O_gN_ha_day, na.rm = TRUE),  # average N2O, ignoring NAs
    n_obs = n(),                                                     # count of daily records in each group (optional, useful for QC)
    .groups = "drop"                                                 # drop grouping after summarise to avoid warnings
  )  %>%
  mutate(
    meas_start_year = year(Meas_StartDate),                          # year of measurement start
    meas_end_year   = year(Meas_EndDate),                            # year of measurement end
    meas_doy_begin  = yday(Meas_StartDate),                          # day-of-year for start date
    meas_doy_end    = yday(Meas_EndDate),                            # day-of-year for end date
    span_days       = as.numeric(Meas_EndDate - Meas_StartDate) + 1  # total number of days in measurement period (+1 to include both endpoints)
  ) %>%
  rename(siteID = siteID_dc) %>%
  mutate(Type = "Cumulative")

# Save annual_avg to a CSV file
write.csv(
  annual_avg,
  file = "cumN2O_annual_avg_calibration.csv",  # output file name
  row.names = FALSE                             # exclude row numbers from the output
)
