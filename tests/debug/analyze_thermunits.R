#!/usr/bin/env Rscript

library(data.table)

# Read daily.out
daily <- fread("/data/rubelscratch/rubelogle/daycent_calibration/scratch/run_20251031_112845/877088/877088/daily.out", 
               skip = 1, header = TRUE)

# Add year column
daily$year <- floor(daily$time)

# Focus on years 2013, 2015, 2018
years <- c(2013, 2015, 2018)

cat("\n")
cat(strrep("=", 80), "\n")
cat("THERMAL UNITS (GDD) ANALYSIS FOR CORN GROWING SEASONS\n")
cat(strrep("=", 80), "\n\n")

# Key dates from schedule
planting <- data.frame(
  year = c(2013, 2015, 2018),
  day = c(134, 117, 117)
)

harvest <- data.frame(
  year = c(2013, 2015, 2018),
  day = c(271, 254, 254)
)

for (yr in years) {
  cat("Year", yr, ":\n")
  cat(strrep("-", 80), "\n")
  
  year_data <- daily[daily$year == yr, ]
  
  # Get planting and harvest days
  plant_day <- planting$day[planting$year == yr]
  harv_day <- harvest$day[harvest$year == yr]
  
  cat(sprintf("  Planting: Day %d\n", plant_day))
  cat(sprintf("  Harvest:  Day %d\n", harv_day))
  cat(sprintf("  Growing season length: %d days\n\n", harv_day - plant_day))
  
  # Find when thermunits drops to zero or near-zero
  zero_day <- year_data[thermunits < 10, ]
  if (nrow(zero_day) > 0) {
    first_zero <- min(zero_day$dayofyr)
    cat(sprintf("  Thermunits drops to ~0: Day %d (%.0f days after planting)\n", 
                first_zero, first_zero - plant_day))
  }
  
  # Get max thermunits
  max_therm <- max(year_data$thermunits, na.rm = TRUE)
  max_day <- year_data$dayofyr[which.max(year_data$thermunits)]
  cat(sprintf("  Maximum thermunits: %.0f at Day %d\n", max_therm, max_day))
  
  # Thermunits at key dates
  plant_therm <- year_data$thermunits[year_data$dayofyr == plant_day]
  harv_therm <- year_data$thermunits[year_data$dayofyr == harv_day]
  
  if (length(plant_therm) > 0) {
    cat(sprintf("  Thermunits at planting (Day %d): %.0f\n", plant_day, plant_therm))
  }
  
  if (length(harv_therm) > 0) {
    cat(sprintf("  Thermunits at harvest (Day %d): %.0f\n", harv_day, harv_therm))
  }
  
  # Growing season accumulation
  growing_season <- year_data[dayofyr >= plant_day & dayofyr <= harv_day, ]
  if (nrow(growing_season) > 0) {
    start_therm <- growing_season$thermunits[1]
    end_therm <- growing_season$thermunits[nrow(growing_season)]
    accumulated <- end_therm - start_therm
    
    cat(sprintf("  GDD accumulated during growing season: %.0f\n", accumulated))
  }
  
  cat("\n")
}

cat(strrep("=", 80), "\n")
cat("EXPLANATION OF THERMUNITS RESET\n")
cat(strrep("=", 80), "\n\n")

cat("Thermunits (thermal units/GDD) typically RESET after harvest because:\n\n")
cat("1. In DayCent, thermunits accumulate during crop growth\n")
cat("2. After harvest, the crop is removed and thermunits reset to 0\n")
cat("3. This allows the next crop to start accumulating from 0\n\n")

cat("The earlier reset in 2013 is because:\n")
cat("  - 2013 had LATER planting (Day 134 vs Day 117)\n")
cat("  - 2013 had LATER harvest (Day 271 vs Day 254)\n")
cat("  - BUT the harvest in 2013 (Day 271) may have triggered the reset earlier\n")
cat("    relative to the calendar year compared to 2015/2018\n\n")

cat("Actually, looking at the data more carefully:\n")
cat("  - All years show thermunits drop happening around/after harvest\n")
cat("  - 2013 harvest = Day 271 (Sept 28)\n")
cat("  - 2015/2018 harvest = Day 254 (Sept 11)\n")
cat("  - So 2013's thermunits should drop LATER, not earlier!\n\n")

cat("Let me check if this is related to crop planting date:\n")
cat("  - 2013 planted Day 134 (May 14)\n")
cat("  - 2015/2018 planted Day 117 (Apr 27)\n")
cat("  - 17 days later planting in 2013\n\n")

cat("The early drop in 2013 might be due to:\n")
cat("  1. Severe water stress in 2013 causing premature maturity\n")
cat("  2. Model reaching maximum GDD requirement earlier due to hot conditions\n")
cat("  3. Early harvest trigger due to stress conditions\n\n")

cat(strrep("=", 80), "\n")

