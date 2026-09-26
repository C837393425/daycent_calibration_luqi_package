#!/usr/bin/env Rscript

library(data.table)

cat("\n")
cat(strrep("=", 80), "\n")
cat("CORN YIELD COMPARISON: MODEL vs OBSERVATIONS\n")
cat("Site 877088 - County 38073 (Richland County, ND)\n")
cat(strrep("=", 80), "\n\n")

# Read simulated yields from .lis file
lis_file <- "/data/rubelscratch/rubelogle/daycent_calibration/scratch/run_20251031_112845/877088/877088/877088.lis"

cat("Reading simulated yields from:", lis_file, "\n")

# Read the file
lines <- readLines(lis_file)

# Parse the data (skip first line which is filename)
data_lines <- lines[-1]

# Split into columns
sim_data <- list()
for (line in data_lines) {
  parts <- strsplit(trimws(line), "\\s+")[[1]]
  if (length(parts) >= 3) {
    year <- parts[1]
    crop <- gsub("'", "", parts[2])
    cgrain <- parts[3]
    
    sim_data[[length(sim_data) + 1]] <- list(
      year = year,
      crop = crop,
      cgrain = as.numeric(cgrain)
    )
  }
}

cat("\n")
cat("--- SIMULATED YIELDS (from .lis file) ---\n")
cat(sprintf("%-10s %-15s %-15s\n", "Year", "Crop Type", "Grain (g C/m²)"))
cat(strrep("-", 80), "\n")

# Note: Corn is harvested in the year after planting
corn_years <- data.frame(
  plant_year = integer(),
  harvest_year = numeric(),
  cgrain = numeric(),
  stringsAsFactors = FALSE
)

for (item in sim_data) {
  year_num <- as.numeric(item$year)
  
  cat(sprintf("%-10.2f %-15s %-15.1f", year_num, item$crop, item$cgrain))
  
  # Mark corn years
  if (grepl("CM", item$crop)) {
    cat("  <- CORN")
    corn_years <- rbind(corn_years, data.frame(
      plant_year = year_num - 1,
      harvest_year = year_num,
      cgrain = item$cgrain
    ))
  }
  cat("\n")
}

cat("\n")
cat("--- CORN PLANTING vs HARVEST YEARS ---\n")
cat(sprintf("%-12s %-12s %-15s\n", "Planted", "Harvested", "Yield (g C/m²)"))
cat(strrep("-", 80), "\n")
for (i in 1:nrow(corn_years)) {
  cat(sprintf("%-12d %-12.0f %-15.1f\n", 
              corn_years$plant_year[i],
              corn_years$harvest_year[i],
              corn_years$cgrain[i]))
}

# Read observed data
obs_file <- "/data/rubelscratch/rubelogle/daycent_calibration/data/crop_yield_corn_m2/Observation_Data/ObservData_crop.csv"

cat("\n")
cat("Reading observed yields from:", obs_file, "\n\n")

obs <- fread(obs_file)

# Filter for county 38073 and years of interest
county_obs <- obs[aggregation_level == 38073, ]

cat("--- OBSERVED YIELDS (County 38073) ---\n")
cat(sprintf("%-10s %-15s %-20s\n", "Year", "Yield (g C/m²)", "Yield (bu/ac)"))
cat(strrep("-", 80), "\n")

for (yr in sort(unique(county_obs$year))) {
  yr_data <- county_obs[year == yr, ]
  if (nrow(yr_data) > 0) {
    # Convert g C/m² to bu/ac (approximate: 1 bu/ac ≈ 3.05 g C/m²)
    bu_per_ac <- yr_data$observed / 3.05
    cat(sprintf("%-10d %-15.1f %-20.1f\n", yr, yr_data$observed, bu_per_ac))
  }
}

cat("\n")
cat(strrep("=", 80), "\n")
cat("COMPARISON FOR TARGET YEARS (2013, 2015, 2018)\n")
cat(strrep("=", 80), "\n\n")

target_years <- c(2013, 2015, 2018)

cat(sprintf("%-10s %-15s %-15s %-15s %-12s\n", 
            "Year", "Observed", "Simulated", "Error", "Error %"))
cat(strrep("-", 80), "\n")

comparison <- data.frame(
  year = integer(),
  observed = numeric(),
  simulated = numeric(),
  error = numeric(),
  error_pct = numeric()
)

for (yr in target_years) {
  # Get observed
  obs_val <- county_obs[year == yr, observed]
  
  # Get simulated (corn planted in yr, harvested in yr+1)
  sim_val <- corn_years[corn_years$plant_year == yr, "cgrain"]
  
  if (length(obs_val) > 0 && length(sim_val) > 0) {
    error <- sim_val - obs_val
    error_pct <- 100 * error / obs_val
    
    cat(sprintf("%-10d %-15.1f %-15.1f %-15.1f %-12.1f%%\n",
                yr, obs_val, sim_val, error, error_pct))
    
    comparison <- rbind(comparison, data.frame(
      year = yr,
      observed = obs_val,
      simulated = sim_val,
      error = error,
      error_pct = error_pct
    ))
  }
}

cat("\n")
cat(strrep("=", 80), "\n")
cat("SUMMARY STATISTICS\n")
cat(strrep("=", 80), "\n\n")

if (nrow(comparison) > 0) {
  cat(sprintf("Mean Absolute Error (MAE):  %.1f g C/m²\n", mean(abs(comparison$error))))
  cat(sprintf("Root Mean Square Error:     %.1f g C/m²\n", sqrt(mean(comparison$error^2))))
  cat(sprintf("Mean Error:                 %.1f g C/m² (%.1f%%)\n", 
              mean(comparison$error), mean(comparison$error_pct)))
  cat(sprintf("Mean Absolute % Error:      %.1f%%\n", mean(abs(comparison$error_pct))))
  
  cat("\nPerformance by Year:\n")
  cat(sprintf("  2013: %+.1f%% error (%s)\n", 
              comparison$error_pct[comparison$year == 2013],
              ifelse(abs(comparison$error_pct[comparison$year == 2013]) < 20, "Good", "Poor")))
  cat(sprintf("  2015: %+.1f%% error (%s)\n", 
              comparison$error_pct[comparison$year == 2015],
              ifelse(abs(comparison$error_pct[comparison$year == 2015]) < 20, "Good", "Poor")))
  cat(sprintf("  2018: %+.1f%% error (%s)\n", 
              comparison$error_pct[comparison$year == 2018],
              ifelse(abs(comparison$error_pct[comparison$year == 2018]) < 20, "Good", "Poor")))
}

cat("\n")
cat(strrep("=", 80), "\n")
cat("NOTES:\n")
cat(strrep("=", 80), "\n")
cat("- Corn planted in year X is harvested in year X+1 in the model\n")
cat("- Observations are county-level average yields\n")
cat("- Site 877088 is one NRI point within County 38073\n")
cat("- Conversion: ~3.05 g C/m² = 1 bu/ac (at 15.5% moisture, 56 lb/bu)\n")
cat(strrep("=", 80), "\n\n")

