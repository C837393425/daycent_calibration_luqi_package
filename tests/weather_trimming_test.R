#!/usr/bin/env Rscript
#' @title Test Weather File Trimming Functionality
#' @description Tests the trim_weather_file_by_schedule() function
#' @details This script tests weather file trimming against schedule files
#'   to ensure proper behavior across different scenarios.

# =============================================================================
# SETUP
# =============================================================================

# Load required packages
library(bayesiancalibr, lib.loc = "/data/rubelscratch/rubelogle/daycent_calibration/rlib")

cat("=======================================================================\n")
cat("WEATHER FILE TRIMMING TEST\n")
cat("=======================================================================\n\n")

# =============================================================================
# TEST SETUP: Use existing simulation directory
# =============================================================================

test_dir <- "/data/rubelscratch/rubelogle/daycent_calibration/scratch/run_20251031_112845/877088/877088"

if (!dir.exists(test_dir)) {
  stop("Test directory not found: ", test_dir)
}

cat("Using test directory:", test_dir, "\n\n")

# Find schedule file
schedule_files <- list.files(test_dir, pattern = "\\.sch$", full.names = TRUE)
schedule_files <- schedule_files[!grepl("_eq\\.sch$|_eq_ext30\\.sch$", schedule_files)]

if (length(schedule_files) == 0) {
  stop("No schedule files found in test directory")
}

schedule_file <- schedule_files[1]
cat("Schedule file:", basename(schedule_file), "\n")

# Find weather file
weather_files <- list.files(test_dir, pattern = "\\.wth$", full.names = TRUE)

if (length(weather_files) == 0) {
  stop("No weather files found in test directory")
}

weather_file <- weather_files[1]
cat("Weather file:", basename(weather_file), "\n\n")

# =============================================================================
# READ CURRENT STATE
# =============================================================================

cat("-----------------------------------------------------------------------\n")
cat("READING CURRENT STATE\n")
cat("-----------------------------------------------------------------------\n\n")

# Read schedule start year
schedule_first_line <- readLines(schedule_file, n = 1, warn = FALSE)
schedule_start_year <- as.integer(sub("\\s+.*$", "", trimws(schedule_first_line)))
cat("Schedule start year:", schedule_start_year, "\n")

# Read weather file
weather_data <- read.table(weather_file, header = FALSE, stringsAsFactors = FALSE)
weather_years <- weather_data[, 3]
weather_start_year <- min(weather_years, na.rm = TRUE)
weather_end_year <- max(weather_years, na.rm = TRUE)
weather_nrows <- nrow(weather_data)

cat("Weather file:\n")
cat("  Start year:", weather_start_year, "\n")
cat("  End year:", weather_end_year, "\n")
cat("  Total rows:", weather_nrows, "\n\n")

# =============================================================================
# TEST 1: Check if trimming is needed
# =============================================================================

cat("-----------------------------------------------------------------------\n")
cat("TEST 1: DETERMINE IF TRIMMING IS NEEDED\n")
cat("-----------------------------------------------------------------------\n\n")

if (weather_start_year > schedule_start_year) {
  cat("ERROR: Weather file starts AFTER schedule start year!\n")
  cat("  Weather starts:", weather_start_year, "\n")
  cat("  Schedule starts:", schedule_start_year, "\n")
  cat("  This should fail validation.\n\n")
  test1_expected <- "FAIL"
} else if (weather_start_year < schedule_start_year) {
  cat("SUCCESS: Weather file needs trimming\n")
  cat("  Weather starts:", weather_start_year, "\n")
  cat("  Schedule starts:", schedule_start_year, "\n")
  rows_to_remove <- sum(weather_years < schedule_start_year)
  cat("  Rows to remove:", rows_to_remove, "\n")
  cat("  Rows remaining:", weather_nrows - rows_to_remove, "\n\n")
  test1_expected <- "TRIM"
} else {
  cat("INFO: Weather file already matches schedule start year\n")
  cat("  Both start at:", schedule_start_year, "\n")
  cat("  No trimming needed.\n\n")
  test1_expected <- "NO_TRIM"
}

# =============================================================================
# TEST 2: Create backup and test trimming
# =============================================================================

cat("-----------------------------------------------------------------------\n")
cat("TEST 2: TEST TRIMMING FUNCTION\n")
cat("-----------------------------------------------------------------------\n\n")

# Create test copies
test_weather_file <- paste0(weather_file, ".test")
test_schedule_file <- paste0(schedule_file, ".test")

file.copy(weather_file, test_weather_file, overwrite = TRUE)
file.copy(schedule_file, test_schedule_file, overwrite = TRUE)

cat("Created test copies:\n")
cat("  Weather:", basename(test_weather_file), "\n")
cat("  Schedule:", basename(test_schedule_file), "\n\n")

# Run trimming function
cat("Running trim_weather_file_by_schedule()...\n\n")

trim_result <- tryCatch({
  trim_weather_file_by_schedule(
    weather_file_path = test_weather_file,
    schedule_file_path = test_schedule_file,
    log_function = cat
  )
  "SUCCESS"
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n")
  "FAILED"
})

cat("\n")

# =============================================================================
# TEST 3: Verify results
# =============================================================================

cat("-----------------------------------------------------------------------\n")
cat("TEST 3: VERIFY TRIMMING RESULTS\n")
cat("-----------------------------------------------------------------------\n\n")

if (trim_result == "SUCCESS") {
  # Read trimmed weather file
  weather_trimmed <- read.table(test_weather_file, header = FALSE, stringsAsFactors = FALSE)
  trimmed_years <- weather_trimmed[, 3]
  trimmed_start_year <- min(trimmed_years, na.rm = TRUE)
  trimmed_end_year <- max(trimmed_years, na.rm = TRUE)
  trimmed_nrows <- nrow(weather_trimmed)

  cat("Trimmed weather file:\n")
  cat("  Start year:", trimmed_start_year, "\n")
  cat("  End year:", trimmed_end_year, "\n")
  cat("  Total rows:", trimmed_nrows, "\n")
  cat("  Rows removed:", weather_nrows - trimmed_nrows, "\n\n")

  # Validation
  if (trimmed_start_year == schedule_start_year) {
    cat("✓ PASS: Trimmed weather file starts at schedule year\n")
  } else {
    cat("✗ FAIL: Trimmed weather file does NOT start at schedule year\n")
    cat("  Expected:", schedule_start_year, "\n")
    cat("  Got:", trimmed_start_year, "\n")
  }

  if (trimmed_end_year == weather_end_year) {
    cat("✓ PASS: End year unchanged\n")
  } else {
    cat("✗ FAIL: End year was modified\n")
  }

  if (test1_expected == "TRIM" && trimmed_nrows < weather_nrows) {
    cat("✓ PASS: Rows were removed as expected\n")
  } else if (test1_expected == "NO_TRIM" && trimmed_nrows == weather_nrows) {
    cat("✓ PASS: No rows removed as expected\n")
  } else {
    cat("✗ FAIL: Unexpected row count change\n")
  }
} else {
  cat("Trimming function failed, skipping verification\n")
}

# =============================================================================
# CLEANUP
# =============================================================================

cat("\n-----------------------------------------------------------------------\n")
cat("CLEANUP\n")
cat("-----------------------------------------------------------------------\n\n")

if (file.exists(test_weather_file)) {
  file.remove(test_weather_file)
  cat("Removed:", basename(test_weather_file), "\n")
}

if (file.exists(test_schedule_file)) {
  file.remove(test_schedule_file)
  cat("Removed:", basename(test_schedule_file), "\n")
}

cat("\n=======================================================================\n")
cat("TEST COMPLETE\n")
cat("=======================================================================\n")
