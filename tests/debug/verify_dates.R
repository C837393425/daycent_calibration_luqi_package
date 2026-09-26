#!/usr/bin/env Rscript

# Parse schedule file and show planting/harvest dates
sch_file <- "/data/rubelscratch/rubelogle/daycent_calibration/scratch/run_20251031_112845/877088/877088/877088.sch"

lines <- readLines(sch_file)

# Get start year
start_year <- as.integer(trimws(strsplit(lines[1], "\\s+")[[1]][1]))

cat("Schedule file:", sch_file, "\n")
cat("Start year:", start_year, "\n\n")

cat(strrep("=", 80), "\n")
cat("PLANTING AND HARVEST DATES USED IN PLOTS\n")
cat(strrep("=", 80), "\n\n")

# Find all corn planting and harvest events
events <- data.frame(
  year = integer(),
  day = integer(),
  event = character(),
  crop = character(),
  date_decimal = numeric(),
  stringsAsFactors = FALSE
)

for (line in lines) {
  if (grepl("^\\s*#", line) || grepl("^\\s*$", line)) next
  
  # Match CROP lines for corn
  if (grepl("CROP\\s+(CM2_11|CM243)", line)) {
    parts <- strsplit(trimws(line), "\\s+")[[1]]
    if (length(parts) >= 4) {
      year_offset <- suppressWarnings(as.integer(parts[1]))
      day <- suppressWarnings(as.integer(parts[2]))
      
      if (!is.na(year_offset) && !is.na(day)) {
        crop <- parts[4]
        year <- start_year + year_offset - 1
        date_decimal <- year + day / 365.25
        
        events <- rbind(events, data.frame(
          year = year,
          day = day,
          event = "PLANTING",
          crop = crop,
          date_decimal = date_decimal,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  # Match HARV lines
  if (grepl("HARV\\s+G", line)) {
    parts <- strsplit(trimws(line), "\\s+")[[1]]
    if (length(parts) >= 3) {
      year_offset <- suppressWarnings(as.integer(parts[1]))
      day <- suppressWarnings(as.integer(parts[2]))
      
      if (!is.na(year_offset) && !is.na(day)) {
        year <- start_year + year_offset - 1
        date_decimal <- year + day / 365.25
        
        events <- rbind(events, data.frame(
          year = year,
          day = day,
          event = "HARVEST",
          crop = NA,
          date_decimal = date_decimal,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
}

# Show all events
cat(sprintf("%-6s %-8s %-10s %-10s %-12s\n", "Year", "Day", "Event", "Crop", "Decimal"))
cat(strrep("-", 80), "\n")
for (i in 1:nrow(events)) {
  cat(sprintf("%-6d %-8d %-10s %-10s %-12.4f\n", 
              events$year[i], 
              events$day[i], 
              events$event[i], 
              ifelse(is.na(events$crop[i]), "-", events$crop[i]),
              events$date_decimal[i]))
}

# Filter for years 2013, 2015, 2018
cat("\n", strrep("=", 80), "\n")
cat("EVENTS FOR YEARS 2013, 2015, 2018 (USED IN PLOTS)\n")
cat(strrep("=", 80), "\n\n")

target_years <- c(2013, 2015, 2018)
for (yr in target_years) {
  cat("Year", yr, ":\n")
  year_events <- events[events$year == yr, ]
  
  if (nrow(year_events) > 0) {
    for (i in 1:nrow(year_events)) {
      # Calculate approximate month and day
      approx_month <- floor(year_events$day[i] / 30.44) + 1
      approx_day_in_month <- year_events$day[i] %% 30.44
      
      cat(sprintf("  %s: Day %d (approx %s %d) → Decimal: %.4f\n",
                  year_events$event[i],
                  year_events$day[i],
                  month.abb[min(approx_month, 12)],
                  round(approx_day_in_month),
                  year_events$date_decimal[i]))
      
      if (year_events$event[i] == "PLANTING") {
        cat(sprintf("    Crop: %s\n", year_events$crop[i]))
      }
    }
  } else {
    cat("  No corn events found\n")
  }
  cat("\n")
}

cat(strrep("=", 80), "\n")
cat("NOTES:\n")
cat("- Green dotted lines in plots = PLANTING dates\n")
cat("- Red dotted lines in plots = HARVEST dates\n")
cat("- Decimal values are used for vertical line positions (year + day/365.25)\n")
cat(strrep("=", 80), "\n")

