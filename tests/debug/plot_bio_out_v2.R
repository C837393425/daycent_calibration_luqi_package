#!/usr/bin/env Rscript
#' @title Plot Variables from DayCent bio.out File with Planting/Harvest Dates
#' @description Create line plots for specified variables with separate panels per year
#'
#' @usage
#' Rscript plot_bio_out_v2.R --file <bio.out> --schedule <file.sch> --years <year1,year2,...> --vars <var1,var2,...> [OPTIONS]

# =============================================================================
# SETUP AND LIBRARY LOADING
# =============================================================================

check_and_load <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Package", pkg, "not found. Attempting to install...\n")
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
  library(pkg, character.only = TRUE)
}

suppressPackageStartupMessages({
  check_and_load("optparse")
  check_and_load("ggplot2")
})

# =============================================================================
# COMMAND LINE ARGUMENT PARSING
# =============================================================================

option_list <- list(
  make_option(c("-f", "--file"), type = "character", default = NULL,
              help = "Path to bio.out file (REQUIRED)", metavar = "FILE"),

  make_option(c("-s", "--schedule"), type = "character", default = NULL,
              help = "Path to schedule file (.sch) for planting/harvest dates (REQUIRED)", metavar = "FILE"),

  make_option(c("-y", "--years"), type = "character", default = NULL,
              help = "Comma-separated list of years to plot (e.g., '2013,2015,2018')",
              metavar = "YEARS"),

  make_option(c("-v", "--vars"), type = "character", default = NULL,
              help = "Comma-separated list of variables to plot",
              metavar = "VARS"),

  make_option(c("--list-vars"), action = "store_true", default = FALSE,
              help = "List all available variables in the file and exit"),

  make_option(c("-o", "--output"), type = "character", default = NULL,
              help = "Output filename for plot (default: bio_out_plot.png)",
              metavar = "FILE"),

  make_option(c("--width"), type = "integer", default = 14,
              help = "Plot width in inches (default: 14)", metavar = "N"),

  make_option(c("--height"), type = "integer", default = 8,
              help = "Plot height in inches (default: 8)", metavar = "N"),

  make_option(c("--dpi"), type = "integer", default = 300,
              help = "Plot resolution in DPI (default: 300)", metavar = "N"),

  make_option(c("--verbose"), action = "store_true", default = FALSE,
              help = "Print detailed progress messages")
)

opt_parser <- OptionParser(
  usage = "Usage: %prog --file <bio.out> --schedule <file.sch> --years <years> --vars <variables> [OPTIONS]",
  option_list = option_list
)

opt <- parse_args(opt_parser)

# =============================================================================
# VALIDATE ARGUMENTS
# =============================================================================

if (is.null(opt$file)) {
  print_help(opt_parser)
  stop("ERROR: --file is required", call. = FALSE)
}

if (!file.exists(opt$file)) {
  stop("ERROR: File not found: ", opt$file, call. = FALSE)
}

# =============================================================================
# LOAD DATA
# =============================================================================

cat("Loading bio.out file:", opt$file, "\n")

bio_data <- tryCatch({
  read.table(opt$file, header = TRUE, sep = "", stringsAsFactors = FALSE)
}, error = function(e) {
  stop("ERROR: Failed to read bio.out file: ", conditionMessage(e), call. = FALSE)
})

if (opt$verbose) {
  cat("Data dimensions:", nrow(bio_data), "rows ×", ncol(bio_data), "columns\n")
}

# List variables if requested
if (opt$`list-vars`) {
  cat("\n", strrep("=", 70), "\n")
  cat("AVAILABLE VARIABLES IN BIO.OUT\n")
  cat(strrep("=", 70), "\n\n")

  all_vars <- names(bio_data)
  cat("Total columns:", length(all_vars), "\n\n")

  for (i in seq_along(all_vars)) {
    cat(sprintf("%2d. %s\n", i, all_vars[i]))
  }

  cat("\n", strrep("=", 70), "\n")
  quit(status = 0, save = "no")
}

# Check required arguments for plotting
if (is.null(opt$schedule)) {
  stop("ERROR: --schedule is required", call. = FALSE)
}

if (is.null(opt$years)) {
  stop("ERROR: --years is required", call. = FALSE)
}

if (is.null(opt$vars)) {
  stop("ERROR: --vars is required", call. = FALSE)
}

# =============================================================================
# PARSE SCHEDULE FILE FOR PLANTING/HARVEST DATES
# =============================================================================

cat("Reading schedule file:", opt$schedule, "\n")

parse_schedule <- function(sch_file) {
  lines <- readLines(sch_file)

  # Get start year from first line
  start_year <- as.integer(trimws(strsplit(lines[1], "\\s+")[[1]][1]))

  # Find events
  events <- data.frame(
    year = integer(),
    day = integer(),
    event = character(),
    crop = character(),
    stringsAsFactors = FALSE
  )

  for (line in lines) {
    # Skip comment lines and empty lines
    if (grepl("^\\s*#", line) || grepl("^\\s*$", line)) next

    # Match CROP lines for corn
    if (grepl("CROP\\s+(CM2_11|CM243)", line)) {
      parts <- strsplit(trimws(line), "\\s+")[[1]]
      if (length(parts) >= 4) {
        year_offset <- suppressWarnings(as.integer(parts[1]))
        day <- suppressWarnings(as.integer(parts[2]))

        if (!is.na(year_offset) && !is.na(day)) {
          crop <- parts[4]

          events <- rbind(events, data.frame(
            year = start_year + year_offset - 1,
            day = day,
            event = "planting",
            crop = crop,
            stringsAsFactors = FALSE
          ))
        }
      }
    }

    # Match HARV lines (harvest following corn planting)
    if (grepl("HARV\\s+G", line)) {
      parts <- strsplit(trimws(line), "\\s+")[[1]]
      if (length(parts) >= 3) {
        year_offset <- suppressWarnings(as.integer(parts[1]))
        day <- suppressWarnings(as.integer(parts[2]))

        if (!is.na(year_offset) && !is.na(day)) {
          events <- rbind(events, data.frame(
            year = start_year + year_offset - 1,
            day = day,
            event = "harvest",
            crop = NA,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }

  return(events)
}

events <- parse_schedule(opt$schedule)

if (opt$verbose) {
  cat("\nPlanting and harvest events:\n")
  print(events)
}

# =============================================================================
# PROCESS DATA
# =============================================================================

bio_data$year <- floor(bio_data$time)

# Parse requested years
year_list <- as.integer(strsplit(opt$years, ",")[[1]])

if (opt$verbose) {
  cat("\nRequested years:", paste(year_list, collapse = ", "), "\n")
}

# Parse requested variables
var_list <- trimws(strsplit(opt$vars, ",")[[1]])

# Check if all requested variables exist
available_vars <- names(bio_data)
missing_vars <- setdiff(var_list, available_vars)

if (length(missing_vars) > 0) {
  cat("ERROR: The following variables were not found:\n")
  for (v in missing_vars) {
    cat("  -", v, "\n")
  }
  stop("Invalid variable names", call. = FALSE)
}

# =============================================================================
# CREATE PLOTS
# =============================================================================

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    script_path <- sub("^--file=", "", file_arg)
    return(dirname(normalizePath(script_path)))
  }
  return(getwd())
}

script_dir <- get_script_dir()

if (is.null(opt$output)) {
  output_file <- file.path(script_dir, "bio_out_plot.png")
} else {
  output_file <- opt$output
}

cat("\nCreating plot with separate panels per year...\n")

# Create separate data frames for each year and variable combination
plot_data_list <- list()

for (yr in year_list) {
  year_data <- bio_data[bio_data$year == yr, ]

  # Calculate proper decimal time from year + day/365.25
  proper_time <- yr + year_data$dayofyr / 365.25

  for (var_name in var_list) {
    plot_data_list[[length(plot_data_list) + 1]] <- data.frame(
      time = proper_time,
      year = yr,
      variable = var_name,
      value = year_data[[var_name]],
      stringsAsFactors = FALSE
    )
  }
}

plot_data_long <- do.call(rbind, plot_data_list)
plot_data_long$variable <- factor(plot_data_long$variable, levels = var_list)
plot_data_long$year <- factor(plot_data_long$year, levels = year_list)

# Create facet labels
plot_data_long$facet_label <- paste0(plot_data_long$variable, " (", plot_data_long$year, ")")

# Prepare event lines for each year
event_lines <- data.frame()
for (yr in year_list) {
  year_events <- events[events$year == yr, ]

  for (var_name in var_list) {
    for (i in 1:nrow(year_events)) {
      event_lines <- rbind(event_lines, data.frame(
        year = factor(yr, levels = year_list),
        variable = factor(var_name, levels = var_list),
        xintercept = yr + year_events$day[i] / 365.25,
        event = year_events$event[i],
        stringsAsFactors = FALSE
      ))
    }
  }
}

# Create plot with facet_grid to control y-axis per variable
p <- ggplot(plot_data_long, aes(x = time, y = value)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  facet_grid(variable ~ year, scales = "free") +
  labs(
    title = "DayCent bio.out with Planting and Harvest Dates",
    x = "Time (year.day)",
    y = "Value"
  ) +
  theme_bw() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "gray90"),
    text = element_text(size = 11),
    axis.text = element_text(size = 9),
    strip.text = element_text(size = 10)
  )

# Add planting lines (green dotted)
if (nrow(event_lines[event_lines$event == "planting", ]) > 0) {
  p <- p + geom_vline(
    data = event_lines[event_lines$event == "planting", ],
    aes(xintercept = xintercept),
    linetype = "dotted",
    color = "darkgreen",
    linewidth = 0.8
  )
}

# Add harvest lines (red dotted)
if (nrow(event_lines[event_lines$event == "harvest", ]) > 0) {
  p <- p + geom_vline(
    data = event_lines[event_lines$event == "harvest", ],
    aes(xintercept = xintercept),
    linetype = "dotted",
    color = "darkred",
    linewidth = 0.8
  )
}

# Save plot
ggsave(output_file, plot = p, width = opt$width, height = opt$height,
       dpi = opt$dpi, bg = "white")

cat("✓ Plot saved to:", output_file, "\n")
cat("  Dimensions:", opt$width, "×", opt$height, "inches @", opt$dpi, "dpi\n")
cat("  Green dotted lines = Planting dates\n")
cat("  Red dotted lines = Harvest dates\n")

cat("\n", strrep("=", 70), "\n")
cat("PLOTTING COMPLETE\n")
cat(strrep("=", 70), "\n")

quit(status = 0, save = "no")
