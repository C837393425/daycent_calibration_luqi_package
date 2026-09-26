#!/usr/bin/env Rscript
#' @title Plot Variables from DayCent daily.out File
#' @description Create line plots for specified variables from DayCent daily.out files
#' @details This script reads daily.out files and creates publication-quality plots
#'   with white backgrounds showing specified variables over time.
#'
#' @usage
#' Rscript plot_daily_out.R --file <daily.out> --years <year1,year2,...> --vars <var1,var2,...> [OPTIONS]
#'
#' @examples
#' # Plot PET and GLAI for a single year
#' Rscript plot_daily_out.R --file daily.out --years 2015 --vars "PET(cm),GLAI"
#'
#' # Plot specific variables for multiple years
#' Rscript plot_daily_out.R --file daily.out --years 2015,2016,2017 --vars "agdefac,bgdefac"
#'
#' # List available variables
#' Rscript plot_daily_out.R --file daily.out --list-vars

# =============================================================================
# SETUP AND LIBRARY LOADING
# =============================================================================

# Check for required packages and install if needed
check_and_load <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Package", pkg, "not found. Attempting to install...\n")
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
  library(pkg, character.only = TRUE)
}

# Suppress warnings during library loading
suppressPackageStartupMessages({
  check_and_load("optparse")
  check_and_load("ggplot2")
})

# =============================================================================
# COMMAND LINE ARGUMENT PARSING
# =============================================================================

option_list <- list(
  make_option(c("-f", "--file"), type = "character", default = NULL,
              help = "Path to daily.out file (REQUIRED)", metavar = "FILE"),

  make_option(c("-y", "--years"), type = "character", default = NULL,
              help = "Comma-separated list of years to plot (e.g., '2015,2016,2017')",
              metavar = "YEARS"),

  make_option(c("-v", "--vars"), type = "character", default = NULL,
              help = "Comma-separated list of variables to plot (e.g., 'PET(cm),GLAI'). Use quotes for names with parentheses.",
              metavar = "VARS"),

  make_option(c("--list-vars"), action = "store_true", default = FALSE,
              help = "List all available variables in the file and exit"),

  make_option(c("-o", "--output"), type = "character", default = NULL,
              help = "Output filename for plot (default: daily_out_plot.png)",
              metavar = "FILE"),

  make_option(c("--width"), type = "integer", default = 10,
              help = "Plot width in inches (default: 10)", metavar = "N"),

  make_option(c("--height"), type = "integer", default = 6,
              help = "Plot height in inches (default: 6)", metavar = "N"),

  make_option(c("--dpi"), type = "integer", default = 300,
              help = "Plot resolution in DPI (default: 300)", metavar = "N"),

  make_option(c("--separate"), action = "store_true", default = FALSE,
              help = "Create separate plots for each variable"),

  make_option(c("--verbose"), action = "store_true", default = FALSE,
              help = "Print detailed progress messages")
)

opt_parser <- OptionParser(
  usage = "Usage: %prog --file <daily.out> [--years <years>] [--vars <variables>] [OPTIONS]",
  option_list = option_list,
  description = "\nPlot specified variables from DayCent daily.out file.\n\nExamples:\n  List variables:       %prog --file daily.out --list-vars\n  Plot PET and GLAI:    %prog --file daily.out --years 2015 --vars 'PET(cm),GLAI'\n  Plot defac vars:      %prog --file daily.out --years 2015,2016 --vars 'agdefac,bgdefac'\n  Separate plots:       %prog --file daily.out --years 2015 --vars 'PET(cm),GLAI' --separate"
)

opt <- parse_args(opt_parser)

# =============================================================================
# VALIDATE ARGUMENTS
# =============================================================================

# Check required arguments
if (is.null(opt$file)) {
  print_help(opt_parser)
  stop("ERROR: --file is required", call. = FALSE)
}

if (!file.exists(opt$file)) {
  stop("ERROR: File not found: ", opt$file, call. = FALSE)
}

# If just listing variables, we can skip other validation
if (!opt$`list-vars`) {
  if (is.null(opt$years)) {
    print_help(opt_parser)
    stop("ERROR: --years is required (or use --list-vars to see available variables)", call. = FALSE)
  }

  if (is.null(opt$vars)) {
    print_help(opt_parser)
    stop("ERROR: --vars is required (or use --list-vars to see available variables)", call. = FALSE)
  }
}

# =============================================================================
# LOAD AND PROCESS DATA
# =============================================================================

cat("Loading daily.out file:", opt$file, "\n")

# Read daily.out file
daily_data <- tryCatch({
  read.table(opt$file, header = TRUE, sep = "", stringsAsFactors = FALSE)
}, error = function(e) {
  stop("ERROR: Failed to read daily.out file: ", conditionMessage(e), call. = FALSE)
})

if (opt$verbose) {
  cat("Data dimensions:", nrow(daily_data), "rows ×", ncol(daily_data), "columns\n")
}

# List variables if requested
if (opt$`list-vars`) {
  cat("\n", strrep("=", 70), "\n")
  cat("AVAILABLE VARIABLES IN DAILY.OUT\n")
  cat(strrep("=", 70), "\n\n")

  all_vars <- names(daily_data)
  cat("Total columns:", length(all_vars), "\n\n")

  for (i in seq_along(all_vars)) {
    cat(sprintf("%2d. %s\n", i, all_vars[i]))
  }

  cat("\n", strrep("=", 70), "\n")
  cat("Use --vars to specify which variables to plot (comma-separated)\n")
  cat("Example: --vars 'PET(cm),GLAI' or --vars 'agdefac,bgdefac'\n")
  cat(strrep("=", 70), "\n\n")

  quit(status = 0, save = "no")
}

# Extract year from time column
daily_data$year <- floor(daily_data$time)

# Parse requested years
year_list <- as.integer(strsplit(opt$years, ",")[[1]])

if (opt$verbose) {
  cat("Requested years:", paste(year_list, collapse = ", "), "\n")
}

# Filter data for requested years
plot_data <- daily_data[daily_data$year %in% year_list, ]

if (nrow(plot_data) == 0) {
  stop("ERROR: No data found for specified years", call. = FALSE)
}

if (opt$verbose) {
  cat("Filtered data:", nrow(plot_data), "rows\n")
  cat("Year range:", min(plot_data$year), "to", max(plot_data$year), "\n")
}

# Parse requested variables
var_list <- trimws(strsplit(opt$vars, ",")[[1]])

if (opt$verbose) {
  cat("Requested variables:", paste(var_list, collapse = ", "), "\n")
}

# Check if all requested variables exist
available_vars <- names(plot_data)
missing_vars <- setdiff(var_list, available_vars)

if (length(missing_vars) > 0) {
  cat("ERROR: The following variables were not found in daily.out:\n")
  for (v in missing_vars) {
    cat("  -", v, "\n")
  }
  cat("\nAvailable variables:\n")
  cat(paste(available_vars, collapse = ", "), "\n")
  cat("\nUse --list-vars to see all available variables\n")
  stop("Invalid variable names", call. = FALSE)
}

if (opt$verbose) {
  cat("All requested variables found\n")
}

# =============================================================================
# CREATE PLOTS
# =============================================================================

# Set default output filename if not specified
# Default: save in the same folder as the script
get_script_dir <- function() {
  # Try multiple methods to get script directory
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    script_path <- sub("^--file=", "", file_arg)
    return(dirname(normalizePath(script_path)))
  }
  # Fallback to current directory
  return(getwd())
}

script_dir <- get_script_dir()

if (is.null(opt$output)) {
  if (opt$separate) {
    output_files <- file.path(script_dir, paste0("daily_out_", gsub("[^a-zA-Z0-9]", "_", var_list), ".png"))
  } else {
    output_file <- file.path(script_dir, "daily_out_plot.png")
  }
} else {
  if (opt$separate) {
    # Split filename and extension
    base_name <- sub("\\.[^.]*$", "", opt$output)
    extension <- sub(".*\\.([^.]*$)", "\\1", opt$output)
    output_files <- paste0(base_name, "_", gsub("[^a-zA-Z0-9]", "_", var_list), ".", extension)
  } else {
    output_file <- opt$output
  }
}

if (!opt$separate) {
  # Combined plot with multiple panels
  cat("\nCreating combined plot...\n")

  # Reshape data for faceting
  n_vars <- length(var_list)
  plot_data_long <- data.frame(
    time = rep(plot_data$time, n_vars),
    year = rep(plot_data$year, n_vars),
    dayofyr = rep(plot_data$dayofyr, n_vars),
    variable = rep(var_list, each = nrow(plot_data)),
    value = unlist(lapply(var_list, function(v) plot_data[[v]])),
    stringsAsFactors = FALSE
  )

  # Ensure variable is a factor with correct ordering
  plot_data_long$variable <- factor(plot_data_long$variable, levels = var_list)

  # Create plot
  # Stack all panels vertically (ncol = 1)
  n_cols <- 1

  p <- ggplot(plot_data_long, aes(x = time, y = value, color = factor(year))) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~ variable, ncol = n_cols, scales = "free_y") +
    labs(
      title = paste("DayCent daily.out:", paste(var_list, collapse = ", ")),
      x = "Time (year.day)",
      y = "Value",
      color = "Year"
    ) +
    theme_bw() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white"),
      legend.background = element_rect(fill = "white"),
      legend.position = "bottom",
      strip.background = element_rect(fill = "gray90"),
      text = element_text(size = 12),
      axis.text = element_text(size = 10)
    ) +
    scale_color_brewer(palette = "Set1")

  # Save plot
  ggsave(output_file, plot = p, width = opt$width, height = opt$height,
         dpi = opt$dpi, bg = "white")

  cat("✓ Plot saved to:", output_file, "\n")
  cat("  Dimensions:", opt$width, "×", opt$height, "inches @", opt$dpi, "dpi\n")

} else {
  # Separate plots for each variable
  cat("\nCreating separate plots...\n")

  for (i in seq_along(var_list)) {
    var_name <- var_list[i]
    output_file <- output_files[i]

    p <- ggplot(plot_data, aes(x = time, y = .data[[var_name]], color = factor(year))) +
      geom_line(linewidth = 1.0) +
      labs(
        title = paste("DayCent daily.out:", var_name),
        x = "Time (year.day)",
        y = var_name,
        color = "Year"
      ) +
      theme_bw() +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white"),
        legend.background = element_rect(fill = "white"),
        legend.position = "bottom",
        text = element_text(size = 12),
        axis.text = element_text(size = 10)
      ) +
      scale_color_brewer(palette = "Set1")

    ggsave(output_file, plot = p, width = opt$width, height = opt$height,
           dpi = opt$dpi, bg = "white")

    cat("✓ Plot", i, "saved to:", output_file, "\n")
  }

  cat("  Dimensions:", opt$width, "×", opt$height, "inches @", opt$dpi, "dpi\n")
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n", strrep("=", 70), "\n")
cat("PLOTTING COMPLETE\n")
cat(strrep("=", 70), "\n\n")

cat("Input file:", opt$file, "\n")
cat("Years plotted:", paste(year_list, collapse = ", "), "\n")
cat("Data points:", nrow(plot_data), "\n")
cat("Variables:", paste(var_list, collapse = ", "), "\n")

if (opt$separate) {
  cat("\nOutput files:\n")
  for (i in seq_along(output_files)) {
    cat(sprintf("  %d. %s\n", i, output_files[i]))
  }
} else {
  cat("\nOutput file:", output_file, "\n")
}

cat("\n", strrep("=", 70), "\n")

quit(status = 0, save = "no")
