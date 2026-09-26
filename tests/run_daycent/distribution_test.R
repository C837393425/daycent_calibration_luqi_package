#!/usr/bin/env Rscript

# Distribution test script for SIR weighted results tables.
# Focus analysis:
#   weighted_value grouped by aggregation_level and year

suppressPackageStartupMessages({
  library(yaml)
  library(DBI)
  library(RMariaDB)
  library(dplyr)
  library(ggplot2)
})

parse_args <- function(args) {
  opts <- list(
    config = "/data/rubelscratch/rubelogle/daycent_calibration/workflows/configs/crop_yield_corn_m5.yaml",
    table = NULL,
    sample_size = 50000L,
    outdir = "/data/rubelscratch/rubelogle/daycent_calibration/tests/run_daycent/distribution_test_output",
    help = FALSE
  )

  for (arg in args) {
    if (arg == "--help" || arg == "-h") {
      opts$help <- TRUE
    } else if (startsWith(arg, "--config=")) {
      opts$config <- sub("^--config=", "", arg)
    } else if (startsWith(arg, "--table=")) {
      opts$table <- sub("^--table=", "", arg)
    } else if (startsWith(arg, "--sample-size=")) {
      opts$sample_size <- as.integer(sub("^--sample-size=", "", arg))
    } else if (startsWith(arg, "--outdir=")) {
      opts$outdir <- sub("^--outdir=", "", arg)
    } else {
      stop("Unknown argument: ", arg)
    }
  }

  opts
}

print_help <- function() {
  cat(
    "Distribution test for weighted SIR results\n",
    "Usage:\n",
    "  Rscript tests/run_daycent/distribution_test.R [options]\n\n",
    "Options:\n",
    "  --config=PATH        YAML config path (default: crop_yield_corn_m5.yaml)\n",
    "  --table=NAME         Override weighted results table name\n",
    "  --sample-size=N      Max rows for plotting (default: 50000)\n",
    "  --outdir=DIR         Output directory for csv/png files\n",
    "  --help, -h           Show this help\n\n",
    "Examples:\n",
    "  Rscript tests/run_daycent/distribution_test.R\n",
    "  Rscript tests/run_daycent/distribution_test.R --table=cornM5_SIR_Results_Weighted_27Mar2026\n",
    sep = ""
  )
}

read_credentials <- function(cred_file) {
  cred <- readLines(path.expand(cred_file), warn = FALSE)
  cred <- trimws(cred)
  cred <- cred[nzchar(cred)]
  if (length(cred) < 2) {
    stop("Credential file must contain at least 2 non-empty lines (username, password): ", cred_file)
  }
  list(username = cred[1], password = cred[2])
}

safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

sample_skewness <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3) {
    return(NA_real_)
  }
  s <- stats::sd(x)
  if (is.na(s) || s == 0) {
    return(0)
  }
  m <- mean(x)
  m3 <- mean((x - m)^3)
  m3 / (s^3)
}

shapiro_p_safe <- function(x, max_n = 5000L) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3) {
    return(NA_real_)
  }
  if (n > max_n) {
    set.seed(42)
    x <- sample(x, size = max_n)
  }
  out <- tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_)
  as.numeric(out)
}

main <- function() {
  opts <- parse_args(commandArgs(trailingOnly = TRUE))
  if (isTRUE(opts$help)) {
    print_help()
    return(invisible(NULL))
  }

  if (!file.exists(opts$config)) {
    stop("Config file not found: ", opts$config)
  }
  config <- yaml::read_yaml(opts$config)

  table_name <- opts$table
  if (is.null(table_name) || !nzchar(table_name)) {
    table_name <- config$file_source$database$database_result$tables$sir_results_weighted
  }
  if (is.null(table_name) || !nzchar(table_name)) {
    stop("Could not resolve weighted results table name from --table or config.")
  }

  host <- config$file_source$database$host
  db_name <- config$file_source$database$database
  cred_file <- config$file_source$database$cred_file %||% "~/.dblogin"
  cred <- read_credentials(cred_file)

  dir.create(opts$outdir, recursive = TRUE, showWarnings = FALSE)

  message("Connecting to DB: ", host, " / ", db_name)
  con <- DBI::dbConnect(
    RMariaDB::MariaDB(),
    host = host,
    dbname = db_name,
    username = cred$username,
    password = cred$password
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  query <- paste0("SELECT aggregation_level, year, weighted_value FROM `", table_name, "`")
  message("Reading table: ", table_name)
  df <- DBI::dbGetQuery(con, query)
  if (nrow(df) == 0) {
    stop("Table exists but has 0 rows: ", table_name)
  }

  # Optional row downsampling for faster plotting on large tables.
  if (!is.na(opts$sample_size) && nrow(df) > opts$sample_size) {
    set.seed(42)
    df <- df[sample(seq_len(nrow(df)), size = opts$sample_size), , drop = FALSE]
    message("Downsampled to ", nrow(df), " rows for plotting")
  }

  summary_txt <- file.path(opts$outdir, "distribution_summary.txt")
  grouped_summary_csv <- file.path(opts$outdir, "weighted_value_by_aggregation_level_year.csv")
  skewness_diagnostics_csv <- file.path(opts$outdir, "weighted_value_skewness_normality_by_aggregation_level_year.csv")

  message("Rows analyzed: ", nrow(df), " | Columns: ", ncol(df))
  message("Writing outputs to: ", opts$outdir)

  required_cols <- c("aggregation_level", "year", "weighted_value")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "))
  }

  df <- df %>%
    mutate(
      aggregation_level = as.character(aggregation_level),
      year = as.integer(year),
      weighted_value = safe_numeric(weighted_value)
    ) %>%
    filter(!is.na(aggregation_level), !is.na(year), is.finite(weighted_value))

  if (nrow(df) == 0) {
    stop("No valid rows after filtering NA/invalid values in aggregation_level/year/weighted_value.")
  }

  sink(summary_txt)
  cat("Distribution Test Report\n")
  cat("========================\n")
  cat("Table:", table_name, "\n")
  cat("Rows analyzed:", nrow(df), "\n")
  cat("Columns:", ncol(df), "\n")
  cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
  cat("Focus variable: weighted_value\n")
  cat("Grouping variables: aggregation_level, year\n")
  cat("Distinct aggregation_level:", dplyr::n_distinct(df$aggregation_level), "\n")
  cat("Distinct year:", dplyr::n_distinct(df$year), "\n\n")
  sink()

  grouped_stats <- df %>%
    group_by(aggregation_level, year) %>%
    summarise(
      n = dplyr::n(),
      mean = mean(weighted_value),
      sd = stats::sd(weighted_value),
      min = min(weighted_value),
      q25 = as.numeric(stats::quantile(weighted_value, 0.25)),
      median = stats::median(weighted_value),
      q75 = as.numeric(stats::quantile(weighted_value, 0.75)),
      max = max(weighted_value),
      .groups = "drop"
    ) %>%
    arrange(aggregation_level, year)

  skewness_stats <- df %>%
    group_by(aggregation_level, year) %>%
    summarise(
      n = dplyr::n(),
      mean = mean(weighted_value),
      median = stats::median(weighted_value),
      mean_minus_median = mean - median,
      skewness_raw = sample_skewness(weighted_value),
      skewness_log1p = sample_skewness(log1p(weighted_value)),
      shapiro_p_raw = shapiro_p_safe(weighted_value),
      shapiro_p_log1p = shapiro_p_safe(log1p(weighted_value)),
      tail_direction = dplyr::case_when(
        is.na(skewness_raw) ~ "unknown",
        skewness_raw > 0.1 ~ "right_tail",
        skewness_raw < -0.1 ~ "left_tail",
        TRUE ~ "approximately_symmetric"
      ),
      .groups = "drop"
    ) %>%
    arrange(aggregation_level, year)

  utils::write.csv(grouped_stats, grouped_summary_csv, row.names = FALSE)
  utils::write.csv(skewness_stats, skewness_diagnostics_csv, row.names = FALSE)

  overall_tail_counts <- skewness_stats %>%
    count(tail_direction, name = "n_groups")

  raw_non_normal <- sum(skewness_stats$shapiro_p_raw < 0.05, na.rm = TRUE)
  log_non_normal <- sum(skewness_stats$shapiro_p_log1p < 0.05, na.rm = TRUE)

  sink(summary_txt, append = TRUE)
  cat("\nSkewness and Normality Diagnostics\n")
  cat("==================================\n")
  cat("Per-group diagnostics CSV:", skewness_diagnostics_csv, "\n\n")
  cat("Tail-direction counts (based on skewness_raw):\n")
  print(overall_tail_counts)
  cat("\nGroups with Shapiro-Wilk p < 0.05 (non-normal evidence):\n")
  cat("  raw weighted_value:", raw_non_normal, "of", nrow(skewness_stats), "\n")
  cat("  log1p(weighted_value):", log_non_normal, "of", nrow(skewness_stats), "\n\n")
  cat("Notes:\n")
  cat("- Positive skewness implies right tail; negative skewness implies left tail.\n")
  cat("- This script checks both raw and log1p scales to assess whether transformation improves symmetry/normality.\n")
  sink()

  p_density <- ggplot(df, aes(x = weighted_value, color = factor(year), fill = factor(year))) +
    geom_density(alpha = 0.2) +
    facet_wrap(~ aggregation_level, scales = "free_y") +
    theme_bw() +
    labs(
      title = "weighted_value Density by aggregation_level and year",
      x = "weighted_value",
      y = "Density",
      color = "year",
      fill = "year"
    )

  p_box <- ggplot(df, aes(x = factor(year), y = weighted_value, fill = factor(year))) +
    geom_boxplot(outlier.size = 0.4, alpha = 0.85) +
    facet_wrap(~ aggregation_level, scales = "free_y") +
    theme_bw() +
    labs(
      title = "weighted_value Boxplot by aggregation_level and year",
      x = "year",
      y = "weighted_value",
      fill = "year"
    ) +
    theme(legend.position = "none")

  ggsave(
    filename = file.path(opts$outdir, "weighted_value_density_by_aggregation_level_year.png"),
    plot = p_density,
    width = 14,
    height = 9,
    dpi = 140
  )
  ggsave(
    filename = file.path(opts$outdir, "weighted_value_boxplot_by_aggregation_level_year.png"),
    plot = p_box,
    width = 14,
    height = 9,
    dpi = 140
  )

  message("Done.")
  message("Summary text: ", summary_txt)
  message("Grouped summary CSV: ", grouped_summary_csv)
  message("Skewness diagnostics CSV: ", skewness_diagnostics_csv)
}

`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs) && length(lhs) > 0 && !is.na(lhs) && nzchar(lhs)) lhs else rhs
}

if (sys.nframe() == 0) {
  main()
}
