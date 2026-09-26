#!/usr/bin/env Rscript

weather_file_1 <- "/data/rubelscratch/rubelogle/daycent_calibration/data/crop_yield_corn_m4/227_520.wth"
weather_file_2 <- "/data/rubelscratch/rubelogle/daycent_calibration/scratch/daycent_sir_38866523_3/SIM_3/132833/5193_4289-101_174.wth"

out_dir <- "/data/rubelscratch/rubelogle/daycent_calibration/tests/figures"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

read_wth_first7 <- function(path, source_name) {
  x <- read.table(path, header = FALSE, stringsAsFactors = FALSE)
  if (ncol(x) < 7) stop("Expected at least 7 columns in: ", path)
  x <- x[, 1:7]
  colnames(x) <- c("day", "month", "year", "doy", "tmin", "tmax", "precip")
  is_leap_year <- function(y) (y %% 4 == 0 & y %% 100 != 0) | (y %% 400 == 0)
  days_in_year <- ifelse(is_leap_year(x$year), 366, 365)
  x$year_x <- x$year + (x$doy - 1) / days_in_year
  x$source <- source_name
  x
}

df1 <- read_wth_first7(weather_file_1, "227_520.wth")
df2 <- read_wth_first7(weather_file_2, "5193_4289-101_174.wth")

all_years <- range(c(df1$year_x, df2$year_x), na.rm = TRUE)
year_ticks <- seq(floor(all_years[1]), ceiling(all_years[2]), by = 1)

save_compare_plot <- function(var, ylab, out_file, force_nonnegative = FALSE) {
  y1 <- df1[[var]]
  y2 <- df2[[var]]
  ylim <- range(c(y1, y2), na.rm = TRUE)
  if (force_nonnegative) ylim[1] <- min(0, ylim[1], na.rm = TRUE)

  png(out_file, width = 1600, height = 800, res = 150)
  plot(
    NA,
    xlim = all_years,
    ylim = ylim,
    xlab = "Year",
    ylab = ylab,
    main = ylab,
    xaxt = "n"
  )
  axis(1, at = year_ticks, labels = year_ticks)
  lines(df1$year_x, y1, col = "#1f77b4")
  lines(df2$year_x, y2, col = "#d62728")
  legend(
    "topright",
    legend = c(df1$source[1], df2$source[1]),
    col = c("#1f77b4", "#d62728"),
    lty = 1,
    bty = "n"
  )
  dev.off()
}

save_compare_plot("tmin", "Min temperature", file.path(out_dir, "weather_compare_tmin.png"))
save_compare_plot("tmax", "Max temperature", file.path(out_dir, "weather_compare_tmax.png"))
save_compare_plot("precip", "Precipitation", file.path(out_dir, "weather_compare_precip.png"), force_nonnegative = TRUE)


