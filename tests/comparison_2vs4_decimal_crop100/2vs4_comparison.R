# Compare cgrain from .lis files: simulateion2 vs simulateion4 (years 2012-2024)

project_dir <- "C:/Users/C837404338/Rscript/daycent_calibration/"
base_dir <- file.path(
  project_dir,
  "tests/comparison_2vs4_decimal_crop100/crop_yield_corn_m2"
)
dir2 <- file.path(base_dir, "simulateion2")
dir4 <- file.path(base_dir, "simulateion4")
out_dir <- file.path(
  project_dir,
  "tests/comparison_2vs4_decimal_crop100"
)

year_min <- 2012
year_max <- 2024

source(file.path(project_dir, "bayesiancalibr/R/daycent.R"))

read_cgrain_lis <- function(lis_path, year_min, year_max) {
  lis <- read_lis_file(lis_path)
  if (!all(c("time", "cgrain") %in% names(lis))) {
    stop("Missing time/cgrain columns in: ", lis_path)
  }

  # Use displayed lis time year (not the package year-1 correction)
  year <- floor(as.numeric(lis$time))
  cgrain <- as.numeric(lis$cgrain)
  crpval <- if ("crpval" %in% names(lis)) {
    gsub("'", "", as.character(lis$crpval))
  } else {
    NA_character_
  }

  keep <- !is.na(year) & year >= year_min & year <= year_max
  data.frame(
    year = year[keep],
    cgrain = cgrain[keep],
    crpval = crpval[keep],
    stringsAsFactors = FALSE
  )
}

classify_crop <- function(crpval) {
  code <- toupper(trimws(as.character(crpval)))
  out <- rep("Other", length(code))
  out[is.na(code) | !nzchar(code)] <- "Unknown"
  out[grepl("^CM", code)] <- "Corn (CM)"
  out[grepl("^SM", code)] <- "Soybean (SM)"
  out[grepl("^SW", code)] <- "Spring wheat (SW)"
  out[grepl("^BAR", code)] <- "Barley (BAR)"
  out[grepl("^OAT", code)] <- "Oats (OAT)"
  out[grepl("^G[0-9]", code)] <- "Grass (G)"
  out
}



sites2 <- basename(list.dirs(dir2, recursive = FALSE, full.names = TRUE))
sites4 <- basename(list.dirs(dir4, recursive = FALSE, full.names = TRUE))
sites <- sort(intersect(sites2, sites4))
sites <- sites[!is.na(sites) & nzchar(sites) & sites != "NA"]
# Keep only sites that have both .lis files
sites <- sites[
  file.exists(file.path(dir2, sites, paste0(sites, ".lis"))) &
    file.exists(file.path(dir4, sites, paste0(sites, ".lis")))
]

if (length(sites) == 0) {
  stop("No common site folders with .lis files found between simulateion2 and simulateion4")
}

cat("Comparing", length(sites), "sites for cgrain,", year_min, "-", year_max, "\n")

comparison <- do.call(rbind, lapply(sites, function(site) {
  lis2 <- file.path(dir2, site, paste0(site, ".lis"))
  lis4 <- file.path(dir4, site, paste0(site, ".lis"))

  if (!file.exists(lis2) || !file.exists(lis4)) {
    cat("WARNING: missing .lis for site", site, "- skipping\n")
    return(NULL)
  }

  d2 <- read_cgrain_lis(lis2, year_min, year_max)
  d4 <- read_cgrain_lis(lis4, year_min, year_max)

  merged <- merge(
    d2,
    d4,
    by = "year",
    suffixes = c("_sim2", "_sim4"),
    all = TRUE
  )
  merged$site <- site
  # Prefer crop code from sim2; fall back to sim4
  merged$crpval <- ifelse(
    !is.na(merged$crpval_sim2) & nzchar(merged$crpval_sim2),
    merged$crpval_sim2,
    merged$crpval_sim4
  )
  merged$crop <- classify_crop(merged$crpval)
  merged$diff_sim4_minus_sim2 <- merged$cgrain_sim4 - merged$cgrain_sim2
  merged$abs_diff <- abs(merged$diff_sim4_minus_sim2)
  merged$pct_diff <- ifelse(
    is.na(merged$cgrain_sim2) | merged$cgrain_sim2 == 0,
    NA_real_,
    100 * merged$diff_sim4_minus_sim2 / merged$cgrain_sim2
  )
  merged[, c(
    "site", "year", "crpval", "crop",
    "cgrain_sim2", "cgrain_sim4",
    "diff_sim4_minus_sim2", "abs_diff", "pct_diff"
  )]
}))

rownames(comparison) <- NULL

# Rows with any difference (beyond tiny numeric noise)
tol <- 1e-8
diff_rows <- comparison[
  !is.na(comparison$abs_diff) & comparison$abs_diff > tol,
  ,
  drop = FALSE
]

cat("\nTotal site-years compared:", nrow(comparison), "\n")
cat("Site-years with |diff| >", tol, ":", nrow(diff_rows), "\n")

if (nrow(diff_rows) > 0) {
  cat("\nDifferences by site:\n")
  print(aggregate(
    abs_diff ~ site,
    data = diff_rows,
    FUN = function(x) c(n = length(x), max = max(x), mean = mean(x))
  ))
  cat("\nLargest absolute differences:\n")
  print(head(diff_rows[order(-diff_rows$abs_diff), ], 20))
} else {
  cat("\nNo cgrain differences found for", year_min, "-", year_max, "\n")
}

out_csv <- file.path(out_dir, "2vs4_cgrain_comparison_2012_2024.csv")
diff_csv <- file.path(out_dir, "2vs4_cgrain_differences_2012_2024.csv")
write.csv(comparison, out_csv, row.names = FALSE)
write.csv(diff_rows, diff_csv, row.names = FALSE)

cat("\nWrote full comparison:", out_csv, "\n")
cat("Wrote differences only:", diff_csv, "\n")

# ---- Plots ----
library(ggplot2)

plot_dir <- file.path(out_dir, "2vs4_plots")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

plot_df <- comparison
plot_df$site <- factor(plot_df$site, levels = sort(unique(plot_df$site)))

# 1) Scatter: sim2 vs sim4 cgrain
p_scatter <- ggplot(plot_df, aes(x = cgrain_sim2, y = cgrain_sim4, color = site)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(alpha = 0.75, size = 2) +
  labs(
    title = "cgrain: simulateion2 vs simulateion4 (2012-2024)",
    x = "cgrain sim2 (gC/m2)",
    y = "cgrain sim4 (gC/m2)",
    color = "Site"
  ) +
  theme_bw() +
  theme(legend.position = "right")
ggsave(
  file.path(plot_dir, "cgrain_scatter_sim2_vs_sim4.png"),
  p_scatter, width = 9, height = 6, dpi = 150
)

# 2) Difference over years by site
p_ts <- ggplot(plot_df, aes(x = year, y = diff_sim4_minus_sim2)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(color = "#2C7BB6") +
  geom_point(size = 1.4, color = "#2C7BB6") +
  facet_wrap(~ site, scales = "free_y") +
  labs(
    title = "cgrain difference (sim4 - sim2) by site and year",
    x = "Year",
    y = "Difference (gC/m2)"
  ) +
  theme_bw() +
  theme(strip.text = element_text(size = 8))
ggsave(
  file.path(plot_dir, "cgrain_diff_timeseries_by_site.png"),
  p_ts, width = 12, height = 8, dpi = 150
)

# 3) Heatmap of absolute difference
p_heat <- ggplot(plot_df, aes(x = factor(year), y = site, fill = abs_diff)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "#ffffcc",
    high = "#bd0026",
    name = "|diff|\n(gC/m2)"
  ) +
  labs(
    title = "Absolute cgrain difference |sim4 - sim2|",
    x = "Year",
    y = "Site"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(
  file.path(plot_dir, "cgrain_absdiff_heatmap.png"),
  p_heat, width = 10, height = 6, dpi = 150
)

# 4) Boxplot of signed difference by site
p_box <- ggplot(plot_df, aes(x = site, y = diff_sim4_minus_sim2)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_boxplot(outlier.size = 1, fill = "#abd9e9") +
  labs(
    title = "Distribution of cgrain differences by site (sim4 - sim2)",
    x = "Site",
    y = "Difference (gC/m2)"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(
  file.path(plot_dir, "cgrain_diff_boxplot_by_site.png"),
  p_box, width = 10, height = 5, dpi = 150
)

# 5) Faceted time series: both runs overlaid
plot_long <- rbind(
  data.frame(
    site = plot_df$site,
    year = plot_df$year,
    cgrain = plot_df$cgrain_sim2,
    run = "sim2",
    stringsAsFactors = FALSE
  ),
  data.frame(
    site = plot_df$site,
    year = plot_df$year,
    cgrain = plot_df$cgrain_sim4,
    run = "sim4",
    stringsAsFactors = FALSE
  )
)
p_overlay <- ggplot(plot_long, aes(x = year, y = cgrain, color = run)) +
  geom_line() +
  geom_point(size = 1.2) +
  scale_color_manual(values = c(sim2 = "#1b9e77", sim4 = "#d95f02")) +
  facet_wrap(~ site, scales = "free_y") +
  labs(
    title = "cgrain time series: simulateion2 vs simulateion4",
    x = "Year",
    y = "cgrain (gC/m2)",
    color = "Run"
  ) +
  theme_bw() +
  theme(strip.text = element_text(size = 8))
ggsave(
  file.path(plot_dir, "cgrain_overlay_timeseries_by_site.png"),
  p_overlay, width = 12, height = 8, dpi = 150
)

cat("\nWrote plots to:", plot_dir, "\n")
cat("  - cgrain_scatter_sim2_vs_sim4.png\n")
cat("  - cgrain_diff_timeseries_by_site.png\n")
cat("  - cgrain_absdiff_heatmap.png\n")
cat("  - cgrain_diff_boxplot_by_site.png\n")
cat("  - cgrain_overlay_timeseries_by_site.png\n")

# ---- Mean difference summaries and plots (by site and crop) ----
mean_by_site <- aggregate(
  cbind(mean_diff = diff_sim4_minus_sim2, mean_abs_diff = abs_diff) ~
    site,
  data = plot_df,
  FUN = mean,
  na.rm = TRUE
)
mean_by_site$n <- as.numeric(table(plot_df$site)[as.character(mean_by_site$site)])

mean_by_crop <- aggregate(
  cbind(mean_diff = diff_sim4_minus_sim2, mean_abs_diff = abs_diff) ~
    crop,
  data = plot_df,
  FUN = mean,
  na.rm = TRUE
)
mean_by_crop$n <- as.numeric(table(plot_df$crop)[as.character(mean_by_crop$crop)])

mean_by_site_crop <- aggregate(
  cbind(mean_diff = diff_sim4_minus_sim2, mean_abs_diff = abs_diff) ~
    site + crop,
  data = plot_df,
  FUN = mean,
  na.rm = TRUE
)
# counts for site x crop
site_crop_n <- as.data.frame(table(site = plot_df$site, crop = plot_df$crop), stringsAsFactors = FALSE)
names(site_crop_n)[3] <- "n"
mean_by_site_crop <- merge(mean_by_site_crop, site_crop_n, by = c("site", "crop"))
mean_by_site_crop <- mean_by_site_crop[mean_by_site_crop$n > 0, ]

write.csv(mean_by_site, file.path(out_dir, "2vs4_mean_diff_by_site.csv"), row.names = FALSE)
write.csv(mean_by_crop, file.path(out_dir, "2vs4_mean_diff_by_crop.csv"), row.names = FALSE)
write.csv(mean_by_site_crop, file.path(out_dir, "2vs4_mean_diff_by_site_crop.csv"), row.names = FALSE)

cat("\nMean difference by crop:\n")
print(mean_by_crop[order(-mean_by_crop$mean_abs_diff), ])

# Focus crops: corn and soybean
cm_sm <- plot_df[plot_df$crop %in% c("Corn (CM)", "Soybean (SM)"), , drop = FALSE]
mean_site_cm_sm <- mean_by_site_crop[
  mean_by_site_crop$crop %in% c("Corn (CM)", "Soybean (SM)"),
  ,
  drop = FALSE
]

# 6) Mean signed diff by site
p_mean_site <- ggplot(mean_by_site, aes(x = reorder(site, mean_diff), y = mean_diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_col(fill = "#2C7BB6") +
  coord_flip() +
  labs(
    title = "Mean cgrain difference by site (sim4 - sim2)",
    subtitle = paste0("Years ", year_min, "-", year_max, "; all crops"),
    x = "Site",
    y = "Mean difference (gC/m2)"
  ) +
  theme_bw()
ggsave(
  file.path(plot_dir, "cgrain_mean_diff_by_site.png"),
  p_mean_site, width = 8, height = 6, dpi = 150
)

# 7) Mean abs and signed diff by crop
p_mean_crop <- ggplot(mean_by_crop, aes(x = reorder(crop, mean_abs_diff), y = mean_diff, fill = crop)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_col() +
  coord_flip() +
  labs(
    title = "Mean cgrain difference by crop (sim4 - sim2)",
    subtitle = paste0("Years ", year_min, "-", year_max),
    x = "Crop",
    y = "Mean difference (gC/m2)",
    fill = "Crop"
  ) +
  theme_bw() +
  theme(legend.position = "none")
ggsave(
  file.path(plot_dir, "cgrain_mean_diff_by_crop.png"),
  p_mean_crop, width = 8, height = 5, dpi = 150
)

p_mean_abs_crop <- ggplot(mean_by_crop, aes(x = reorder(crop, mean_abs_diff), y = mean_abs_diff, fill = crop)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Mean absolute cgrain difference by crop |sim4 - sim2|",
    subtitle = paste0("Years ", year_min, "-", year_max),
    x = "Crop",
    y = "Mean |difference| (gC/m2)"
  ) +
  theme_bw() +
  theme(legend.position = "none")
ggsave(
  file.path(plot_dir, "cgrain_mean_absdiff_by_crop.png"),
  p_mean_abs_crop, width = 8, height = 5, dpi = 150
)

# 8) Mean diff by site for Corn and Soybean only
if (nrow(mean_site_cm_sm) > 0) {
  p_mean_site_cmsm <- ggplot(
    mean_site_cm_sm,
    aes(x = reorder(site, mean_diff), y = mean_diff, fill = crop)
  ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    coord_flip() +
    scale_fill_manual(values = c("Corn (CM)" = "#1b9e77", "Soybean (SM)" = "#d95f02")) +
    labs(
      title = "Mean cgrain difference by site: Corn (CM) vs Soybean (SM)",
      subtitle = paste0("Years ", year_min, "-", year_max, "; sim4 - sim2"),
      x = "Site",
      y = "Mean difference (gC/m2)",
      fill = "Crop"
    ) +
    theme_bw()
  ggsave(
    file.path(plot_dir, "cgrain_mean_diff_by_site_corn_soy.png"),
    p_mean_site_cmsm, width = 9, height = 6, dpi = 150
  )

  p_mean_abs_site_cmsm <- ggplot(
    mean_site_cm_sm,
    aes(x = reorder(site, mean_abs_diff), y = mean_abs_diff, fill = crop)
  ) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    coord_flip() +
    scale_fill_manual(values = c("Corn (CM)" = "#1b9e77", "Soybean (SM)" = "#d95f02")) +
    labs(
      title = "Mean |cgrain| difference by site: Corn (CM) vs Soybean (SM)",
      subtitle = paste0("Years ", year_min, "-", year_max, "; |sim4 - sim2|"),
      x = "Site",
      y = "Mean |difference| (gC/m2)",
      fill = "Crop"
    ) +
    theme_bw()
  ggsave(
    file.path(plot_dir, "cgrain_mean_absdiff_by_site_corn_soy.png"),
    p_mean_abs_site_cmsm, width = 9, height = 6, dpi = 150
  )
}

# 9) Faceted box/violin of year-level diffs for CM and SM
if (nrow(cm_sm) > 0) {
  p_cmsm_box <- ggplot(cm_sm, aes(x = crop, y = diff_sim4_minus_sim2, fill = crop)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_boxplot(outlier.size = 1.2) +
    scale_fill_manual(values = c("Corn (CM)" = "#1b9e77", "Soybean (SM)" = "#d95f02")) +
    labs(
      title = "cgrain difference distribution: Corn vs Soybean",
      subtitle = paste0("All sites, ", year_min, "-", year_max, "; sim4 - sim2"),
      x = NULL,
      y = "Difference (gC/m2)"
    ) +
    theme_bw() +
    theme(legend.position = "none")
  ggsave(
    file.path(plot_dir, "cgrain_diff_boxplot_corn_soy.png"),
    p_cmsm_box, width = 6, height = 5, dpi = 150
  )
}

cat("  - cgrain_mean_diff_by_site.png\n")
cat("  - cgrain_mean_diff_by_crop.png\n")
cat("  - cgrain_mean_absdiff_by_crop.png\n")
cat("  - cgrain_mean_diff_by_site_corn_soy.png\n")
cat("  - cgrain_mean_absdiff_by_site_corn_soy.png\n")
cat("  - cgrain_diff_boxplot_corn_soy.png\n")
