#' GSA Step 6: Plot Sensitivity Indices
#'
#' Creates comprehensive plots of first-order and total-order sensitivity indices
#' for Global Sensitivity Analysis results. Generates bar plots, comparison plots,
#' and parameter ranking visualizations.
#'
#' @param config Configuration list (typically loaded from YAML)
#' @param gsa_method GSA method name (e.g., "soboljansen", "sobol", etc.)
#' @param date_stamp Optional date stamp override (uses config if NULL)
#' @param output_base Optional output base directory override (uses config if NULL)
#' @param top_n Number of top parameters to display in plots (default 10)
#' @param save_plots Logical, whether to save plots to disk (default TRUE)
#' @param plot_format Plot format ("png", "pdf", "both") (default "png")
#' @param plot_width Plot width in inches (default 12)
#' @param plot_height Plot height in inches (default 8)
#' @param verbose Logical, whether to print progress messages (default TRUE)
#'
#' @return List containing:
#'   \item{plots}{List of ggplot objects created}
#'   \item{data}{Processed sensitivity indices data}
#'   \item{summary}{Summary statistics}
#'   \item{output_paths}{Paths where plots were saved (if save_plots = TRUE)}
#'
#' @importFrom ggplot2 ggplot aes geom_col geom_point geom_errorbar theme_minimal
#' @importFrom ggplot2 labs scale_fill_manual scale_color_manual coord_flip
#' @importFrom ggplot2 theme element_text facet_wrap ggsave
#' @importFrom dplyr filter arrange desc slice_max mutate
#' @importFrom tidyr pivot_longer
#' @export
gsa_step6_plot_sensitivity_indices <- function(config,
                                               gsa_method = "soboljansen",
                                               date_stamp = NULL,
                                               output_base = NULL,
                                               top_n = 10,
                                               save_plots = TRUE,
                                               plot_format = "png",
                                               plot_width = 12,
                                               plot_height = 8,
                                               verbose = TRUE) {
  
  # Load required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package is required for data manipulation")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("tidyr package is required for data reshaping")
  }
  
  # Validate inputs
  if (missing(config)) stop("Configuration object is required")
  
  # Set defaults from config
  if (is.null(date_stamp)) {
    if (config$project$date_stamp == "auto") {
      date_stamp <- format(Sys.Date(), "%d%b%Y")
    } else {
      date_stamp <- config$project$date_stamp
    }
  }
  
  if (is.null(output_base)) {
    output_base <- config$paths$output_base
  }
  
  # Setup paths
  lairice_root <- config$paths$lairice_root
  results_path <- file.path(lairice_root, output_base, date_stamp, "GSA", gsa_method, "Results")
  
  if (verbose) {
    cat("====================================================================\n")
    cat("GSA Step 6: Plotting Sensitivity Indices\n")
    cat("GSA Method:", gsa_method, "\n")
    cat("Date Stamp:", date_stamp, "\n")
    cat("Results Path:", results_path, "\n")
    cat("====================================================================\n")
  }
  
  # Load sensitivity indices
  fsi_file <- file.path(results_path, "NH3_First_Order_SI.rds")
  tsi_file <- file.path(results_path, "NH3_Total_Order_SI.rds")
  
  if (!file.exists(fsi_file)) {
    stop("First-order sensitivity indices file not found: ", fsi_file)
  }
  if (!file.exists(tsi_file)) {
    stop("Total-order sensitivity indices file not found: ", tsi_file)
  }
  
  fsi_data <- readRDS(fsi_file)
  tsi_data <- readRDS(tsi_file)
  
  if (verbose) {
    cat("Loaded sensitivity indices:\n")
    cat("  First-order indices:", nrow(fsi_data), "parameter-variable combinations\n")
    cat("  Total-order indices:", nrow(tsi_data), "parameter-variable combinations\n")
    cat("  Variables:", length(unique(fsi_data$Variable)), "\n")
    cat("  Parameters:", length(unique(fsi_data$params)), "\n")
  }
  
  # Prepare data for plotting
  fsi_plot_data <- fsi_data %>%
    dplyr::mutate(
      Index_Type = "First-Order",
      Index_Value = frstsi,
      Lower_CI = frstsi.lci,
      Upper_CI = frstsi.uci,
      Std_Error = frstsi.std.error
    ) %>%
    dplyr::select(params, Variable, Index_Type, Index_Value, Lower_CI, Upper_CI, Std_Error, GSA_Method)
  
  tsi_plot_data <- tsi_data %>%
    dplyr::mutate(
      Index_Type = "Total-Order",
      Index_Value = totsi,
      Lower_CI = totsi.lci,
      Upper_CI = totsi.uci,
      Std_Error = totsi.std.error
    ) %>%
    dplyr::select(params, Variable, Index_Type, Index_Value, Lower_CI, Upper_CI, Std_Error, GSA_Method)
  
  # Combine data
  combined_data <- rbind(fsi_plot_data, tsi_plot_data)
  
  # Create variable groups for better visualization
  combined_data <- combined_data %>%
    dplyr::mutate(
      Variable_Group = case_when(
        grepl("indNH3", Variable) ~ "Individual NH3 Likelihood",
        grepl("cumNH3", Variable) ~ "Cumulative NH3 Likelihood", 
        grepl("indUrea", Variable) ~ "Individual Urea Likelihood",
        Variable %in% c("NH3", "urea") ~ "Primary Outputs",
        Variable %in% c("N2O", "nit_N2O", "dnit_N2O", "dnit_N2", "NO") ~ "N Cycling",
        Variable %in% c("netMin1", "netMin2") ~ "N Mineralization",
        Variable %in% c("spHf", "crnf", "aglivc", "aglivn") ~ "Plant/Soil Variables",
        TRUE ~ "Other"
      ),
      Variable_Short = case_when(
        Variable == "indNH3_ln_independent_logLkhood" ~ "IndNH3_Independent",
        Variable == "indNH3_ln_logLkhood_rS" ~ "IndNH3_rS", 
        Variable == "indNH3_ln_logLkhood_rSY" ~ "IndNH3_rSY",
        Variable == "cumNH3_ln_independent_logLkhood" ~ "CumNH3_Independent",
        Variable == "cumNH3_ln_logLkhood_rS" ~ "CumNH3_rS",
        Variable == "cumNH3_ln_logLkhood_rSY" ~ "CumNH3_rSY",
        Variable == "indUrea_ln_independent_logLkhood" ~ "IndUrea_Independent", 
        Variable == "indUrea_ln_logLkhood_rS" ~ "IndUrea_rS",
        Variable == "indUrea_ln_logLkhood_rSY" ~ "IndUrea_rSY",
        TRUE ~ Variable
      )
    )
  
  # Initialize plots list
  plots <- list()
  output_paths <- list()
  
  # Plot 1: Top parameters by total-order sensitivity index (aggregated across variables)
  if (verbose) cat("Creating Plot 1: Top parameters by total-order sensitivity...\n")
  
  top_params_data <- tsi_plot_data %>%
    dplyr::group_by(params) %>%
    dplyr::summarise(
      Mean_TSI = mean(Index_Value, na.rm = TRUE),
      Max_TSI = max(Index_Value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(Mean_TSI)) %>%
    dplyr::slice_max(Mean_TSI, n = top_n)
  
  p1 <- ggplot2::ggplot(top_params_data, ggplot2::aes(x = reorder(params, Mean_TSI), y = Mean_TSI)) +
    ggplot2::geom_col(fill = "steelblue", alpha = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Top", top_n, "Parameters by Mean Total-Order Sensitivity Index"),
      subtitle = paste("GSA Method:", gsa_method, "| Date:", date_stamp),
      x = "Parameters",
      y = "Mean Total-Order Sensitivity Index",
      caption = "Averaged across all output variables"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 12)
    )
  
  plots$top_parameters <- p1
  
  # Plot 2: Comparison of First-Order vs Total-Order indices for key variables
  if (verbose) cat("Creating Plot 2: First-Order vs Total-Order comparison...\n")
  
  key_variables <- c("NH3", "urea", "N2O", "indNH3_ln_logLkhood_rS", "cumNH3_ln_logLkhood_rS")
  key_variables <- intersect(key_variables, unique(combined_data$Variable))
  
  comparison_data <- combined_data %>%
    dplyr::filter(Variable %in% key_variables, params %in% top_params_data$params[1:8]) %>%
    dplyr::mutate(
      params = factor(params, levels = top_params_data$params[1:8])
    )
  
  p2 <- ggplot2::ggplot(comparison_data, ggplot2::aes(x = params, y = Index_Value, fill = Index_Type)) +
    ggplot2::geom_col(position = "dodge", alpha = 0.7) +
    ggplot2::facet_wrap(~Variable_Short, scales = "free_y", ncol = 2) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("First-Order" = "lightcoral", "Total-Order" = "steelblue")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "First-Order vs Total-Order Sensitivity Indices",
      subtitle = "Key Variables and Top Parameters",
      x = "Parameters",
      y = "Sensitivity Index",
      fill = "Index Type"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      strip.text = ggplot2::element_text(size = 10, face = "bold"),
      legend.position = "bottom"
    )
  
  plots$comparison <- p2
  
  # Plot 3: Sensitivity indices with confidence intervals for NH3 variables
  if (verbose) cat("Creating Plot 3: NH3 variables with confidence intervals...\n")
  
  nh3_data <- combined_data %>%
    dplyr::filter(grepl("NH3|nh3", Variable, ignore.case = TRUE)) %>%
    dplyr::filter(params %in% top_params_data$params[1:6])
  
  p3 <- ggplot2::ggplot(nh3_data, ggplot2::aes(x = reorder(params, Index_Value), y = Index_Value, color = Index_Type)) +
    ggplot2::geom_point(size = 3, alpha = 0.7) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, alpha = 0.7) +
    ggplot2::facet_wrap(~Variable_Short, scales = "free", ncol = 2) +
    ggplot2::coord_flip() +
    ggplot2::scale_color_manual(values = c("First-Order" = "red", "Total-Order" = "blue")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "NH3-Related Variables: Sensitivity Indices with Confidence Intervals",
      subtitle = "Top 6 Parameters",
      x = "Parameters", 
      y = "Sensitivity Index",
      color = "Index Type"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      strip.text = ggplot2::element_text(size = 10, face = "bold"),
      legend.position = "bottom"
    )
  
  plots$nh3_confidence <- p3
  
  # Plot 4: Heatmap of sensitivity indices
  if (verbose) cat("Creating Plot 4: Sensitivity heatmap...\n")
  
  heatmap_data <- tsi_plot_data %>%
    dplyr::filter(params %in% top_params_data$params[1:10]) %>%
    dplyr::mutate(
      Variable_Short = case_when(
        Variable == "indNH3_ln_logLkhood_rS" ~ "IndNH3_rS",
        Variable == "cumNH3_ln_logLkhood_rS" ~ "CumNH3_rS", 
        Variable == "indUrea_ln_logLkhood_rS" ~ "IndUrea_rS",
        TRUE ~ Variable
      )
    ) %>%
    dplyr::filter(Variable_Short %in% c("NH3", "urea", "N2O", "IndNH3_rS", "CumNH3_rS", "IndUrea_rS"))
  
  p4 <- ggplot2::ggplot(heatmap_data, ggplot2::aes(x = Variable_Short, y = reorder(params, Index_Value), fill = Index_Value)) +
    ggplot2::geom_tile(alpha = 0.8) +
    ggplot2::scale_fill_gradient2(low = "white", mid = "yellow", high = "red", midpoint = 0.01) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Total-Order Sensitivity Index Heatmap",
      subtitle = "Top Parameters vs Key Variables",
      x = "Variables",
      y = "Parameters",
      fill = "Total SI"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.text = ggplot2::element_text(size = 10)
    )
  
  plots$heatmap <- p4
  
  # Save plots if requested
  if (save_plots) {
    plots_dir <- file.path(results_path, "plots")
    if (!dir.exists(plots_dir)) {
      dir.create(plots_dir, recursive = TRUE)
    }
    
    if (verbose) cat("Saving plots to:", plots_dir, "\n")
    
    plot_names <- names(plots)
    for (i in seq_along(plots)) {
      plot_name <- plot_names[i]
      
      if (plot_format %in% c("png", "both")) {
        png_path <- file.path(plots_dir, paste0("gsa_step6_", plot_name, ".png"))
        ggplot2::ggsave(png_path, plots[[i]], width = plot_width, height = plot_height, dpi = 300)
        output_paths[[paste0(plot_name, "_png")]] <- png_path
      }
      
      if (plot_format %in% c("pdf", "both")) {
        pdf_path <- file.path(plots_dir, paste0("gsa_step6_", plot_name, ".pdf"))
        ggplot2::ggsave(pdf_path, plots[[i]], width = plot_width, height = plot_height)
        output_paths[[paste0(plot_name, "_pdf")]] <- pdf_path
      }
    }
  }
  
  # Create summary statistics
  summary_stats <- list(
    n_parameters = length(unique(combined_data$params)),
    n_variables = length(unique(combined_data$Variable)),
    top_parameter = top_params_data$params[1],
    max_tsi = max(tsi_plot_data$Index_Value, na.rm = TRUE),
    max_fsi = max(fsi_plot_data$Index_Value, na.rm = TRUE),
    plots_created = length(plots)
  )
  
  if (verbose) {
    cat("====================================================================\n")
    cat("GSA Step 6 Complete\n")
    cat("Parameters analyzed:", summary_stats$n_parameters, "\n")
    cat("Variables analyzed:", summary_stats$n_variables, "\n")
    cat("Top parameter:", summary_stats$top_parameter, "\n")
    cat("Max Total SI:", round(summary_stats$max_tsi, 4), "\n")
    cat("Max First SI:", round(summary_stats$max_fsi, 4), "\n")
    cat("Plots created:", summary_stats$plots_created, "\n")
    cat("====================================================================\n")
  }
  
  return(list(
    plots = plots,
    data = combined_data,
    summary = summary_stats,
    output_paths = output_paths
  ))
}

#' GSA Step 7: Plot Likelihood Results
#'
#' Creates comprehensive plots of likelihood analysis results including goodness-of-fit
#' statistics, parameter relationships, and likelihood surfaces for Global Sensitivity Analysis.
#'
#' @param config Configuration list (typically loaded from YAML)
#' @param gsa_method GSA method name (e.g., "soboljansen", "sobol", etc.)
#' @param date_stamp Optional date stamp override (uses config if NULL)
#' @param output_base Optional output base directory override (uses config if NULL)
#' @param n_best Number of best parameter sets to highlight (default 250)
#' @param save_plots Logical, whether to save plots to disk (default TRUE)
#' @param plot_format Plot format ("png", "pdf", "both") (default "png")
#' @param plot_width Plot width in inches (default 12)
#' @param plot_height Plot height in inches (default 8)
#' @param verbose Logical, whether to print progress messages (default TRUE)
#'
#' @return List containing:
#'   \item{plots}{List of ggplot objects created}
#'   \item{data}{Processed likelihood data}
#'   \item{summary}{Summary statistics}
#'   \item{output_paths}{Paths where plots were saved (if save_plots = TRUE)}
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_density geom_histogram theme_minimal
#' @importFrom ggplot2 labs scale_color_manual scale_fill_manual facet_wrap ggsave
#' @importFrom ggplot2 theme element_text geom_smooth stat_density_2d
#' @importFrom dplyr filter arrange desc slice_max mutate select
#' @export
gsa_step7_plot_results_likelihood <- function(config,
                                              gsa_method = "soboljansen",
                                              date_stamp = NULL,
                                              output_base = NULL,
                                              n_best = 250,
                                              save_plots = TRUE,
                                              plot_format = "png",
                                              plot_width = 12,
                                              plot_height = 8,
                                              verbose = TRUE) {
  
  # Load required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package is required for data manipulation")
  }
  
  # Validate inputs
  if (missing(config)) stop("Configuration object is required")
  
  # Set defaults from config
  if (is.null(date_stamp)) {
    if (config$project$date_stamp == "auto") {
      date_stamp <- format(Sys.Date(), "%d%b%Y")
    } else {
      date_stamp <- config$project$date_stamp
    }
  }
  
  if (is.null(output_base)) {
    output_base <- config$paths$output_base
  }
  
  # Setup paths
  lairice_root <- config$paths$lairice_root
  results_path <- file.path(lairice_root, output_base, date_stamp, "GSA", gsa_method, "Results")
  
  if (verbose) {
    cat("====================================================================\n")
    cat("GSA Step 7: Plotting Likelihood Results\n")
    cat("GSA Method:", gsa_method, "\n")
    cat("Date Stamp:", date_stamp, "\n")
    cat("Results Path:", results_path, "\n")
    cat("====================================================================\n")
  }
  
  # Load likelihood and aggregate data
  lkhd_file <- file.path(results_path, paste0("Likelihood_Combined_", gsa_method, ".rds"))
  aggr_file <- file.path(results_path, paste0("Aggregated_Combined_", gsa_method, ".rds"))
  
  if (!file.exists(lkhd_file)) {
    stop("Combined likelihood file not found: ", lkhd_file)
  }
  if (!file.exists(aggr_file)) {
    stop("Combined aggregate file not found: ", aggr_file)
  }
  
  lkhd_data <- readRDS(lkhd_file)
  aggr_data <- readRDS(aggr_file)
  
  # Load MC draws to get parameter values
  mc_file <- file.path(dirname(results_path), paste0("mc_GSA_draw_", gsa_method, ".rds"))
  if (!file.exists(mc_file)) {
    stop("MC draw file not found: ", mc_file)
  }
  mc_data <- readRDS(mc_file)
  
  if (verbose) {
    cat("Loaded data:\n")
    cat("  Likelihood records:", nrow(lkhd_data), "\n")
    cat("  Aggregate records:", nrow(aggr_data), "\n")
    cat("  MC parameter draws:", nrow(mc_data), "\n")
    cat("  Variables in likelihood:", length(unique(lkhd_data$Variable)), "\n")
  }
  
  # Prepare likelihood data for plotting
  # Focus on key likelihood components
  lkhd_plot_data <- lkhd_data %>%
    dplyr::filter(Variable %in% c("Individual-NH3", "Cumulative-NH3", "Individual-Urea")) %>%
    dplyr::select(SampleID, Variable, ln_logLkhood_rS, ln_logLkhood_rSY, ln_independent_logLkhood,
                  Pearson_Correlation, Bayesian_R2, RMSE, Bias) %>%
    dplyr::mutate(
      Variable_Short = case_when(
        Variable == "Individual-NH3" ~ "IndNH3",
        Variable == "Cumulative-NH3" ~ "CumNH3", 
        Variable == "Individual-Urea" ~ "IndUrea",
        TRUE ~ Variable
      )
    )
  
  # Merge with parameter data
  combined_data <- merge(lkhd_plot_data, mc_data, by = "SampleID")
  
  # Calculate combined likelihood for ranking
  likelihood_summary <- lkhd_plot_data %>%
    dplyr::group_by(SampleID) %>%
    dplyr::summarise(
      Combined_ln_rS = sum(ln_logLkhood_rS, na.rm = TRUE),
      Combined_ln_rSY = sum(ln_logLkhood_rSY, na.rm = TRUE),
      Combined_ln_independent = sum(ln_independent_logLkhood, na.rm = TRUE),
      Mean_R2 = mean(Bayesian_R2, na.rm = TRUE),
      Mean_Correlation = mean(Pearson_Correlation, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(Combined_ln_rS))
  
  # Identify best parameter sets
  best_samples <- likelihood_summary$SampleID[1:min(n_best, nrow(likelihood_summary))]
  
  # Add ranking to combined data
  combined_data <- combined_data %>%
    dplyr::left_join(likelihood_summary, by = "SampleID") %>%
    dplyr::mutate(
      Is_Best = SampleID %in% best_samples,
      Rank_Category = case_when(
        SampleID %in% best_samples[1:50] ~ "Top 50",
        SampleID %in% best_samples[1:100] ~ "Top 100",
        SampleID %in% best_samples ~ paste("Top", n_best),
        TRUE ~ "Others"
      )
    )
  
  # Initialize plots list
  plots <- list()
  output_paths <- list()
  
  # Plot 1: Likelihood distribution by variable
  if (verbose) cat("Creating Plot 1: Likelihood distributions...\n")
  
  p1 <- ggplot2::ggplot(lkhd_plot_data, ggplot2::aes(x = ln_logLkhood_rS, fill = Variable_Short)) +
    ggplot2::geom_density(alpha = 0.6) +
    ggplot2::facet_wrap(~Variable_Short, scales = "free") +
    ggplot2::scale_fill_manual(values = c("IndNH3" = "red", "CumNH3" = "blue", "IndUrea" = "green")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Log-Likelihood Distributions (rS Method)",
      subtitle = paste("GSA Method:", gsa_method, "| Date:", date_stamp),
      x = "Log-Likelihood (rS)",
      y = "Density",
      fill = "Variable"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      strip.text = ggplot2::element_text(size = 10, face = "bold"),
      legend.position = "bottom"
    )
  
  plots$likelihood_distributions <- p1
  
  # Plot 2: Goodness-of-fit metrics
  if (verbose) cat("Creating Plot 2: Goodness-of-fit metrics...\n")
  
  gof_data <- combined_data %>%
    dplyr::select(SampleID, Variable_Short, Pearson_Correlation, Bayesian_R2, RMSE, Is_Best) %>%
    tidyr::pivot_longer(cols = c(Pearson_Correlation, Bayesian_R2), 
                        names_to = "Metric", values_to = "Value")
  
  p2 <- ggplot2::ggplot(gof_data, ggplot2::aes(x = Value, fill = Is_Best)) +
    ggplot2::geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
    ggplot2::facet_grid(Metric ~ Variable_Short, scales = "free") +
    ggplot2::scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "gray"), 
                               labels = c("TRUE" = paste("Best", n_best), "FALSE" = "Others")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Goodness-of-Fit Metrics Distribution",
      subtitle = paste("Highlighting Best", n_best, "Parameter Sets"),
      x = "Metric Value",
      y = "Count",
      fill = "Parameter Set"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      strip.text = ggplot2::element_text(size = 9, face = "bold"),
      legend.position = "bottom"
    )
  
  plots$goodness_of_fit <- p2
  
  # Plot 3: Parameter relationships for best sets
  if (verbose) cat("Creating Plot 3: Parameter relationships...\n")
  
  # Select key parameters for plotting
  key_params <- c("urea_km", "vmax_cap", "urea_st0", "nh3_vmax_cap", "urea_kui", "nh3_st0")
  available_params <- intersect(key_params, names(combined_data))
  
  if (length(available_params) >= 2) {
    param_data <- combined_data %>%
      dplyr::filter(Variable_Short == "IndNH3") %>%  # Use one variable to avoid duplicates
      dplyr::select(SampleID, all_of(available_params[1:min(4, length(available_params))]), 
                    Combined_ln_rS, Is_Best, Rank_Category)
    
    # Create scatter plot for first two parameters
    p3 <- ggplot2::ggplot(param_data, ggplot2::aes_string(x = available_params[1], y = available_params[2], 
                                                         color = "Rank_Category", size = "Combined_ln_rS")) +
      ggplot2::geom_point(alpha = 0.6) +
      ggplot2::scale_color_manual(values = setNames(c("red", "orange", "yellow", "gray"), 
                                                     c("Top 50", "Top 100", paste("Top", n_best), "Others"))) +
      ggplot2::scale_size_continuous(range = c(0.5, 3)) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Parameter Space Exploration",
        subtitle = paste("Colored by Performance Ranking"),
        x = available_params[1],
        y = available_params[2],
        color = "Rank Category",
        size = "Combined Log-Likelihood"
      ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 14, face = "bold"),
        legend.position = "right"
      )
    
    plots$parameter_space <- p3
  }
  
  # Plot 4: Likelihood vs model performance
  if (verbose) cat("Creating Plot 4: Likelihood vs performance...\n")
  
  performance_data <- combined_data %>%
    dplyr::filter(Variable_Short == "IndNH3") %>%
    dplyr::select(SampleID, ln_logLkhood_rS, Pearson_Correlation, Bayesian_R2, RMSE, Is_Best)
  
  p4 <- ggplot2::ggplot(performance_data, ggplot2::aes(x = ln_logLkhood_rS, y = Bayesian_R2, color = Is_Best)) +
    ggplot2::geom_point(alpha = 0.6, size = 1.5) +
    ggplot2::geom_smooth(method = "lm", se = TRUE, alpha = 0.3) +
    ggplot2::scale_color_manual(values = c("TRUE" = "red", "FALSE" = "gray"),
                               labels = c("TRUE" = paste("Best", n_best), "FALSE" = "Others")) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Log-Likelihood vs Bayesian R²",
      subtitle = "Individual NH3 Variable",
      x = "Log-Likelihood (rS)",
      y = "Bayesian R²",
      color = "Parameter Set"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      legend.position = "bottom"
    )
  
  plots$likelihood_vs_performance <- p4
  
  # Plot 5: Combined likelihood ranking
  if (verbose) cat("Creating Plot 5: Combined likelihood ranking...\n")
  
  ranking_data <- likelihood_summary %>%
    dplyr::mutate(Rank = 1:nrow(likelihood_summary)) %>%
    dplyr::slice_head(n = 1000)  # Show top 1000 for clarity
  
  p5 <- ggplot2::ggplot(ranking_data, ggplot2::aes(x = Rank, y = Combined_ln_rS)) +
    ggplot2::geom_point(alpha = 0.6, color = "steelblue") +
    ggplot2::geom_vline(xintercept = n_best, color = "red", linetype = "dashed", size = 1) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Parameter Set Ranking by Combined Log-Likelihood",
      subtitle = paste("Red line shows cutoff for best", n_best, "sets"),
      x = "Rank",
      y = "Combined Log-Likelihood (rS)",
      caption = "Showing top 1000 parameter sets"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold")
    )
  
  plots$likelihood_ranking <- p5
  
  # Save plots if requested
  if (save_plots) {
    plots_dir <- file.path(results_path, "plots")
    if (!dir.exists(plots_dir)) {
      dir.create(plots_dir, recursive = TRUE)
    }
    
    if (verbose) cat("Saving plots to:", plots_dir, "\n")
    
    plot_names <- names(plots)
    for (i in seq_along(plots)) {
      plot_name <- plot_names[i]
      
      if (plot_format %in% c("png", "both")) {
        png_path <- file.path(plots_dir, paste0("gsa_step7_", plot_name, ".png"))
        ggplot2::ggsave(png_path, plots[[i]], width = plot_width, height = plot_height, dpi = 300)
        output_paths[[paste0(plot_name, "_png")]] <- png_path
      }
      
      if (plot_format %in% c("pdf", "both")) {
        pdf_path <- file.path(plots_dir, paste0("gsa_step7_", plot_name, ".pdf"))
        ggplot2::ggsave(pdf_path, plots[[i]], width = plot_width, height = plot_height)
        output_paths[[paste0(plot_name, "_pdf")]] <- pdf_path
      }
    }
  }
  
  # Create summary statistics
  summary_stats <- list(
    n_samples = nrow(likelihood_summary),
    n_variables = length(unique(lkhd_plot_data$Variable)),
    best_likelihood = max(likelihood_summary$Combined_ln_rS, na.rm = TRUE),
    mean_r2_best = mean(combined_data$Mean_R2[combined_data$Is_Best], na.rm = TRUE),
    mean_correlation_best = mean(combined_data$Mean_Correlation[combined_data$Is_Best], na.rm = TRUE),
    plots_created = length(plots)
  )
  
  if (verbose) {
    cat("====================================================================\n")
    cat("GSA Step 7 Complete\n")
    cat("Samples analyzed:", summary_stats$n_samples, "\n")
    cat("Variables analyzed:", summary_stats$n_variables, "\n")
    cat("Best combined likelihood:", round(summary_stats$best_likelihood, 2), "\n")
    cat("Mean R² (best sets):", round(summary_stats$mean_r2_best, 3), "\n")
    cat("Mean correlation (best sets):", round(summary_stats$mean_correlation_best, 3), "\n")
    cat("Plots created:", summary_stats$plots_created, "\n")
    cat("====================================================================\n")
  }
  
  return(list(
    plots = plots,
    data = list(
      likelihood = lkhd_plot_data,
      combined = combined_data,
      summary = likelihood_summary
    ),
    summary = summary_stats,
    output_paths = output_paths
  ))
}
