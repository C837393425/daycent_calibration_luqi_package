#!/usr/bin/env Rscript
#=======================================================================================  
#  PURPOSE:   Run GSA Step 4 (A & B): Combine Results and Calculate Sensitivity Indices
#             - Step 4A: Combine individual job group likelihood and aggregate results  
#             - Step 4B: Calculate first-order and total-order sensitivity indices
#             Uses generalized functions from bayesiancalibr with automatic aggregate detection
#
#  AUTHOR:    Claude Code (claude.ai/code)
#             Based on original implementation by Ram Gurung
#             Colorado State University
#             Natural Resource Ecology Laboratory
#
#  Project: Land-CRAFT DayCent Calibration Framework
#=======================================================================================  

# Default config path for interactive usage
# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Get config path (and optional GSA method) for library setup
# Usage: Rscript gsa_step4_combine_TSI_calculation.R <config.yaml> [gsa_method]
if (length(args) >= 1) {
    config_path <- args[1]
} else {
    stop("Usage: Rscript gsa_step4_combine_TSI_calculation.R <config.yaml> [gsa_method]")
    # config_path <- "workflows/configs/crop_yield_corn_all.yaml"
}

# Optional CLI method (e.g. soboljansen); defaults applied below if missing
gsa_method_cli <- if (length(args) >= 2 && !grepl("^--", args[2])) {
    args[2]
} else {
    NULL
}


# Load YAML to get custom library path (minimal setup)
if (!require(yaml, quietly = TRUE)) {
    stop("yaml package not available. Please run step0_r_setup.R first.")
}

# Load configuration
config <- yaml::read_yaml(config_path)

# Set custom library path (packages should already be installed by step0_r_setup.R)
if (!is.null(config$r_config$rlibpaths)) {
    .libPaths(new = c(config$r_config$rlibpaths, .libPaths()))
}

# Load required packages
suppressMessages({
    library(bayesiancalibr)
})

#=======================================================================================
# Configuration and Setup
#=======================================================================================

cat("========================================================================\n")
cat("GSA Step 4: Results Combination and Sensitivity Index Calculation\n")
cat("  Step 4A: Combine individual job group results\n")
cat("  Step 4B: Calculate first-order and total-order sensitivity indices\n")
cat("========================================================================\n")

# Display configuration
cat("Configuration:\n")
cat("  Root Directory:", config$paths$lairice_root, "\n")
cat("  Output Base:", config$paths$output_base, "\n")
cat("  Date Stamp:", config$project$date_stamp, "\n")
cat("  GSA Methods:", paste(config$gsa$gsa_methods, collapse = ", "), "\n")
cat("  Project Name:", config$project$name, "\n")
cat("  Target Variable:", config$target_variable, "\n")
cat("\n")

# Determine which GSA method to process
# Priority: CLI args[2] > gsa_method_override > soboljansen (6th list entry) > first method
if (!is.null(gsa_method_cli) && nzchar(gsa_method_cli)) {
    gsa_methods_to_process <- gsa_method_cli
} else if (exists("gsa_method_override") && !is.null(gsa_method_override)) {
    gsa_methods_to_process <- gsa_method_override
} else if ("soboljansen" %in% unlist(config$gsa$gsa_methods)) {
    gsa_methods_to_process <- "soboljansen"
} else if (length(config$gsa$gsa_methods) >= 6) {
    # Use soboljansen (6th method) as primary when present by position
    gsa_methods_to_process <- config$gsa$gsa_methods[[6]]
} else {
    gsa_methods_to_process <- config$gsa$gsa_methods[[1]]
}

cat("Processing GSA method(s):", paste(gsa_methods_to_process, collapse = ", "), "\n\n")

#=======================================================================================
# Run Individual Steps
#=======================================================================================

cat("=== Running GSA Step 4 (Parts A & B) ===\n")

# Step 4A: Combine Results
cat("--- Step 4A: Combining Results ---\n")
step4_results <- tryCatch({
    gsa_step4a_combine_results(
        config = config,
        gsa_method = gsa_methods_to_process[1],  
        verbose = TRUE
    )
}, error = function(e4) {
    cat("ERROR in Step 4A:", e4$message, "\n")
    stop("Cannot proceed without Step 4A completion")
})

cat("Step 4A completed successfully\n")
cat("  Likelihood files processed:", step4_results$n_likelihood_files, "\n")
cat("  Aggregate files processed:", step4_results$n_aggregate_files, "\n")

# Step 4B: Calculate Sensitivity Indices
cat("\n--- Step 4B: Calculating Sensitivity Indices ---\n")
step5_results <- tryCatch({
    gsa_step4b_calculate_sensitivity(
        config = config,
        gsa_methods = gsa_methods_to_process[1],  
        verbose = TRUE
    )
}, error = function(e5) {
    cat("ERROR in Step 4B:", e5$message, "\n")
    stop("Step 4B failed")
})

cat("Step 4B completed successfully\n")
if (!is.null(step5_results$first_order_indices)) {
    cat("  First-order indices:", nrow(step5_results$first_order_indices), "\n")
}
if (!is.null(step5_results$total_order_indices)) {
    cat("  Total-order indices:", nrow(step5_results$total_order_indices), "\n")
}

# Display key results
if (!is.null(step5_results$first_order_indices)) {
    cat("\n=== TOP 10 MOST SENSITIVE PARAMETERS (First-Order) ===\n")
    
    fsi <- step5_results$first_order_indices

    target_variable <- config$target_variable
    if (is.null(target_variable) || length(target_variable) == 0) {
        target_variable <- NULL
    } else {
        target_variable <- as.character(target_variable)[1]
        if (!nzchar(trimws(target_variable))) {
            target_variable <- NULL
        }
    }

    # Show top parameters for target variable if available
    if (!is.null(target_variable) && target_variable %in% fsi$Variable) {
        target_fsi <- fsi[fsi$Variable == target_variable, ]
        top_target <- target_fsi[order(-target_fsi$frstsi), ][1:min(10, nrow(target_fsi)), ]
        
        cat("For", config$target_variable, ":\n")
        print(top_target[, c("params", "frstsi", "frstsi.lci", "frstsi.uci")])
    } else {
        # Show top overall parameters
        top_overall <- fsi[order(-fsi$frstsi), ][1:min(10, nrow(fsi)), ]
        cat("Overall top parameters:\n")
        print(top_overall[, c("params", "Variable", "frstsi", "frstsi.lci", "frstsi.uci")])
    }
}

if (!is.null(step5_results$total_order_indices)) {
    cat("\n=== TOP 10 MOST SENSITIVE PARAMETERS (Total-Order) ===\n")
    
    tsi <- step5_results$total_order_indices
    
    # Show top parameters for target variable if available
    if (!is.null(target_variable) && target_variable %in% tsi$Variable) {
        target_tsi <- tsi[tsi$Variable == target_variable, ]
        top_target_total <- target_tsi[order(-target_tsi$totsi), ][1:min(10, nrow(target_tsi)), ]
        
        cat("For", config$target_variable, ":\n")
        print(top_target_total[, c("params", "totsi", "totsi.lci", "totsi.uci")])
    } else {
        # Show top overall parameters
        top_overall_total <- tsi[order(-tsi$totsi), ][1:min(10, nrow(tsi)), ]
        cat("Overall top parameters:\n")
        print(top_overall_total[, c("params", "Variable", "totsi", "totsi.lci", "totsi.uci")])
    }
}

# Interaction analysis
if (!is.null(step5_results$first_order_indices) && 
    !is.null(step5_results$total_order_indices)) {
    
    cat("\n=== PARAMETER INTERACTION ANALYSIS ===\n")
    
    # Focus on target emission variable if available
    target_var <- if (!is.null(target_variable) && target_variable %in% fsi$Variable) {
        target_variable
    } else {
        unique(fsi$Variable)[1]
    }
    
    fsi_target <- fsi[fsi$Variable == target_var, c("params", "frstsi")]
    tsi_target <- tsi[tsi$Variable == target_var, c("params", "totsi")]
    
    interaction_analysis <- merge(fsi_target, tsi_target, by = "params")
    interaction_analysis$interaction_effect <- interaction_analysis$totsi - interaction_analysis$frstsi
    interaction_analysis <- interaction_analysis[order(-interaction_analysis$interaction_effect), ]
    
    cat("For variable:", target_var, "\n")
    cat("Parameters with strongest interaction effects:\n")
    print(head(interaction_analysis, 5))
    
    # Summary statistics
    cat("\nSummary for", target_var, ":\n")
    cat("  Sum of first-order indices:", round(sum(interaction_analysis$frstsi), 3), "\n")
    cat("  Sum of total-order indices:", round(sum(interaction_analysis$totsi), 3), "\n")
    cat("  Total interaction effect:", round(sum(interaction_analysis$interaction_effect), 3), "\n")
    
    if (sum(interaction_analysis$totsi) > 1.2) {
        cat("  Note: Strong parameter interactions detected (sum of total indices > 1.2)\n")
    } else if (sum(interaction_analysis$totsi) > 1.05) {
        cat("  Note: Moderate parameter interactions detected\n")
    } else {
        cat("  Note: Parameters act mostly independently\n")
    }
}

cat("\n=== SUCCESS: GSA Step 4 (A & B) completed successfully ===\n")
cat("  Step 4A: Results combination complete\n")
cat("  Step 4B: Sensitivity index calculation complete\n")
cat("  Ready for Step 5: Quarto report generation\n")

