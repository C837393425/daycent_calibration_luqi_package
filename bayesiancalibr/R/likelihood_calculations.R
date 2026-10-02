
getDIC <- function(model) {
  # Extract the deviance at the posterior mean (fixed and random effects)
  deviance_mean <- as.numeric(-2 * logLik(model))
  
  # Extract the predicted values and calculate the residual sum of squares (RSS)
  y <- model@frame[[1]]
  fitted <- fitted(model)
  rss <- sum((y - fitted)^2)
  
  # Estimate the effective number of parameters (pD)
  # Assuming pD ~ variance of the deviance across iterations
  n_params <- length(fixef(model)) + length(ranef(model))
  
  # Approximate posterior mean deviance by adjusting with residuals
  mean_deviance <- deviance_mean + rss / length(y)
  
  # Calculate DIC
  dic <- mean_deviance + n_params
  
  return(dic)
}


calculate_gofs <- function(merged_data){
  #=======================================================================================
  #  PURPOSE:   Calculate Goodness-of-Fit-Statistics (GOFS), including log-Likelihood. 
  #             A Linear Mixed Effect (LME) fit with zero intercept and no fixed effect, 
  #             (i.e. variance structure only) will be fitted for the measured - modeled
  #             residual.
  #
  #  Input Arguments:      
  #      - merged_data:    a data.frame object with measured and modeled values and 
  #                        grouping infomation with at least the following columns:
  #                         - obs             Observed data (untransformed)
  #                         - mod             Modeled data (untransformed)
  #                         - resi            Residuals (Observed - Modeled) (untransformed)
  #                         - ln_obs          ln(Observed + 1) (Natural-log transformed)
  #                         - ln_mod          ln(Modeled + 1) (Natural-log transformed)
  #                         - ln_resi         ln(Observed + 1) - ln(Modeled + 1)      
  #
  #  Output: 
  #   
  #     - Rslt       a data.frame object with Goodness of Fit Statistics:
  #
  #=======================================================================================
  # EXAMPLE-1: merged_data = indNH3_data
  # EXAMPLE-2: merged_data = cumNH3_data
  # EXAMPLE-3: merged_data = indUrea_data
  
  #===========================================================================
  # Calculate Goodness-of-fit for evaluation dataset
  result <- tryCatch({
    
    
    # If rSite is present in merged_data, replace siteID with rSite
    # We will use rsite as random factor instead of siteID
    
    if ("rSite" %in% names(merged_data)) {
      
      # Drop the original siteID column (no error if it doesn't exist)
      merged_data$siteID <- NULL
      
      # Rename rSite to siteID
      names(merged_data)[names(merged_data) == "rSite"] <- "siteID"
    }
    
    
    n1 = nrow(merged_data)
    
    merged_data = merged_data[!is.na(merged_data$ln_resi), ]
    n2 = nrow(merged_data)
    
    PCor = cor(merged_data$obs, merged_data$mod)
    BayesianR2 = var(merged_data$mod) / (var(merged_data$mod) + var(merged_data$resi))
    RMSE = sqrt(mean(merged_data$resi^2))
    Bias = mean(merged_data$resi)
    
    ln_PCor = cor(merged_data$ln_obs, merged_data$ln_mod)
    ln_BayesianR2 = var(merged_data$ln_mod) / (var(merged_data$ln_mod) + var(merged_data$ln_resi))
    ln_RMSE = sqrt(mean(merged_data$ln_resi^2))
    ln_Bias = mean(merged_data$ln_resi)
    
    #===========================================================================
    # Calculate likelihood with independent assumptions
    ind_sigma           = sqrt(mean(merged_data$resi^2)) 
    ind_logLkhood       = -n2 * log(ind_sigma) - (1 / (2 * ind_sigma^2)) * sum(merged_data$resi^2)
    
    ln_ind_sigma        = sqrt(mean(merged_data$ln_resi^2)) 
    ln_ind_logLkhood    = -n2 * log(ln_ind_sigma) - (1 / (2 * ln_ind_sigma^2)) * sum(merged_data$ln_resi^2)
    
    
    #===========================================================================
    # Initialise all mixed-model outputs as NA, so a model that is skipped
    # (missing columns) or fails simply stays NA and the other model is unaffected
    #===========================================================================
    stat_rS  <- c("logLkhood", "sigma_site", "sigma_Resi", "AIC", "BIC", "DIC", "RMSE", "MAE")
    stat_rSY <- c("logLkhood", "sigma_site", "sigma_siteyr", "sigma_Resi", "AIC", "BIC", "DIC", "RMSE", "MAE")
    for (v in c(paste0(stat_rS,  "_rS"),  paste0("ln_", stat_rS,  "_rS"),
                paste0(stat_rSY, "_rSY"), paste0("ln_", stat_rSY, "_rSY"))) {
      assign(v, NA)
    }
    
        
    #===========================================================================
    # Calculate likelihood with random site/Season model (rSY = random Site/Season)
    # Protected with tryCatch to handle cases where nested random effects are not feasible
    #===========================================================================
    rS_success <- FALSE
    if ("siteID" %in% names(merged_data)) {
      rS_success <- tryCatch({
        
        lmerFit_rS          = lmer(resi ~ -1 + (1|siteID), data = merged_data)
        varcor_rS           = VarCorr(lmerFit_rS)
        sigmas_rS           = as.data.frame(varcor_rS)[,c(1,5)]
        sigma_site_rS       = sigmas_rS[sigmas_rS$grp == "siteID", 2]
        sigma_Resi_rS       = sigmas_rS[sigmas_rS$grp == "Residual", 2]
        logLkhood_rS        = logLik(lmerFit_rS)
        AIC_rS              = AIC(lmerFit_rS)
        BIC_rS              = BIC(lmerFit_rS)
        DIC_rS              = getDIC(lmerFit_rS)
        RMSE_rS             = sqrt(mean(residuals(lmerFit_rS)^2))
        MAE_rS              = mean(abs(residuals(lmerFit_rS)))
        
        ln_lmerFit_rS       = lmer(ln_resi ~ -1 + (1|siteID), data = merged_data)
        ln_varcor_rS        = VarCorr(ln_lmerFit_rS)
        ln_sigmas_rS        = as.data.frame(ln_varcor_rS)[,c(1,5)]
        ln_sigma_site_rS    = ln_sigmas_rS[ln_sigmas_rS$grp == "siteID", 2]
        ln_sigma_Resi_rS    = ln_sigmas_rS[ln_sigmas_rS$grp == "Residual", 2]
        ln_logLkhood_rS     = logLik(ln_lmerFit_rS)
        ln_AIC_rS           = AIC(ln_lmerFit_rS)
        ln_BIC_rS           = BIC(ln_lmerFit_rS)
        ln_DIC_rS           = getDIC(ln_lmerFit_rS)
        ln_RMSE_rS          = sqrt(mean(residuals(ln_lmerFit_rS)^2))
        ln_MAE_rS           = mean(abs(residuals(ln_lmerFit_rS)))
        
        TRUE
      }, error = function(e) {
        message("Random site model (rS) failed: ", conditionMessage(e))
        FALSE
      })
    }
    
    #===========================================================================
    # Random site/Season model (rSY): run only if siteID AND SeasonID are present
    #===========================================================================
    rSY_success <- FALSE
    if (all(c("siteID", "SeasonID") %in% names(merged_data))) {
      rSY_success <- tryCatch({
        
        lmerFit_rSY          = lmer(resi ~ -1 + (1|siteID/SeasonID), data = merged_data)
        varcor_rSY           = VarCorr(lmerFit_rSY)
        sigmas_rSY           = as.data.frame(varcor_rSY)[,c(1,5)]
        sigma_site_rSY       = sigmas_rSY[sigmas_rSY$grp == "siteID", 2]
        sigma_siteyr_rSY     = sigmas_rSY[sigmas_rSY$grp == "SeasonID:siteID", 2]
        sigma_Resi_rSY       = sigmas_rSY[sigmas_rSY$grp == "Residual", 2]
        logLkhood_rSY        = logLik(lmerFit_rSY)
        AIC_rSY              = AIC(lmerFit_rSY)
        BIC_rSY              = BIC(lmerFit_rSY)
        DIC_rSY              = getDIC(lmerFit_rSY)
        RMSE_rSY             = sqrt(mean(residuals(lmerFit_rSY)^2))
        MAE_rSY              = mean(abs(residuals(lmerFit_rSY)))
        
        ln_lmerFit_rSY       = lmer(ln_resi ~ -1 + (1|siteID/SeasonID), data = merged_data)
        ln_varcor_rSY        = VarCorr(ln_lmerFit_rSY)
        ln_sigmas_rSY        = as.data.frame(ln_varcor_rSY)[,c(1,5)]
        ln_sigma_site_rSY    = ln_sigmas_rSY[ln_sigmas_rSY$grp == "siteID", 2]
        ln_sigma_siteyr_rSY  = ln_sigmas_rSY[ln_sigmas_rSY$grp == "SeasonID:siteID", 2]
        ln_sigma_Resi_rSY    = ln_sigmas_rSY[ln_sigmas_rSY$grp == "Residual", 2]
        ln_logLkhood_rSY     = logLik(ln_lmerFit_rSY)
        ln_AIC_rSY           = AIC(ln_lmerFit_rSY)
        ln_BIC_rSY           = BIC(ln_lmerFit_rSY)
        ln_DIC_rSY           = getDIC(ln_lmerFit_rSY)
        ln_RMSE_rSY          = sqrt(mean(residuals(ln_lmerFit_rSY)^2))
        ln_MAE_rSY           = mean(abs(residuals(ln_lmerFit_rSY)))
        
        TRUE
      }, error = function(e) {
        message("Random site/season model (rSY) failed: ", conditionMessage(e))
        FALSE
      })
    }
    
    
    #===========================================================================
    Rslt <- data.frame("total_data_size"              = n1,
                       "used_data_size"               = n2,
                       "Pearson_Correlation"          = PCor,    
                       "Bayesian_R2"                  = BayesianR2,
                       "RMSE"                         = RMSE,
                       "Bias"                         = Bias, 
                       "ln_PCor"                      = ln_PCor, 
                       "ln_BayesianR2"                = ln_BayesianR2,
                       "ln_RMSE"                      = ln_RMSE,
                       "ln_Bias"                      = ln_Bias,
                       
                       # log-likelihood estimates
                       "independent_logLkhood"        = ind_logLkhood,
                       "independent_sigma"            = ind_sigma,
                       "ln_independent_logLkhood"     = ln_ind_logLkhood,
                       "ln_independent_sigma"         = ln_ind_sigma,
                       
                       "logLkhood_rS"                 = logLkhood_rS,
                       "sigma_site_rS"                = sigma_site_rS,
                       "sigma_Resi_rS"                = sigma_Resi_rS,
                       "AIC_rS"                       = AIC_rS,
                       "BIC_rS"                       = BIC_rS,
                       "DIC_rS"                       = DIC_rS,
                       "RMSE_rS"                      = RMSE_rS,
                       "MAE_rS"                       = MAE_rS,
                       
                       "ln_logLkhood_rS"              = ln_logLkhood_rS,
                       "ln_sigma_site_rS"             = ln_sigma_site_rS,
                       "ln_sigma_Resi_rS"             = ln_sigma_Resi_rS,
                       "ln_AIC_rS"                    = ln_AIC_rS,
                       "ln_BIC_rS"                    = ln_BIC_rS,
                       "ln_DIC_rS"                    = ln_DIC_rS,
                       "ln_RMSE_rS"                   = ln_RMSE_rS,
                       "ln_MAE_rS"                    = ln_MAE_rS,
                       
                       "logLkhood_rSY"                = logLkhood_rSY,
                       "sigma_site_rSY"               = sigma_site_rSY,
                       "sigma_siteyr_rSY"             = sigma_siteyr_rSY,
                       "sigma_Resi_rSY"               = sigma_Resi_rSY,
                       "AIC_rSY"                      = AIC_rSY,
                       "BIC_rSY"                      = BIC_rSY,
                       "DIC_rSY"                      = DIC_rSY,
                       "RMSE_rSY"                     = RMSE_rSY,
                       "MAE_rSY"                      = MAE_rSY,
                       
                       "ln_logLkhood_rSY"             = ln_logLkhood_rSY,
                       "ln_sigma_site_rSY"            = ln_sigma_site_rSY,
                       "ln_sigma_siteyr_rSY"          = ln_sigma_siteyr_rSY,
                       "ln_sigma_Resi_rSY"            = ln_sigma_Resi_rSY,
                       "ln_AIC_rSY"                   = ln_AIC_rSY,
                       "ln_BIC_rSY"                   = ln_BIC_rSY,
                       "ln_DIC_rSY"                   = ln_DIC_rSY,
                       "ln_RMSE_rSY"                  = ln_RMSE_rSY,
                       "ln_MAE_rSY"                   = ln_MAE_rSY,
                       
                       "Status"                       = 0,
                       "Comment"                      = "No Error")
    cat("Goodness-of-fit-statistics calculated successfully....\n")
    return(Rslt)
  }, error = function(e) {
    
    Rslt <- data.frame("total_data_size"              = 0,
                       "used_data_size"               = 0,
                       "Pearson_Correlation"          = 0,    
                       "Bayesian_R2"                  = 0,
                       "RMSE"                         = 0,
                       "Bias"                         = 0, 
                       "ln_PCor"                      = 0, 
                       "ln_BayesianR2"                = 0,
                       "ln_RMSE"                      = 0,
                       "ln_Bias"                      = 0,
                       
                       # log-likelihood estimates
                       "independent_logLkhood"        = 0,
                       "independent_sigma"            = 0,
                       "ln_independent_logLkhood"     = 0,
                       "ln_independent_sigma"         = 0,
                       
                       "logLkhood_rS"                 = 0,
                       "sigma_site_rS"                = 0,
                       "sigma_Resi_rS"                = 0,
                       "AIC_rS"                       = 0,
                       "BIC_rS"                       = 0,
                       "DIC_rS"                       = 0,
                       "RMSE_rS"                      = 0,
                       "MAE_rS"                       = 0,
                       
                       "ln_logLkhood_rS"              = 0,
                       "ln_sigma_site_rS"             = 0,
                       "ln_sigma_Resi_rS"             = 0,
                       "ln_AIC_rS"                    = 0,
                       "ln_BIC_rS"                    = 0,
                       "ln_DIC_rS"                    = 0,
                       "ln_RMSE_rS"                   = 0,
                       "ln_MAE_rS"                    = 0,
                       
                       "logLkhood_rSY"                = 0,
                       "sigma_site_rSY"               = 0,
                       "sigma_siteyr_rSY"             = 0,
                       "sigma_Resi_rSY"               = 0,
                       "AIC_rSY"                      = 0,
                       "BIC_rSY"                      = 0,
                       "DIC_rSY"                      = 0,
                       "RMSE_rSY"                     = 0,
                       "MAE_rSY"                      = 0,
                       
                       "ln_logLkhood_rSY"             = 0,
                       "ln_sigma_site_rSY"            = 0,
                       "ln_sigma_siteyr_rSY"          = 0,
                       "ln_sigma_Resi_rSY"            = 0,
                       "ln_AIC_rSY"                   = 0,
                       "ln_BIC_rSY"                   = 0,
                       "ln_DIC_rSY"                   = 0,
                       "ln_RMSE_rSY"                  = 0,
                       "ln_MAE_rSY"                   = 0,
                       
                       "Status"                       = 1,
                       "Comment"                      = as.character(e$message))
    
    cat("Error occured during Goodness-of-fit-statistics calculation....\n")
    message("Error: ", conditionMessage(e), "\n")
    
    return(Rslt)
  })
  
  return(result)
}






