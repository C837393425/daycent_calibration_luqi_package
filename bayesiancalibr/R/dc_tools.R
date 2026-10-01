#=======================================================================================  
#  FILENAME:  dc_tools.R
#
#  PURPOSE:   A collection of R function for LAI-Rice rev-53 to:
#               - update model parameters
#               - read and organize model outputs
#               - ToDo: more tools
#
#  AUTHOR:    Ram Gurung (06 November 2023) updated from Dissertation Work
#             Natural Resource Ecology Laboratory
#             Colorado State University
#=======================================================================================

update_fix100_parameters  <- function(paramsdf, save_copy = FALSE){
  # PURPOSE: update fix.100 with new set of parameter values.
  #
  # ARGUMENTS:
  #
  # paramsdf      a data frame with two columns as follows:
  #                 - File:      File name for the parameters
  #                 - Parameter: Name of the parameter
  #                 - value:     value for the parameters
  #
  # save_copy     a logical indicating if a copy of old fert.100 to be saved.
  #                 - defalut to FALSE. 
  #
  #---------------------------------------------------------------------------
  # Set options to print numbers in regular format
  options(scipen = 999, digits = 10)
  
  tryCatch({
    
    if(!file.exists("fix.100")){stop("fix.100 file not found in ", getwd())}
    
    fix100    = readLines("fix.100")
    paramsdf2 = paramsdf[paramsdf$File %in% c("fix.100"), ]
    
    if(!is.null(paramsdf2) & nrow(paramsdf2) > 0){
      
      params_name  = paramsdf2$Parameter
      params_value = paramsdf2$value
      
      for(pars in params_name){
        
        temp_pos = grep(glob2rx(paste0("*", pars, "*")), fix100)
        if(length(temp_pos) != 1){
          
          stop("---- parater ", pars, " found in ", length(temp_pos), " lines.")
          
        }
        
        temp_params  = params_value[which(params_name == pars)]
        ncharVal     = nchar(temp_params)
        # Ensure minimum spacing and handle long parameter values
        spaces_needed = max(1, 15 - ncharVal)  # Minimum 1 space
        white_spaces = paste0(rep(" ", spaces_needed), collapse = "")
        
        fix100[temp_pos] = paste0(temp_params, white_spaces, pars)
        
      }
    }
    
    
    if(save_copy){
      
      old_fix100 = paste0("old_fix_", str_replace_all(format(Sys.Date(), "%d %b %Y"), "\\s", ""), ".100")
      file.copy("fix.100", old_fix100, overwrite = TRUE)
      
    }
    
    if(fix100[length(fix100)] != ""){
      
      fix100 = c(fix100, "")
      
    }
    
    file.remove("fix.100")
    con_fix100 = file("fix.100", open='w')
    writeLines(fix100, con_fix100)
    close(con_fix100)
    
    # Reset options to their default values
    options(scipen = 0, digits = 7)
    return(0)
    
  }, error = function(e){
    
    cat("---- An error occurred: ", conditionMessage(e), "\n")
    
    # Reset options to their default values
    options(scipen = 0, digits = 7)
    return(1) # return 0 to indicate failure
    
  })
}


update_fert100_parameters <- function(paramsdf, save_copy = FALSE){
  # PURPOSE: update fert.100 with new set of parameter values in paramsDFdf data frame object.
  #
  # ARGUMENTS:
  #   parmsdf       a data frame with three columns as follows:
  #                   - File:      File name for the parameters
  #                   - Parameter: Name of the parameter
  #                   - value:     value for the parameters
  #
  #   save_copy     a logical indicating if a copy of old fert.100 to be saved.
  #                   - default to false. 
  #---------------------------------------------------------------------------
  
  # Set options to print numbers in regular format
  options(scipen = 999, digits = 10)
  
  tryCatch({
    
    if(!file.exists("fert.100")){stop("fert.100 file not found in ", getwd())}
    
    fert100   =  readLines("fert.100")
    paramsdf2 =  paramsdf[paramsdf$File %in% c("fert.100"), ]
    
    if(!is.null(paramsdf2) & nrow(paramsdf2) > 0){
      
      params_name  = paramsdf2$Parameter
      params_value = paramsdf2$value
      
      for(pars in params_name){
        
        temp_pos = grep(glob2rx(paste0("*", pars, "*")), fert100)
        
        if(length(temp_pos) == 0){
          
          stop("---- parater ", pars, " Not found in fert.100.")
          
        }
        
        temp_params       = params_value[which(params_name == pars)]
        ncharVal     = nchar(temp_params)
        # Ensure minimum spacing and handle long parameter values
        spaces_needed = max(1, 11 - ncharVal)  # Minimum 1 space
        white_spaces = paste0(rep(" ", spaces_needed), collapse = "")
        fert100[temp_pos] = paste0(temp_params, white_spaces, pars)
        
      }
    }
    
    if(save_copy){
      
      old_fert100 = paste0("old_fert_", str_replace_all(format(Sys.Date(), "%d %b %Y"), "\\s", ""), ".100")
      file.copy("fert.100", old_fert100, overwrite = TRUE)
      
    }
    
    if(fert100[length(fert100)] != ""){
      
      fert100 = c(fert100, "")
      
    }
    
    file.remove("fert.100")
    con_fert100 = file("fert.100", open='w')
    writeLines(fert100, con_fert100)
    close(con_fert100)
    
    # Reset options to their default values
    options(scipen = 0, digits = 7)
    return(0)
    
  }, error = function(e){
    
    cat("---- An error occurred: ", conditionMessage(e), "\n")
    
    # Reset options to their default values
    options(scipen = 0, digits = 7)
    return(1) # return 1 to indicate failure
    
  })
}


update_cult100_parameters <- function(culteffK_value, save_copy){
  # PURPOSE: update event A-K in cult.100 with updated cult effect for event K. 
  #
  # ARGUMENTS:
  #   culteffK_value  a numerical value for cult effect for tillage event K
  #                     - NOTE: 11.7796 last updated by Ryan S. 8/2/2023
  #   save_copy       a logical indicating if a copy of old fert.100 to be saved.
  #                     - default to false. 
  #---------------------------------------------------------------------------
  # Set options to print numbers in regular format
  options(scipen = 999, digits = 10)
  
  tryCatch({
    
    if(!file.exists("cult.100")){stop("cult.100 file not found in ", getwd())}
    
    cult100     = readLines("cult.100")
    tillages    = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K")
    till_adjust = c(0.0144, 0.1156, 0.1878, 0.2678, 0.3167, 0.3911, 0.4733, 0.5622, 0.6300, 0.9711, 1.0)
    
    for(i in 1:length(tillages)){
      
      till_pos = grep(glob2rx(paste0(tillages[i], " *")), cult100)
      
      if(length(till_pos) != 1){
        
        stop("---- Cult event ", tillages[i], " found in ", length(till_pos), " lines.")
        
      }
      
      culteff                = round(1 + (culteffK_value -1)*till_adjust[i], 4)
      
      ncharVal     = nchar(culteff)
      # Ensure minimum spacing and handle long parameter values
      spaces_needed = max(1, 15 - ncharVal)  # Minimum 1 space
      white_spaces = paste0(rep(" ", spaces_needed), collapse = "")
      
      cult100[till_pos + 8]  = paste0(culteff, white_spaces, "CLTEFF(1)")
      cult100[till_pos + 9]  = paste0(culteff, white_spaces, "CLTEFF(2)")
      cult100[till_pos + 11] = paste0(culteff, white_spaces, "CLTEFF(4)")
      
    }
    #
    # remove cult.100 file
    if(save_copy){
      
      old_cult100 = paste0("old_cult_", str_replace_all(format(Sys.Date(), "%d %b %Y"), "\\s", ""), ".100")
      file.copy("cult.100", old_cult100, overwrite = TRUE)
      
    }
    
    if(cult100[length(cult100)] != ""){
      
      cult100 = c(cult100, "")
      
    }
    
    file.remove("cult.100")
    con_cult100 = file("cult.100", open='w')
    writeLines(cult100, con_cult100)
    close(con_cult100)
    
    # Reset options to their default values
    options(scipen = 0, digits = 7)
    return(0)
    
  }, error = function(e){
    
    cat("---- An error occurred: ", conditionMessage(e), "\n")
    
    return(1) # return 1 to indicate failure
    
  })
  
}


update_crop100_parameters <- function(paramsdf, crop, save_copy = FALSE){
  # PURPOSE: update fix.100 with new set of parameter values.
  #
  # ARGUMENTS:
  #   parmsdf       a data frame with two columns as follows:
  #                   - File:      File name for the parameters
  #                   - Parameter: Name of the parameter
  #                   - value:     value for the parameters
  #   crop          a character shring with crop event name in crop.100
  #   save_copy     a logical indicating if a copy of old fert.100 to be saved.
  #                   - defalut to FALSE. 
  #
  #---------------------------------------------------------------------------
  options(scipen = 999, digits = 10)
  
  tryCatch({
    
    if(!file.exists("crop.100")){stop("crop.100 file not found in ", getwd())}
    
    crop100   = readLines("crop.100")
    crop_pos  = grep(glob2rx(paste0(crop," *")), crop100)
    
    if(length(crop_pos) != 1){
      
      stop("---- crop event ", crop, " found in ", length(crop_pos), " lines.")
      
    }
    
    paramsdf2  = paramsdf[paramsdf$File %in% c("crop.100"), ]
    
    if(!is.null(paramsdf2) & nrow(paramsdf2) > 0){
      
      params_name   = paramsdf2$Parameter
      params_value  = paramsdf2$value
      
      for(pars in params_name){
        
        temp_pos = grep(glob2rx(paste0("*", pars, "*")), crop100)
        
        if(length(temp_pos) < 1){
          
          stop("---- crop.100 parameter ", pars, " not found.")
          
        }
        
        parm_pos    = min(temp_pos[temp_pos > crop_pos])
        temp_params = params_value[which(params_name == pars)]
        ncharVal     = nchar(temp_params)
        # Ensure minimum spacing and handle long parameter values
        spaces_needed = max(1, 18 - ncharVal)  # Minimum 1 space
        white_spaces = paste0(rep(" ", spaces_needed), collapse = "")
        
        crop100[parm_pos] = paste0(temp_params, white_spaces, pars)
        
      }
    }
    
    if(save_copy){
      
      old_crop100 = paste0("old_crop_", str_replace_all(format(Sys.Date(), "%d %b %Y"), "\\s", ""), ".100")
      file.copy("crop.100", old_crop100, overwrite = TRUE)
      
    }
    
    
    file.remove("crop.100")
    con_crop100 = file("crop.100", open='w')
    writeLines(crop100, con_crop100)
    close(con_crop100)
    
    # Reset options to their default values
    options(scipen = 0, digits = 7)
    return(0)
    
  }, error = function(e){
    
    cat("---- An error occurred: ", conditionMessage(e), "\n")
    
    # Reset options to their default values
    options(scipen = 0, digits = 7)
    return(1) # return 1 to indicate failure
    
  })
  
}


update_sitepar_parameters <- function(paramsdf, save_copy = FALSE){
  # PURPOSE: update sitepar.in with new set of parameter values.
  #
  # ARGUMENTS:
  #   parmasdf       a data frame with two columns as follows:
  #                    - File:      File name for the parameters
  #                    - Parameter: Name of the parameter or "Key Word"
  #                    - value:     value for the parameters
  #   save_copy     a logical indicating if a copy of old fert.100 to be saved.
  #                   - default set to FALSE. 
  #---------------------------------------------------------------------------
  options(scipen = 999, digits = 10)
  
  tryCatch({
    
    if(!file.exists("sitepar.in")){stop("sitepar.in file not found in ", getwd())}
    
    sitepar = readLines("sitepar.in")
    
    paramsdf2 = paramsdf[paramsdf$File %in% c("sitepar.in"), ]
    
    if(!is.null(paramsdf2) & nrow(paramsdf2) > 0){
      
      for(i in 1:nrow(paramsdf2)){
        
        pName = paramsdf2$Parameter[i]
        #-----------------------------------------------------------------------
        # get the line associated with the parameter
        param.pos  = grep(glob2rx(paste("*", pName, "*", sep = "")), sitepar)
        param.disc = unlist(strsplit(sitepar[param.pos], "[/]"))[2]
        #-----------------------------------------------------------------------
        # get all parameters associated with the FERT event
        pVal               = paramsdf2$value[i]
        ncharVal           = nchar(pVal)
        sitepar[param.pos] = paste(pVal, paste(rep(" ", max(3, (9-ncharVal))), collapse = ""), "/", param.disc, sep="")
        
      }
    }
    
    if(save_copy){
      
      old_sitepar = paste0("old_sitepar_", str_replace_all(format(Sys.Date(), "%d %b %Y"), "\\s", ""), ".in")
      file.copy("sitepar.in", old_sitepar, overwrite = TRUE)
      
    }
    
    
    con_sitepar = file("sitepar.in", open='w')
    writeLines(sitepar, con_sitepar)
    close(con_sitepar)
    
    # Reset options to their default values
    options(scipen = 0, digits = 7)
    
    return(0)
    
  }, error = function(e){
    
    cat("---- An error occurred: ", conditionMessage(e), "\n")
    
    # Reset options to their default values
    options(scipen = 0, digits = 7)
    return(1) # return 1 to indicate failure
    
  })
}


update_site100_parameters <- function(site100_file, paramsdf, save_copy = FALSE){
  # PURPOSE: update extended site.100 with new set of parameter values.
  #
  # ARGUMENTS:
  #   site100_file  a character string for <site>.100 to be updated
  #   parmasdf      a data frame with two columns as follows:
  #                   - File:      File name for the parameters
  #                   - Parameter: Name of the parameter or "Key Word"
  #                   - value:     value for the parameters
  #   save_copy     a logical indicating if a copy of old fert.100 to be saved.
  #                   - default set to FALSE. 
  #---------------------------------------------------------------------------
  options(scipen = 999, digits = 10)
  
  tryCatch({
    
    if(!file.exists(site100_file)){stop(site100_file, " file not found.")}
    
    site100 <- readLines(site100_file)
    
    if(!is.null(paramsdf)){
      
      paramsdf2 <- paramsdf[paramsdf$File %in% c("site.100"), ]
      
      if(!is.null(paramsdf2) & nrow(paramsdf2) > 0){
        
        for(i in 1:nrow(paramsdf2)){
          
          pName <- paramsdf2$Parameter[i]
          pVal <- paramsdf2$value[i]
          
          param.pos <- grep(glob2rx(paste("* ", pName, "$", sep = "")), site100)
          if(length(param.pos) == 1){
            pVal   = ifelse(pVal < 0, as.character(pVal), paste0(" ", pVal))
            # Ensure minimum spacing and handle long parameter values
            spaces_needed = max(1, 15 - nchar(pVal))  # Minimum 1 space
            site100[param.pos] <- paste0(pVal,  paste0(rep(" ", spaces_needed),  collapse = ""), pName)
          }else{
            stop("Failed updating extended site.100")
          }
          
        }
      }
    }
    
    
    if(save_copy){
      
      site100_file2 <- substring(site100_file, 1, (nchar(site100_file) - 4))
      
      old_site100 = paste0("old_", site100_file2, "_", str_replace_all(format(Sys.Date(), "%d %b %Y"), "\\s", ""), ".100")
      file.copy(site100_file, old_site100, overwrite = TRUE)
      
    }
    
    file.remove(site100_file)
    
    con_site100 <- file(site100_file, open='w')
    writeLines(site100, con_site100)
    close(con_site100)
    
    # Reset options to their default values
    options(scipen = 0, digits = 7)
    
    return(0)
    
  }, error = function(e){
    
    cat("---- An error occurred: ", conditionMessage(e), "\n")
    
    # Reset options to their default values
    options(scipen = 0, digits = 7)
    
    return(1) # return 1 to indicate failure
    
  })
  
}


check_site100_soils_hydlic_properties <- function(ext_site100_file){
  
  # constants
  row_per_soil_layer = 12
  
  tryCatch({
    
    if(!file.exists(ext_site100_file)){stop("Invalid extended site.100 file : \n\t", ext_site100_file, "\n")}
    
    ext_site100par = readLines(ext_site100_file)
    
    # just get the soils properties similar to soils.in (redacted) file:
    pos1           = grep(glob2rx("*** Soil layer parameters"), ext_site100par)
    pos2           = grep(glob2rx(paste("*** Enhanced extend parameters", sep = "")), ext_site100par)
    
    soils_vec      = ext_site100par[(pos1+1):(pos2-1)]
    
    # Remove leading and trailing white spaces
    soils_vec      = trimws(soils_vec)
    
    # Split the strings using white spaces
    soils_vec_list = strsplit(soils_vec, "\\s+")
    
    # Convert the result into a data frame
    soils_df <- as.data.frame(matrix(unlist(soils_vec_list), ncol = 2, byrow = TRUE))
    
    # Rename the columns if needed
    colnames(soils_df) <- c("Column1", "Column2")
    
    nsoil_layers = nrow(soils_df)/row_per_soil_layer
    
    if(!all.equal((nsoil_layers %% 1), 0)){
      
      cat("--- Some compnents of soil larys might be missing for ", ext_site100_file, "\n")
      stop("Calculated soil layers: ", nsoil_layers, " not an integer.", "\n")
      
    }
    
    cat("checking site.100 file:", ext_site100_file, ".\n")
    
    for(j in 1:nsoil_layers){
      
      soil_layer_df            = soils_df[grep(glob2rx(paste0("*(",j,")*")), soils_df$Column2), ]
      
      lower_depth_cm           = as.numeric(soil_layer_df$Column1[grep(glob2rx(paste0("*SLDPMX*")), soil_layer_df$Column2)])
      bulk_density             = as.numeric(soil_layer_df$Column1[grep(glob2rx(paste0("*SLBLKD*")), soil_layer_df$Column2)])
      field_capacity           = as.numeric(soil_layer_df$Column1[grep(glob2rx(paste0("*SLFLDC*")), soil_layer_df$Column2)])
      wilting_point            = as.numeric(soil_layer_df$Column1[grep(glob2rx(paste0("*SLWLTP*")), soil_layer_df$Column2)])
      evaporation_coefficient  = as.numeric(soil_layer_df$Column1[grep(glob2rx(paste0("*SLECOF*")), soil_layer_df$Column2)])
      root_frac                = as.numeric(soil_layer_df$Column1[grep(glob2rx(paste0("*SLTCOF*")), soil_layer_df$Column2)])
      sand_frac                = as.numeric(soil_layer_df$Column1[grep(glob2rx(paste0("*SLSAND*")), soil_layer_df$Column2)])
      clay_frac                = as.numeric(soil_layer_df$Column1[grep(glob2rx(paste0("*SLCLAY*")), soil_layer_df$Column2)])
      organic_matter_frac      = as.numeric(soil_layer_df$Column1[grep(glob2rx(paste0("*SLORGF*")), soil_layer_df$Column2)])
      deltamin                 = as.numeric(soil_layer_df$Column1[grep(glob2rx(paste0("*SLCLIM*")), soil_layer_df$Column2)])
      ksat                     = as.numeric(soil_layer_df$Column1[grep(glob2rx(paste0("*SLSATC*")), soil_layer_df$Column2)])
      pH                       = as.numeric(soil_layer_df$Column1[grep(glob2rx(paste0("*SLPH*")), soil_layer_df$Column2)])
      
      
      sand_j <- 100*sand_frac
      clay_j <- 100*clay_frac
      
      cat("---- LAYER-",j,": Sand: ",sand_j, " Clay: ", clay_j, " \n")
      
      # value form soils.in
      s_fc_raw_j    = field_capacity
      s_wp_raw_j    = wilting_point
      s_ksat_cm_sec = ksat
      s_bd_raw_j    = bulk_density
      
      
      # calculated using original Saxton's equation:
      # Saxton, K. E., Rawls, W. J., Romberger, J. S., & Papendick, R. I. (1986). 
      # Estimating Generalized Soil-water Characteristics from Texture. 
      # Soil Science Society of America Journal, 50(4), 1031–1036. 
      # https://doi.org/10.2136/sssaj1986.03615995005000040039x
      
      o_acoef_j     = exp(-4.396-0.0715*clay_j-4.88*10^(-4)*sand_j^2-4.285*10^(-5)*sand_j^2*clay_j)
      o_bcoef_j     = (-3.14)-2.22*10^(-3)*clay_j^2-3.484*10^(-5)*sand_j^2*clay_j
      o_sat_j       = 0.332-7.251*10^(-4)*sand_j+0.1276*log(clay_j, base = 10)
      o_fc_raw_j    = (0.333/o_acoef_j)^(1/o_bcoef_j)
      o_wp_raw_j    = (15/o_acoef_j)^(1/o_bcoef_j)
      o_ksat_raw_j  = exp((12.012-0.0755*sand_j)+(-3.895+0.03671*sand_j-0.1103*clay_j+8.7546*10^(-4)*clay_j^2)/o_sat_j)
      o_ksat_cm_sec = o_ksat_raw_j/3600
      o_bd_raw_j    = (1 - o_sat_j)*2.65
      
      # adjusted soil hydraulic properties
      #
      c_fc_adj_j    = o_fc_raw_j + o_fc_raw_j*(0.07)
      c_wp_adj_j    = o_wp_raw_j + o_wp_raw_j*(-0.15)
      c_bd_adj_j    = o_bd_raw_j + o_bd_raw_j*(0.08)
      
      equal_within_margin <- function(value1, value2, margin_percent = 1) {
        
        margin <- margin_percent/100
        
        min_value <- min(value1, value2)
        max_value <- max(value1, value2)
        diff_value <- max_value - min_value
        
        return(diff_value <= margin * min_value)
        
      }
      
      # check for original saxton's equation: Field Capacity
      if (equal_within_margin(value1 = s_fc_raw_j, value2 = o_fc_raw_j)) {
        
        cat("---- ---- Field Capacity estimated using Saxton et al. (1986).\n",
            "---- ---- ---- Saxton's          = ", o_fc_raw_j, " and \n", 
            "---- ---- ---- soils.in file     = ", s_fc_raw_j, ".\n")
        
        
      } else if (equal_within_margin(value1 = s_fc_raw_j, value2 = c_fc_adj_j)) {
        
        cat("---- ---- Field Capacity from Saxton et al. (1986) is increased by 7% .\n",
            "---- ---- ---- Adjusted Saxton's = ", c_fc_adj_j, " and \n", 
            "---- ---- ---- soils.in file     = ", s_fc_raw_j, ".\n")
        
      } else{
        
        cat("---- ---- has a Field Capacity value of ", s_fc_raw_j, " but should be either \n",
            "---- ---- ---- Saxton's          = ", o_fc_raw_j, " or \n", 
            "---- ---- ---- Asjusted Saxton's = ", c_fc_adj_j, ".\n")
        
      }
      
      # check for original saxton's equation: Wilting Point
      if (equal_within_margin(value1 = s_wp_raw_j, value2 = o_wp_raw_j)) {
        
        cat("---- ---- Wilting Point estimated using Saxton et al. (1986).\n",
            "---- ---- ---- Saxton's          = ", o_wp_raw_j, " and \n", 
            "---- ---- ---- soils.in file     = ", s_wp_raw_j, ".\n")
        
        
      } else if (equal_within_margin(value1 = s_wp_raw_j, value2 = c_wp_adj_j)) {
        
        cat("---- ---- Wilting Point from Saxton et al. (1986) is decreased by 15% .\n",
            "---- ---- ---- Adjusted Saxton's = ", c_wp_adj_j, " and \n", 
            "---- ---- ---- soils.in file     = ", s_wp_raw_j, ".\n")
        
        
      } else{
        
        cat("---- ---- has a Wilting Point value of ", s_wp_raw_j, " but should be either \n",
            "---- ---- ---- Saxton's          = ", o_wp_raw_j, " or \n",
            "---- ---- ---- Asjusted Saxton's = ", c_wp_adj_j, ".\n")
        
      }
      
      # check for original saxton's equation: Bulk Density
      if (equal_within_margin(value1 = s_bd_raw_j, value2 = o_bd_raw_j)) {
        
        cat("---- ---- Soil Bulk Density estimated using Saxton et al. (1986).\n",
            "---- ---- ---- Saxton's          = ", o_bd_raw_j, " and \n", 
            "---- ---- ---- soils.in file     = ", s_bd_raw_j, ".\n")
        
        
      } else if (equal_within_margin(value1 = s_bd_raw_j, value2 = c_bd_adj_j)) {
        
        cat("---- ---- Wilting Point from Saxton et al. (1986) is decreased by 8% .\n",
            "---- ---- ---- Adjusted Saxton's = ", c_bd_adj_j, " and \n", 
            "---- ---- ---- soils.in file     = ", s_bd_raw_j, ".\n")
        
      } else{
        
        cat("---- ---- has a Bulk Density value of ", s_bd_raw_j, " but should be either \n",
            "---- ---- ---- Saxton's          = ", o_bd_raw_j, " or \n", 
            "---- ---- ---- Asjusted Saxton's = ", c_bd_adj_j, ".\n")
        
      }
      
    }
    
    return(0)
    
  }, error = function(e){
    
    cat("---- An error occurred: ", conditionMessage(e), "\n")
    
    return(1) # return 1 to indicate failure
    
  }) 
}


check_soilsIn_soils_hydlic_properties <- function(site_path){
  
  cat("checking soils.in file in ", site_path, "/soils.in file .\n")
  
  tryCatch({
    
    if(!file.exists(file.path(site_path, "soils.in"))){stop("Invalid extended soils.in file in : \n\t", site_path, "\n")}
    
    soils_in <- read.table(file.path(site_path, "soils.in"))
    
    names(soils_in) <- c("upper_depth_cm", "lower_depth_cm",
                         "bulk_density", "field_capacity", "wilting_point", 
                         "evaporation_coefficient", "root_frac", 
                         "sand_frac", "clay_frac", "organic_matter_frac",
                         "deltamin", "ksat", "pH")
    
    for(j in 1:nrow(soils_in)){
      
      sand_j <- 100*soils_in$sand_frac[j]
      clay_j <- 100*soils_in$clay_frac[j]
      
      cat("---- LAYER-",j,": Sand: ",sand_j, " Clay: ", clay_j, " \n")
      
      # value form soils.in
      s_fc_raw_j    = soils_in$field_capacity[j]
      s_wp_raw_j    = soils_in$wilting_point[j]
      s_ksat_cm_sec = soils_in$ksat[j]
      s_bd_raw_j    = soils_in$bulk_density[j]
      
      # calculated using original Saxton's equation:
      # Saxton, K. E., Rawls, W. J., Romberger, J. S., & Papendick, R. I. (1986). 
      # Estimating Generalized Soil-water Characteristics from Texture. 
      # Soil Science Society of America Journal, 50(4), 1031–1036. 
      # https://doi.org/10.2136/sssaj1986.03615995005000040039x
      
      o_acoef_j     = exp(-4.396-0.0715*clay_j-4.88*10^(-4)*sand_j^2-4.285*10^(-5)*sand_j^2*clay_j)
      o_bcoef_j     = (-3.14)-2.22*10^(-3)*clay_j^2-3.484*10^(-5)*sand_j^2*clay_j
      o_sat_j       = 0.332-7.251*10^(-4)*sand_j+0.1276*log(clay_j, base = 10)
      o_fc_raw_j    = (0.333/o_acoef_j)^(1/o_bcoef_j)
      o_wp_raw_j    = (15/o_acoef_j)^(1/o_bcoef_j)
      o_ksat_raw_j  = exp((12.012-0.0755*sand_j)+(-3.895+0.03671*sand_j-0.1103*clay_j+8.7546*10^(-4)*clay_j^2)/o_sat_j)
      o_ksat_cm_sec = o_ksat_raw_j/3600
      o_bd_raw_j    = (1 - o_sat_j)*2.65
      
      # corrected soils.in file. Talked to SW and he doesn't know the source: In brief:
      #   - field capacity in increased by 7%
      #   - wilting point is decreased by 15% 
      #   - bulk density is decreased by 8%
      
      c_fc_adj_j    = o_fc_raw_j + o_fc_raw_j*(0.07)
      c_wp_adj_j    = o_wp_raw_j + o_wp_raw_j*(-0.15)
      c_bd_adj_j    = o_bd_raw_j + o_bd_raw_j*(0.08)
      
      equal_within_margin <- function(value1, value2, margin_percent = 1) {
        
        margin <- margin_percent/100
        
        min_value <- min(value1, value2)
        max_value <- max(value1, value2)
        diff_value <- max_value - min_value
        
        return(diff_value <= margin * min_value)
        
      }
      
      # check for original saxton's equation: Field Capacity
      if (equal_within_margin(value1 = s_fc_raw_j, value2 = o_fc_raw_j)) {
        
        cat("---- ---- Field Capacity estimated using Saxton et al. (1986).\n",
            "---- ---- ---- Saxton's          = ", o_fc_raw_j, " and \n", 
            "---- ---- ---- soils.in file     = ", s_fc_raw_j, ".\n")
        
      } else if (equal_within_margin(value1 = s_fc_raw_j, value2 = c_fc_adj_j)) {
        
        cat("---- ---- Field Capacity from Saxton et al. (1986) is increased by 7% .\n",
            "---- ---- ---- Adjusted Saxton's = ", c_fc_adj_j, " and \n", 
            "---- ---- ---- soils.in file     = ", s_fc_raw_j, ".\n")
        
      } else{
        
        cat("---- ---- has a Field Capacity value of ", s_fc_raw_j, " but should be either \n",
            "---- ---- ---- Saxton's          = ", o_fc_raw_j, " or \n", 
            "---- ---- ---- Asjusted Saxton's = ", c_fc_adj_j, ".\n")
        
      }
      
      # check for original saxton's equation: Wilting Point
      if (equal_within_margin(value1 = s_wp_raw_j, value2 = o_wp_raw_j)) {
        
        cat("---- ---- Wilting Point estimated using Saxton et al. (1986).\n",
            "---- ---- ---- Saxton's          = ", o_wp_raw_j, " and \n", 
            "---- ---- ---- soils.in file     = ", s_wp_raw_j, ".\n")
        
      } else if (equal_within_margin(value1 = s_wp_raw_j, value2 = c_wp_adj_j)) {
        
        cat("---- ---- Wilting Point from Saxton et al. (1986) is decreased by 15% .\n",
            "---- ---- ---- Adjusted Saxton's = ", c_wp_adj_j, " and \n", 
            "---- ---- ---- soils.in file     = ", s_wp_raw_j, ".\n")
        
      } else{
        
        cat("---- ---- has a Wilting Point value of ", s_wp_raw_j, " but should be either \n",
            "---- ---- ---- Saxton's          = ", o_wp_raw_j, " or \n",
            "---- ---- ---- Asjusted Saxton's = ", c_wp_adj_j, ".\n")
        
      }
      
      # check for original saxton's equation: Bulk Density
      if (equal_within_margin(value1 = s_bd_raw_j, value2 = o_bd_raw_j)) {
        
        cat("---- ---- Soil Bulk Density estimated using Saxton et al. (1986).\n",
            "---- ---- ---- Saxton's          = ", o_bd_raw_j, " and \n", 
            "---- ---- ---- soils.in file     = ", s_bd_raw_j, ".\n")
        
      } else if (equal_within_margin(value1 = s_bd_raw_j, value2 = c_bd_adj_j)) {
        
        cat("---- ---- Wilting Point from Saxton et al. (1986) is decreased by 8% .\n",
            "---- ---- ---- Adjusted Saxton's = ", c_bd_adj_j, " and \n", 
            "---- ---- ---- soils.in file     = ", s_bd_raw_j, ".\n")
        
      } else{
        
        cat("---- ---- has a Bulk Density value of ", s_bd_raw_j, " but should be either \n",
            "---- ---- ---- Saxton's          = ", o_bd_raw_j, " or \n", 
            "---- ---- ---- Asjusted Saxton's = ", c_bd_adj_j, ".\n")
        
      }
      
    }
    
    return(0)
    
  }, error = function(e){
    
    cat("---- An error occurred: ", conditionMessage(e), "\n")
    
    return(1) # return 0 to indicate failure
    
  }) 
}


strip_dot_sch <- function(sch_file_name){
  #=======================================================================================
  #  PURPOSE:   strip ".sch" from DayCent schedule file name
  #
  #  Input Arguments:      
  #      - sch_file:           a character string with DayCent schedule file name to be run
  #=======================================================================================
  
  # Handle NULL, NA, or empty inputs
  if (is.null(sch_file_name) || is.na(sch_file_name) || sch_file_name == "") {
    return(sch_file_name)
  }
  
  # Check Schedule File extension
  if(substring(sch_file_name, (nchar(sch_file_name) - 3), nchar(sch_file_name)) == ".sch"){
    sch_file <- substring(sch_file_name, 1, (nchar(sch_file_name) - 4))
    
  }else{
    sch_file = sch_file_name
    
  }
  return(sch_file)
}


update_outfilesIn <- function(what2output = NULL){
  #
  # UNDER CONSTRUCTION:
  #
  # Input Orguments: 
  #     
  if(is.null(what2output)){
    # Set all outfiles.in flag to 0 (i.e. no output generated)
    # ideal for Equilibrium run or when you only want output from .lis file
    
  }else{
    
  }
}


create_daycent_runfile <- function(ExpSite_path){
  
  #create run file (Experimental Sites)
  
  # Get all experiment site files and filter `treatment.sch` files
  all_files_from <- list.files(ExpSite_path, recursive = TRUE, full.names = TRUE)
  filtered_files <- grep("\\.(sch)$", all_files_from, value = TRUE)
  exclusion_keywords <- c("base", "eq", "ext30")
  filtered_files <- filtered_files[!grepl(paste(exclusion_keywords, collapse = "|"), basename(filtered_files))]
  
  siteID <- sub(".*/([^/]+)/[^/]+$", "\\1", filtered_files)
  treatment_schedule <- sub(".*/(.*?)$", "\\1", filtered_files)
  
  df1 <- data.frame(
    siteID = siteID, 
    treatment_schedule = treatment_schedule,
    stringsAsFactors = FALSE
  )
  
  # Filter `base.sch` files with specific exclusions
  filtered_files <- grep("base\\.(sch)$", all_files_from, value = TRUE)
  siteID <- sub(".*/([^/]+)/[^/]+$", "\\1", filtered_files)
  extended_site_base <- sub(".*/(.*?)$", "\\1", filtered_files)
  
  df2 <- data.frame(
    siteID = siteID,
    base_schedule = extended_site_base,
    stringsAsFactors = FALSE
  )
  
  # Filter `_eq.sch` files with specific exclusions
  filtered_files <- grep("_eq\\.(sch)$", all_files_from, value = TRUE)
  siteID <- sub(".*/([^/]+)/[^/]+$", "\\1", filtered_files)
  extended_site_base <- sub(".*/(.*?)$", "\\1", filtered_files)
  
  df3 <- data.frame(
    siteID = siteID,
    equil_schedule = extended_site_base,
    stringsAsFactors = FALSE
  )
  
  # Filter `_eq_ext30.sch` files with specific exclusions
  filtered_files <- grep("_eq_ext30\\.(sch)$", all_files_from, value = TRUE)
  siteID <- sub(".*/([^/]+)/[^/]+$", "\\1", filtered_files)
  extended_site_base <- sub(".*/(.*?)$", "\\1", filtered_files)
  
  df4 <- data.frame(
    siteID = siteID,
    equil_ext30_schedule = extended_site_base,
    stringsAsFactors = FALSE
  )
  # Merge dataframes and prepare the result
  runFile <- merge(df1, df2, all = TRUE)
  runFile <- merge(  runFile, df3, all = TRUE)
  runFile <- merge(  runFile,df4, all = TRUE)
  runFile <- runFile[, c("siteID", "equil_schedule", "equil_ext30_schedule", "base_schedule", "treatment_schedule")]
  ##################################
  # user defined update for "US_becker_potato" site
  runFile$base_schedule[runFile$siteID == "US_becker_potato"] = "minn_base_rye.sch"
  runFile$base_schedule[runFile$siteID == "US_becker_potato" & runFile$treatment_schedule == "minn_control2008_rev.sch"] = "minn_base_sybn.sch"
  runFile$base_schedule[runFile$siteID == "US_becker_potato" & runFile$treatment_schedule == "minn_control2009_rev.sch"] = "minn_base_2007.sch"

  return(runFile)  
}



combine_mod_mes_NH3 <- function(dRslt_NH3, mes_NH3){
  #=======================================================================================
  #  PURPOSE:   combine Modeled and Measured NH3 data
  #               - calculate the average NH3-flux for a measurement window
  #
  #  Input Arguments:      
  #      - dRslt_NH3:     a data.frame object with Daily modeled NH3-flux with 
  #                       at least the following columns:
  #                         - TreatmentID      DayCent .sch file name
  #                         - mod_date         modeled date
  #                         - mod_NH3          modeled NH3-flux (g NH3-N/ha/day)
  #      - mes_NH3:       a data.frame object with measured NH3-flux with the 
  #                       at least the following columns:
  #                         - siteID           Experimental Site Idetification Name
  #                         - TreatmentID      DayCent .sch file name
  #                         - meas_start_date  Measurement start date
  #                         - meas_end_date    Measurement end date
  #
  #  Output: a list of two following data.frames:
  #   
  #     - data_NH3:       a data.frame object with modeled NH3-flux and span days added
  #                       to mes_NH3 input data.frame. All column in mes_NH3 will be 
  #                       preserved. New column names are:
  #                         - mod_NH3vol_gN_ha_day  average modeled NH3-flux
  #                         - mod_span_days         number of days of the measurement window
  #
  #     - missing_NH3:    a data.frame object with measurement windows with missing 
  #                       modeled values. The data.frame will have the following columns:
  #                         - siteID: 
  #                         - TreatmentID      DayCent .sch file name
  #                         - meas_start_date  Measurement start date
  #                         - meas_end_date    Measurement end date
  #
  #=======================================================================================
  
  data_NH3 = mes_NH3
  data_NH3$mod_NH3vol_gN_ha_day = NA
  data_NH3$mod_span_days = NA
  
  missing_NH3 = NULL
  for(i in 1:nrow(mes_NH3)){
    site_id    = mes_NH3$siteID[i]
    trt_id     = mes_NH3$treatment_schedule[i]
    start_date = mes_NH3$meas_start_date[i] 
    end_date   = mes_NH3$meas_end_date[i]
    
    temp_NH3   = dRslt_NH3[dRslt_NH3$TreatmentID == trt_id & dRslt_NH3$mod_date >= start_date & dRslt_NH3$mod_date <= end_date, ]
    if(nrow(temp_NH3) > 0 ){
      data_NH3$mod_NH3vol_gN_ha_day[i] = mean(temp_NH3$mod_NH3, na.rm = TRUE)
      data_NH3$mod_span_days[i]        = sum(!is.na(temp_NH3$mod_NH3))
    }else{
      temp_mdf = data.frame("siteID" = site_id,
                            "treatment_schedule" = trt_id,
                            "meas_start_date" = start_date,
                            "meas_end_date"   = end_date)
      missing_NH3 = rbind(missing_NH3, temp_mdf)
    }
  }
  rtn_list = list("data_NH3" = data_NH3, 
                  "missing_NH3" = missing_NH3)
  return(rtn_list)
  
}


combine_mod_mes_Urea <- function(dRslt_Urea, mes_Urea){
  #=======================================================================================
  #  PURPOSE:   combine Modeled and Measured Urea data
  #
  #  Input Arguments:      
  #      - dRslt_Urea:    a data.frame object with Daily modeled NH3-flux with 
  #                       at least the following columns:
  #                         - TreatmentID      DayCent .sch file name
  #                         - mod_date         modeled date
  #                         - mod_Urea         modeled Urea-left (g Urea-N/m^2)
  #      - mes_Urea:      a data.frame object with measured Urea in the filed with the 
  #                       at least the following columns:
  #                         - siteID           Experimental Site Idetification Name
  #                         - TreatmentID      DayCent .sch file name
  #                         - mes_date         Measurement date
  #
  #  Output: a list of two following data.frames:
  #   
  #     - data_Urea       a data.frame object with modeled Urea-left added
  #                       to mes_Urea input data.frame. All column in mes_Urea will be 
  #                       preserved. New column names are:
  #                         - mod_Urea         modeled Urea-left 
  #
  #     - missing_NH3:    a data.frame object with measurement windows with missing 
  #                       modeled values. The data.frame will have the following columns:
  #                         - siteID: 
  #                         - TreatmentID      DayCent .sch file name
  #                         - mes_date         Measurement date
  #
  #=======================================================================================
  data_Urea = mes_Urea
  data_Urea$mod_urea_gN_m2 = NA
  
  missing_Urea = NULL
  for(i in 1:nrow(mes_Urea)){
    site_id    = mes_Urea$siteID[i]
    trt_id     = mes_Urea$treatment_schedule[i]
    mes_date   = mes_Urea$mes_date[i] 
    
    temp_urea   = dRslt_Urea[dRslt_Urea$TreatmentID == trt_id & dRslt_Urea$mod_date == mes_date, ]
    if(nrow(temp_urea) > 0 ){
      data_Urea$mod_urea_gN_m2[i] = temp_urea$mod_Urea
    }else{
      temp_mdf = data.frame("siteID" = site_id,
                            "treatment_schedule" = trt_id,
                            "mes_date" = mes_date)
      missing_Urea = rbind(missing_Urea, temp_mdf)
    }
  }
  rtn_list = list("data_Urea" = data_Urea, 
                  "missing_Urea" = missing_Urea)
  return(rtn_list)
  
}

combine_mod_soc_annual <- function(annualRslt_SOC, soc_annual){
  #=======================================================================================
  #  PURPOSE:   combine Modeled and Measured SOC data
  #               - calculate the average SOC for a measurement window
  #
  #  Input Arguments:      
  #      - annualRslt_SOC:      a data.frame object with annual modeled SOC with 
  #                             at least the following columns:
  #                               - TreatmentID        DayCent .sch file name
  #                               - mod_date           modeled date
  #                               - mod_SOC            modeled SOC (g C/m2)
  #      - soc_annual:          a data.frame object with measured SOC with the 
  #                             at least the following columns:
  #                               - siteID             Experimental Site Idetification Name
  #                               - treatment_schedule DayCent .sch file name
  #                               - meas_year          Measurement year
  #
  #  Output: a list of two following data.frames:
  #   
  #     - data_SOC:       a data.frame object with modeled SOC and span years added
  #                       to mes_SOC input data.frame. All column in mes_SOC will be 
  #                       preserved. New column names are:
  #                         - mod_SOC_gC_m2          average modeled SOC
  #                         - mod_span_years         number of years of the measurement window
  #
  #     - missing_SOC:    a data.frame object with measurement windows with missing 
  #                       modeled values. The data.frame will have the following columns:
  #                         - siteID: 
  #                         - TreatmentID      DayCent .sch file name
  #                         - meas_year        Measurement year
  #
  #=======================================================================================
  
  data_SOC = soc_annual
  data_SOC$mod_SOC_gC_m2 = NA
  data_SOC$mod_span_years = NA
  
  missing_SOC = NULL
  for(i in 1:nrow(soc_annual)){
    site_id    = soc_annual$siteID[i]
    trt_id     = soc_annual$treatment_schedule[i]
    year       = soc_annual$meas_year[i] 
    
    temp_SOC   = annualRslt_SOC[annualRslt_SOC$TreatmentID == trt_id & annualRslt_SOC$year == year, ]
    if(nrow(temp_SOC) > 0 ){
      data_SOC$mod_SOC_gC_m2[i]  = mean(temp_SOC$mod_SOC, na.rm = TRUE)
      data_SOC$mod_span_years[i] = sum(!is.na(temp_SOC$mod_SOC))
    }else{
      temp_mdf = data.frame("siteID" = site_id,
                            "treatment_schedule" = trt_id,
                            "meas_year" = year)
      missing_SOC = rbind(missing_SOC, temp_mdf)
    }
  }
  rtn_list = list("data_SOC" = data_SOC, 
                  "missing_SOC" = missing_SOC)
  return(rtn_list)
  
}

combine_mod_crop_annual <- function(weightedRslt_crop, crop_annual){
  #=======================================================================================
  #  PURPOSE:   combine Modeled and Measured SOC data
  #               - calculate the average SOC for a measurement window
  #
  #  Input Arguments:      
  #      - weightedRslt_crop:      a data.frame object with annual modeled SOC with 
  #                             at least the following columns:
  #                               - TreatmentID        DayCent .sch file name
  #                               - mod_date           modeled date
  #                               - mod_SOC            modeled SOC (g C/m2)
  #      - crop_annual:          a data.frame object with measured crop with the 
  #                             at least the following columns:
  #                               - siteID             Experimental Site Idetification Name
  #                               - treatment_schedule DayCent .sch file name
  #                               - meas_year          Measurement year
  #
  #  Output: a list of two following data.frames:
  #   
  #     - data_SOC:       a data.frame object with modeled SOC and span years added
  #                       to mes_SOC input data.frame. All column in mes_SOC will be 
  #                       preserved. New column names are:
  #                         - mod_SOC_gC_m2          average modeled SOC
  #                         - mod_span_years         number of years of the measurement window
  #
  #     - missing_SOC:    a data.frame object with measurement windows with missing 
  #                       modeled values. The data.frame will have the following columns:
  #                         - siteID: 
  #                         - TreatmentID      DayCent .sch file name
  #                         - meas_year        Measurement year
  #
  #=======================================================================================
  
  data_crop = crop_annual
  data_crop$mod_cgrain = NA
  data_crop$mod_span_years = NA

  missing_crop = NULL
  for(i in 1:nrow(crop_annual)){
    site_id    = crop_annual$siteID[i]
    trt_id     = crop_annual$treatment_schedule[i]
    year       = crop_annual$meas_year[i] 
    
    temp_crop  = weightedRslt_crop[weightedRslt_crop$aggregation_level == site_id & weightedRslt_crop$year == year, ]
    if(nrow(temp_crop) > 0 ){
      data_crop$mod_cgrain[i]  = mean(temp_crop$mod_cgrain, na.rm = TRUE)
      data_crop$mod_span_years[i] = sum(!is.na(temp_crop$mod_cgrain))
      
    }else{
      temp_mdf = data.frame("siteID" = site_id,
                            "treatment_schedule" = trt_id,
                            "meas_year" = year)
      missing_crop = rbind(missing_crop, temp_mdf)
    }
  }
  rtn_list = list("data_crop" = data_crop, 
                  "missing_crop" = missing_crop)
  return(rtn_list)
  
}

#' Generic Model-Measurement Combination Function
#'
#' A generic function that combines model outputs with measurements for different
#' variable types, supporting various temporal matching strategies.
#'
#' @param model_data Data frame with model outputs containing:
#'   - TreatmentID: Treatment identifier
#'   - mod_date: Model date
#'   - mod_value: Model value (column name depends on variable type)
#' @param obs_data Data frame with observations
#' @param var_config Variable configuration from config file
#' @param verbose Logical, whether to print progress messages
#'
#' @return List containing:
#'   \item{data}{Combined model-observation data frame}
#'   \item{missing}{Missing data records}
#'
#' @export
combine_mod_mes <- function(model_data, obs_data, var_config, verbose = FALSE) {
  
  matching_type <- var_config$matching_type
  
  if (matching_type == "individual_windows") {
    # Individual measurement windows (e.g., NH3 flux measurements)
    return(combine_mod_mes_windows(model_data, obs_data, var_config, "individual", verbose))
    
  } else if (matching_type == "cumulative_windows") {
    # Cumulative measurement windows (e.g., cumulative NH3 measurements)
    return(combine_mod_mes_windows(model_data, obs_data, var_config, "cumulative", verbose))
    
  } else if (matching_type == "point_measurements") {
    # Point measurements (e.g., Urea concentrations on specific dates)
    return(combine_mod_mes_points(model_data, obs_data, var_config, verbose))
    
  } else {
    stop("Unknown matching_type: ", matching_type)
  }
}

#' Generic Window-Based Model-Measurement Combination
#'
#' Combines model outputs with measurements for window-based measurements
#' (both individual and cumulative).
#'
#' @param model_data Model data frame
#' @param obs_data Observation data frame
#' @param var_config Variable configuration
#' @param window_type Type of window ("individual" or "cumulative")
#' @param verbose Logical, whether to print progress messages
#'
#' @return List containing combined data and missing records
#'
#' @export
combine_mod_mes_windows <- function(model_data, obs_data, var_config, window_type = "individual", verbose = FALSE) {
  
  # Determine model value column name based on variable type
  if (var_config$model_output == "NH3.N") {
    model_col <- "mod_NH3"
    output_col <- "mod_NH3vol_gN_ha_day"
  } else if (var_config$model_output == "DayCent_N2O")  {
    model_col <- "mod_N2O"
    output_col <- "mod_N2O_gN_ha_day"
  } else {
    model_col <- "mod_value"
    output_col <- paste0("mod_", gsub("\\.", "_", var_config$model_output), "_avg")
  }
  
  # Initialize output data frame
  combined_data <- obs_data
  combined_data[[output_col]] <- NA
  combined_data$mod_span_days <- NA
  
  missing_data <- NULL
  
  for (i in 1:nrow(obs_data)) {
    site_id <- obs_data$siteID[i]
    trt_id <- obs_data$treatment_schedule[i]
    start_date <- obs_data$meas_start_date[i]
    end_date <- obs_data$meas_end_date[i]
    
    # Check whether the observed data carries the list of dates that were used in the average
    if ("obs_dates" %in% names(obs_data)) {
      
      # Extract model data for this measurement window
      temp_model_all <- model_data[model_data$TreatmentID == trt_id & 
                                 model_data$mod_date >= start_date & 
                                 model_data$mod_date <= end_date, ]
      
      # obs_dates[i] is ONE text string, e.g. "2005-06-01; 2005-06-02; ..."
      # Split it on ";" and convert to Date so each date can be matched individually
      obs_date_list <- unlist(strsplit(obs_data$obs_dates[i], ";"))   # split into separate strings
      obs_date_list <- trimws(obs_date_list)                          # remove stray spaces
      obs_date_list <- as.Date(obs_date_list, format = "%Y-%m-%d")    # convert text to Date
      
      obs_date_list <- obs_date_list[!is.na(obs_date_list)]           # drop blanks / NA
      
      # Keep only the model days that have an observation
      temp_model <- temp_model_all[temp_model_all$mod_date %in% obs_date_list, ]
      
    } else {
    
    # Extract model data for this measurement window
    temp_model <- model_data[model_data$TreatmentID == trt_id & 
                           model_data$mod_date >= start_date & 
                           model_data$mod_date <= end_date, ]
    
    }
  
    if (nrow(temp_model) > 0) {
      # Calculate average for the window
      combined_data[[output_col]][i] <- mean(temp_model[[model_col]], na.rm = TRUE)
      combined_data$mod_span_days[i] <- sum(!is.na(temp_model[[model_col]]))
    } else {
      # Record missing data
      missing_record <- data.frame(
        siteID = site_id,
        treatment_schedule = trt_id,
        meas_start_date = start_date,
        meas_end_date = end_date,
        stringsAsFactors = FALSE
      )
      missing_data <- rbind(missing_data, missing_record)
    }
  }
  
  return(list(
    data = combined_data,
    missing = missing_data
  ))
}

#' Generic Point-Based Model-Measurement Combination
#'
#' Combines model outputs with measurements for point-based measurements
#' (specific dates).
#'
#' @param model_data Model data frame
#' @param obs_data Observation data frame
#' @param var_config Variable configuration
#' @param verbose Logical, whether to print progress messages
#'
#' @return List containing combined data and missing records
#'
#' @export
combine_mod_mes_points <- function(model_data, obs_data, var_config, verbose = FALSE) {
  
  # Determine model value column name and output column name
  if (length(var_config$model_output) > 1) {
    # Multiple variables (e.g., surface + subsurface urea)
    model_col <- "mod_Urea"
    output_col <- "mod_urea_gN_m2"
  } else if (var_config$model_output == "urea_left") {
    # Single urea variable
    model_col <- "mod_Urea"
    output_col <- "mod_urea_gN_m2"
  } else {
    model_col <- "mod_value"
    output_col <- paste0("mod_", gsub("\\.", "_", var_config$model_output))
  }
  
  # Initialize output data frame
  combined_data <- obs_data
  combined_data[[output_col]] <- NA
  
  missing_data <- NULL
  
  for (i in 1:nrow(obs_data)) {
    site_id <- obs_data$siteID[i]
    trt_id <- obs_data$treatment_schedule[i]
    mes_date <- obs_data$mes_date[i]
    
    # Extract model data for this specific date
    temp_model <- model_data[model_data$TreatmentID == trt_id & 
                           model_data$mod_date == mes_date, ]
    
    if (nrow(temp_model) > 0) {
      # Use the model value for this date
      if (length(var_config$model_output) > 1) {
        # For multiple variables, sum them (e.g., surface + subsurface)
        combined_data[[output_col]][i] <- sum(temp_model[[model_col]], na.rm = TRUE)
      } else {
        combined_data[[output_col]][i] <- temp_model[[model_col]][1]
      }
    } else {
      # Record missing data
      missing_record <- data.frame(
        siteID = site_id,
        treatment_schedule = trt_id,
        mes_date = mes_date,
        stringsAsFactors = FALSE
      )
      missing_data <- rbind(missing_data, missing_record)
    }
  }
  
  return(list(
    data = combined_data,
    missing = missing_data
  ))
}


#' Generic DayCent Parameter Update System (Dispatcher)
#'
#' This function provides a generic interface for updating DayCent parameter files
#' by routing parameters to the appropriate file-specific update functions based
#' on the File column in the input data frame.
#'
#' @param params_df Data frame with columns: File, Parameter, value
#'   - File: Target file name (e.g., "fix.100", "site.100", "fert.100")
#'   - Parameter: Parameter name to update
#'   - value: New parameter value
#' @param simulation_dir Directory containing DayCent input files (default: current directory)
#' @param site100_file Name of the site.100 file for site parameter updates (default: "site.100")
#' @param save_copy Logical indicating whether to save backup copies of modified files (default: FALSE)
#' @param verbose Logical indicating whether to print progress messages (default: TRUE)
#' @param crop_name Character string specifying the crop name for crop.100 parameter updates (required for crop.100 files)
#'
#' @return Integer status code: 0 = success, 1 = error
#'
#' @details
#' This dispatcher function routes parameter updates to the appropriate file-specific
#' functions:
#' - fix.100 parameters -> update_fix100_parameters()
#' - site.100 parameters -> update_site100_parameters()
#' - fert.100 parameters -> update_fert100_parameters()
#' - cult.100 parameters -> update_cult100_parameters()
#' - crop.100 parameters -> update_crop100_parameters() (requires crop_name)
#'
#' The function changes to the specified simulation directory, performs all updates,
#' and returns to the original working directory.
#'
#' @examples
#' \dontrun{
#' # Create parameter data frame
#' params <- data.frame(
#'   File = c("fix.100", "site.100", "fert.100"),
#'   Parameter = c("PABRES", "VMAXCAP", "UREAKM"), 
#'   value = c(0.5, 0.025, 100)
#' )
#' 
#' # Update parameters
#' result <- update_daycent_parameters(params, "/path/to/simulation")
#' }
#'
#' @export
update_daycent_parameters <- function(params_df, simulation_dir = ".",
                                     site100_file = "site.100",
                                     save_copy = FALSE, verbose = TRUE,
                                     crop_name = NULL) {
  
  # Validate inputs
  if (missing(params_df)) {
    stop("Parameter data frame (params_df) is required")
  }
  
  required_cols <- c("File", "Parameter", "value")
  if (!all(required_cols %in% names(params_df))) {
    stop("params_df must contain columns: ", paste(required_cols, collapse = ", "))
  }
  
  if (nrow(params_df) == 0) {
    if (verbose) cat("No parameters to update.\n")
    return(0)
  }
  
  # Set working directory
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  
  tryCatch({
    setwd(simulation_dir)
    
    if (verbose) {
      cat("Updating DayCent parameters in:", simulation_dir, "\n")
      cat("Total parameters to update:", nrow(params_df), "\n")
    }
    
    # Group parameters by file
    file_groups <- split(params_df, params_df$File)
    
    # Track overall success
    overall_status <- 0
    
    # Dispatch to appropriate function for each file
    for (file_name in names(file_groups)) {
      file_params <- file_groups[[file_name]]
      
      if (verbose) {
        cat("Updating", nrow(file_params), "parameters in", file_name, "\n")
      }
      
      # Route to appropriate update function
      if (file_name == "fix.100") {
        result <- update_fix100_parameters(file_params, save_copy = save_copy)
      } else if (file_name == "site.100") {
        # Smart site.100 file detection
        actual_site_file <- site100_file
        if (site100_file == "site.100") {
          # Look for site-specific site.100 files (pattern: *_site.100)
          site_files <- list.files(path = ".", pattern = "*_site\\.100$")
          if (length(site_files) > 0) {
            actual_site_file <- site_files[1]  # Use first match
            if (verbose) {
              cat("Auto-detected site-specific file:", actual_site_file, "\n")
            }
          } else if (verbose) {
            cat("No site-specific site.100 file found, using:", site100_file, "\n")
          }
        }
        result <- update_site100_parameters(actual_site_file, file_params, save_copy = save_copy)  
      } else if (file_name == "fert.100") {
        result <- update_fert100_parameters(file_params, save_copy = save_copy)
      } else if (file_name == "cult.100") {
        # Extract K_CLTEFF parameter value for cult.100 updates
        k_clteff_params <- file_params[file_params$Parameter == "K_CLTEFF", ]
        if (nrow(k_clteff_params) > 0) {
          k_clteff_value <- k_clteff_params$value[1]
          result <- update_cult100_parameters(k_clteff_value, save_copy = save_copy)
        } else {
          warning("K_CLTEFF parameter not found in cult.100 parameters. Skipping.")
          result <- 0  # Continue without error
        }
      } else if (file_name == "crop.100") {
        # For crop.100, we need the crop name from configuration
        if (is.null(crop_name) || crop_name == "") {
          stop("crop_name parameter is required for crop.100 parameter updates. ",
               "Please specify the crop name (e.g., 'C6') in the configuration or function call.")
        }
        result <- update_crop100_parameters(file_params, crop_name, save_copy = save_copy)
      } else {
        warning("Unknown file type: ", file_name, ". Skipping.")
        next
      }
      
      # Check result and track errors
      if (result != 0) {
        cat("Error updating parameters in", file_name, "\n")
        overall_status <- 1
      } else if (verbose) {
        cat("Successfully updated parameters in", file_name, "\n")
      }
    }
    
    if (verbose) {
      if (overall_status == 0) {
        cat("All parameter updates completed successfully.\n")
      } else {
        cat("Some parameter updates failed. Check error messages above.\n")
      }
    }
    
    return(overall_status)
    
  }, error = function(e) {
    cat("Error in update_daycent_parameters:", conditionMessage(e), "\n")
    return(1)
  })
}


