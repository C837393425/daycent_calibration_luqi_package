.libPaths(c("rlib", .libPaths()))
library(bayesiancalibr)
library(yaml)

repo <- "/data/rubelscratch/rubelogle/daycent_calibration"
helper_env <- new.env(parent = globalenv())
sys.source(
  file.path(repo, "workflows/scripts/crop_yield_all/Validation/validation_helpers.R"),
  envir = helper_env
)

resolve_config_paths <- function(config) {
  config$paths$lairice_root <- repo
  for (path_key in names(config$paths)) {
    path_value <- config$paths[[path_key]]
    if (is.character(path_value) && length(path_value) == 1 && nzchar(path_value) && !grepl("^/", path_value)) {
      config$paths[[path_key]] <- file.path(repo, path_value)
    }
  }
  config
}

g3_config <- resolve_config_paths(yaml::read_yaml(file.path(repo, "workflows/configs/crop_yield_hay_G3.yaml")))
alf_config <- resolve_config_paths(yaml::read_yaml(file.path(repo, "workflows/configs/crop_yield_hay_ALF.yaml")))
corn_config <- resolve_config_paths(yaml::read_yaml(file.path(repo, "workflows/configs/crop_yield_corn_m2.yaml")))

if (!identical(helper_env$normalize_validation_crop_names(g3_config), "G3")) {
  stop("Expected G3 config crop name to normalize to G3 only")
}

if (!isTRUE(helper_env$validation_site_crop_matrix_required(g3_config))) {
  stop("Expected hay G3 config to require site crop matrix for agcprd workflows")
}

if (!isTRUE(helper_env$validation_site_crop_matrix_required(alf_config))) {
  stop("Expected hay ALF config to require site crop matrix for agcprd workflows")
}

if (!isTRUE(bayesiancalibr::crop_calibration_enabled(corn_config, corn_config$daycent$crop$name))) {
  stop("Expected corn config to satisfy package crop_calibration_enabled")
}

if (!isTRUE(helper_env$validation_site_crop_matrix_required(corn_config))) {
  stop("Expected corn config to require site crop matrix via package gate")
}

if (isTRUE(bayesiancalibr::crop_calibration_enabled(g3_config, g3_config$daycent$crop$name))) {
  stop("Expected package crop_calibration_enabled to remain FALSE for hay agcprd config")
}

cat("Validation hay crop matrix gate OK\n")
