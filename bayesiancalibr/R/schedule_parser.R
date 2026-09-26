#' Parse DayCent Schedule File for Crop Information
#'
#' Extracts crop planting information by year from DayCent schedule file content.
#' Parses the schedule format to identify which crops are grown in each year.
#'
#' @param schedule_content Character string containing the complete schedule file content
#' @param target_crop Character string specifying the crop to search for (e.g., "C6")
#' @param verbose Logical indicating whether to print parsing progress
#'
#' @return Data frame with columns:
#'   - year: Calendar year (e.g., 2011, 2012, ...)
#'   - has_target_crop: Logical indicating if target crop was planted in that year
#'   - all_crops: Character vector listing all crops planted in that year
#'
#' @details
#' The function parses DayCent schedule files which have the format:
#' - Output starting year is specified in the block section
#' - CROP events follow the pattern: [tab][year_offset][tab][day][tab]CROP [crop_name]
#' - Year offset 1 = output starting year, year offset 2 = output starting year + 1, etc.
#'
#' The function identifies all crop planting events and determines which years
#' contain the target crop for use in crop-aware weighted aggregation.
#'
#' @examples
#' \dontrun{
#' # Parse schedule content for corn (C6)
#' schedule_content <- get_schedule_file_from_database(config, "854585", "", "base")
#' crop_years <- parse_schedule_for_crops(schedule_content, "C6")
#'
#' # Show years with corn planting
#' corn_years <- crop_years[crop_years$has_target_crop, ]
#' print(corn_years)
#' }
#'
#' @export
parse_schedule_for_crops <- function(schedule_content, target_crop, verbose = FALSE) {

  if (is.null(schedule_content) || nchar(schedule_content) == 0) {
    if (verbose) cat("Empty or null schedule content provided\n")
    return(data.frame(year = integer(0), has_target_crop = logical(0), all_crops = character(0)))
  }

  # Split content into lines
  lines <- strsplit(schedule_content, "\n", fixed = TRUE)[[1]]

  if (verbose) {
    cat("Parsing schedule with", length(lines), "lines for target crop(s):",
        paste(target_crop, collapse = ", "), "\n")
  }

  # Find output starting year from the block section
  # Look for pattern like:
  # 1               Block
  # 2020            Last year
  # 10              Repeats # years
  # 2011            Output starting year

  output_starting_year <- NULL

  for (i in 1:(length(lines) - 3)) {
    line1 <- trimws(lines[i])
    line2 <- trimws(lines[i + 1])
    line3 <- trimws(lines[i + 2])
    line4 <- trimws(lines[i + 3])

    # Look for Block -> Last year -> Repeats -> Output starting year pattern
    if (grepl("Block\\s*$", line1, ignore.case = TRUE) &&
        grepl("Last year\\s*$", line2, ignore.case = TRUE) &&
        grepl("Repeats.*years\\s*$", line3, ignore.case = TRUE) &&
        grepl("Output starting year\\s*$", line4, ignore.case = TRUE)) {

      # Extract the year from line4's preceding line (should be line i+3-1 = i+2, but we need the number before "Output starting year")
      # Actually, the number should be on the same line as "Output starting year"
      year_match <- regmatches(line4, regexec("^(\\d{4})\\s+Output starting year", line4, ignore.case = TRUE))[[1]]
      if (length(year_match) >= 2) {
        output_starting_year <- as.numeric(year_match[2])
        break
      }
    }
  }

  # Alternative search: look for line that starts with 4-digit year followed by "Output starting year"
  if (is.null(output_starting_year)) {
    for (i in 1:length(lines)) {
      line <- trimws(lines[i])
      if (grepl("^\\d{4}\\s+Output starting year", line, ignore.case = TRUE)) {
        output_starting_year <- as.numeric(sub("\\s+Output starting year.*$", "", line, ignore.case = TRUE))
        break
      }
    }
  }

  if (is.null(output_starting_year)) {
    if (verbose) cat("Warning: Could not find output starting year in schedule, assuming 2011\n")
    output_starting_year <- 2011
  }

  if (verbose) {
    cat("Output starting year identified as:", output_starting_year, "\n")
  }

  event_pattern <- "^\\s*(\\d+)\\s+(\\d+)\\s+(\\S+)(?:\\s+(.*?))?\\s*(?:#.*)?$"
  schedule_events <- list()

  for (i in seq_along(lines)) {
    matches <- regmatches(lines[i], regexec(event_pattern, lines[i]))[[1]]
    if (length(matches) >= 4) {
      year_number <- as.integer(matches[2])
      day_number <- as.integer(matches[3])
      event_name <- toupper(matches[4])
      event_arg <- if (length(matches) >= 5) trimws(matches[5]) else ""

      schedule_events[[length(schedule_events) + 1]] <- list(
        year = output_starting_year + year_number - 1L,
        day = day_number,
        event = event_name,
        arg = event_arg
      )
    }
  }

  finalize_crop_cycle <- function(active_cycle, end_year = NULL, end_event = NULL, fallback_reason = NULL) {
    if (is.null(active_cycle)) {
      return(NULL)
    }

    resolved_year <- if (!is.null(end_year) && !is.na(end_year)) end_year else active_cycle$planting_year
    resolved_event <- if (!is.null(end_event) && nzchar(end_event)) end_event else "PLANTING_YEAR"

    if (verbose) {
      cat(
        "Resolved crop cycle:",
        active_cycle$crop,
        "planting_year=", active_cycle$planting_year,
        "terminator=", resolved_event,
        "eligibility_year=", resolved_year,
        if (!is.null(fallback_reason) && nzchar(fallback_reason)) paste0(" reason=", fallback_reason) else "",
        "\n",
        sep = ""
      )
    }

    list(
      crop = active_cycle$crop,
      year = resolved_year
    )
  }

  crop_cycles <- list()
  active_cycle <- NULL

  for (event in schedule_events) {
    if (identical(event$event, "CROP")) {
      crop_name <- strsplit(event$arg, "\\s+")[[1]][1]
      crop_name <- trimws(crop_name)

      if (!is.null(active_cycle)) {
        crop_cycles[[length(crop_cycles) + 1]] <- finalize_crop_cycle(
          active_cycle,
          end_year = active_cycle$last_year,
          end_event = if (!is.null(active_cycle$last_year)) "LAST" else "PLANTING_YEAR",
          fallback_reason = if (!is.null(active_cycle$last_year)) {
            "next crop encountered before HARV"
          } else {
            "next crop encountered without HARV or LAST"
          }
        )
      }

      active_cycle <- list(
        crop = crop_name,
        planting_year = event$year,
        last_year = NULL
      )

      if (verbose) {
        cat("Found crop planting:", crop_name, "in", event$year, "\n")
      }
    } else if (!is.null(active_cycle) && identical(event$event, "LAST")) {
      active_cycle$last_year <- event$year
    } else if (!is.null(active_cycle) && identical(event$event, "HARV")) {
      crop_cycles[[length(crop_cycles) + 1]] <- finalize_crop_cycle(
        active_cycle,
        end_year = event$year,
        end_event = "HARV"
      )
      active_cycle <- NULL
    }
  }

  if (!is.null(active_cycle)) {
    crop_cycles[[length(crop_cycles) + 1]] <- finalize_crop_cycle(
      active_cycle,
      end_year = active_cycle$last_year,
      end_event = if (!is.null(active_cycle$last_year)) "LAST" else "PLANTING_YEAR",
      fallback_reason = if (!is.null(active_cycle$last_year)) {
        "schedule ended without HARV"
      } else {
        "schedule ended without HARV or LAST"
      }
    )
  }

  if (length(crop_cycles) == 0) {
    if (verbose) cat("No crop events found in schedule\n")
    return(data.frame(year = integer(0), has_target_crop = logical(0), all_crops = character(0)))
  }

  # Convert to data frame
  events_df <- do.call(rbind, lapply(crop_cycles, function(x) {
    data.frame(year = x$year, crop = x$crop, stringsAsFactors = FALSE)
  }))

  # Group by year and determine if target crop is present
  years <- unique(events_df$year)
  results <- data.frame(
    year = years,
    has_target_crop = logical(length(years)),
    all_crops = character(length(years)),
    stringsAsFactors = FALSE
  )

  for (i in 1:nrow(results)) {
    year <- results$year[i]
    year_crops <- events_df[events_df$year == year, "crop"]

    # TRUE if any configured target crop (scalar or vector) appears in this year
    results$has_target_crop[i] <- any(target_crop %in% year_crops)

    # Store all crops for this year
    results$all_crops[i] <- paste(unique(year_crops), collapse = ",")
  }

  # Sort by year
  results <- results[order(results$year), ]
  rownames(results) <- NULL

  if (verbose) {
    cat("Parsing complete. Found", nrow(results), "years with crops\n")
    target_years <- sum(results$has_target_crop)
    cat("Target crop(s)", paste(target_crop, collapse = ", "), "found in", target_years, "years\n")
  }

  return(results)
}
