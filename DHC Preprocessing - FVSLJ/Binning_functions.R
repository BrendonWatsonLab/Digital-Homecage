# ============================================================================
# Script: generate_binned_data_original.R
# Author: Noah Muscat
# Date: 2025-06-24
# Description:
#   This script provides the original `create_binned_data` function, extracted
#   directly from the master analysis script without modification. It bins
#   event data into specified time intervals, calculating the count and
#   total duration of events for each label within each bin.
#
# Dependencies: data.table, lubridate
#
# ============================================================================

# Load Required Libraries
library(data.table)
library(lubridate)

# Global Variable Definition
CHANNEL_LABELS <- c(
  "Water_1_Beambreak", "Water_2_Beambreak", "Food_1_Beambreak",
  "Food_2_Beambreak", "Water_1_Dispense", "Water_2_Dispense",
  "Food_1_Dispense", "Food_2_Dispense"
)


# Main Function (Exact copy from original script) 

# Create binned summary data (Counts and Durations) using data.table
create_binned_data <- function(event_data, bin_width) {
  # Input Validation
  required_cols_binned <- c("StartTime", "Label", "Duration")
  if (!is.data.frame(event_data) ||
      !all(required_cols_binned %in% names(event_data))) {
    stop(
      paste("Input 'event_data' must be a data frame with columns:",
            paste(required_cols_binned, collapse=", "))
    )
  }
  if (!lubridate::is.POSIXct(event_data$StartTime)) {
    stop("'StartTime' column must be POSIXct.")
  }
  if (!is.numeric(event_data$Duration)) {
    stop("'Duration' column must be numeric (in seconds).")
  }
  
  # Setup 
  all_labels <- unique(CHANNEL_LABELS)
  expected_count_cols <- paste("Count", all_labels, sep = "_")
  expected_duration_cols <- paste("TotalDuration", all_labels, sep = "_")
  expected_value_cols <- c(expected_count_cols, expected_duration_cols)
  final_ordered_cols <- c("BinStartTime", expected_value_cols)
  
  # Create Empty Output Structure (Helper)
  create_empty_binned_dt <- function() {
    empty_dt <- data.table::data.table(
      BinStartTime = as.POSIXct(character())
    )
    for (col in expected_value_cols) {
      col_type <- if(startsWith(col, "Count_")) integer(0) else numeric(0)
      empty_dt[, (col) := col_type]
    }
    try(data.table::setcolorder(empty_dt, neworder = final_ordered_cols),
        silent = TRUE)
    return(empty_dt)
  }
  
  # Handle Empty Input
  if (nrow(event_data) == 0) {
    message(
      paste("Input data for binning ('", bin_width, "') is empty.",
            "Returning empty data.table.", sep = "")
    )
    return(create_empty_binned_dt())
  }
  
  message(paste("  Binning data by:", bin_width, "using data.table"))
  
  # Convert to data.table and Aggregate
  event_dt <- data.table::as.data.table(event_data)
  event_dt <- event_dt[!is.na(StartTime)] # Remove rows with NA StartTime
  
  if (nrow(event_dt) == 0) {
    message("No valid StartTime entries found after NA removal for binning.")
    return(create_empty_binned_dt())
  }
  
  # Floor date and aggregate counts/durations
  event_dt[, BinStartTime := lubridate::floor_date(StartTime, unit = bin_width)]
  binned_dt <- event_dt[,
                        .(Count = .N, TotalDuration = sum(Duration, na.rm = TRUE)),
                        by = .(BinStartTime, Label)
  ]
  
  # Reshape to Wide Format
  binned_wide_dt <- tryCatch({
    data.table::dcast(
      binned_dt,
      BinStartTime ~ Label,
      value.var = c("Count", "TotalDuration"),
      fill = 0,
      sep = "_"
    )
  }, error = function(e) {
    message("!!! ERROR during data.table::dcast: ", e$message)
    return(data.table::data.table()) # Return empty table on dcast error
  })
  
  # Ensure All Expected Columns Exist
  current_columns <- names(binned_wide_dt)
  missing_cols <- setdiff(expected_value_cols, current_columns)
  
  if (length(missing_cols) > 0) {
    suppressWarnings({
      for (col in missing_cols) {
        # Assign correct type
        col_type <- if (startsWith(col, "Count_")) 0L else 0.0
        binned_wide_dt[, (col) := col_type]
      }
    })
  }
  
  # Final Structuring and Ordering
  binned_final <- NULL
  if ("BinStartTime" %in% names(binned_wide_dt) && nrow(binned_wide_dt) > 0) {
    # Keep only expected columns
    final_select_cols <- intersect(final_ordered_cols, names(binned_wide_dt))
    binned_wide_dt <- binned_wide_dt[, ..final_select_cols] # data.table subset
    
    # Ensure final column order and row order
    data.table::setcolorder(binned_wide_dt, neworder = final_select_cols)
    data.table::setorder(binned_wide_dt, BinStartTime)
    binned_final <- binned_wide_dt
  } else {
    # Handle cases where dcast failed or produced empty results
    message("  Result after dcast/column handling is empty. Creating empty structure.")
    binned_final <- create_empty_binned_dt()
  }
  
  return(binned_final)
} # End create_binned_data

# Example Usage
# sample_event_data would be replaced with file5_data

# Generate Binned Data for Different Intervals

# 1. Generate 1-minute binned data (like File 6)
binned_by_minute <- create_binned_data(
  event_data = sample_event_data,
  bin_width = "1 min"
)
cat("\n Binned by Minute (First 5 Rows) \n")
print(head(binned_by_minute, 5))


# 2. Generate 1-hour binned data (like File 7)
binned_by_hour <- create_binned_data(
  event_data = sample_event_data,
  bin_width = "1 hour"
)
cat("\n Binned by Hour \n")
print(binned_by_hour)


# 3. Generate 1-day binned data (like File 8)
binned_by_day <- create_binned_data(
  event_data = sample_event_data,
  bin_width = "1 day"
)
cat("\n Binned by Day \n")
print(binned_by_day)
