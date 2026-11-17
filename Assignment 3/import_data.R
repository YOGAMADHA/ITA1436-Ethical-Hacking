# =============================================================================
# import_data.R  -  Module 1/2: reusable import + export helpers
# Base R only. All functions validate their inputs and raise clear errors.
# =============================================================================

# Column layouts the library expects (shared by every module)
SCAN_HOST_COLS    <- c("ip_address", "hostname", "os", "scan_session", "scan_timestamp")
SCAN_SERVICE_COLS <- c("ip_address", "port", "protocol", "service_name",
                       "state", "version", "scan_session", "scan_timestamp")

# Stop with a helpful message unless 'df' is a data frame holding 'required_cols'.
.assert_df <- function(df, required_cols, arg = "df") {
  if (!is.data.frame(df)) {
    stop("'", arg, "' must be a data frame, got: ", class(df)[1], call. = FALSE)
  }
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

# Read a scan CSV as text, normalise column names, check the layout.
#   path          : path to the CSV file
#   required_cols : columns that must exist (extra columns are dropped)
# Returns a data frame of character columns; blank / "NA" / "NULL" become NA.
read_scan_csv <- function(path, required_cols) {
  if (!is.character(path) || length(path) != 1 || is.na(path)) {
    stop("'path' must be a single character string.", call. = FALSE)
  }
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  if (file.info(path)$size == 0) stop("File is empty: ", path, call. = FALSE)

  df <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                    na.strings = c("", "NA", "N/A", "NULL", "null"),
                    strip.white = TRUE),
    error = function(e) stop("Could not read CSV '", path, "': ",
                             conditionMessage(e), call. = FALSE)
  )
  names(df) <- tolower(trimws(names(df)))
  .assert_df(df, required_cols)
  df <- df[, required_cols, drop = FALSE]
  rownames(df) <- NULL
  df
}

# Import a host inventory CSV (columns: SCAN_HOST_COLS).
import_hosts <- function(path) read_scan_csv(path, SCAN_HOST_COLS)

# Import a service scan CSV (TCP and/or UDP rows; columns: SCAN_SERVICE_COLS).
import_services <- function(path) read_scan_csv(path, SCAN_SERVICE_COLS)

# Write a data frame to CSV (creates the folder if needed) and verify the file.
export_csv <- function(df, path) {
  if (!is.data.frame(df)) stop("Cannot export: object is not a data frame.", call. = FALSE)
  if (!is.character(path) || length(path) != 1) stop("'path' must be a single string.", call. = FALSE)
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  utils::write.csv(df, path, row.names = FALSE, na = "")
  if (!file.exists(path) || file.info(path)$size == 0) {
    stop("Export failed or produced an empty file: ", path, call. = FALSE)
  }
  invisible(path)
}

# Print a titled preview of a data frame (first n rows).
print_table <- function(df, title = "", n = 10) {
  if (nzchar(title)) cat("\n--", title, "--\n")
  print(utils::head(df, n), row.names = FALSE)
  if (nrow(df) > n) cat("... (", nrow(df) - n, " more rows; ", nrow(df), " total)\n", sep = "")
  invisible(df)
}
