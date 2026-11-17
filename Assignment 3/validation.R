# =============================================================================
# validation.R  -  Module 2: record-level validation
# Every validator is vectorised and NA-safe (NA is never "valid").
# validate_hosts()/validate_services() return a long "issues" table:
#   dataset | row | field | issue | severity | value
# 'row' is the data-row number (file line = row + 1 because of the header).
# =============================================================================

# TRUE for well-formed IPv4 addresses (4 octets, each 0-255).
is_valid_ipv4 <- function(x) {
  x <- as.character(x)
  out <- rep(FALSE, length(x))
  ok <- !is.na(x) & grepl("^[0-9]{1,3}(\\.[0-9]{1,3}){3}$", x)
  if (any(ok)) {
    out[ok] <- vapply(strsplit(x[ok], ".", fixed = TRUE),
                      function(p) all(as.integer(p) <= 255L), logical(1))
  }
  out
}

# TRUE for whole-number ports 1-65535 (rejects "abc", "-5", "0", "70000", "22.5").
is_valid_port <- function(x) {
  x <- as.character(x)
  out <- rep(FALSE, length(x))
  ok <- !is.na(x) & grepl("^[0-9]+$", x)
  if (any(ok)) {
    n <- suppressWarnings(as.numeric(x[ok]))
    out[ok] <- !is.na(n) & n >= 1 & n <= 65535
  }
  out
}

# TRUE for TCP / UDP (case-insensitive).
is_valid_protocol <- function(x) {
  x <- as.character(x)
  !is.na(x) & toupper(x) %in% c("TCP", "UDP")
}

# TRUE for nmap-style port states.
is_valid_state <- function(x) {
  x <- as.character(x)
  !is.na(x) & tolower(x) %in% c("open", "closed", "filtered", "open|filtered")
}

# Parse "YYYY-mm-dd HH:MM:SS" (UTC) -> POSIXct; unparseable values become NA.
parse_timestamp <- function(x) {
  as.POSIXct(as.character(x), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

# TRUE for parseable timestamps.
is_valid_timestamp <- function(x) {
  x <- as.character(x)
  !is.na(x) & !is.na(parse_timestamp(x))
}

# TRUE for scan-session IDs made of letters, digits, '_' and '-'.
is_valid_scan_session <- function(x) {
  x <- as.character(x)
  !is.na(x) & grepl("^[A-Za-z0-9_-]+$", x)
}

# Build issue rows for one (field, issue) combination; NULL when nothing found.
.make_issues <- function(dataset, rows, field, issue, severity, df) {
  if (length(rows) == 0) return(NULL)
  vals <- if (field %in% names(df)) as.character(df[[field]][rows]) else rep(NA_character_, length(rows))
  data.frame(dataset = dataset, row = rows, field = field, issue = issue,
             severity = severity, value = vals, stringsAsFactors = FALSE)
}

# Generic validator used by validate_hosts()/validate_services().
#   rules    : named list  field -> validator function
#   required : fields whose problems are severity "error"
#   optional : fields whose problems are severity "warning"
#   key_cols : columns that must be unique (conflicting duplicates)
validate_dataset <- function(df, dataset, rules, required, optional, key_cols) {
  .assert_df(df, c(required, optional))
  issues <- list()
  for (f in c(required, optional)) {
    sev <- if (f %in% required) "error" else "warning"
    miss <- which(is.na(df[[f]]))
    issues[[length(issues) + 1]] <- .make_issues(dataset, miss, f, "missing_value", sev, df)
    if (!is.null(rules[[f]])) {
      bad <- which(!is.na(df[[f]]) & !rules[[f]](df[[f]]))
      issues[[length(issues) + 1]] <- .make_issues(dataset, bad, f, "invalid_value", sev, df)
    }
  }
  full_dup <- duplicated(df)
  key_dup  <- duplicated(df[, key_cols, drop = FALSE]) & !full_dup
  issues[[length(issues) + 1]] <- .make_issues(dataset, which(full_dup), key_cols[1],
                                               "duplicate_record", "error", df)
  issues[[length(issues) + 1]] <- .make_issues(dataset, which(key_dup), key_cols[1],
                                               "conflicting_duplicate_key", "error", df)
  out <- do.call(rbind, issues)
  if (is.null(out)) {
    out <- data.frame(dataset = character(0), row = integer(0), field = character(0),
                      issue = character(0), severity = character(0), value = character(0),
                      stringsAsFactors = FALSE)
  }
  rownames(out) <- NULL
  out
}

# Validate a host inventory data frame.
validate_hosts <- function(df, dataset = "hosts") {
  validate_dataset(df, dataset,
    rules = list(ip_address = is_valid_ipv4, scan_session = is_valid_scan_session,
                 scan_timestamp = is_valid_timestamp),
    required = c("ip_address", "scan_session", "scan_timestamp"),
    optional = c("hostname", "os"),
    key_cols = c("ip_address", "scan_session"))
}

# Validate a service scan data frame (TCP and/or UDP).
validate_services <- function(df, dataset = "services") {
  validate_dataset(df, dataset,
    rules = list(ip_address = is_valid_ipv4, port = is_valid_port,
                 protocol = is_valid_protocol, state = is_valid_state,
                 scan_session = is_valid_scan_session, scan_timestamp = is_valid_timestamp),
    required = c("ip_address", "port", "protocol", "state", "scan_session", "scan_timestamp"),
    optional = c("service_name", "version"),
    key_cols = c("ip_address", "port", "protocol", "scan_session"))
}

# Count issues per dataset / field / issue / severity.
summarize_issues <- function(issues) {
  if (!is.data.frame(issues)) stop("'issues' must be a data frame.", call. = FALSE)
  if (nrow(issues) == 0) {
    return(data.frame(dataset = character(0), field = character(0), issue = character(0),
                      severity = character(0), count = integer(0), stringsAsFactors = FALSE))
  }
  tb <- as.data.frame(table(dataset = issues$dataset, field = issues$field,
                            issue = issues$issue, severity = issues$severity),
                      stringsAsFactors = FALSE)
  tb <- tb[tb$Freq > 0, , drop = FALSE]
  names(tb)[names(tb) == "Freq"] <- "count"
  tb <- tb[order(tb$dataset, -tb$count), , drop = FALSE]
  rownames(tb) <- NULL
  tb
}
