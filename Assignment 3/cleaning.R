# =============================================================================
# cleaning.R  -  Module 2: cleaning, transformation, merging, pipeline
# Cleaning rules (documented so the report can cite them):
#   * trim whitespace, blank -> NA
#   * drop rows whose REQUIRED fields are missing/invalid (ip, port, protocol,
#     state, session, timestamp)
#   * standardise: protocol UPPER, state/hostname/service_name lower
#   * fill optional gaps: hostname/service_name/version -> "unknown", os -> "Unknown"
#   * remove duplicates (after standardising) keeping the first occurrence
#   * timestamps re-written as "YYYY-mm-dd HH:MM:SS" text; port -> integer
# =============================================================================

# NA-safe case helpers (base toupper/tolower can turn NA into "NA")
.to_upper <- function(x) { y <- toupper(x); y[is.na(x)] <- NA_character_; y }
.to_lower <- function(x) { y <- tolower(x); y[is.na(x)] <- NA_character_; y }

# Trim whitespace and turn empty strings into NA for every column.
.std_text <- function(x) {
  x <- trimws(as.character(x))
  x[!is.na(x) & x == ""] <- NA_character_
  x
}

# Clean a host inventory data frame (see rules above). Returns a new data frame.
clean_hosts <- function(df) {
  .assert_df(df, SCAN_HOST_COLS)
  d <- df[, SCAN_HOST_COLS, drop = FALSE]
  d[] <- lapply(d, .std_text)
  keep <- is_valid_ipv4(d$ip_address) & is_valid_scan_session(d$scan_session) &
          is_valid_timestamp(d$scan_timestamp)
  d <- d[keep, , drop = FALSE]
  d$hostname <- .to_lower(d$hostname)
  d$hostname[is.na(d$hostname)] <- "unknown"
  d$os[is.na(d$os)] <- "Unknown"
  d$scan_timestamp <- format(parse_timestamp(d$scan_timestamp), "%Y-%m-%d %H:%M:%S")
  d <- d[!duplicated(d[, c("ip_address", "scan_session")]), , drop = FALSE]
  rownames(d) <- NULL
  d
}

# Clean a service scan data frame (TCP and/or UDP). Returns a new data frame.
clean_services <- function(df) {
  .assert_df(df, SCAN_SERVICE_COLS)
  d <- df[, SCAN_SERVICE_COLS, drop = FALSE]
  d[] <- lapply(d, .std_text)
  d$protocol <- .to_upper(d$protocol)
  d$state    <- .to_lower(d$state)
  keep <- is_valid_ipv4(d$ip_address) & is_valid_port(d$port) &
          is_valid_protocol(d$protocol) & is_valid_state(d$state) &
          is_valid_scan_session(d$scan_session) & is_valid_timestamp(d$scan_timestamp)
  d <- d[keep, , drop = FALSE]
  d$port <- as.integer(d$port)
  d$service_name <- .to_lower(d$service_name)
  d$service_name[is.na(d$service_name)] <- "unknown"
  d$version[is.na(d$version)] <- "unknown"
  d$scan_timestamp <- format(parse_timestamp(d$scan_timestamp), "%Y-%m-%d %H:%M:%S")
  d <- d[!duplicated(d[, c("ip_address", "port", "protocol", "scan_session")]), , drop = FALSE]
  rownames(d) <- NULL
  d
}

# Merge cleaned hosts into cleaned services (one row per service record).
# Services whose host is missing from the inventory are kept and flagged with
# host_in_inventory = FALSE (hostname "unknown", os "Unknown").
merge_host_service <- function(hosts, services) {
  .assert_df(hosts, c("ip_address", "scan_session", "hostname", "os"), "hosts")
  .assert_df(services, SCAN_SERVICE_COLS, "services")
  h <- hosts[!duplicated(hosts[, c("ip_address", "scan_session")]),
             c("ip_address", "scan_session", "hostname", "os"), drop = FALSE]
  h$host_in_inventory <- TRUE
  m <- merge(services, h, by = c("ip_address", "scan_session"), all.x = TRUE, sort = FALSE)
  m$host_in_inventory[is.na(m$host_in_inventory)] <- FALSE
  m$hostname[is.na(m$hostname)] <- "unknown"
  m$os[is.na(m$os)] <- "Unknown"
  cols <- c("scan_session", "ip_address", "hostname", "os", "port", "protocol",
            "service_name", "state", "version", "scan_timestamp", "host_in_inventory")
  m <- m[, cols, drop = FALSE]
  m <- m[order(m$scan_session, m$ip_address, m$protocol, m$port), , drop = FALSE]
  rownames(m) <- NULL
  m
}

# Record counts before/after cleaning for the datasets of one pipeline run.
build_count_table <- function(hosts_raw, hosts_clean, services_raw, services_clean, merged) {
  data.frame(
    dataset = c("hosts", "services", "merged"),
    records_before = c(nrow(hosts_raw), nrow(services_raw), nrow(services_clean)),
    records_after  = c(nrow(hosts_clean), nrow(services_clean), nrow(merged)),
    stringsAsFactors = FALSE
  ) -> out
  out$records_removed <- out$records_before - out$records_after
  out
}

# One-call pipeline used by main.R, the stress tests and the UDP extension:
# validate -> clean -> merge. Works on any data frames with the standard columns.
process_scan_data <- function(hosts_raw, services_raw) {
  .assert_df(hosts_raw, SCAN_HOST_COLS, "hosts_raw")
  .assert_df(services_raw, SCAN_SERVICE_COLS, "services_raw")
  issues <- rbind(validate_hosts(hosts_raw), validate_services(services_raw))
  hosts    <- clean_hosts(hosts_raw)
  services <- clean_services(services_raw)
  merged   <- merge_host_service(hosts, services)
  list(issues = issues, hosts = hosts, services = services, merged = merged,
       counts = build_count_table(hosts_raw, hosts, services_raw, services, merged))
}
