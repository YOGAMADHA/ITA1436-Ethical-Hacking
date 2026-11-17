# =============================================================================
# udp_extension.R  -  Module 5: UDP data, extra hosts/services, multi-session
# Re-uses the existing validate_/clean_/merge_ functions (no changes to them).
# =============================================================================

# Import a UDP scan CSV (same layout as the TCP services file).
import_udp_scans <- function(path) import_services(path)

# Import -> validate -> clean a UDP scan file.
# Extra rule: rows whose protocol is not UDP are flagged and removed.
process_udp_scans <- function(path) {
  raw <- import_udp_scans(path)
  issues <- validate_services(raw, dataset = "udp_scans")
  wrong <- which(!is.na(raw$protocol) & toupper(raw$protocol) != "UDP")
  extra <- .make_issues("udp_scans", wrong, "protocol", "wrong_protocol_for_dataset", "warning", raw)
  if (!is.null(extra)) issues <- rbind(issues, extra)
  clean <- clean_services(raw)
  clean <- clean[clean$protocol == "UDP", , drop = FALSE]
  rownames(clean) <- NULL
  list(raw = raw, issues = issues, clean = clean)
}

# Import -> validate -> clean a multi-session scan file.
process_multi_session_scans <- function(path) {
  raw <- import_services(path)
  issues <- validate_services(raw, dataset = "multi_session_scans")
  list(raw = raw, issues = issues, clean = clean_services(raw))
}

# Add host rows for (ip, session) pairs seen in services but absent from the
# inventory. Hostname/OS are inherited from the same IP when known.
extend_host_inventory <- function(hosts, services) {
  .assert_df(hosts, SCAN_HOST_COLS, "hosts")
  .assert_df(services, SCAN_SERVICE_COLS, "services")
  d <- services[!duplicated(services[, c("ip_address", "scan_session")]),
                c("ip_address", "scan_session", "scan_timestamp"), drop = FALSE]
  known <- paste(hosts$ip_address, hosts$scan_session)
  new <- d[!(paste(d$ip_address, d$scan_session) %in% known), , drop = FALSE]
  if (nrow(new) == 0) return(hosts)
  m <- match(new$ip_address, hosts$ip_address)
  new$hostname <- ifelse(is.na(m), "unknown", hosts$hostname[m])
  new$os       <- ifelse(is.na(m), "Unknown", hosts$os[m])
  out <- rbind(hosts, new[, SCAN_HOST_COLS])
  rownames(out) <- NULL
  out
}

# Stack several cleaned service data frames (named list) and drop duplicates
# of the same ip/port/protocol/session. A 'scan_source' column records the origin.
combine_scan_sources <- function(sources) {
  if (!is.list(sources) || length(sources) == 0 || is.null(names(sources))) {
    stop("'sources' must be a non-empty named list of data frames.", call. = FALSE)
  }
  parts <- lapply(names(sources), function(nm) {
    d <- sources[[nm]]
    .assert_df(d, SCAN_SERVICE_COLS, nm)
    d <- d[, SCAN_SERVICE_COLS, drop = FALSE]
    d$scan_source <- nm
    d
  })
  out <- do.call(rbind, parts)
  out <- out[!duplicated(out[, c("ip_address", "port", "protocol", "scan_session")]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# One row per scan session: size, protocol split, time span.
summarize_sessions <- function(services) {
  .assert_df(services, c("scan_session", "ip_address", "protocol", "state", "scan_timestamp"), "services")
  if (nrow(services) == 0) stop("No records to summarise.", call. = FALSE)
  rows <- lapply(sort(unique(services$scan_session)), function(s) {
    d <- services[services$scan_session == s, , drop = FALSE]
    data.frame(scan_session = s, records = nrow(d), hosts = length(unique(d$ip_address)),
               tcp_records = sum(d$protocol == "TCP"), udp_records = sum(d$protocol == "UDP"),
               open_records = sum(d$state == "open"), first_scan = min(d$scan_timestamp),
               last_scan = max(d$scan_timestamp), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# Differences between two sessions, restricted to hosts scanned in BOTH.
# change: new_in_b | missing_in_b | state_changed
compare_sessions <- function(services, session_a, session_b) {
  .assert_df(services, c("ip_address", "port", "protocol", "state", "scan_session"), "services")
  key <- c("ip_address", "port", "protocol")
  a <- services[services$scan_session == session_a, c(key, "state"), drop = FALSE]
  b <- services[services$scan_session == session_b, c(key, "state"), drop = FALSE]
  empty <- data.frame(session_a = character(0), session_b = character(0), ip_address = character(0),
                      port = integer(0), protocol = character(0), state_a = character(0),
                      state_b = character(0), change = character(0), stringsAsFactors = FALSE)
  if (nrow(a) == 0 || nrow(b) == 0) return(empty)
  both <- intersect(a$ip_address, b$ip_address)
  a <- a[a$ip_address %in% both, , drop = FALSE]
  b <- b[b$ip_address %in% both, , drop = FALSE]
  m <- merge(a, b, by = key, all = TRUE, suffixes = c("_a", "_b"))
  m$change <- ifelse(is.na(m$state_a), "new_in_b",
              ifelse(is.na(m$state_b), "missing_in_b",
              ifelse(m$state_a != m$state_b, "state_changed", "unchanged")))
  m <- m[m$change != "unchanged", , drop = FALSE]
  if (nrow(m) == 0) return(empty)
  m$session_a <- session_a; m$session_b <- session_b
  m <- m[order(m$ip_address, m$port), c("session_a", "session_b", "ip_address", "port",
                                        "protocol", "state_a", "state_b", "change")]
  rownames(m) <- NULL
  m
}

# Compare every consecutive pair of sessions (sorted by session ID).
compare_all_sessions <- function(services) {
  s <- sort(unique(services$scan_session))
  if (length(s) < 2) stop("Need at least two scan sessions to compare.", call. = FALSE)
  do.call(rbind, lapply(seq_len(length(s) - 1), function(i) compare_sessions(services, s[i], s[i + 1])))
}

# Run the whole extension: UDP + multi-session + additional hosts/services.
run_udp_extension <- function(hosts_clean, services_clean, udp_path, multi_path) {
  udp   <- process_udp_scans(udp_path)
  multi <- process_multi_session_scans(multi_path)
  combined_services <- combine_scan_sources(list(tcp_baseline = services_clean,
                                                 udp = udp$clean, multi_session = multi$clean))
  combined_hosts <- extend_host_inventory(hosts_clean, combined_services)
  merged_all <- merge_host_service(combined_hosts, combined_services)
  udp_merged <- merged_all[merged_all$protocol == "UDP", , drop = FALSE]
  list(udp = udp, multi = multi, udp_clean = udp$clean, multi_clean = multi$clean,
       combined_services = combined_services, combined_hosts = combined_hosts,
       merged_all = merged_all,
       udp_summary = if (nrow(udp_merged) > 0) summarize_services(udp_merged) else data.frame(),
       session_summary = summarize_sessions(combined_services),
       session_changes = compare_all_sessions(combined_services))
}

# Backward-compatibility check: re-run the ORIGINAL TCP pipeline after the
# extension was loaded and confirm the summaries are identical to the baseline.
check_backward_compatibility <- function(hosts_path, services_path, baseline_host_summary,
                                         baseline_service_summary) {
  r <- process_scan_data(import_hosts(hosts_path), import_services(services_path))
  hs <- summarize_hosts(r$merged, classify_exposure(r$merged))
  ss <- summarize_services(r$merged)
  ok_h <- isTRUE(all.equal(hs, baseline_host_summary))
  ok_s <- isTRUE(all.equal(ss, baseline_service_summary))
  list(ok = ok_h && ok_s, host_summary_identical = ok_h, service_summary_identical = ok_s)
}
