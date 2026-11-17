# =============================================================================
# host_analysis.R  -  Module 3: host summary and statistics
# Risk score = 5 * High + 3 * Medium + 1 * Low (distinct exposed services)
# =============================================================================

# One row per host IP: open ports, TCP/UDP split, risk counts and score.
#   merged   : output of merge_host_service()
#   exposure : optional output of classify_exposure(merged) (computed if NULL)
summarize_hosts <- function(merged, exposure = NULL) {
  .assert_df(merged, c("ip_address", "hostname", "os", "port", "protocol", "state",
                       "scan_session", "scan_timestamp"), "merged")
  if (nrow(merged) == 0) stop("No records to summarise.", call. = FALSE)
  if (is.null(exposure)) exposure <- classify_exposure(merged)
  .assert_df(exposure, c("ip_address", "port", "protocol", "risk_level", "exposure_status"), "exposure")
  d <- merged
  idx  <- split(seq_len(nrow(d)), d$ip_address)
  eidx <- split(seq_len(nrow(exposure)), exposure$ip_address)
  rows <- lapply(names(idx), function(ip) {
    i <- idx[[ip]]
    opn <- i[d$state[i] == "open"]
    host_names <- d$hostname[i]; host_names <- host_names[host_names != "unknown"]
    e <- eidx[[ip]]
    risk <- character(0)
    if (!is.null(e)) {
      ex <- exposure[e, , drop = FALSE]
      ex <- ex[ex$exposure_status == "Exposed", , drop = FALSE]
      ex <- ex[!duplicated(paste(ex$port, ex$protocol)), , drop = FALSE]
      risk <- ex$risk_level
    }
    hi <- sum(risk == "High"); me <- sum(risk == "Medium"); lo <- sum(risk == "Low")
    data.frame(
      ip_address = ip,
      hostname = if (length(host_names)) host_names[1] else "unknown",
      os = d$os[i[1]],
      records = length(i),
      open_ports = length(unique(paste(d$port[opn], d$protocol[opn]))),
      tcp_open = length(unique(d$port[opn][d$protocol[opn] == "TCP"])),
      udp_open = length(unique(d$port[opn][d$protocol[opn] == "UDP"])),
      possibly_open = sum(d$state[i] == "open|filtered"),
      high_risk = hi, medium_risk = me, low_risk = lo,
      risk_score = 5 * hi + 3 * me + lo,
      overall_risk = if (hi > 0) "High" else if (me > 0) "Medium" else if (lo > 0) "Low" else "None",
      sessions_scanned = length(unique(d$scan_session[i])),
      last_scan = max(d$scan_timestamp[i]),
      stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out <- out[order(-out$risk_score, out$ip_address), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Descriptive statistics (n, min, mean, median, max, sd) for numeric columns.
describe_numeric <- function(df, cols) {
  .assert_df(df, cols)
  rows <- lapply(cols, function(cn) {
    x <- df[[cn]]
    if (!is.numeric(x)) stop("Column '", cn, "' is not numeric.", call. = FALSE)
    data.frame(metric = cn, n = sum(!is.na(x)), min = min(x, na.rm = TRUE),
               mean = round(mean(x, na.rm = TRUE), 2), median = median(x, na.rm = TRUE),
               max = max(x, na.rm = TRUE), sd = round(sd(x, na.rm = TRUE), 2),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
