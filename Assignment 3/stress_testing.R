# =============================================================================
# stress_testing.R  -  Module 4: synthetic data generator + benchmark harness
# All timings are measured with proc.time() on the machine that runs main.R.
# =============================================================================

STRESS_STAGES <- c("validation", "cleaning", "merge", "exposure", "host_summary", "service_summary")

# Generate SYNTHETIC scan data with a known share of dirty records.
#   n_records  : number of service records
#   seed       : RNG seed (reproducible)
#   dirty_rate : approximate share of deliberately bad rows
# Returns list(hosts, services) using documentation-only IP ranges (RFC 5737).
generate_synthetic_scan_data <- function(n_records, seed = 42, dirty_rate = 0.08) {
  if (!is.numeric(n_records) || length(n_records) != 1 || n_records < 10) {
    stop("'n_records' must be a single number >= 10.", call. = FALSE)
  }
  n_records <- as.integer(n_records)
  set.seed(seed)
  prefixes <- c("192.0.2", "198.51.100", "203.0.113")
  all_ips  <- as.vector(outer(prefixes, 1:254, paste, sep = "."))
  n_hosts  <- max(5L, min(length(all_ips), as.integer(ceiling(n_records / 10))))
  ips      <- sample(all_ips, n_hosts)
  base     <- as.POSIXct("2026-01-15 08:00:00", tz = "UTC")
  stamp    <- function(n) format(base + sample(0:86400, n, replace = TRUE), "%Y-%m-%d %H:%M:%S")

  hosts <- data.frame(
    ip_address = ips,
    hostname = sprintf("host-%03d.example.test", seq_len(n_hosts)),
    os = sample(c("Windows Server 2019", "Ubuntu 22.04", "Debian 12", "FreeBSD 13", "Cisco IOS"),
                n_hosts, replace = TRUE),
    scan_session = "SYN-STRESS", scan_timestamp = stamp(n_hosts), stringsAsFactors = FALSE)

  ports <- c(21, 22, 23, 25, 53, 69, 80, 110, 123, 135, 139, 161, 443, 445, 512, 1433, 3306, 3389, 5900, 8080)
  names_ <- c("ftp", "ssh", "telnet", "smtp", "domain", "tftp", "http", "pop3", "ntp", "msrpc",
              "netbios-ssn", "snmp", "https", "microsoft-ds", "exec", "ms-sql", "mysql",
              "ms-wbt-server", "vnc", "http-proxy")
  pidx <- sample(seq_along(ports), n_records, replace = TRUE)
  services <- data.frame(
    ip_address = sample(ips, n_records, replace = TRUE),
    port = as.character(ports[pidx]),
    protocol = ifelse(ports[pidx] %in% c(53, 69, 123, 161), "UDP", "TCP"),
    service_name = names_[pidx],
    state = sample(c("open", "closed", "filtered", "open|filtered"), n_records,
                   replace = TRUE, prob = c(0.45, 0.35, 0.15, 0.05)),
    version = paste0("v", sample(1:5, n_records, replace = TRUE), ".", sample(0:9, n_records, replace = TRUE)),
    scan_session = "SYN-STRESS", scan_timestamp = stamp(n_records), stringsAsFactors = FALSE)

  # Inject dirty rows: invalid IP, invalid port, missing service name, exact duplicates
  k <- max(1L, as.integer(floor(n_records * dirty_rate / 4)))
  pick <- sample(n_records, min(n_records, 4L * k))
  g <- split(pick, rep(1:4, length.out = length(pick)))
  services$ip_address[g[[1]]]   <- "999.0.0.1"
  services$port[g[[2]]]         <- "70000"
  services$service_name[g[[3]]] <- NA_character_
  src <- sample(setdiff(seq_len(n_records), g[[4]]), length(g[[4]]))
  services[g[[4]], ] <- services[src, ]
  hosts$hostname[sample(n_hosts, max(1L, n_hosts %/% 20L))] <- NA_character_
  hosts <- rbind(hosts, hosts[1, ])                 # one exact duplicate host
  rownames(hosts) <- NULL; rownames(services) <- NULL
  list(hosts = hosts, services = services)
}

# Time an expression; returns list(value, seconds).
.time_stage <- function(expr) {
  t0 <- proc.time()[["elapsed"]]
  val <- force(expr)
  list(value = val, seconds = proc.time()[["elapsed"]] - t0)
}

# Run every pipeline stage once and time each stage.
# Returns list(times = named numeric, total, records_after, issues).
benchmark_pipeline <- function(hosts, services) {
  times <- numeric(0)
  run <- function(name, expr) {
    r <- .time_stage(expr)
    times[name] <<- r$seconds
    r$value
  }
  issues   <- run("validation", rbind(validate_hosts(hosts), validate_services(services)))
  cl       <- run("cleaning", list(h = clean_hosts(hosts), s = clean_services(services)))
  merged   <- run("merge", merge_host_service(cl$h, cl$s))
  exposure <- run("exposure", classify_exposure(merged))
  hs       <- run("host_summary", summarize_hosts(merged, exposure))
  ss       <- run("service_summary", summarize_services(merged))
  list(times = times, total = sum(times), records_after = nrow(merged), issues = nrow(issues))
}

# Benchmark the full pipeline on 'n_records' synthetic records.
#   repetitions : number of timed runs (median reported); errors are captured, not fatal
# Returns a one-row data frame (times in seconds).
run_stress_test <- function(n_records, repetitions = 3L, seed = 42, verbose = TRUE) {
  if (!is.numeric(n_records) || length(n_records) != 1 || n_records < 10) {
    stop("'n_records' must be a single number >= 10.", call. = FALSE)
  }
  data <- generate_synthetic_scan_data(n_records, seed)
  errs <- character(0)
  runs <- vector("list", repetitions)
  for (r in seq_len(repetitions)) {
    runs[[r]] <- tryCatch(benchmark_pipeline(data$hosts, data$services),
                          error = function(e) { errs <<- c(errs, conditionMessage(e)); NULL })
  }
  ok <- Filter(Negate(is.null), runs)
  row <- list(records = as.integer(n_records), repetitions = repetitions,
              successful_runs = length(ok), failed_runs = length(errs))
  if (length(ok) > 0) {
    med <- sapply(STRESS_STAGES, function(s) stats::median(sapply(ok, function(x) x$times[[s]])))
    for (s in STRESS_STAGES) row[[paste0(s, "_sec")]] <- round(med[[s]], 4)
    total <- sum(med)
    row$total_sec <- round(total, 4)
    row$ms_per_record <- round(1000 * total / n_records, 4)
    row$bottleneck_stage <- names(which.max(med))
    row$bottleneck_pct <- round(100 * max(med) / total, 1)
    row$records_after_cleaning <- ok[[1]]$records_after
    row$validation_issues <- ok[[1]]$issues
  } else {
    for (s in STRESS_STAGES) row[[paste0(s, "_sec")]] <- NA_real_
    row$total_sec <- NA_real_; row$ms_per_record <- NA_real_
    row$bottleneck_stage <- NA_character_; row$bottleneck_pct <- NA_real_
    row$records_after_cleaning <- NA_integer_; row$validation_issues <- NA_integer_
  }
  row$error_message <- if (length(errs)) paste(unique(errs), collapse = "; ") else ""
  res <- as.data.frame(row, stringsAsFactors = FALSE)
  if (verbose) {
    cat("\nStress test:", n_records, "records,", repetitions, "runs (median seconds)\n")
    print(t(res[, c("successful_runs", "failed_runs", paste0(STRESS_STAGES, "_sec"), "total_sec",
                    "ms_per_record", "bottleneck_stage", "bottleneck_pct",
                    "records_after_cleaning", "validation_issues")]), quote = FALSE)
  }
  res
}

# Compare the smallest and largest stress-test rows.
# scaling_factor = time ratio / record ratio  (1 = linear, <1 = better than linear)
compare_stress_results <- function(results) {
  .assert_df(results, c("records", "total_sec", "ms_per_record", "records_after_cleaning"), "results")
  results <- results[!is.na(results$total_sec), , drop = FALSE]
  if (nrow(results) < 2) stop("Need at least two successful stress-test sizes to compare.", call. = FALSE)
  small <- results[which.min(results$records), ]
  large <- results[which.max(results$records), ]
  cmp <- data.frame(
    metric = c("records", "total_sec", "ms_per_record", "records_after_cleaning"),
    small = c(small$records, small$total_sec, small$ms_per_record, small$records_after_cleaning),
    large = c(large$records, large$total_sec, large$ms_per_record, large$records_after_cleaning),
    stringsAsFactors = FALSE)
  cmp$ratio <- round(cmp$large / pmax(cmp$small, 1e-9), 2)
  scaling <- (large$total_sec / max(small$total_sec, 1e-6)) / (large$records / small$records)
  list(table = cmp, scaling_factor = round(scaling, 3),
       interpretation = if (scaling <= 1.1) "near-linear or better than linear scaling"
                        else if (scaling <= 2) "mildly super-linear scaling"
                        else "clearly super-linear scaling (investigate bottleneck)")
}
