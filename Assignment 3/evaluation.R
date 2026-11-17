# =============================================================================
# evaluation.R  -  Module 6: joint evaluation
# Every test is computed from real objects produced by the run; nothing is
# hard-coded as "PASS". Pass criteria (thresholds) are stated in the 'expected'
# column and can be changed here.
# =============================================================================

# TRUE/FALSE + short text for a test that must raise an error matching 'pattern'.
.expect_error <- function(expr, pattern) {
  res <- tryCatch({ force(expr); "no error raised" }, error = function(e) conditionMessage(e))
  list(pass = !identical(res, "no error raised") && grepl(pattern, res, ignore.case = TRUE),
       actual = substr(res, 1, 70))
}

# Small hand-made data used for known-answer tests.
.mini_services <- function() {
  data.frame(
    ip_address = c("192.0.2.1", "192.0.2.1", "192.0.2.1", "192.0.2.2", "192.0.2.2"),
    port = c(23L, 443L, 22L, 23L, 161L),
    protocol = c("TCP", "TCP", "TCP", "TCP", "UDP"),
    service_name = c("telnet", "https", "ssh", "telnet", "snmp"),
    state = c("open", "open", "closed", "open", "open|filtered"),
    version = rep("x", 5), scan_session = rep("T1", 5),
    scan_timestamp = rep("2026-01-01 00:00:00", 5), stringsAsFactors = FALSE)
}
.mini_hosts <- function() {
  data.frame(ip_address = c("192.0.2.1", "192.0.2.2"), hostname = c("a.example.test", "b.example.test"),
             os = c("Debian 12", "Ubuntu 22.04"), scan_session = "T1",
             scan_timestamp = "2026-01-01 00:00:00", stringsAsFactors = FALSE)
}

# Run all evaluation tests.
#   ctx : list with  paths (named list of expected output files), hosts_raw, services_raw,
#         validation_issues, hosts, services, merged, host_summary, exposure,
#         stress (data frame), stress_cmp (compare_stress_results()), ext, compat
# Returns a data frame: category, test, expected, actual, result (+ category summary rows).
run_evaluation <- function(ctx) {
  tests <- list()
  add <- function(category, test, expected, actual, pass) {
    tests[[length(tests) + 1]] <<- data.frame(category = category, test = test,
      expected = expected, actual = as.character(actual),
      result = if (isTRUE(pass)) "PASS" else "FAIL", stringsAsFactors = FALSE)
  }

  # ---- Functionality -------------------------------------------------------
  present <- vapply(ctx$paths, function(p) file.exists(p) && file.info(p)$size > 0, logical(1))
  add("Functionality", "Required output CSVs exist and are non-empty", "all present",
      paste0(sum(present), "/", length(present), " present"), all(present))
  add("Functionality", "Merge keeps one row per cleaned service record",
      "rows(merged) == rows(cleaned services)", paste(nrow(ctx$merged), "vs", nrow(ctx$services)),
      nrow(ctx$merged) == nrow(ctx$services))
  missing_ips <- setdiff(unique(ctx$merged$ip_address), ctx$host_summary$ip_address)
  add("Functionality", "Host summary covers every scanned host", "0 hosts missing",
      paste(length(missing_ips), "missing"), length(missing_ips) == 0)
  add("Functionality", "Exposure report lists only open / possibly-open services",
      "states subset of open, open|filtered", paste(sort(unique(ctx$exposure$state)), collapse = ","),
      nrow(ctx$exposure) > 0 && all(ctx$exposure$state %in% c("open", "open|filtered")))

  # ---- Accuracy (known-answer tests) ---------------------------------------
  ip_ok <- identical(is_valid_ipv4(c("192.0.2.1", "256.1.1.1", "192.0.2", "a.b.c.d", NA)),
                     c(TRUE, FALSE, FALSE, FALSE, FALSE))
  add("Accuracy", "is_valid_ipv4 on 5 known inputs", "T,F,F,F,F", ip_ok, ip_ok)
  port_ok <- identical(is_valid_port(c("22", "0", "65535", "65536", "abc", "-5", NA)),
                       c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE))
  add("Accuracy", "is_valid_port on 7 known inputs", "T,F,T,F,F,F,F", port_ok, port_ok)
  ts_ok <- identical(is_valid_timestamp(c("2026-03-01 09:00:00", "2026-13-45 99:00:00", NA)),
                     c(TRUE, FALSE, FALSE))
  add("Accuracy", "is_valid_timestamp on 3 known inputs", "T,F,F", ts_ok, ts_ok)
  ex <- classify_exposure(.mini_services())
  add("Accuracy", "classify_exposure: 4 exposed rows, 2 High (telnet)", "4 rows / 2 High",
      paste(nrow(ex), "rows /", sum(ex$risk_level == "High"), "High"),
      nrow(ex) == 4 && sum(ex$risk_level == "High") == 2)
  mm <- merge_host_service(.mini_hosts(), .mini_services())
  hs <- summarize_hosts(mm)
  hs <- hs[order(hs$ip_address), ]
  add("Accuracy", "summarize_hosts open_ports for 2 known hosts", "2 and 1",
      paste(hs$open_ports, collapse = " and "), identical(as.integer(hs$open_ports), c(2L, 1L)))
  dup <- rbind(.mini_services(), .mini_services()[1, ]); dup$protocol[6] <- "tcp"
  add("Accuracy", "clean_services removes duplicate after case-normalising", "5 rows",
      paste(nrow(clean_services(dup)), "rows"), nrow(clean_services(dup)) == 5)

  # ---- Data validation -----------------------------------------------------
  vi <- ctx$validation_issues
  add("Data validation", "Dirty input produces error-severity issues in hosts and services",
      "issues in both datasets", paste(sum(vi$severity == "error" & vi$dataset == "hosts"), "host /",
      sum(vi$severity == "error" & vi$dataset == "services"), "service"),
      any(vi$severity == "error" & vi$dataset == "hosts") && any(vi$severity == "error" & vi$dataset == "services"))
  re <- rbind(validate_hosts(ctx$hosts), validate_services(ctx$services))
  add("Data validation", "Cleaned data re-validates with 0 issues", "0 issues",
      paste(nrow(re), "issues"), nrow(re) == 0)
  bad <- .mini_services(); bad$port <- as.character(bad$port); bad$port[1] <- "70000"
  bad$state[2] <- NA; bad <- rbind(bad, bad[3, ])
  kinds <- unique(validate_services(bad)$issue)
  add("Data validation", "Crafted bad data triggers invalid, missing and duplicate checks",
      "3 issue types", paste(sort(kinds), collapse = ","),
      all(c("invalid_value", "missing_value", "duplicate_record") %in% kinds))

  # ---- Error handling ------------------------------------------------------
  e1 <- .expect_error(import_hosts("no_such_file.csv"), "not found")
  add("Error handling", "Missing file -> clear error", "error 'not found'", e1$actual, e1$pass)
  e2 <- .expect_error(validate_services("not a data frame"), "data frame")
  add("Error handling", "Non-data-frame input -> clear error", "error 'data frame'", e2$actual, e2$pass)
  e3 <- .expect_error(clean_services(.mini_services()[, 1:3]), "missing required")
  add("Error handling", "Missing columns -> clear error", "error 'missing required'", e3$actual, e3$pass)
  tmp <- tempfile(fileext = ".csv"); writeLines(c("ip_address,port", "192.0.2.1,22"), tmp)
  e4 <- .expect_error(import_services(tmp), "missing required")
  add("Error handling", "CSV with wrong layout -> clear error", "error 'missing required'", e4$actual, e4$pass)
  e5 <- .expect_error(classify_exposure(NULL), "data frame")
  add("Error handling", "NULL input -> clear error", "error 'data frame'", e5$actual, e5$pass)
  e6 <- .expect_error(generate_synthetic_scan_data(3), ">= 10")
  add("Error handling", "Invalid size argument -> clear error", "error '>= 10'", e6$actual, e6$pass)

  # ---- Performance ---------------------------------------------------------
  st <- ctx$stress
  big <- st[which.max(st$records), ]; small <- st[which.min(st$records), ]
  add("Performance", paste0(small$records, " records finish in < 5 s"), "< 5 s",
      paste0(small$total_sec, " s"), !is.na(small$total_sec) && small$total_sec < 5)
  add("Performance", paste0(big$records, " records finish in < 30 s"), "< 30 s",
      paste0(big$total_sec, " s"), !is.na(big$total_sec) && big$total_sec < 30)
  add("Performance", "No failed stress-test runs", "0 failed", paste(sum(st$failed_runs), "failed"),
      sum(st$failed_runs) == 0)

  # ---- Reusability ---------------------------------------------------------
  r1 <- tryCatch(process_scan_data(generate_synthetic_scan_data(200, seed = 7)$hosts,
                                   generate_synthetic_scan_data(200, seed = 7)$services),
                 error = function(e) NULL)
  add("Reusability", "Same pipeline runs unchanged on a new 200-record dataset", "rows > 0",
      if (is.null(r1)) "error" else paste(nrow(r1$merged), "merged rows"),
      !is.null(r1) && nrow(r1$merged) > 0)
  add("Reusability", "Same cleaning/merge functions handle UDP and multi-session files",
      "both > 0 rows", paste(nrow(ctx$ext$udp_clean), "UDP /", nrow(ctx$ext$multi_clean), "multi-session"),
      nrow(ctx$ext$udp_clean) > 0 && nrow(ctx$ext$multi_clean) > 0)

  # ---- Scalability ---------------------------------------------------------
  sc <- ctx$stress_cmp$scaling_factor
  add("Scalability", "Time grows no faster than 1.5x the record growth", "scaling factor <= 1.5",
      sc, !is.na(sc) && sc <= 1.5)
  add("Scalability", "Cost per record at large size <= 3x cost at small size", "ratio <= 3",
      round(big$ms_per_record / max(small$ms_per_record, 1e-9), 2),
      !is.na(big$ms_per_record) && big$ms_per_record <= 3 * max(small$ms_per_record, 1e-9))

  # ---- Overall reliability -------------------------------------------------
  add("Overall reliability", "Existing TCP pipeline unchanged after UDP/multi-session extension",
      "summaries identical", paste0("host=", ctx$compat$host_summary_identical,
      ", service=", ctx$compat$service_summary_identical), isTRUE(ctx$compat$ok))
  res <- do.call(rbind, tests)
  rate <- mean(res$result == "PASS")
  add("Overall reliability", "At least 90% of all tests pass", ">= 90%",
      paste0(round(100 * rate, 1), "% (", sum(res$result == "PASS"), "/", nrow(res), ")"), rate >= 0.9)
  res <- do.call(rbind, tests)

  # Category summary rows appended at the end
  cats <- unique(res$category)
  summ <- do.call(rbind, lapply(cats, function(cn) {
    r <- res[res$category == cn, ]
    data.frame(category = cn, test = "CATEGORY SUMMARY", expected = "all tests pass",
               actual = paste0(sum(r$result == "PASS"), "/", nrow(r), " passed"),
               result = if (all(r$result == "PASS")) "PASS" else "FAIL", stringsAsFactors = FALSE)
  }))
  out <- rbind(res, summ)
  rownames(out) <- NULL
  out
}
