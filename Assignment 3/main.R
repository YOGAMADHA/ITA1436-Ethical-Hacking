# =============================================================================
# main.R  -  Q7 Reusable Scanning & Enumeration R Function Library
# Course: Ethical Hacking (ITA14)  |  Assessment Tool 3  |  Q7
#
# HOW TO RUN (RStudio on Windows):
#   1. Double-click Q7_Reusable_Scanning_Enumeration_R_Library.Rproj
#      (or File > Open Project / set the working directory to the project folder)
#   2. Open R/main.R and click "Source"   -- or run in the Console:
#        source("R/main.R")
#   From a terminal:  Rscript R/main.R
#
# The script NEVER touches the network. It only reads the synthetic CSV files
# in data/ and writes results to outputs/.
# =============================================================================

options(stringsAsFactors = FALSE, width = 160, warn = 1)

# --- locate the project folder, whatever way the script was started ----------
find_project_root <- function() {
  candidates <- c(getwd(), dirname(getwd()))
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f) == 1) candidates <- c(dirname(dirname(normalizePath(f, winslash = "/"))), candidates)
  of <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(of)) candidates <- c(dirname(dirname(normalizePath(of, winslash = "/"))), candidates)
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(p)) candidates <- c(dirname(dirname(p)), candidates)
  }
  for (cand in unique(candidates)) {
    if (file.exists(file.path(cand, "R", "main.R")) && dir.exists(file.path(cand, "data"))) {
      return(normalizePath(cand, winslash = "/"))
    }
  }
  stop("Cannot find the project folder. Open the .Rproj file or use setwd() to the folder ",
       "that contains R/, data/ and outputs/.", call. = FALSE)
}

banner <- function(txt) cat("\n", strrep("=", 78), "\n  ", txt, "\n", strrep("=", 78), "\n", sep = "")

ROOT <- find_project_root()
P <- function(...) file.path(ROOT, ...)

# =============================================================================
banner("MODULE 1: FUNCTION LIBRARY - loading R/ files")
# =============================================================================
lib_files <- c("import_data.R", "validation.R", "cleaning.R", "host_analysis.R",
               "service_analysis.R", "stress_testing.R", "udp_extension.R", "evaluation.R")
for (f in lib_files) {
  before <- ls(globalenv())
  source(P("R", f), local = FALSE)
  new_fun <- setdiff(ls(globalenv()), before)
  new_fun <- new_fun[vapply(new_fun, function(n) is.function(get(n, globalenv())), logical(1))]
  cat(sprintf("  loaded %-20s %2d functions\n", f, length(new_fun)))
}
dir.create(P("outputs"), showWarnings = FALSE)
cat("Project root:", ROOT, "\n")
cat("R version   :", R.version.string, "\n")
cat("Data labels : SYNTHETIC test data (RFC 5737 documentation IP ranges)\n")

# =============================================================================
banner("MODULE 2a: DATA IMPORT")
# =============================================================================
hosts_raw    <- import_hosts(P("data", "hosts.csv"))
services_raw <- import_services(P("data", "services.csv"))
cat("hosts.csv    :", nrow(hosts_raw), "rows x", ncol(hosts_raw), "columns\n")
cat("services.csv :", nrow(services_raw), "rows x", ncol(services_raw), "columns\n")
print_table(hosts_raw, "hosts.csv (first 8 rows)", 8)
print_table(services_raw, "services.csv (first 8 rows)", 8)

# =============================================================================
banner("MODULE 2b: DATA VALIDATION")
# =============================================================================
issues <- rbind(validate_hosts(hosts_raw), validate_services(services_raw))
issue_summary <- summarize_issues(issues)
cat("Total issues found:", nrow(issues), " (errors:", sum(issues$severity == "error"),
    ", warnings:", sum(issues$severity == "warning"), ")\n")
print_table(issue_summary, "Issues by dataset / field / type", 40)
print_table(issues, "Issue detail (first 12; full list in validation_report.csv)", 12)
export_csv(issues, P("outputs", "validation_report.csv"))

# =============================================================================
banner("MODULE 2c: DATA CLEANING, MERGE AND RECORD COUNTS")
# =============================================================================
res <- process_scan_data(hosts_raw, services_raw)
cat("Record counts BEFORE vs AFTER cleaning:\n")
print(res$counts, row.names = FALSE)
print_table(res$merged, "Merged host + service data (first 8 rows)", 8)
export_csv(res$hosts,    P("outputs", "cleaned_hosts.csv"))
export_csv(res$services, P("outputs", "cleaned_services.csv"))
export_csv(res$merged,   P("outputs", "merged_scan_data.csv"))

# =============================================================================
banner("MODULE 3a: HOST SUMMARY")
# =============================================================================
exposure     <- classify_exposure(res$merged)
host_summary <- summarize_hosts(res$merged, exposure)
print_table(host_summary, "Host summary (top 10 by risk score)", 10)
cat("\nHosts by overall risk:\n"); print(table(host_summary$overall_risk))
print_table(describe_numeric(host_summary, c("records", "open_ports", "risk_score")),
            "Statistical summary of host metrics", 10)
export_csv(host_summary, P("outputs", "host_summary.csv"))

# =============================================================================
banner("MODULE 3b: SERVICE SUMMARY")
# =============================================================================
service_summary <- summarize_services(res$merged)
print_table(service_summary, "Service summary (top 12 by open records)", 12)
export_csv(service_summary, P("outputs", "service_summary.csv"))

# =============================================================================
banner("MODULE 3c: EXPOSURE AND RISKY CONFIGURATION REPORT")
# =============================================================================
cat("Exposed services by risk level:\n"); print(table(exposure$risk_level))
print_table(exposure, "Exposure report (highest risk first)", 12)
export_csv(exposure, P("outputs", "exposure_report.csv"))
config_findings <- flag_configuration_issues(res$merged)
print_table(config_findings, "Risky configuration findings", 10)
export_csv(config_findings, P("outputs", "config_findings.csv"))

# =============================================================================
banner("MODULE 4: STRESS TESTING (50 vs 5,000 records)")
# =============================================================================
cat("\n--- 50 records ---")
stress_50   <- run_stress_test(50, repetitions = 5)
cat("\n--- 5,000 records ---")
stress_5000 <- run_stress_test(5000, repetitions = 3)
stress <- rbind(stress_50, stress_5000)
stress_cmp <- compare_stress_results(stress)
cat("\nPerformance comparison (large / small):\n"); print(stress_cmp$table, row.names = FALSE)
cat("Scaling factor:", stress_cmp$scaling_factor, "->", stress_cmp$interpretation, "\n")
cat("Slowest stage at 5,000 records:", stress_5000$bottleneck_stage,
    "(", stress_5000$bottleneck_pct, "% of total time )\n")
if (any(stress$failed_runs > 0)) cat("ERRORS DURING STRESS TEST:", paste(stress$error_message, collapse = " | "), "\n")
export_csv(stress, P("outputs", "stress_test_results.csv"))

# =============================================================================
banner("MODULE 5: SYSTEM REDESIGN - UDP, extra hosts/services, multiple sessions")
# =============================================================================
ext <- run_udp_extension(res$hosts, res$services,
                         P("data", "udp_scans.csv"), P("data", "multi_session_scans.csv"))
cat("UDP file          :", nrow(ext$udp$raw), "raw ->", nrow(ext$udp_clean), "clean records\n")
cat("Multi-session file:", nrow(ext$multi$raw), "raw ->", nrow(ext$multi_clean), "clean records\n")
cat("Combined services :", nrow(ext$combined_services), "records,",
    length(unique(ext$combined_hosts$ip_address)), "hosts\n")
print_table(summarize_issues(rbind(ext$udp$issues, ext$multi$issues)), "Validation issues in the new files", 30)
print_table(ext$session_summary, "Scan sessions", 10)
print_table(ext$session_changes, "Changes between consecutive sessions (hosts scanned in both)", 10)
print_table(ext$udp_summary, "UDP service summary", 10)
export_csv(ext$udp_summary,      P("outputs", "udp_service_summary.csv"))
export_csv(ext$session_summary,  P("outputs", "session_summary.csv"))
export_csv(ext$session_changes,  P("outputs", "session_changes.csv"))
export_csv(ext$merged_all,       P("outputs", "combined_scan_data.csv"))
compat <- check_backward_compatibility(P("data", "hosts.csv"), P("data", "services.csv"),
                                       host_summary, service_summary)
cat("\nBackward compatibility (original TCP summaries unchanged):", compat$ok, "\n")

# =============================================================================
banner("MODULE 6: JOINT EVALUATION")
# =============================================================================
expected_files <- c("cleaned_hosts.csv", "cleaned_services.csv", "merged_scan_data.csv",
                    "host_summary.csv", "service_summary.csv", "exposure_report.csv",
                    "validation_report.csv", "stress_test_results.csv")
ctx <- list(paths = setNames(as.list(P("outputs", expected_files)), expected_files),
            hosts_raw = hosts_raw, services_raw = services_raw, validation_issues = issues,
            hosts = res$hosts, services = res$services, merged = res$merged,
            host_summary = host_summary, exposure = exposure,
            stress = stress, stress_cmp = stress_cmp, ext = ext, compat = compat)
evaluation <- run_evaluation(ctx)
print(evaluation[, c("category", "test", "actual", "result")], row.names = FALSE, right = FALSE)
export_csv(evaluation, P("outputs", "evaluation_report.csv"))
cat("\nOverall: ", sum(evaluation$result[evaluation$test != "CATEGORY SUMMARY"] == "PASS"), " of ",
    sum(evaluation$test != "CATEGORY SUMMARY"), " tests passed\n", sep = "")

# =============================================================================
banner("FINAL CSV EXPORTS")
# =============================================================================
files <- list.files(P("outputs"), pattern = "\\.csv$", full.names = TRUE)
info <- data.frame(file = basename(files), size_bytes = file.info(files)$size,
                   rows = vapply(files, function(f) nrow(utils::read.csv(f)), numeric(1)))
rownames(info) <- NULL
print(info, row.names = FALSE)
cat("\nAll outputs written to:", P("outputs"), "\n")
cat("Finished at", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
