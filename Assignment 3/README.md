# Q7 - Reusable Scanning & Enumeration R Function Library
**Course:** Ethical Hacking (ITA14) | **Assessment Tool 3** | **Question 7**
**Environment:** R / RStudio on Windows | **Dependencies:** none (base R only)

> **Honest status note.** This project was written in an environment where R and RStudio were
> **not available**, so the code could **not be executed there**. No outputs or screenshots were
> fabricated: `outputs/` is empty and `screenshots/` contains only capture instructions. Everything
> is produced the first time you run `R/main.R` on your machine. Please run it once and fix/report
> any error before submitting. Benchmark numbers and test results in your report must come from
> **your** run.

## Safety / data
* The program **only reads CSV files** in `data/` - it performs **no network scanning**.
* All data is **synthetic** (see `data/DATA_README.txt`), using RFC 5737 documentation ranges
  `192.0.2.0/24`, `198.51.100.0/24`, `203.0.113.0/24`. It is not real scan output.
* Input rows: `hosts.csv` 32, `services.csv` 126, `udp_scans.csv` 53, `multi_session_scans.csv` 143
  (deliberately including missing values, invalid IPs/ports/protocols/states/timestamps, duplicates).

## How to run
1. Extract the ZIP. 2. Double-click `Q7_Reusable_Scanning_Enumeration_R_Library.Rproj`.
3. Open `R/main.R` -> **Source**, or in the Console: `source("R/main.R")`.
Command line: `Rscript R/main.R` (from the project folder). More detail: `requirements_or_setup.txt`.

## Structure
| File | Purpose |
|---|---|
| `R/main.R` | Runs Modules 1-6, prints results, writes all CSVs |
| `R/import_data.R` | `read_scan_csv`, `import_hosts/services`, `export_csv`, `print_table`, `.assert_df` |
| `R/validation.R` | `is_valid_ipv4/port/protocol/state/timestamp/scan_session`, `validate_hosts/services`, `summarize_issues` |
| `R/cleaning.R` | `clean_hosts/services`, `merge_host_service`, `process_scan_data` (validate->clean->merge) |
| `R/host_analysis.R` | `summarize_hosts`, `describe_numeric` |
| `R/service_analysis.R` | `risky_service_table`, `classify_exposure`, `summarize_services`, `flag_configuration_issues` |
| `R/stress_testing.R` | `generate_synthetic_scan_data`, `benchmark_pipeline`, `run_stress_test`, `compare_stress_results` |
| `R/udp_extension.R` | UDP import/clean, multi-session handling, host inventory extension, session comparison, backward-compat check |
| `R/evaluation.R` | `run_evaluation` - 28 computed tests across the 8 evaluation criteria |
| `data/` | the four synthetic input CSVs |

## Outputs created by `main.R` (in `outputs/`)
Required: `cleaned_hosts.csv`, `cleaned_services.csv`, `merged_scan_data.csv`, `host_summary.csv`,
`service_summary.csv`, `exposure_report.csv`, `validation_report.csv`, `stress_test_results.csv`,
`evaluation_report.csv`.
Extras (Modules 3/5): `config_findings.csv`, `udp_service_summary.csv`, `session_summary.csv`,
`session_changes.csv`, `combined_scan_data.csv`.

## Module notes
* **M2 cleaning rules:** trim/blank->NA; drop rows with invalid required fields; standardise case;
  fill optional gaps with "unknown"; de-duplicate after standardising; timestamps as text `YYYY-mm-dd HH:MM:SS`.
* **M3 risk score:** 5 x High + 3 x Medium + 1 x Low (distinct exposed services); risk levels come from the
  editable reference table in `risky_service_table()` (a teaching list, not an official standard).
* **M4:** each size is run repeatedly; the **median** per stage is reported with the slowest stage
  (bottleneck). Scaling factor = time ratio / record ratio.
* **M5:** UDP (`open|filtered` state supported), extra hosts/services not in the inventory, sessions
  SYN-S2/SYN-S3, UDP-specific check (TCP rows in a UDP file are flagged/removed). The original
  functions are re-run afterwards and compared with their earlier results.
* **M6:** thresholds (e.g. 5,000 records < 30 s, scaling factor <= 1.5, pass rate >= 90%) are **my own
  stated assumptions**, written in the `expected` column; change them in `R/evaluation.R` if your
  assignment specifies others.

## Assumptions
The original assignment text from the earlier conversation was not available to me, so the module list
in your prompt was used as the requirements. Folder/file names follow your specification.
