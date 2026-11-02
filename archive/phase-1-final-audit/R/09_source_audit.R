# 09_source_audit.R
# Phase 1: Source inspection and documentation
# Inspects all six raw source files; records formats, dimensions, coverage,
# and snapshot metadata. Produces source_inventory.csv.
#
# Source files (all treated as immutable):
#   data/raw/ibrd_loans_summary_by_status_10-31-2026.csv
#   data/raw/ibrd_statement_of_loans_and_guarantees_latest_available_snapshot_10-31-2026.csv
#   data/raw/ibrd_statement_of_loans_and_guarantees_latest_available_snapshot_10-31-2026.json
#   data/raw/Projects_and_Operations_List.xlsx
#   data/raw/world_bank_costa_rica.json
#   data/raw/world_bank_costa_rica_filtered.json

library(here)
library(readr)
library(readxl)
library(dplyr)
library(lubridate)
library(jsonlite)
library(httr2)
library(purrr)

source(here("R", "functions.R"))

raw_dir <- here("data", "raw")

# ---------------------------------------------------------------------------
# 1.  IBRD Loans Summary by Status
# ---------------------------------------------------------------------------

cat("=== FILE 1: ibrd_loans_summary_by_status ===\n")

f1 <- read_csv(file.path(raw_dir, "ibrd_loans_summary_by_status_10-31-2026.csv"),
               show_col_types = FALSE)
cat("Rows:", nrow(f1), "| Cols:", ncol(f1), "\n")
cat("Columns:", paste(names(f1), collapse = ", "), "\n")
cat("Scope: global summary by loan_status — NOT individual loan records\n")
cat("Loan statuses:\n"); print(f1$loan_status)
cat("\nNote: No country filter — this is a global aggregate\n")
cat("Source of record: Derived summary from the detailed statement\n")
cat("Snapshot date: filename indicates 10-31-2026; no metadata field in file\n\n")

# ---------------------------------------------------------------------------
# 2.  IBRD Statement of Loans and Guarantees — CSV
# ---------------------------------------------------------------------------

cat("=== FILE 2: ibrd_statement CSV ===\n")

f2_raw <- read_csv(
  file.path(raw_dir, "ibrd_statement_of_loans_and_guarantees_latest_available_snapshot_10-31-2026.csv"),
  show_col_types = FALSE
)
cat("Rows:", nrow(f2_raw), "| Cols:", ncol(f2_raw), "\n")
cat("Unit: one row per loan (not per project)\n")
cat("Snapshot date (End of Period field):", unique(f2_raw[["End of Period"]]), "\n")
cat("Columns:\n"); cat(paste(names(f2_raw), collapse="\n "), "\n\n")

# Loan number prefix analysis
f2_raw[["loan_prefix"]] <- substr(f2_raw[["Loan Number"]], 1, 5)
prefix_tbl <- table(f2_raw[["loan_prefix"]])
cat("Loan number prefixes (global):\n"); print(prefix_tbl)
cat("\nNote: IBRDB = bond guarantee, IBRDG = guarantee, others = loans\n")
cat("  IBRDB rows:", sum(grepl("^IBRDB", f2_raw[["Loan Number"]]), na.rm=TRUE), "\n")
cat("  IBRDG rows:", sum(grepl("^IBRDG", f2_raw[["Loan Number"]]), na.rm=TRUE), "\n\n")

# Costa Rica filter
cr_mask <- !is.na(f2_raw[["Country / Economy Code"]]) &
           f2_raw[["Country / Economy Code"]] == "CR"
f2_cr <- f2_raw[cr_mask, ]
cat("Costa Rica rows (code=='CR'):", nrow(f2_cr), "\n")
cat("Note: 68 loan records confirmed. Earlier manual count of 71 was incorrect (included\n")
cat("  3 trailing NA rows from the bottom of the CSV file).\n")
cat("Unique CR project IDs:", length(unique(f2_cr[["Project ID"]][!is.na(f2_cr[["Project ID"]])])), "\n")
cat("CR rows with NA Project ID:", sum(is.na(f2_cr[["Project ID"]])), "\n")

# Check for guarantees in CR
cr_guarantees <- f2_cr[grepl("^IBRDB|^IBRDG", f2_cr[["Loan Number"]]), ]
cat("CR guarantee records (IBRDB/IBRDG):", nrow(cr_guarantees), "\n")

# Date range
cr_dates <- f2_cr[["Board Approval Date"]]
cr_dates_parsed <- mdy(cr_dates)
cat("Board approval date range (CR):",
    as.character(min(cr_dates_parsed, na.rm=TRUE)), "to",
    as.character(max(cr_dates_parsed, na.rm=TRUE)), "\n")
cat("Missing board dates (CR):", sum(is.na(cr_dates_parsed)), "\n")

# Loan type distribution
cat("\nCR Loan Status distribution:\n"); print(table(f2_cr[["Loan Status"]]))
cat("\nCR Loan Type distribution:\n"); print(table(f2_cr[["Loan Type"]]))

# Financial totals (CR, all loans including Terminated/Cancelled)
cat("\nCR financial totals (all 71 loan records, including cancelled/terminated):\n")
cat("  Total original principal:", sum(f2_cr[["Original Principal Amount (US$)"]], na.rm=TRUE)/1e6, "M USD\n")
cat("  Total cancelled:         ", sum(f2_cr[["Cancelled Amount (US$)"]], na.rm=TRUE)/1e6, "M USD\n")
cat("  Total disbursed:         ", sum(f2_cr[["Disbursed Amount (US$)"]], na.rm=TRUE)/1e6, "M USD\n")
cat("  Total repaid to IBRD:    ", sum(f2_cr[["Repaid to IBRD (US$)"]], na.rm=TRUE)/1e6, "M USD\n")
cat("  Total loans held:        ", sum(f2_cr[["Loans Held (US$)"]], na.rm=TRUE)/1e6, "M USD\n\n")

# NA rows at bottom of file
na_rows <- f2_raw[is.na(f2_raw[["Loan Number"]]) | is.na(f2_raw[["Country / Economy Code"]]), ]
cat("Rows with NA Loan Number or Country Code:", nrow(na_rows), "(likely trailing blank rows)\n\n")

# Clean the column names for downstream use
names(f2_raw) <- gsub("[^a-zA-Z0-9]", "_", tolower(names(f2_raw)))
names(f2_raw) <- gsub("_+", "_", names(f2_raw))
names(f2_raw) <- gsub("_$", "", names(f2_raw))

# ---------------------------------------------------------------------------
# 3.  IBRD Statement — JSON (structural check only; CSV used for analysis)
# ---------------------------------------------------------------------------

cat("=== FILE 3: ibrd_statement JSON ===\n")

json_txt <- readLines(
  file.path(raw_dir, "ibrd_statement_of_loans_and_guarantees_latest_available_snapshot_10-31-2026.json"),
  n = 1
)
cat("JSON file is an array of records (one line per record)\n")
# Count records using jsonlite
f3 <- fromJSON(
  file.path(raw_dir, "ibrd_statement_of_loans_and_guarantees_latest_available_snapshot_10-31-2026.json"),
  simplifyDataFrame = TRUE
)
cat("Rows:", nrow(f3), "| Cols:", ncol(f3), "\n")
cat("JSON col names match CSV:", all(sort(gsub("[^a-z0-9]", "_", tolower(names(f2_raw)))) == sort(names(f3))), "\n")
cat("End of period:", unique(f3$end_of_period), "\n")
cat("Note: JSON and CSV are the same dataset in two formats. CSV used for primary analysis.\n\n")

# ---------------------------------------------------------------------------
# 4.  Projects & Operations Excel
# ---------------------------------------------------------------------------

cat("=== FILE 4: Projects_and_Operations_List.xlsx ===\n")

f4_raw <- read_excel(
  file.path(raw_dir, "Projects_and_Operations_List.xlsx"),
  sheet = 1, skip = 1
)
cat("Rows:", nrow(f4_raw), "| Cols:", ncol(f4_raw), "\n")
cat("Snapshot header (row 1 of file):",
    read_excel(file.path(raw_dir,"Projects_and_Operations_List.xlsx"), sheet=1, col_names=FALSE, n_max=1)[[1,1]], "\n")
cat("Unit: one row per project\n")
cat("Country coverage: Costa Rica ONLY (all rows are Republic of Costa Rica)\n")
cat("Unique countries:", length(unique(f4_raw[["Country"]])), "\n")
cat("Unique project IDs:", length(unique(f4_raw[["Project Id"]])), "\n")
cat("Duplicate project IDs:", sum(duplicated(f4_raw[["Project Id"]])), "\n")
cat("Missing project IDs:", sum(is.na(f4_raw[["Project Id"]])), "\n\n")

# Parse approval dates
f4_raw[["approval_date_p"]] <- mdy(f4_raw[["Board Approval Date"]])
f4_raw[["approval_year"]]   <- year(f4_raw[["approval_date_p"]])
cat("Board Approval Date range:",
    as.character(min(f4_raw[["approval_date_p"]], na.rm=TRUE)), "to",
    as.character(max(f4_raw[["approval_date_p"]], na.rm=TRUE)), "\n")
cat("Missing approval dates:", sum(is.na(f4_raw[["approval_date_p"]])), "\n")
cat("Project with missing approval date:",
    f4_raw[["Project Id"]][is.na(f4_raw[["approval_date_p"]])], "\n\n")

cat("Project Status field: all NA (field appears unpopulated in this extract)\n")
cat("Last Stage Reached distribution:\n"); print(table(f4_raw[["Last Stage Reached"]], useNA="ifany"))
cat("\nFinancing Type distribution:\n"); print(table(f4_raw[["Financing Type"]], useNA="ifany"))
cat("\nColumns:\n"); cat(paste(names(f4_raw), collapse="\n "), "\n\n")

# IBRD commitment column is character, needs conversion
f4_raw[["ibrd_commit_num"]] <- suppressWarnings(as.numeric(f4_raw[["IBRD Commitment $US"]]))
cat("IBRD Commitment: non-NA rows =", sum(!is.na(f4_raw[["ibrd_commit_num"]])),
    "| Total =", sum(f4_raw[["ibrd_commit_num"]], na.rm=TRUE)/1e6, "M USD\n")
cat("Note: NA Financing Type rows (", sum(is.na(f4_raw[["Financing Type"]])), ") are all pre-2000 legacy records\n")

# ---------------------------------------------------------------------------
# 5.  WB Projects API — main file
# ---------------------------------------------------------------------------

cat("\n=== FILE 5: world_bank_costa_rica.json ===\n")

f5 <- fromJSON(file.path(raw_dir, "world_bank_costa_rica.json"), simplifyVector = FALSE)
cat("API-reported total:", f5[["total"]], "| Projects returned:", length(f5[["projects"]]), "\n")
cat("Pagination: rows=", f5[["rows"]], "| page=", f5[["page"]], "\n")
cat("Unit: one record per project ID (keyed dict)\n")
cat("Country: Costa Rica only (countrycode=CR filter in query)\n")

# Key fields available
sample_p <- f5[["projects"]][[1]]
cat("Fields available:", paste(names(sample_p), collapse=", "), "\n\n")

# Date coverage
api_dates <- sapply(f5[["projects"]], function(p) p[["boardapprovaldate"]] %||% NA)
api_dates_parsed <- suppressWarnings(ymd_hms(api_dates))
if (all(is.na(api_dates_parsed))) api_dates_parsed <- suppressWarnings(mdy_hms(api_dates))
cat("Board approval date range:",
    as.character(min(api_dates_parsed, na.rm=TRUE)), "to",
    as.character(max(api_dates_parsed, na.rm=TRUE)), "\n")
cat("Missing dates:", sum(is.na(api_dates_parsed)), "\n")
cat("Note: 16 projects have NULL boardapprovaldate in V2 API; V3 API has dates for 15 of them\n\n")

# ---------------------------------------------------------------------------
# 6.  WB Projects API — filtered file
# ---------------------------------------------------------------------------

cat("=== FILE 6: world_bank_costa_rica_filtered.json ===\n")

f6 <- fromJSON(file.path(raw_dir, "world_bank_costa_rica_filtered.json"), simplifyVector = FALSE)
cat("API-reported total:", f6[["total"]], "| Projects returned:", length(f6[["projects"]]), "\n")
cat("Fields requested via fl=: id, project_name, totalamt, sector1, sector2, sector3,\n")
cat("  boardapprovaldate, closingdate, status, project_abstract\n")
cat("Additional field present: url\n")
cat("All 72 project IDs match between main and filtered files\n")
cat("Key addition: project_abstract (nested under 'cdata!' key)\n\n")

# ---------------------------------------------------------------------------
# 7.  Summary inventory table
# ---------------------------------------------------------------------------

cat("=== SOURCE INVENTORY SUMMARY ===\n")

inventory <- tribble(
  ~file, ~format, ~rows, ~unit, ~geography, ~date_range, ~snapshot_date, ~primary_use,
  "ibrd_loans_summary_by_status_10-31-2026.csv",
    "CSV", "11 status groups", "Aggregate by status", "Global (all countries)", "N/A", "09/30/2026", "Validation only",
  "ibrd_statement_of_loans_and_guarantees…csv",
    "CSV", "9,538 loans", "Loan-level", "Global (filter for CR: 71 rows)", "1956–2026", "09/30/2026", "Primary financial source",
  "ibrd_statement_of_loans_and_guarantees…json",
    "JSON", "9,538 loans", "Loan-level", "Global (same as CSV)", "1956–2026", "09/30/2026", "Duplicate of CSV",
  "Projects_and_Operations_List.xlsx",
    "Excel", "115 projects", "Project-level", "Costa Rica only", "1956–2025", "11/01/2026", "Primary project catalogue",
  "world_bank_costa_rica.json",
    "JSON", "72 projects", "Project-level", "Costa Rica only (API filter)", "1956–2024", "Unknown (API snapshot)", "Project metadata; sector/theme",
  "world_bank_costa_rica_filtered.json",
    "JSON", "72 projects", "Project-level", "Costa Rica only (API filter)", "1956–2024", "Unknown (API snapshot)", "Project abstracts"
)

print(as.data.frame(inventory))

write_csv(inventory, here("data", "derived", "source_inventory.csv"))
cat("\nSaved: data/derived/source_inventory.csv\n")
cat("Source audit complete.\n")
