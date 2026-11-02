# 10_project_reconciliation.R
# Phase 1: Build a canonical project-level inventory by reconciling three sources.
# Outputs: projects_master.csv, project_source_reconciliation.csv, unmatched_projects.csv

library(here)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(jsonlite)

source(here("R", "functions.R"))

raw_dir  <- here("data", "raw")
proc_dir <- here("data", "processed")
der_dir  <- here("data", "derived")

# ---------------------------------------------------------------------------
# 1.  Load and clean each source
# ---------------------------------------------------------------------------

# --- Source A: Projects & Operations Excel ---
xlsx_raw <- read_excel(
  file.path(raw_dir, "Projects_and_Operations_List.xlsx"),
  sheet = 1, skip = 1
)

xlsx_clean <- xlsx_raw |>
  rename(
    project_id      = `Project Id`,
    project_name_xl = `Project Name`,
    region          = Region,
    country         = Country,
    last_stage      = `Last Stage Reached`,
    financing_type  = `Financing Type`,
    pdo             = `Project Development Objective`,
    board_date_raw  = `Board Approval Date`,
    effective_date_raw = `Effective Date`,
    closing_date_raw   = `Project Closing Date`,
    total_proj_cost_raw = `Total Project Cost $US`,
    ibrd_commit_raw     = `IBRD Commitment $US`,
    ida_commit_raw      = `IDA Commitment $US`,
    total_commit_raw    = `Total IBRD, IDA and GRANT Commitment $US`,
    grant_amt_raw       = `Grant Amount $US`,
    sector1_xl = `Sector 1`,
    sector2_xl = `Sector 2`,
    sector3_xl = `Sector 3`,
    lending_instrument_xl = `Lending Instrument`
  ) |>
  mutate(
    board_date_xl    = mdy(board_date_raw),
    approval_year_xl = year(board_date_xl),
    ibrd_commit_xl   = suppressWarnings(as.numeric(ibrd_commit_raw)),
    ida_commit_xl    = suppressWarnings(as.numeric(ida_commit_raw)),
    grant_amt_xl     = suppressWarnings(as.numeric(grant_amt_raw)),
    source_xl        = TRUE
  ) |>
  select(project_id, project_name_xl, board_date_xl, approval_year_xl,
         last_stage, financing_type, lending_instrument_xl,
         ibrd_commit_xl, ida_commit_xl, grant_amt_xl,
         sector1_xl, sector2_xl, sector3_xl, pdo, source_xl)

cat("Projects xlsx: ", nrow(xlsx_clean), "rows |", length(unique(xlsx_clean$project_id)), "unique IDs\n")

# --- Source B: WB Projects API V2 ---
api_raw  <- fromJSON(file.path(raw_dir, "world_bank_costa_rica.json"),       simplifyVector = FALSE)
apif_raw <- fromJSON(file.path(raw_dir, "world_bank_costa_rica_filtered.json"), simplifyVector = FALSE)

safe_pull <- function(proj, field) {
  val <- proj[[field]]
  if (is.null(val) || length(val) == 0) return(NA_character_)
  as.character(val[[1]])
}

api_clean <- purrr::map_dfr(names(api_raw$projects), function(id) {
  p  <- api_raw$projects[[id]]
  pf <- apif_raw$projects[[id]]
  # Abstract from filtered file
  ab <- pf[["project_abstract"]]
  abstract <- if (!is.null(ab)) {
    coalesce(ab[["cdata!"]], ab[["#cdata-section"]], as.character(ab[[1]]))
  } else NA_character_
  abstract <- if (!is.null(abstract) && !is.na(abstract) &&
                  nchar(trimws(abstract)) > 0) trimws(abstract) else NA_character_

  # Parse date (V2 API format can be ISO or MM/DD/YYYY)
  bd_raw <- safe_pull(p, "boardapprovaldate")
  bd <- suppressWarnings(coalesce(ymd_hms(bd_raw), mdy_hms(bd_raw)))

  tibble(
    project_id       = id,
    project_name_api = safe_pull(p, "project_name"),
    board_date_api   = bd,
    approval_year_api = if (!is.na(bd)) year(bd) else NA_integer_,
    status_api       = safe_pull(p, "status"),
    totalamt_api_raw = safe_pull(p, "totalamt"),
    totalamt_api     = suppressWarnings(as.numeric(gsub(",", "", safe_pull(p, "totalamt")))),
    lendinginstr_api = safe_pull(p, "lendinginstr"),
    sector1_api      = {s <- p$sector1; if (!is.null(s) && !is.null(s$Name) && nchar(s$Name) > 0) s$Name else NA_character_},
    abstract_api     = abstract,
    source_api       = TRUE
  )
})

cat("API: ", nrow(api_clean), "rows |", length(unique(api_clean$project_id)), "unique IDs\n")

# --- Source C: IBRD Loans (project-level aggregation) ---
loan_raw <- read_csv(
  file.path(raw_dir, "ibrd_statement_of_loans_and_guarantees_latest_available_snapshot_10-31-2026.csv"),
  show_col_types = FALSE
)
# Clean column names
names(loan_raw) <- gsub("[^a-zA-Z0-9]", "_", tolower(names(loan_raw)))
names(loan_raw) <- gsub("_+", "_", names(loan_raw))
names(loan_raw) <- gsub("_$", "", names(loan_raw))

cr_loans <- loan_raw |>
  filter(!is.na(country_economy_code), country_economy_code == "CR",
         !is.na(loan_number))

cat("CR loan records:", nrow(cr_loans), "\n")
cat("  of which IBRDB/IBRDG (guarantees):", sum(grepl("^IBRDB|^IBRDG", cr_loans$loan_number)), "\n")
cat("  Regular loans:", sum(!grepl("^IBRDB|^IBRDG", cr_loans$loan_number)), "\n")

# Aggregate to project level — NEVER join loan-level to project-level in a way
# that multiplies project rows. Loans with the same project_id are the same project.
cr_proj <- cr_loans |>
  filter(!is.na(project_id)) |>
  group_by(project_id) |>
  summarise(
    project_name_loan   = first(project_name),
    n_loans             = n(),
    board_date_loan_raw = first(board_approval_date),
    # Aggregate financial fields
    total_original_principal = sum(original_principal_amount_us, na.rm = TRUE),
    total_cancelled          = sum(cancelled_amount_us, na.rm = TRUE),
    total_disbursed          = sum(disbursed_amount_us, na.rm = TRUE),
    total_repaid             = sum(repaid_to_ibrd_us, na.rm = TRUE),
    total_loans_held         = sum(loans_held_us, na.rm = TRUE),
    # Keep track of loan statuses (a project may have loans in different statuses)
    loan_statuses            = paste(sort(unique(loan_status)), collapse = "; "),
    loan_numbers             = paste(sort(loan_number), collapse = "; "),
    loan_types               = paste(sort(unique(loan_type)), collapse = "; "),
    .groups = "drop"
  ) |>
  mutate(
    board_date_loan   = mdy(board_date_loan_raw),
    approval_year_loan = year(board_date_loan),
    source_loans      = TRUE
  )

cat("Unique CR project IDs in loans:", nrow(cr_proj), "\n")

# ---------------------------------------------------------------------------
# 2.  Three-way reconciliation
# ---------------------------------------------------------------------------

all_ids <- unique(c(xlsx_clean$project_id, api_clean$project_id, cr_proj$project_id))
cat("\nTotal unique project IDs across all sources:", length(all_ids), "\n")

recon <- data.frame(
  project_id = all_ids,
  in_xlsx    = all_ids %in% xlsx_clean$project_id,
  in_api     = all_ids %in% api_clean$project_id,
  in_loans   = all_ids %in% cr_proj$project_id
) |>
  left_join(xlsx_clean |> select(project_id, project_name_xl, approval_year_xl,
                                  last_stage, financing_type, ibrd_commit_xl, grant_amt_xl),
            by = "project_id") |>
  left_join(api_clean  |> select(project_id, project_name_api, approval_year_api,
                                  status_api, totalamt_api, lendinginstr_api),
            by = "project_id") |>
  left_join(cr_proj    |> select(project_id, project_name_loan, approval_year_loan,
                                  n_loans, total_original_principal, total_cancelled,
                                  total_disbursed, loan_statuses, loan_numbers),
            by = "project_id") |>
  mutate(
    source_count = as.integer(in_xlsx) + as.integer(in_api) + as.integer(in_loans),

    # Consensus project name (prefer xlsx > api > loan)
    project_name = coalesce(project_name_xl, project_name_api, project_name_loan),

    # Consensus approval year
    approval_year = coalesce(approval_year_xl, approval_year_api, approval_year_loan),

    # Membership category
    membership = case_when(
      in_xlsx &  in_api &  in_loans ~ "All three sources",
      in_xlsx &  in_api & !in_loans ~ "xlsx + API only",
      in_xlsx & !in_api &  in_loans ~ "xlsx + loans only",
     !in_xlsx &  in_api &  in_loans ~ "API + loans only",
      in_xlsx & !in_api & !in_loans ~ "xlsx only",
     !in_xlsx &  in_api & !in_loans ~ "API only",
     !in_xlsx & !in_api &  in_loans ~ "Loans only",
      TRUE                          ~ "Unknown"
    )
  )

cat("\n=== THREE-WAY RECONCILIATION ===\n")
membership_tbl <- table(recon$membership)
print(membership_tbl)

cat("\n=== xlsx + loans but NOT in API ===\n")
xl_loan <- recon |> filter(in_xlsx, !in_api, in_loans) |> arrange(approval_year)
cat("Count:", nrow(xl_loan), "\n")
xl_loan |>
  select(project_id, project_name, approval_year, loan_statuses, total_original_principal) |>
  mutate(total_original_principal = fmt_millions(total_original_principal)) |>
  as.data.frame() |>
  print()

cat("\n=== xlsx-only (not in API, not in loans) ===\n")
xo <- recon |> filter(in_xlsx, !in_api, !in_loans) |> arrange(approval_year)
cat("Count:", nrow(xo), "\n")
cat("Financing types:\n"); print(table(xo$financing_type, useNA="ifany"))
cat("Last stage:\n"); print(table(xo$last_stage, useNA="ifany"))
cat("Year range:", min(xo$approval_year, na.rm=TRUE), "to", max(xo$approval_year, na.rm=TRUE), "\n")

# ---------------------------------------------------------------------------
# 3.  Explain the 72-vs-115 discrepancy
# ---------------------------------------------------------------------------

cat("\n=== EXPLAINING 72 vs 115 DISCREPANCY ===\n")
cat("
The WB Projects API (V2, countrycode=CR, rows=100) returned 72 records.
The Projects & Operations Excel contains 115 records.
All 72 API records are a subset of the 115 Excel records.
The 43 additional records in the Excel are explained by the following categories:
\n")

# What are the 43 extra in xlsx?
extra_43 <- recon |> filter(in_xlsx, !in_api) |> arrange(approval_year)
cat("Summary of 43 extra (in xlsx, not in API):\n")
cat("  In loans (IBRD): ", sum(extra_43$in_loans), "\n")
cat("  Not in loans:    ", sum(!extra_43$in_loans), "\n\n")
cat("  Financing type breakdown:\n")
print(table(extra_43$financing_type, useNA="ifany"))
cat("\n  Last Stage Reached:\n")
print(table(extra_43$last_stage, useNA="ifany"))
cat("\n  Approval year range:", min(extra_43$approval_year, na.rm=TRUE),
    "to", max(extra_43$approval_year, na.rm=TRUE), "\n")

cat("\n  REASON 1 — Legacy pre-API records (older P006xxx projects not in V2 search results):\n")
old_proj <- extra_43 |> filter(grepl("^P006", project_id), is.na(financing_type))
cat("    Count:", nrow(old_proj), "| Year range:",
    min(old_proj$approval_year, na.rm=TRUE), "to", max(old_proj$approval_year, na.rm=TRUE), "\n")

cat("\n  REASON 2 — Grants and Other (GEF, trust funds, carbon finance):\n")
grants_other <- extra_43 |> filter(financing_type %in% c("Grants","Other"))
cat("    Count:", nrow(grants_other), "\n")
print(grants_other |> select(project_id, project_name, approval_year, financing_type, last_stage))

cat("\n  REASON 3 — Pre-2000 IBRD loans not indexed in V2 API search results:\n")
old_ibrd <- extra_43 |>
  filter(in_loans, !financing_type %in% c("Grants","Other") | is.na(financing_type)) |>
  filter(approval_year <= 2000 | is.na(approval_year))
cat("    Count:", nrow(old_ibrd), "\n")
print(old_ibrd |> select(project_id, project_name, approval_year, loan_statuses) |> arrange(approval_year))

cat("\n  REASON 4 — Post-1990 IBRD projects in loans, missing from API (key finding):\n")
missing_key <- extra_43 |>
  filter(in_loans) |>
  filter(approval_year >= 1990 | is.na(approval_year))
cat("    Count:", nrow(missing_key), "\n")
missing_key |>
  select(project_id, project_name, approval_year, total_original_principal, loan_statuses) |>
  mutate(total_original_principal = fmt_millions(total_original_principal)) |>
  print()

cat("\n  NOTE: P148435 ($420M Health Insurance, 2016) is a critical missing record.\n")
cat("    It is in the xlsx and loans but NOT in the API (72-record extract).\n")
cat("    Its exclusion caused earlier incorrect conclusion that 2015-2019 had no lending.\n")

# ---------------------------------------------------------------------------
# 4.  Build projects_master: all 115 with merged data
# ---------------------------------------------------------------------------

projects_master <- recon |>
  arrange(approval_year, project_id) |>
  select(
    project_id, project_name, approval_year,
    in_xlsx, in_api, in_loans, source_count, membership,
    last_stage, financing_type, lendinginstr_api, loan_statuses, loan_numbers,
    ibrd_commit_xl, totalamt_api, total_original_principal, total_cancelled,
    total_disbursed, n_loans
  ) |>
  mutate(
    # Net committed = original principal - cancelled
    # This is NOT "money received"; it represents the original commitment
    # minus portions cancelled before or during implementation.
    net_principal_after_cancellation = total_original_principal - total_cancelled
  )

cat("\n=== PROJECTS MASTER SUMMARY ===\n")
cat("Total rows:", nrow(projects_master), "\n")
cat("With IBRD loan record:", sum(projects_master$in_loans, na.rm=TRUE), "\n")
cat("In study period 1980-2025:", sum(!is.na(projects_master$approval_year) &
    projects_master$approval_year >= 1980 & projects_master$approval_year <= 2025), "\n")

# ---------------------------------------------------------------------------
# 5.  Save outputs
# ---------------------------------------------------------------------------

write_csv(projects_master, file.path(proc_dir, "projects_master.csv"))
write_csv(recon, file.path(proc_dir, "project_source_reconciliation.csv"))

# Unmatched projects (xlsx only — need investigation)
unmatched <- recon |>
  filter(in_xlsx, !in_api | !in_loans) |>
  arrange(approval_year) |>
  select(project_id, project_name, approval_year, membership,
         last_stage, financing_type, ibrd_commit_xl, grant_amt_xl,
         total_original_principal, loan_statuses)

write_csv(unmatched, file.path(proc_dir, "unmatched_projects.csv"))

# Clean loan records for downstream use
cr_loans_clean <- cr_loans |>
  mutate(
    board_approval_date_p = mdy(board_approval_date),
    approval_year         = year(board_approval_date_p),
    is_guarantee          = grepl("^IBRDB|^IBRDG", loan_number),
    net_principal         = original_principal_amount_us - cancelled_amount_us
  )
write_csv(cr_loans_clean, file.path(proc_dir, "loan_records_clean.csv"))

cat("\nSaved:\n")
cat("  data/processed/projects_master.csv\n")
cat("  data/processed/project_source_reconciliation.csv\n")
cat("  data/processed/unmatched_projects.csv\n")
cat("  data/processed/loan_records_clean.csv\n")
cat("Project reconciliation complete.\n")
