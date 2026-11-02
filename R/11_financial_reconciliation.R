# 11_financial_reconciliation.R
# Phase 1: Financial definitions, reconciliation, and known discrepancy investigation.
# Outputs: project_finance_summary.csv, financial_reconciliation.csv

library(here)
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)

source(here("R", "functions.R"))

raw_dir  <- here("data", "raw")
proc_dir <- here("data", "processed")
der_dir  <- here("data", "derived")

# ---------------------------------------------------------------------------
# 1.  Load clean data
# ---------------------------------------------------------------------------

loans    <- read_csv(file.path(proc_dir, "loan_records_clean.csv"),          show_col_types = FALSE)
projects <- read_csv(file.path(proc_dir, "projects_master.csv"),              show_col_types = FALSE)
summary_raw <- read_csv(
  file.path(raw_dir, "ibrd_loans_summary_by_status_10-31-2026.csv"),
  show_col_types = FALSE
)
xlsx_raw <- read_excel(file.path(raw_dir, "Projects_and_Operations_List.xlsx"), sheet=1, skip=1)

cat("Loan records loaded:", nrow(loans), "\n")
cat("Projects master loaded:", nrow(projects), "\n")

# ---------------------------------------------------------------------------
# 2.  Field definitions (documented from WB source)
# ---------------------------------------------------------------------------

cat("\n=== IBRD FINANCIAL FIELD DEFINITIONS ===\n")
cat("
Fields in the IBRD Statement of Loans and Guarantees (end_of_period: 09/30/2026):

original_principal_amount_us:
  The original loan amount in US dollars as approved by the IBRD Board. This is the
  commitment amount at the time of board approval. DOES NOT equal money received.

cancelled_amount_us:
  The portion of the original principal that has been cancelled. Includes: (a) unused
  undisbursed amounts cancelled at project close; (b) amounts cancelled during
  implementation; and (c) for Terminated loans, the entire approved amount may be
  cancelled if the loan agreement was never signed or the loan was never effective.

undisbursed_amount_us:
  The remaining balance not yet drawn down as of the snapshot date.

disbursed_amount_us:
  The cumulative amount drawn down by the borrower as of the snapshot date.
  THIS IS A CUMULATIVE BALANCE, not an annual flow.

repaid_to_ibrd_us:
  Cumulative principal repaid to IBRD as of snapshot date.

due_to_ibrd_us:
  Outstanding principal still owed (disbursed minus repaid plus exchange adjustment).

borrower_s_obligation_us:
  Total outstanding obligations to IBRD (loans_held + due_3rd_party).

loans_held_us:
  IBRD's net exposure: original disbursed amount minus repayments and third-party sales.

DERIVED MEASURE:
  net_principal = original_principal_amount_us - cancelled_amount_us
  This represents the amount that was (or is expected to be) committed net of cancellations.
  It is NOT the same as disbursed amount, nor money received. For Terminated loans with
  full cancellation, net_principal = 0.

IMPORTANT CAVEATS:
  - Disbursement figures are snapshot balances, not annual flows.
  - Do not assign cumulative disbursements to the approval year.
  - Original principal in nominal USD: comparing across time without inflation adjustment
    overstates older loans' real value or understates newer loans'.
  - For Terminated loans: original_principal = cancelled_amount (net = 0, nothing disbursed).
\n")

# ---------------------------------------------------------------------------
# 3.  CR financial summary by project
# ---------------------------------------------------------------------------

cr_loans_only <- loans |>
  filter(!is_guarantee, !is.na(project_id))

# Project-level aggregation
proj_finance <- cr_loans_only |>
  group_by(project_id) |>
  summarise(
    n_loans                  = n(),
    board_approval_date      = first(board_approval_date_p),
    approval_year            = first(approval_year),
    total_original_principal = sum(original_principal_amount_us, na.rm = TRUE),
    total_cancelled          = sum(cancelled_amount_us, na.rm = TRUE),
    total_disbursed          = sum(disbursed_amount_us, na.rm = TRUE),
    total_repaid             = sum(repaid_to_ibrd_us, na.rm = TRUE),
    total_loans_held         = sum(loans_held_us, na.rm = TRUE),
    net_principal            = total_original_principal - total_cancelled,
    loan_statuses            = paste(sort(unique(loan_status)), collapse = "; "),
    loan_numbers             = paste(sort(loan_number), collapse = "; "),
    .groups = "drop"
  ) |>
  left_join(projects |> select(project_id, project_name, in_xlsx, in_api, last_stage,
                                financing_type),
            by = "project_id")

cat("=== PROJECT-LEVEL FINANCIAL SUMMARY ===\n")
cat("Total projects with IBRD loan records:", nrow(proj_finance), "\n")
cat("Total original principal (all CR loans, 1956–2025):",
    fmt_billions(sum(proj_finance$total_original_principal)), "\n")
cat("Total cancelled:", fmt_millions(sum(proj_finance$total_cancelled)), "\n")
cat("Total net principal (orig - cancelled):", fmt_billions(sum(proj_finance$net_principal)), "\n")
cat("Total disbursed (cumulative, as of 09/30/2026):", fmt_billions(sum(proj_finance$total_disbursed)), "\n\n")

# Status-based breakdown
cat("Loan status categories for CR projects:\n")
proj_finance |>
  group_by(loan_statuses) |>
  summarise(
    n = n(),
    total_principal = sum(total_original_principal)/1e6,
    total_disbursed = sum(total_disbursed)/1e6,
    .groups = "drop"
  ) |>
  arrange(desc(total_principal)) |>
  mutate(across(where(is.numeric), ~ round(.x, 1))) |>
  print()

# ---------------------------------------------------------------------------
# 4.  Known discrepancy: P148435 ($420M health insurance)
# ---------------------------------------------------------------------------

cat("\n=== DISCREPANCY 1: P148435 — Strengthening Universal Health Insurance ===\n")

p148_loans <- loans |> filter(project_id == "P148435")
cat("Number of loan records:", nrow(p148_loans), "\n")
cat("Loan number:", p148_loans$loan_number[1], "\n")
cat("Board approval date:", p148_loans$board_approval_date[1], "\n")
cat("Original principal:", fmt_millions(p148_loans$original_principal_amount_us[1]), "\n")
cat("Cancelled:", fmt_millions(p148_loans$cancelled_amount_us[1]), "\n")
cat("Disbursed:", fmt_millions(p148_loans$disbursed_amount_us[1]), "\n")
cat("Loan status:", p148_loans$loan_status[1], "\n")
cat("Loan type:", p148_loans$loan_type[1], "\n\n")

p148_xl <- xlsx_raw[xlsx_raw[["Project Id"]] == "P148435", ]
cat("In xlsx Last Stage:", p148_xl$`Last Stage Reached`[1], "\n")
cat("In xlsx Financing Type:", p148_xl$`Financing Type`[1], "\n")
cat("In xlsx Board Approval Date:", p148_xl$`Board Approval Date`[1], "\n")
cat("In API: NO — P148435 is absent from the 72-record V2 API extract.\n")

cat("
FINDING: P148435 is a $420M Investment Project Financing (SCL/IPF) for health
insurance reform, approved 2016-03-15. It is present in both the IBRD loan dataset
and the Projects & Operations Excel, but ABSENT from the WB Projects V2 API extract
used in the earlier 72-project analysis.

Its exclusion caused the incorrect conclusion that 2015-2019 had no substantial IBRD
lending. Adding P148435 changes the picture materially:
  - 2016 had $420M in IBRD commitments (largest single project at that date)
  - The '2015-2019 gap' is partially incorrect: 2016 had a major loan
  - The genuine gap years (no IBRD loans) are 2013, 2015, 2017, and 2019
\n")

# ---------------------------------------------------------------------------
# 5.  Known discrepancy: P504033 (amount comparison)
# ---------------------------------------------------------------------------

cat("=== DISCREPANCY 2: P504033 — Sustainable Agriculture ===\n")

p504_loans <- loans |> filter(project_id == "P504033")
cat("Loan original principal:", fmt_millions(p504_loans$original_principal_amount_us[1]), "\n")
cat("Loan number:", p504_loans$loan_number[1], "\n")
cat("Board approval date:", p504_loans$board_approval_date[1], "\n")
cat("Loan status:", p504_loans$loan_status[1], "\n")

p504_xl <- xlsx_raw[xlsx_raw[["Project Id"]] == "P504033", ]
ibrd_col_val <- suppressWarnings(as.numeric(p504_xl[["IBRD Commitment $US"]]))
cat("\nxlsx IBRD Commitment:", fmt_millions(ibrd_col_val[1]), "\n")

# API totalamt
cat("API totalamt: $150M (from projects_all.csv)\n")
cat("
FINDING: P504033 shows three different values:
  - API totalamt: $150,000,000 (from V2 API)
  - xlsx IBRD Commitment $US: $120,000,000 (Projects & Operations Excel)
  - Loan original principal: $120,000,000 (IBRD Statement, IBRD98080)
  
The loan dataset and xlsx agree on $120M. The V2 API totalamt of $150M is inconsistent.
This is likely a revision in the API record (the V2 API may have used a different field
or an earlier version of the approval amount). The IBRD Statement is the authoritative
source for original principal; $120M should be used for financial analysis.
NOTE: The project was approved 2025-05-02, after the 1980-2025 study period end.
It is reported separately as an early 2026-snapshot record.\n")

# ---------------------------------------------------------------------------
# 6.  Validate summary file against detailed data
# ---------------------------------------------------------------------------

cat("\n=== VALIDATION: IBRD LOANS SUMMARY vs DETAIL ===\n")

# Aggregate detailed CR data by loan status
detail_by_status <- loans |>
  filter(!is.na(loan_status)) |>
  group_by(loan_status) |>
  summarise(
    n                    = n(),
    original_principal   = sum(original_principal_amount_us, na.rm=TRUE),
    cancelled            = sum(cancelled_amount_us, na.rm=TRUE),
    undisbursed          = sum(undisbursed_amount_us, na.rm=TRUE),
    disbursed            = sum(disbursed_amount_us, na.rm=TRUE),
    repaid_to_ibrd       = sum(repaid_to_ibrd_us, na.rm=TRUE),
    due_to_ibrd          = sum(due_to_ibrd_us, na.rm=TRUE),
    .groups = "drop"
  )

cat("Note: The summary file aggregates the ENTIRE global portfolio, not CR only.\n")
cat("Cannot validate CR totals against the global summary directly.\n")
cat("The summary file is a global aggregate by loan status—it provides an independent\n")
cat("check on global totals, not country-specific totals.\n\n")
cat("CR total by loan status (from detailed data):\n")
print(detail_by_status |>
        mutate(across(c(original_principal, cancelled, disbursed), ~ round(.x/1e6,1))) |>
        select(loan_status, n, original_principal, cancelled, disbursed))

# ---------------------------------------------------------------------------
# 7.  1980-2025 study period financial summary
# ---------------------------------------------------------------------------

cat("\n=== STUDY PERIOD (1980-2025): IBRD LOANS FINANCIAL SUMMARY ===\n")

study_loans <- proj_finance |>
  filter(!is.na(approval_year), approval_year >= 1980, approval_year <= 2025)

cat("Projects with IBRD loans, approved 1980-2025:", nrow(study_loans), "\n")
cat("Total original principal:", fmt_billions(sum(study_loans$total_original_principal)), "\n")
cat("Total cancelled:", fmt_millions(sum(study_loans$total_cancelled)), "\n")
cat("Net principal (orig - cancelled):", fmt_billions(sum(study_loans$net_principal)), "\n")
cat("Total disbursed (cumulative):", fmt_billions(sum(study_loans$total_disbursed)), "\n\n")

cat("By decade:\n")
study_loans |>
  mutate(decade = paste0(10 * (approval_year %/% 10), "s")) |>
  group_by(decade) |>
  summarise(
    n = n(),
    original_principal = sum(total_original_principal)/1e6,
    net_principal = sum(net_principal)/1e6,
    disbursed = sum(total_disbursed)/1e6,
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 0))) |>
  print()

cat("\nTop 10 projects by original principal (1980-2025):\n")
study_loans |>
  arrange(desc(total_original_principal)) |>
  head(10) |>
  select(project_id, project_name, approval_year, total_original_principal,
         total_cancelled, net_principal, loan_statuses) |>
  mutate(
    total_original_principal = fmt_millions(total_original_principal),
    total_cancelled          = fmt_millions(total_cancelled),
    net_principal            = fmt_millions(net_principal)
  ) |>
  print()

# ---------------------------------------------------------------------------
# 8.  Save outputs
# ---------------------------------------------------------------------------

write_csv(proj_finance, file.path(proc_dir, "project_finance_summary.csv"))

# Financial reconciliation table
fin_recon <- tibble(
  item = c(
    "P148435 status",
    "P504033 amount discrepancy",
    "Terminated loans - P006945, P006942, P006940 (SAL/adjustment precursors)",
    "API totalamt vs loan principal — general pattern",
    "3 trailing NA rows in loan CSV"
  ),
  finding = c(
    "$420M Health Insurance APL, 2016. In xlsx and loans, NOT in V2 API. Changes 2015-2019 gap assessment.",
    "API: $150M; xlsx and loan statement: $120M. Loan statement is authoritative.",
    "Fully Cancelled status; original amounts shown in cancelled column. Not actual lending.",
    "API totalamt sometimes reflects planned/proposed amounts; loan statement shows approved principal.",
    "Rows 69-71 in raw CSV are blank/NA. Ignored in analysis."
  ),
  resolution = c(
    "Use projects_master.csv which includes P148435 from xlsx. Do not rely on API-only extract.",
    "Use $120M from loan statement for financial analysis.",
    "Net principal = 0 for these projects. Include in project count; exclude from net financial totals.",
    "Use original_principal_amount from loan statement as definitive commitment amount.",
    "Filtered out by !is.na(loan_number) in cleaning step."
  )
)
write_csv(fin_recon, file.path(der_dir, "financial_reconciliation.csv"))

cat("Saved:\n")
cat("  data/processed/project_finance_summary.csv\n")
cat("  data/derived/financial_reconciliation.csv\n")
cat("Financial reconciliation complete.\n")
