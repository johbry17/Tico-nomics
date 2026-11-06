# 09_validation.R
# Final validation checks before publication.
# Cross-checks counts, totals, duplicates, and important project presence.

source(here::here("R", "00_setup.R"))

cat("\n========== VALIDATION ==========\n\n")

errors <- character(0)
warnings <- character(0)

check <- function(condition, msg, type = "ERROR") {
  passed <- isTRUE(condition)
  if (passed) {
    cat("  OK:", msg, "\n")
  } else {
    issue <- paste(if (type == "ERROR") "ERROR:" else "WARN:", msg)

    if (type == "ERROR") {
      errors <<- c(errors, issue)
    } else {
      warnings <<- c(warnings, issue)
    }
  }
}

# ---------------------------------------------------------------------------
# 1.  Raw files exist and are readable
# ---------------------------------------------------------------------------

cat("--- Raw files ---\n")
raw_files <- c(
  "ibrd_loans_summary_by_status_10-31-2026.csv",
  "ibrd_statement_of_loans_and_guarantees_latest_available_snapshot_10-31-2026.csv",
  "Projects_and_Operations_List.xlsx",
  "world_bank_costa_rica.json",
  "IFC_Projects_Costa Rica.xlsx",
  "World_Bank_Group_Guarantees_MIGA-projects.csv"
)
for (f in raw_files) {
  check(file.exists(file.path(RAW, f)), paste("Raw file exists:", f))
}

# ---------------------------------------------------------------------------
# 2.  Processed files exist
# ---------------------------------------------------------------------------

cat("--- Processed files ---\n")
proc_files <- c(
  "ibrd_loans_clean.csv",
  "ibrd_projects.csv",
  "projects_universe.csv",
  "ifc_projects.csv",
  "miga_projects.csv",
  "wdi_clean.csv",
  "us_gdp_deflator.csv"
)
for (f in proc_files) {
  check(file.exists(file.path(PROC, f)), paste("Processed file exists:", f))
}

# ---------------------------------------------------------------------------
# 3.  IBRD project counts and financial totals
# ---------------------------------------------------------------------------

cat("\n--- IBRD counts and totals ---\n")
loans <- read_csv(
  file.path(PROC, "ibrd_loans_clean.csv"),
  show_col_types = FALSE
)
projects <- read_csv(
  file.path(PROC, "ibrd_projects.csv"),
  show_col_types = FALSE
)

# Loan record count
check(nrow(loans) == 68, paste("CR loan records = 68 (got", nrow(loans), ")"))
check(
  sum(loans$is_guarantee, na.rm = TRUE) == 0,
  "No IBRDB/IBRDG guarantees in CR"
)

# Unique project IDs
n_proj <- length(unique(loans$project_id[!is.na(loans$project_id)]))
check(
  n_proj == 59,
  paste("59 unique CR project IDs in loans (got", n_proj, ")")
)

# Financial totals (Phase 1 reference: $4.9B principal, $479M cancelled, $4.4B net, $3.2B disbursed)
total_prin <- sum(
  loans$original_principal_amount_us[!loans$is_guarantee],
  na.rm = TRUE
)
check(
  total_prin >= 4.8e9 & total_prin <= 5.1e9,
  paste("Total principal ~$4.9B (got", fmt_billions(total_prin), ")")
)

total_canc <- sum(loans$cancelled_amount_us[!loans$is_guarantee], na.rm = TRUE)
check(
  total_canc >= 4e8 & total_canc <= 5.5e8,
  paste("Total cancelled ~$479M (got", fmt_millions(total_canc), ")")
)

total_disb <- sum(loans$disbursed_amount_us[!loans$is_guarantee], na.rm = TRUE)
check(
  total_disb >= 3e9 & total_disb <= 3.5e9,
  paste("Total disbursed ~$3.2B (got", fmt_billions(total_disb), ")")
)

# P148435 must be present
p148 <- loans |> filter(project_id == "P148435")
check(
  nrow(p148) == 1,
  "P148435 (Health Insurance $420M, 2016) present in loan records"
)
check(
  p148$original_principal_amount_us[1] == 420000000,
  paste(
    "P148435 principal = $420M (got",
    fmt_millions(p148$original_principal_amount_us[1]),
    ")"
  )
)
check(
  p148$approval_year[1] == 2016,
  paste("P148435 approval year = 2016 (got", p148$approval_year[1], ")")
)

# No duplicate project IDs in project-level table
check(
  !any(duplicated(projects$project_id)),
  "No duplicate project IDs in ibrd_projects.csv"
)

# ---------------------------------------------------------------------------
# 4.  Study period totals
# ---------------------------------------------------------------------------

cat("\n--- Study period (1980-2025) ---\n")
scope <- projects |>
  filter(!is.na(approval_year), approval_year >= 1980, approval_year <= 2025)
check(
  nrow(scope) >= 30 & nrow(scope) <= 40,
  paste("In-scope projects (expected ~35, got", nrow(scope), ")")
)

scope_prin <- sum(scope$total_principal, na.rm = TRUE)
check(
  scope_prin >= 4.5e9 & scope_prin <= 5e9,
  paste("1980-2025 principal ~$4.6B (got", fmt_billions(scope_prin), ")")
)

# Deflator validation: 1980->2025 multiplier ~3.2-3.4
deflator <- read_csv(
  file.path(PROC, "us_gdp_deflator.csv"),
  show_col_types = FALSE
)
d2025 <- deflator$defl[deflator$year == 2025]
d1980 <- deflator$defl[deflator$year == 1980]
mult <- d2025 / d1980
check(!is.na(d2025) & !is.na(d1980), "Deflator available for 2025 and 1980")
check(
  mult >= 3.0 & mult <= 3.7,
  paste("1980->2025 deflator multiplier ~3.3 (got", round(mult, 3), ")")
)

# ---------------------------------------------------------------------------
# 5.  IFC validations
# ---------------------------------------------------------------------------

# No negative net_principal at project level (Terminated-with-zero-original loans must be 0)
neg_net <- projects |> filter(net_principal < 0)
check(
  nrow(neg_net) == 0,
  paste("No negative net_principal values (got", nrow(neg_net), ")")
)

# P178320 (COVID Terminated): original_principal=0, net=0, cancelled=120M
p178 <- loans |> filter(project_id == "P178320")
if (nrow(p178) > 0) {
  check(
    p178$original_principal_amount_us[1] == 0,
    "P178320 (COVID Terminated) original_principal = 0 (as per IBRD statement)"
  )
  check(
    p178$net_principal[1] == 0,
    "P178320 (COVID Terminated) net_principal capped at 0"
  )
}

# Abstract theme coverage: all 5 periods must appear in theme table
themes <- read_csv(
  file.path(DER, "abstract_themes.csv"),
  show_col_types = FALSE
)
all_per <- c("1980–1989", "1990–1999", "2000–2009", "2010–2019", "2020–2025")
check(
  all(all_per %in% as.character(themes$period)),
  "All 5 periods present in abstract_themes.csv"
)
# 1990-1999 should have n_with_abstract=0 (no abstracts in API)
nineties <- themes |> filter(period == "1990–1999") |> dplyr::slice(1)
check(
  nineties$n_with_abstract == 0,
  "1990–1999 correctly shows 0 abstracts available"
)

# Sector coverage: fraction with data
sec_long <- read_csv(file.path(DER, "sector_long.csv"), show_col_types = FALSE)
n_sec_proj <- dplyr::n_distinct(sec_long$project_id)
check(
  n_sec_proj >= 20 & n_sec_proj <= 25,
  paste("Sector data available for", n_sec_proj, "of 35 in-scope projects")
)

# Two projects tied at $500M (2009 and 2023); neither uniquely largest
top10 <- read_csv(file.path(DER, "ibrd_top10.csv"), show_col_types = FALSE)
n_500m <- sum(top10$total_principal == 500000000)
check(
  n_500m == 2,
  paste("Two projects tied at $500M (2009 DPL + 2023 DPL), got", n_500m)
)

# BAC CR Green in scope (projected year 2025, within window)
# Note: ifc loaded in section 5 below; must read it here for this check
ifc_tmp <- read_csv(file.path(PROC, "ifc_projects.csv"), show_col_types = FALSE)
bac_in <- ifc_tmp$in_scope[ifc_tmp$project_number == "50864"]
check(
  length(bac_in) == 1 && bac_in,
  "BAC CR Green (50864) is in-scope (best_year=2025)"
)

cat("\n--- IFC ---\n")
ifc <- ifc_tmp
check(
  nrow(ifc) == 30,
  paste("30 unique IFC project numbers (got", nrow(ifc), ")")
)

# Insulet CR (pipeline) must not be in historical analysis scope
check(
  !any(ifc$project_number == "51930" & ifc$in_scope & !ifc$is_pipeline),
  "Insulet CR (51930, pipeline 2026) correctly excluded from in_scope"
)

bac_amt <- ifc$ifc_invest_usd_m[ifc$project_number == "50864"]
check(
  !is.na(bac_amt) && bac_amt == 200,
  paste("BAC CR Green amount = $200M (got", bac_amt, ")")
)

# ---------------------------------------------------------------------------
# 6.  MIGA validations
# ---------------------------------------------------------------------------

cat("\n--- MIGA ---\n")
miga <- read_csv(file.path(PROC, "miga_projects.csv"), show_col_types = FALSE)
miga_raw_all <- read_csv(
  file.path(PROC, "miga_records_raw.csv"),
  show_col_types = FALSE
)
check(
  nrow(miga_raw_all) == 12,
  paste("MIGA raw records = 12 (got", nrow(miga_raw_all), ")")
)
check(nrow(miga) == 10, paste("MIGA deduplicated = 10 (got", nrow(miga), ")"))

# Project 3639 deduplication: only 1 row should remain
proj3639_count <- sum(miga$project_id == 3639)
check(
  proj3639_count == 1,
  paste("Project 3639 deduplicated to 1 row (got", proj3639_count, ")")
)

# ---------------------------------------------------------------------------
# 7.  Figure files exist
# ---------------------------------------------------------------------------

cat("\n--- Figures ---\n")
figs <- c(
  "fig1_ibrd_timeline",
  "fig2_instrument_mix",
  "fig3_sector_composition",
  "fig4_top_projects",
  "fig5_abstract_themes",
  "fig6_development_trajectory",
  "fig7_ifc_investments",
  "fig8_nominal_vs_real"
)
for (f in figs) {
  check(
    file.exists(file.path(FIG, paste0(f, ".png"))),
    paste("Figure exists:", f)
  )
}

# ---------------------------------------------------------------------------
# 8.  Final summary
# ---------------------------------------------------------------------------

cat("\n========== VALIDATION RESULTS ==========\n")
cat("Passed checks: implied by OK messages above\n")
if (length(errors) > 0) {
  cat("\nERRORS (", length(errors), "):\n")
  cat(paste(errors, collapse = "\n"), "\n")
} else {
  cat("ERRORS: None\n")
}
if (length(warnings) > 0) {
  cat("\nWARNINGS (", length(warnings), "):\n")
  cat(paste(warnings, collapse = "\n"), "\n")
} else {
  cat("WARNINGS: None\n")
}

if (length(errors) > 0) {
  stop("Validation failed. See errors above.")
} else {
  cat("\nValidation PASSED. Repository is ready for publication.\n")
}
