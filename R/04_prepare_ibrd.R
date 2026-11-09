# 04_prepare_ibrd.R
# Clean and aggregate IBRD loan records.
# Merges financial data with project metadata; calculates real 2025 USD values.
# Saves: data/processed/ibrd_loans_clean.csv, data/processed/ibrd_projects.csv,
#        data/derived/ibrd_annual.csv, data/derived/ibrd_by_period.csv

source(here::here("R", "00_setup.R"))

cat("\n========== IBRD DATA PREPARATION ==========\n\n")

# ---------------------------------------------------------------------------
# 1.  Load inputs
# ---------------------------------------------------------------------------

ibrd_raw <- read_csv(
  file.path(
    RAW,
    "ibrd_statement_of_loans_and_guarantees_latest_available_snapshot_10-31-2026.csv"
  ),
  show_col_types = FALSE
)
projects <- read_csv(
  file.path(PROC, "projects_universe.csv"),
  show_col_types = FALSE
)
ibrd_elig <- read_csv(
  file.path(PROC, "ibrd_eligible_projects.csv"),
  show_col_types = FALSE
)
deflator <- read_csv(
  file.path(PROC, "us_gdp_deflator.csv"),
  show_col_types = FALSE
)

cat("IBRD raw rows:", nrow(ibrd_raw), "\n")
cat("Deflator coverage:", min(deflator$year), "-", max(deflator$year), "\n")

defl_2025 <- deflator$defl[deflator$year == 2025]
cat("Deflator 2025 value:", defl_2025, "(base year 2020=100)\n\n")

# ---------------------------------------------------------------------------
# 2.  Clean loan records
# ---------------------------------------------------------------------------

# Standardise column names
names(ibrd_raw) <- gsub("[^a-zA-Z0-9]", "_", tolower(names(ibrd_raw)))
names(ibrd_raw) <- gsub("_+", "_", names(ibrd_raw))
names(ibrd_raw) <- gsub("_$", "", names(ibrd_raw))

ibrd_loans <- ibrd_raw |>
  filter(
    !is.na(country_economy_code),
    country_economy_code == "CR",
    !is.na(loan_number)
  ) |>
  mutate(
    board_date = lubridate::mdy(board_approval_date),
    approval_year = year(board_date),
    effective_date = lubridate::mdy(effective_date_most_recent),
    closed_date = lubridate::mdy(closed_date_most_recent),
    is_guarantee = grepl("^IBRDB|^IBRDG", loan_number),
    # net_principal: original commitment minus cancellations.
    # For Terminated loans where original_principal_amount_us = 0:
    # The IBRD statement records these as cancelled_amount > 0 but original = 0.
    # This is a known IBRD accounting convention for loans that were approved but
    # never became legally effective. Net commitment is analytically zero, not negative.
    net_principal = pmax(original_principal_amount_us - cancelled_amount_us, 0),
    # Inflation adjustment to 2025 USD
    defl_year = deflator$defl[match(approval_year, deflator$year)],
    real_principal_2025 = if_else(
      !is.na(defl_year) & defl_year > 0,
      original_principal_amount_us * (defl_2025 / defl_year),
      NA_real_
    ),
    real_net_2025 = if_else(
      !is.na(defl_year) & defl_year > 0,
      net_principal * (defl_2025 / defl_year),
      NA_real_
    ),
    # Period grouping
    period = dplyr::case_when(
      is.na(approval_year) ~ NA_character_,
      approval_year < 1980 ~ "Pre-1980",
      approval_year <= 1989 ~ "1980–1989",
      approval_year <= 1999 ~ "1990–1999",
      approval_year <= 2009 ~ "2000–2009",
      approval_year <= 2019 ~ "2010–2019",
      approval_year <= 2025 ~ "2020–2025",
      TRUE ~ "Post-scope"
    ),
    # Automated instrument classification: instrument_group_auto
    # This is the raw rule-based result, preserved for auditing.
    # Rules apply loan_type codes and project-name patterns.
    # Manual overrides are applied separately below.
    instrument_group_auto = dplyr::case_when(
      # Structural Adjustment Loans by name
      grepl(
        "\\bSAL\\s*(I|II|III|1|2|3)\\b|STRUCT.{0,5}ADJ|STRCTRL ADJ",
        project_name,
        ignore.case = TRUE
      ) ~ "Development Policy Loan",
      # NPL loan type — historically used for non-project/policy-based operations.
      # NOTE: P006919 is the only CR NPL and is a road infrastructure project;
      # it is manually overridden below. Flag all NPL projects for review.
      grepl("^NPL$", loan_type) ~ "Development Policy Loan",
      # Modern DPLs: explicit "DPL", "Development Policy", "DDO" in name
      grepl(
        "\\bDPL\\b|DEVELOPMENT POLICY|\\bDDO\\b",
        project_name,
        ignore.case = TRUE
      ) ~ "Development Policy Loan",
      # Fiscal and Decarbonization series
      grepl(
        "FISCAL.{1,10}DECARB",
        project_name,
        ignore.case = TRUE
      ) ~ "Development Policy Loan",
      # P508347: "Fiscal Management and Green Growth" confirmed DPL in source
      project_id == "P508347" ~ "Development Policy Loan",
      # Everything else: investment / TA / P4R
      TRUE ~ "Investment Project Financing"
    )
    # Note: P006922 (Export Development 1983) is classified DPL via manual override,
    # not automated rules. Its automated result is "Investment Project Financing".
  )

cat("CR loan records:", nrow(ibrd_loans), "\n")
cat("  Regular loans:", sum(!ibrd_loans$is_guarantee), "\n")
cat("  Guarantees (IBRDB/IBRDG):", sum(ibrd_loans$is_guarantee), "\n")
cat("  Missing approval year:", sum(is.na(ibrd_loans$approval_year)), "\n")
cat("  Loan types:\n")
print(table(ibrd_loans$loan_type))
cat("  Loan statuses:\n")
print(table(ibrd_loans$loan_status))

# ---------------------------------------------------------------------------
# 2b. Apply manual instrument overrides
# ---------------------------------------------------------------------------

overrides <- read_csv(
  file.path(here::here("data", "manual", "instrument_overrides.csv")),
  show_col_types = FALSE
)

# Validate: all override project IDs exist in the loan data
bad_ids <- setdiff(overrides$project_id, ibrd_loans$project_id)
if (length(bad_ids) > 0) {
  warning(
    "Override project IDs not found in loan data: ",
    paste(bad_ids, collapse = ", ")
  )
}

# Join overrides and apply; retain auto result in instrument_group_auto
ibrd_loans <- ibrd_loans |>
  left_join(
    overrides |> select(project_id, instrument_group_override, override_reason),
    by = "project_id"
  ) |>
  mutate(
    override_applied = !is.na(instrument_group_override),
    instrument_group = if_else(
      override_applied,
      instrument_group_override,
      instrument_group_auto
    )
  )

cat("\nManual overrides applied:", sum(ibrd_loans$override_applied), "\n")
ibrd_loans |>
  filter(override_applied) |>
  select(
    project_id,
    project_name,
    loan_type,
    instrument_group_auto,
    instrument_group
  ) |>
  as.data.frame() |>
  print()

# ---------------------------------------------------------------------------
# 3.  Financial validation against Phase 1 reference points
# ---------------------------------------------------------------------------

cat("\n=== FINANCIAL VALIDATION ===\n")
all_cr <- ibrd_loans |> filter(!is_guarantee)
cat(
  "All CR regular loans — original principal:",
  fmt_billions(sum(all_cr$original_principal_amount_us, na.rm = TRUE)),
  "\n"
)
cat(
  "  Phase 1 reference: ~$4.9B — match:",
  round(sum(all_cr$original_principal_amount_us, na.rm = TRUE) / 1e9, 1) >= 4.8,
  "\n"
)
cat(
  "All CR regular loans — cancelled:",
  fmt_millions(sum(all_cr$cancelled_amount_us, na.rm = TRUE)),
  "\n"
)
cat("  Phase 1 reference: ~$479M\n")
cat(
  "All CR regular loans — net principal:",
  fmt_billions(sum(all_cr$net_principal, na.rm = TRUE)),
  "\n"
)
cat("  Phase 1 reference: ~$4.4B\n")
cat(
  "All CR regular loans — disbursed:",
  fmt_billions(sum(all_cr$disbursed_amount_us, na.rm = TRUE)),
  "\n"
)
cat("  Phase 1 reference: ~$3.2B\n")

# Check P148435
p148 <- ibrd_loans |> filter(project_id == "P148435")
cat("\nP148435 (Health Insurance, 2016):", nrow(p148), "loan record(s)\n")
cat(
  "  Original principal:",
  fmt_millions(p148$original_principal_amount_us[1]),
  "\n"
)
cat("  Status:", p148$loan_status[1], "\n")

# ---------------------------------------------------------------------------
# 4.  Project-level aggregation
# ---------------------------------------------------------------------------

ibrd_projects <- ibrd_loans |>
  filter(!is_guarantee, !is.na(project_id)) |>
  group_by(project_id) |>
  summarise(
    n_loans = n(),
    approval_year = first(approval_year),
    board_date = first(board_date),
    period = first(period),
    instrument_group = paste(sort(unique(instrument_group)), collapse = "; "),
    instrument_group_auto = paste(
      sort(unique(instrument_group_auto)),
      collapse = "; "
    ),
    override_applied = any(override_applied),
    override_reason = first(na.omit(override_reason)),
    loan_type_codes = paste(sort(unique(loan_type)), collapse = "; "),
    loan_status_summary = paste(sort(unique(loan_status)), collapse = "; "),
    loan_numbers = paste(sort(loan_number), collapse = "; "),
    # Financial aggregates
    total_principal = sum(original_principal_amount_us, na.rm = TRUE),
    total_cancelled = sum(cancelled_amount_us, na.rm = TRUE),
    total_disbursed = sum(disbursed_amount_us, na.rm = TRUE),
    total_repaid = sum(repaid_to_ibrd_us, na.rm = TRUE),
    total_loans_held = sum(loans_held_us, na.rm = TRUE),
    net_principal = pmax(total_principal - total_cancelled, 0),
    real_principal_2025 = sum(real_principal_2025, na.rm = TRUE),
    real_net_2025 = sum(real_net_2025, na.rm = TRUE),
    .groups = "drop"
  ) |>
  # Merge project metadata from the universe
  left_join(
    projects |>
      select(
        project_id,
        project_name,
        financing_type,
        last_stage,
        sector1_xl,
        sector2_xl,
        sector3_xl,
        pdo,
        is_ibrd_loan,
        is_grant,
        is_other,
        in_scope,
        pre_1980
      ),
    by = "project_id"
  )

cat("\n=== PROJECT-LEVEL IBRD SUMMARY ===\n")
cat("Projects with loan records:", nrow(ibrd_projects), "\n")
study_proj <- ibrd_projects |>
  filter(in_scope == TRUE | (approval_year >= 1980 & approval_year <= 2025))
cat("In-scope 1980-2025:", nrow(study_proj), "\n")
cat(
  "Total original principal (1980-2025):",
  fmt_billions(sum(study_proj$total_principal)),
  "\n"
)
cat("  Phase 1 reference: ~$4.6B\n")
cat(
  "Real 2025 USD (1980-2025):",
  fmt_billions(sum(study_proj$real_principal_2025, na.rm = TRUE)),
  "\n"
)
cat(
  "Net principal (1980-2025):",
  fmt_billions(sum(study_proj$net_principal, na.rm = TRUE)),
  "\n"
)

# Top 10 by principal
cat("\nTop 10 by original principal (1980-2025):\n")
study_proj |>
  arrange(desc(total_principal)) |>
  head(10) |>
  select(
    project_id,
    project_name,
    approval_year,
    total_principal,
    net_principal,
    instrument_group,
    loan_status_summary
  ) |>
  mutate(
    total_principal = fmt_millions(total_principal),
    net_principal = fmt_millions(net_principal)
  ) |>
  as.data.frame() |>
  print()

# ---------------------------------------------------------------------------
# 5.  Annual summary
# ---------------------------------------------------------------------------

all_years <- tibble(approval_year = 1980L:2025L)

ibrd_annual <- study_proj |>
  group_by(approval_year) |>
  summarise(
    n_projects = n(),
    total_principal = sum(total_principal),
    net_principal = sum(net_principal),
    real_principal = sum(real_principal_2025, na.rm = TRUE),
    total_cancelled = sum(total_cancelled),
    n_dpl = sum(grepl("Development Policy", instrument_group)),
    n_ipf = sum(grepl("Investment", instrument_group)),
    .groups = "drop"
  ) |>
  right_join(all_years, by = "approval_year") |>
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) |>
  arrange(approval_year)

cat("\nAnnual summary (first and last years):\n")
print(rbind(head(ibrd_annual, 5), tail(ibrd_annual, 5)))

# Period summary
ibrd_by_period <- study_proj |>
  filter(!is.na(period)) |>
  group_by(period) |>
  summarise(
    n_projects = n(),
    total_principal = sum(total_principal),
    net_principal = sum(net_principal),
    real_principal = sum(real_principal_2025, na.rm = TRUE),
    n_dpl = sum(grepl("Development Policy", instrument_group)),
    n_ipf = sum(grepl("Investment", instrument_group)),
    .groups = "drop"
  ) |>
  mutate(
    pct_nominal = round(100 * total_principal / sum(total_principal), 1),
    pct_real = round(100 * real_principal / sum(real_principal), 1)
  )

cat("\nBy period:\n")
print(ibrd_by_period)

# ---------------------------------------------------------------------------
# 5b.  Classification review table (auditable output)
# ---------------------------------------------------------------------------

# Determine the basis of each automated classification
classify_basis <- function(proj_name, loan_type_code) {
  dplyr::case_when(
    grepl(
      "\\bSAL\\s*(I|II|III|1|2|3)\\b|STRUCT.{0,5}ADJ|STRCTRL ADJ",
      proj_name,
      ignore.case = TRUE
    ) ~ "Name match: SAL/structural adjustment",
    grepl("^NPL$", loan_type_code) ~
      "Loan type NPL — flag for review: rule assigns DPL but NPL alone is insufficient",
    grepl(
      "\\bDPL\\b|DEVELOPMENT POLICY|\\bDDO\\b",
      proj_name,
      ignore.case = TRUE
    ) ~ "Name match: DPL/Development Policy/DDO",
    grepl("FISCAL.{1,10}DECARB", proj_name, ignore.case = TRUE) ~
      "Name match: Fiscal…Decarbonization pattern",
    TRUE ~ "Default rule: Investment Project Financing"
  )
}

classification_review <- ibrd_projects |>
  filter(!is.na(approval_year), approval_year >= 1980, approval_year <= 2025) |>
  mutate(
    auto_basis = classify_basis(project_name, loan_type_codes)
  ) |>
  # P508347 override basis not captured by function; fix inline
  mutate(
    auto_basis = if_else(
      project_id == "P508347" &
        auto_basis == "Default rule: Investment Project Financing",
      "Project-ID hardcode: P508347 confirmed DPL",
      auto_basis
    )
  ) |>
  select(
    project_id,
    project_name,
    approval_year,
    loan_type_codes,
    instrument_group_auto,
    instrument_group,
    override_applied,
    override_reason,
    auto_basis
  ) |>
  arrange(approval_year)

cat("\n=== CLASSIFICATION REVIEW SUMMARY ===\n")
cat("Projects by final instrument_group:\n")
print(table(classification_review$instrument_group))
cat(
  "\nProjects with manual override:",
  sum(classification_review$override_applied),
  "\n"
)
cat("\nAuto basis distribution:\n")
print(table(classification_review$auto_basis))

# Flag NPL projects specifically for review
npls <- classification_review |> filter(grepl("NPL", auto_basis))
if (nrow(npls) > 0) {
  cat("\nNPL-classified projects (require manual review):\n")
  npls |>
    select(
      project_id,
      project_name,
      approval_year,
      instrument_group_auto,
      instrument_group,
      override_applied
    ) |>
    as.data.frame() |>
    print()
}

write_csv(
  classification_review,
  file.path(DER, "instrument_classification_review.csv")
)
cat("Saved: data/derived/instrument_classification_review.csv\n")

# ---------------------------------------------------------------------------
# 6.  Save
# ---------------------------------------------------------------------------

write_csv(ibrd_loans, file.path(PROC, "ibrd_loans_clean.csv"))
write_csv(ibrd_projects, file.path(PROC, "ibrd_projects.csv"))
write_csv(ibrd_annual, file.path(DER, "ibrd_annual.csv"))
write_csv(ibrd_by_period, file.path(DER, "ibrd_by_period.csv"))

cat("\nSaved:\n")
cat("  data/processed/ibrd_loans_clean.csv\n")
cat("  data/processed/ibrd_projects.csv\n")
cat("  data/derived/ibrd_annual.csv\n")
cat("  data/derived/ibrd_by_period.csv\n")
cat("IBRD preparation complete.\n")
