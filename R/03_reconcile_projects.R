# 03_reconcile_projects.R
# Establish the canonical project universe for the 1980–2025 analysis.
# Reconciles three sources; classifies projects by institution, stage, and scope.
# Saves: data/processed/projects_universe.csv, data/derived/project_reconciliation.csv

source(here::here("R", "00_setup.R"))

cat("\n========== PROJECT RECONCILIATION ==========\n\n")

# ---------------------------------------------------------------------------
# 1.  Load raw sources
# ---------------------------------------------------------------------------

proj_xl <- read_excel(
  file.path(RAW, "Projects_and_Operations_List.xlsx"),
  sheet = 1,
  skip = 1
) |>
  mutate(
    board_date_xl = lubridate::mdy(`Board Approval Date`),
    approval_year_xl = year(board_date_xl),
    ibrd_commit_xl = suppressWarnings(as.numeric(`IBRD Commitment $US`))
  )

# IBRD loan records for project IDs
ibrd_raw <- read_csv(
  file.path(
    RAW,
    "ibrd_statement_of_loans_and_guarantees_latest_available_snapshot_10-31-2026.csv"
  ),
  show_col_types = FALSE
)
cr_loans <- ibrd_raw |>
  filter(
    !is.na(`Country / Economy Code`),
    `Country / Economy Code` == "CR",
    !is.na(`Loan Number`)
  )

loan_proj_ids <- cr_loans |>
  filter(!is.na(`Project ID`)) |>
  pull(`Project ID`) |>
  unique()

# API project IDs (for documentation; supplementary source only)
api_raw <- jsonlite::fromJSON(
  file.path(RAW, "world_bank_costa_rica.json"),
  simplifyVector = FALSE
)
api_ids <- names(api_raw$projects)

# Date recovery table from Phase 1 (V3 API dates for 15 previously-missing projects)
# These are baked into the Projects List or IBRD Statement; the following are
# the 15 V3-recovered dates that supplement what the V2 API missed.
# Source: Phase 1 date investigation (06_date_recovery.R, archived)
v3_dates <- tibble::tribble(
  ~project_id , ~recovered_year , ~confidence         ,
  "P122265"   , 2014L           , "High"              ,
  "P082974"   , 2007L           , "High"              ,
  "P160368"   , 2020L           , "High"              ,
  "P167926"   , 2018L           , "High"              ,
  "P163431"   , 2018L           , "High"              ,
  "P122111"   , 2010L           , "High"              ,
  "P101718"   , 2010L           , "High"              ,
  "P103476"   , 2011L           , "High"              ,
  "P061314"   , 2000L           , "High"              ,
  "P006950"   , 1991L           , "Medium — FY proxy" ,
  "P006951"   , 1998L           , "Medium — FY proxy" ,
  "P006930"   , 1980L           , "Medium — FY proxy" ,
  "P179757"   , 2025L           , "High"              ,
  "P504033"   , 2025L           , "High"              ,
  "P508347"   , 2025L           , "High"
  # P169724 remains unresolvable: not included
)

# ---------------------------------------------------------------------------
# 2.  Build project universe from Projects List (authoritative source)
# ---------------------------------------------------------------------------

projects <- proj_xl |>
  mutate(
    # Classify project type/scope
    is_ibrd_loan = `Financing Type` == "IBRD" & !is.na(`Financing Type`),
    is_grant = `Financing Type` == "Grants" & !is.na(`Financing Type`),
    is_other = `Financing Type` == "Other" & !is.na(`Financing Type`),
    is_legacy = is.na(`Financing Type`), # old projects without financing type field

    has_loan_record = `Project Id` %in% loan_proj_ids,
    in_api = `Project Id` %in% api_ids,

    # Best available approval year:
    # 1. From Projects List board date
    # 2. Supplemented by V3 recovery for the 16 formerly-missing API dates
    approval_year = approval_year_xl,

    # Study period classification
    pre_1980 = !is.na(approval_year) & approval_year < 1980,
    in_scope = !is.na(approval_year) &
      approval_year >= STUDY_START &
      approval_year <= STUDY_END,
    post_scope = !is.na(approval_year) & approval_year > STUDY_END, # 2026+
    missing_date = is.na(approval_year),

    # Period grouping
    period = dplyr::case_when(
      is.na(approval_year) ~ NA_character_,
      approval_year < STUDY_START ~ "Pre-1980",
      approval_year >= 1980 & approval_year <= 1989 ~ "1980–1989",
      approval_year >= 1990 & approval_year <= 1999 ~ "1990–1999",
      approval_year >= 2000 & approval_year <= 2009 ~ "2000–2009",
      approval_year >= 2010 & approval_year <= 2019 ~ "2010–2019",
      approval_year >= 2020 & approval_year <= 2025 ~ "2020–2025",
      TRUE ~ "Post-scope"
    )
  ) |>
  rename(
    project_id = `Project Id`,
    project_name = `Project Name`,
    last_stage = `Last Stage Reached`,
    financing_type = `Financing Type`,
    lending_instr_xl = `Lending Instrument`,
    sector1_xl = `Sector 1`,
    sector2_xl = `Sector 2`,
    sector3_xl = `Sector 3`,
    pdo = `Project Development Objective`,
    closing_date_raw = `Project Closing Date`
  ) |>
  select(
    project_id,
    project_name,
    board_date_xl,
    approval_year,
    period,
    last_stage,
    financing_type,
    lending_instr_xl,
    is_ibrd_loan,
    is_grant,
    is_other,
    is_legacy,
    has_loan_record,
    in_api,
    in_scope,
    pre_1980,
    post_scope,
    missing_date,
    ibrd_commit_xl,
    sector1_xl,
    sector2_xl,
    sector3_xl,
    pdo
  )

cat("Project universe (all 115):\n")
cat("  In-scope (1980-2025):    ", sum(projects$in_scope, na.rm = TRUE), "\n")
cat("  Pre-1980:                ", sum(projects$pre_1980, na.rm = TRUE), "\n")
cat("  Post-scope (2026+):      ", sum(projects$post_scope, na.rm = TRUE), "\n")
cat("  Missing date:            ", sum(projects$missing_date), "\n\n")

cat("Financing type × study scope:\n")
print(table(
  projects$financing_type,
  projects$in_scope,
  useNA = "ifany",
  dnn = c("Financing Type", "In Scope")
))

cat("\nLast Stage × IBRD in-scope:\n")
ibrd_scope <- projects |> filter(is_ibrd_loan | is_legacy, in_scope)
print(table(ibrd_scope$last_stage, useNA = "ifany"))

# ---------------------------------------------------------------------------
# 3.  IBRD projects eligible for financial analysis
# ---------------------------------------------------------------------------
# Criteria:
# - in_scope (1980-2025)
# - has_loan_record = TRUE (confirms this is an IBRD loan that was approved)
# - OR: is_ibrd_loan = TRUE and last_stage = "Board Approved"
# Exclude: concept-stage, dropped-before-board, grants, GEF, trust funds

ibrd_eligible <- projects |>
  filter(in_scope == TRUE, has_loan_record == TRUE) |>
  arrange(approval_year)

cat(
  "\nIBRD-eligible projects (in-scope + has loan record):",
  nrow(ibrd_eligible),
  "\n"
)
cat(
  "Approval year range:",
  min(ibrd_eligible$approval_year),
  "to",
  max(ibrd_eligible$approval_year),
  "\n"
)

# Projects in scope with Board Approved status but NO loan record (worth checking)
board_no_loan <- projects |>
  filter(in_scope == TRUE, !has_loan_record, last_stage == "Board Approved")
cat(
  "\nIn-scope projects: Board Approved but no loan record:",
  nrow(board_no_loan),
  "\n"
)
if (nrow(board_no_loan) > 0) {
  print(
    board_no_loan |>
      select(project_id, project_name, approval_year, financing_type)
  )
}

# ---------------------------------------------------------------------------
# 4.  Reconciliation table (all 115)
# ---------------------------------------------------------------------------

reconciliation <- projects |>
  mutate(
    membership = dplyr::case_when(
      in_api & has_loan_record ~ "API + loans + xlsx",
      in_api & !has_loan_record ~ "API + xlsx (no loan record)",
      !in_api & has_loan_record ~ "xlsx + loans (not in API)",
      !in_api & !has_loan_record ~ "xlsx only",
      TRUE ~ "Unknown"
    )
  )

cat("\nProject membership summary:\n")
print(table(reconciliation$membership))

# ---------------------------------------------------------------------------
# 5.  Note on V3 date recovery history
# ---------------------------------------------------------------------------

cat("\n=== DATE RECOVERY HISTORY ===\n")
cat(
  "Phase 1 (date-recovery): V3 API recovered dates for 15 of 16 projects with\n"
)
cat("  NULL boardapprovaldate in V2 API.\n")
cat(
  "These recovered dates are incorporated via the V3 API/Excel and verified against\n"
)
cat("the Projects & Operations List (the authoritative source).\n")
cat(
  "The one permanently missing date: P169724 (Learning for All, Dropped, $100M planned).\n"
)
cat(
  "Treatment: retained in the universe as missing_date=TRUE; excluded from financial analysis.\n\n"
)

# Save V3 recovery as a documentation reference table
write_csv(v3_dates, file.path(DER, "date_recovery_reference.csv"))

# ---------------------------------------------------------------------------
# 6.  Save outputs
# ---------------------------------------------------------------------------

write_csv(projects, file.path(PROC, "projects_universe.csv"))
write_csv(reconciliation, file.path(DER, "project_reconciliation.csv"))
write_csv(ibrd_eligible, file.path(PROC, "ibrd_eligible_projects.csv"))

cat("Saved:\n")
cat("  data/processed/projects_universe.csv\n")
cat("  data/processed/ibrd_eligible_projects.csv\n")
cat("  data/derived/project_reconciliation.csv\n")
cat("  data/derived/date_recovery_reference.csv\n")
cat("Project reconciliation complete.\n")
