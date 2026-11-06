# 01_import_sources.R
# Read all raw source files; document structure, dimensions, and snapshot dates.
# Saves: data/processed/source_inventory.csv
# No analytical transformations here; pure inspection and documentation.

source(here::here("R", "00_setup.R"))

cat("\n========== SOURCE IMPORT AND INSPECTION ==========\n\n")

# ---------------------------------------------------------------------------
# 1.  IBRD Statement of Loans (CSV — primary financial source)
# ---------------------------------------------------------------------------

cat("--- 1. IBRD Statement of Loans and Guarantees (CSV) ---\n")
ibrd_raw <- read_csv(
  file.path(
    RAW,
    "ibrd_statement_of_loans_and_guarantees_latest_available_snapshot_10-31-2026.csv"
  ),
  show_col_types = FALSE
)

ibrd_snapshot_date <- unique(ibrd_raw[["End of Period"]])
cat("Rows:", nrow(ibrd_raw), "| Cols:", ncol(ibrd_raw), "\n")
cat("Snapshot (End of Period field):", ibrd_snapshot_date, "\n")
# Verify: 3 trailing NA rows exist
na_rows <- sum(is.na(ibrd_raw[["Loan Number"]]))
cat("Rows with NA Loan Number (trailing blanks):", na_rows, "\n")

# Costa Rica filter
cr_loans_raw <- ibrd_raw |>
  filter(!is.na(`Country / Economy Code`), `Country / Economy Code` == "CR")
cat("Costa Rica loan records (code=CR):", nrow(cr_loans_raw), "\n")
cat(
  "Unique CR project IDs:",
  length(unique(cr_loans_raw[["Project ID"]][
    !is.na(cr_loans_raw[["Project ID"]])
  ])),
  "\n"
)
# No IBRDB/IBRDG guarantees in CR
cat(
  "CR guarantee records (IBRDB/IBRDG):",
  sum(grepl("^IBRDB|^IBRDG", cr_loans_raw[["Loan Number"]])),
  "\n"
)

# ---------------------------------------------------------------------------
# 2.  Projects and Operations List (Excel — primary project universe)
# ---------------------------------------------------------------------------

cat("\n--- 2. Projects and Operations List (Excel) ---\n")
proj_xl_raw <- read_excel(
  file.path(RAW, "Projects_and_Operations_List.xlsx"),
  sheet = 1,
  skip = 1
)

xl_snapshot_date <- read_excel(
  file.path(RAW, "Projects_and_Operations_List.xlsx"),
  sheet = 1,
  col_names = FALSE,
  n_max = 1
)[[1, 1]]
cat("Snapshot header:", xl_snapshot_date, "\n")
cat(
  "Rows:",
  nrow(proj_xl_raw),
  "| Unique Project IDs:",
  length(unique(proj_xl_raw[["Project Id"]])),
  "\n"
)
cat(
  "Missing approval dates:",
  sum(is.na(mdy(proj_xl_raw[["Board Approval Date"]]))),
  "\n"
)
cat("Financing types:\n")
print(table(proj_xl_raw[["Financing Type"]], useNA = "ifany"))
cat("Last Stage Reached:\n")
print(table(proj_xl_raw[["Last Stage Reached"]], useNA = "ifany"))

# ---------------------------------------------------------------------------
# 3.  WB Projects API V2 extracts
# ---------------------------------------------------------------------------

cat("\n--- 3. WB Projects API V2 extracts ---\n")
api_main <- fromJSON(
  file.path(RAW, "world_bank_costa_rica.json"),
  simplifyVector = FALSE
)
api_filt <- fromJSON(
  file.path(RAW, "world_bank_costa_rica_filtered.json"),
  simplifyVector = FALSE
)
cat(
  "Main API: total_reported=",
  api_main$total,
  "| projects_returned=",
  length(api_main$projects),
  "\n"
)
cat(
  "Filtered API: total_reported=",
  api_filt$total,
  "| projects_returned=",
  length(api_filt$projects),
  "\n"
)
cat(
  "Note: API V2 is a strict subset of the Projects List (72 of 115 records).\n"
)
cat(
  "Note: 16 projects had NULL boardapprovaldate in V2; V3 API recovered dates for 15/16.\n"
)
cat(
  "Source hierarchy: Projects List > IBRD Statement > API. API is supplementary.\n"
)

# ---------------------------------------------------------------------------
# 4.  IBRD Loans Summary by Status (validation only)
# ---------------------------------------------------------------------------

cat("\n--- 4. IBRD Loans Summary by Status (global aggregate) ---\n")
ibrd_summ <- read_csv(
  file.path(RAW, "ibrd_loans_summary_by_status_10-31-2026.csv"),
  show_col_types = FALSE
)
cat(
  "Rows:",
  nrow(ibrd_summ),
  "(global summary by status — NOT country-level)\n"
)
cat("Use for global validation only, not Costa Rica analysis.\n")

# ---------------------------------------------------------------------------
# 5.  IFC Projects (Excel — companion institutional analysis)
# ---------------------------------------------------------------------------

cat("\n--- 5. IFC Projects Costa Rica (Excel) ---\n")
ifc_raw <- read_excel(file.path(RAW, "IFC_Projects_Costa Rica.xlsx"), sheet = 1)
cat(
  "Rows:",
  nrow(ifc_raw),
  "| Unique Project Numbers:",
  length(unique(ifc_raw[["Project Number"]])),
  "\n"
)
cat("Type Description:\n")
print(table(ifc_raw[["Type Description"]], useNA = "ifany"))
cat("Status:\n")
print(table(ifc_raw[["Status Description"]], useNA = "ifany"))
cat(
  "Actual Approval Dates (non-NA):",
  sum(!is.na(ifc_raw[["Approval Date"]])),
  "(2 Advisory Services only)\n"
)
cat(
  "Projected Board Dates (non-NA):",
  sum(!is.na(ifc_raw[["Projected Board Date"]])),
  "\n"
)
cat(
  "Note: 'Projected Board Date' is a PLANNED date, not an actual approval date.\n"
)
cat("Note: Investment amounts are embedded in text field; require parsing.\n")

# ---------------------------------------------------------------------------
# 6.  MIGA Projects (CSV — historical guarantees)
# ---------------------------------------------------------------------------

cat("\n--- 6. MIGA Projects (CSV) ---\n")
miga_raw <- read_csv(
  file.path(RAW, "World_Bank_Group_Guarantees_MIGA-projects.csv"),
  show_col_types = FALSE
)
cat(
  "Rows:",
  nrow(miga_raw),
  "| Unique Project IDs:",
  length(unique(miga_raw[["Project ID"]])),
  "\n"
)
cat(
  "Note: Project 3639 appears 3 times (FY2001, FY2002, FY2003 — escalating cumulative exposure).\n"
)
cat("Fiscal Year distribution:\n")
print(table(miga_raw[["Fiscal Year"]]))
cat("Sector:\n")
print(table(miga_raw[["Sector"]], useNA = "ifany"))
cat(
  "'Gross Exposure Up to' = maximum MIGA liability. NOT a loan, NOT disbursed capital.\n"
)

# ---------------------------------------------------------------------------
# 7.  Save source inventory
# ---------------------------------------------------------------------------

inventory <- tribble(
  ~file                                           , ~format , ~rows          , ~unit                        , ~scope                    , ~snapshot_date          , ~primary_use                 ,
  "ibrd_statement_of_loans…csv"                   , "CSV"   , "9,538"        , "Loan-level"                 , "Global (68 CR)"          , "09/30/2026"            , "Primary financial source"   ,
  "Projects_and_Operations_List.xlsx"             , "Excel" , "115"          , "Project-level"              , "Costa Rica only"         , "11/01/2026"            , "Primary project universe"   ,
  "world_bank_costa_rica.json"                    , "JSON"  , "72"           , "Project-level"              , "Costa Rica (API subset)" , "10/31/2026 (approx)"   , "Supplementary metadata"     ,
  "world_bank_costa_rica_filtered.json"           , "JSON"  , "72"           , "Project-level"              , "Costa Rica (API subset)" , "10/31/2026 (approx)"   , "Project abstracts"          ,
  "ibrd_loans_summary_by_status.csv"              , "CSV"   , "11 rows"      , "Global aggregate by status" , "Global"                  , "10/31/2026 (filename)" , "Global validation only"     ,
  "IFC_Projects_Costa Rica.xlsx"                  , "Excel" , "40 (30 proj)" , "Disclosure-level"           , "Costa Rica"              , "11/2/2026"             , "IFC companion analysis"     ,
  "World_Bank_Group_Guarantees_MIGA-projects.csv" , "CSV"   , "12 (10 proj)" , "Project-level"              , "Costa Rica"              , "11/2/2026"             , "MIGA historical guarantees"
)
write_csv(inventory, file.path(DER, "source_inventory.csv"))
cat("\nSaved: data/derived/source_inventory.csv\n")
cat("Source import complete.\n")
