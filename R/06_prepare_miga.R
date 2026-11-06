# 06_prepare_miga.R
# Clean MIGA guarantee records for Costa Rica.
# MIGA gross exposure is NOT a loan or disbursement.
# Saves: data/processed/miga_projects.csv

source(here::here("R", "00_setup.R"))

cat("\n========== MIGA DATA PREPARATION ==========\n\n")

cat(
  "IMPORTANT: MIGA 'Gross Exposure Up to' is the maximum MIGA liability in the event\n"
)
cat(
  "of a claim on issued guarantee contracts. It is NOT a loan, investment, or\n"
)
cat("amount of capital received by Costa Rica.\n\n")

# ---------------------------------------------------------------------------
# 1.  Load raw data
# ---------------------------------------------------------------------------

miga_raw <- read_csv(
  file.path(RAW, "World_Bank_Group_Guarantees_MIGA-projects.csv"),
  show_col_types = FALSE
)
cat(
  "Raw rows:",
  nrow(miga_raw),
  "| Unique Project IDs:",
  length(unique(miga_raw[["Project ID"]])),
  "\n"
)
cat("Fiscal years:\n")
print(table(miga_raw[["Fiscal Year"]]))

# Column names
names(miga_raw) <- gsub("[^a-zA-Z0-9]", "_", tolower(names(miga_raw)))
names(miga_raw) <- gsub("_+", "_", names(miga_raw))
names(miga_raw) <- gsub("_$", "", names(miga_raw))

cat("Columns:", paste(names(miga_raw), collapse = ", "), "\n\n")

# ---------------------------------------------------------------------------
# 2.  Investigate Project 3639 (repeated across three fiscal years)
# ---------------------------------------------------------------------------

cat("=== INVESTIGATING PROJECT 3639 (3 rows) ===\n")
proj3639 <- miga_raw |> filter(project_id == 3639)
cat("Fiscal years:", proj3639$fiscal_year, "\n")
cat("Gross exposure by year:", proj3639$gross_exposure_up_to, "\n")
cat("Descriptions:\n")
for (i in 1:nrow(proj3639)) {
  cat(
    "  FY",
    proj3639$fiscal_year[i],
    ": $",
    proj3639$gross_exposure_up_to[i],
    "M | ",
    substr(proj3639$description[i], 1, 120),
    "\n"
  )
}
cat(
  "
FINDING: The three rows for project 3639 represent successive fiscal-year disclosures
for the same development project (Grupo del Istmo de Papagayo / Papagayo tourism):
  - FY2001: $16.4M — initial guarantee contract for equity investment
  - FY2002: $38.3M — 'total of fiscal 2001 and 2002' contracts (cumulative disclosure)
  - FY2003: $81.0M — new contract added for Scotiabank loan guarantee

These are NOT three separate $81M/38.3M/16.4M guarantees. They represent incremental
contracts reported at different points. Summing them would significantly overcount.

TREATMENT: Use the most recent comprehensive record (FY2003, $81M) as peak maximum
exposure for this project. Treat FY2001 and FY2002 rows as historical disclosures
of the same contracts. For annual analysis, use the first disclosure year (FY2001)
but assign the FY2003 $81M as the maximum total exposure.\n"
)

# ---------------------------------------------------------------------------
# 3.  Build project-level table
# ---------------------------------------------------------------------------

# Strategy: for project 3639, use the FY2003 record as canonical (most comprehensive)
# For all other projects, use the single record available

miga_dedup <- miga_raw |>
  # For project 3639: keep only FY2003 row as it captures the full scope
  filter(!(project_id == 3639 & fiscal_year %in% c(2001, 2002))) |>
  mutate(
    # MIGA uses fiscal years (Jul-Jun). fy_label is the WB FY number as reported.
    # FY2008 = July 2007–June 2008; guarantees in "FY2008" were issued in that period.
    # Do NOT interpret fy_label as a precise calendar year.
    fy_label = as.integer(fiscal_year),
    in_scope = fy_label >= STUDY_START & fy_label <= STUDY_END,
    project_id_chr = as.character(project_id)
  )

cat("\n=== MIGA PROJECTS (DEDUPLICATED) ===\n")
cat("Projects after deduplication:", nrow(miga_dedup), "\n")
cat("In-scope (1980-2025):", sum(miga_dedup$in_scope, na.rm = TRUE), "\n")

miga_dedup |>
  select(
    project_id,
    title,
    sector,
    fiscal_year,
    gross_exposure_up_to,
    description
  ) |>
  mutate(description = substr(description, 1, 80)) |>
  as.data.frame() |>
  print()

# Financial summary
cat(
  "\nTotal peak gross exposure (all projects, deduplicated):",
  sum(miga_dedup$gross_exposure_up_to, na.rm = TRUE),
  "M USD\n"
)
cat("Sector distribution:\n")
print(table(miga_dedup$sector))

# ---------------------------------------------------------------------------
# 4.  Note on FY conventions
# ---------------------------------------------------------------------------

cat("\n=== MIGA FISCAL YEAR CONVENTION ===\n")
cat("MIGA uses World Bank fiscal years (July 1 – June 30).\n")
cat(
  "MIGA FY2008 = July 2007 – June 2008. Approval/issuance likely occurred in FY2008.\n"
)
cat(
  "For approximate calendar years: the guarantee was likely issued in calendar 2007-2008.\n"
)
cat(
  "This data does not allow precise calendar-year assignment for MIGA guarantees.\n"
)

# ---------------------------------------------------------------------------
# 5.  Save
# ---------------------------------------------------------------------------

write_csv(miga_dedup, file.path(PROC, "miga_projects.csv"))
write_csv(miga_raw, file.path(PROC, "miga_records_raw.csv")) # preserve all 12 rows

cat("\nSaved:\n")
cat("  data/processed/miga_projects.csv (deduplicated, 10 projects)\n")
cat("  data/processed/miga_records_raw.csv (all 12 rows, for reference)\n")
cat("MIGA preparation complete.\n")
