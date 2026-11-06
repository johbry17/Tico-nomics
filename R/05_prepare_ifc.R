# 05_prepare_ifc.R
# Clean and deduplicate IFC project disclosures for Costa Rica.
# IFC investment amounts are NOT comparable to IBRD loan principal.
# Saves: data/processed/ifc_projects.csv

source(here::here("R", "00_setup.R"))

cat("\n========== IFC DATA PREPARATION ==========\n\n")

cat(
  "IMPORTANT: IFC investment amounts represent IFC's own-account investment in\n"
)
cat(
  "PRIVATE entities, not sovereign loans. They are NOT comparable to IBRD principal.\n\n"
)

# ---------------------------------------------------------------------------
# 1.  Load raw data
# ---------------------------------------------------------------------------

ifc_raw <- read_excel(file.path(RAW, "IFC_Projects_Costa Rica.xlsx"), sheet = 1)
cat(
  "Raw rows:",
  nrow(ifc_raw),
  "| Unique Project Numbers:",
  length(unique(ifc_raw[["Project Number"]])),
  "\n"
)

# ---------------------------------------------------------------------------
# 2.  Parse investment amounts from text field
# ---------------------------------------------------------------------------

# The "Investment as Approved by The Board" field contains text like:
#   "15.0 million (USD)Product LineIFC Investment..."
#   "100.0 million (USD) View IaAbTB..."
# Extract the leading number as IFC's own investment amount.

parse_ifc_amount <- function(txt) {
  if (is.na(txt) || nchar(trimws(txt)) == 0) {
    return(NA_real_)
  }
  # Pattern: leading number (with possible decimal) followed by "million"
  m <- regmatches(
    txt,
    regexpr("^\\s*([0-9]+\\.?[0-9]*)\\s*million", txt, ignore.case = TRUE)
  )
  if (length(m) == 0 || nchar(m) == 0) {
    return(NA_real_)
  }
  as.numeric(regmatches(m, regexpr("^\\s*([0-9]+\\.?[0-9]*)", m)))
}

ifc_raw[["ifc_invest_parsed"]] <- sapply(
  ifc_raw[["Investment as Approved by The Board"]],
  parse_ifc_amount
)

# Validate parsing
cat(
  "Parsed amounts (non-NA):",
  sum(!is.na(ifc_raw[["ifc_invest_parsed"]])),
  "of",
  nrow(ifc_raw),
  "\n"
)
cat("Sample parsed values (first 5 rows with amounts):\n")
ifc_raw |>
  filter(!is.na(ifc_invest_parsed)) |>
  select(`Project Number`, `Project Name`, ifc_invest_parsed) |>
  head(5) |>
  as.data.frame() |>
  print()

# ---------------------------------------------------------------------------
# 3.  One row per unique project number
# ---------------------------------------------------------------------------

ifc_projects <- ifc_raw |>
  group_by(`Project Number`) |>
  summarise(
    project_name = first(`Project Name`),
    type_desc = first(`Type Description`),
    status = first(`Status Description`),
    sector = first(Sector[!is.na(Sector)]),
    business_area = first(`Business Area Description`[
      !is.na(`Business Area Description`)
    ]),
    # Date fields
    approval_date_act = first(`Approval Date`[!is.na(`Approval Date`)]),
    projected_board_dt = first(`Projected Board Date`[
      !is.na(`Projected Board Date`)
    ]),
    # Amount (use first non-NA parsed amount across disclosure docs)
    ifc_invest_usd_m = first(ifc_invest_parsed[!is.na(ifc_invest_parsed)]),
    n_docs = n(),
    investment_text = first(`Investment as Approved by The Board`[
      !is.na(`Investment as Approved by The Board`)
    ]),
    description = first(`Project Description`[!is.na(`Project Description`)]),
    .groups = "drop"
  ) |>
  rename(project_number = `Project Number`) |>
  mutate(
    # Use projected board date as best available proxy for approval year
    # IMPORTANT: this is projected/planned, NOT confirmed actual approval
    best_date = dplyr::coalesce(approval_date_act, projected_board_dt),
    best_year = year(best_date),
    date_type = dplyr::case_when(
      !is.na(approval_date_act) ~ "Actual (Advisory only)",
      !is.na(projected_board_dt) ~ "Projected board date (indicative)",
      TRUE ~ "No date available"
    ),
    in_scope = !is.na(best_year) &
      best_year >= STUDY_START &
      best_year <= STUDY_END,
    is_advisory = type_desc == "Advisory Services",
    is_investment = type_desc == "Investment",
    is_pipeline = status == "Pending",

    # Sector simplification
    sector_group = dplyr::case_when(
      grepl(
        "Banking|Finance|Capital|SME|Housing Finance|Trade",
        sector,
        ignore.case = TRUE
      ) ~ "Financial Services",
      grepl(
        "Hotel|Tourism|Hospitality",
        sector,
        ignore.case = TRUE
      ) ~ "Tourism / Hospitality",
      grepl(
        "Hydro|Renewable|Power|Energy",
        sector,
        ignore.case = TRUE
      ) ~ "Energy",
      grepl(
        "Airport|Transportation",
        sector,
        ignore.case = TRUE
      ) ~ "Transportation",
      grepl("Medical|Health", sector, ignore.case = TRUE) ~ "Health",
      grepl(
        "Fruit|Vegetable|Agri|Farm",
        sector,
        ignore.case = TRUE
      ) ~ "Agriculture / Food",
      grepl("Retail|Supermarket", sector, ignore.case = TRUE) ~ "Retail",
      !is.na(sector) ~ "Other",
      TRUE ~ "Unclassified"
    )
  )

cat("\n=== IFC PROJECT SUMMARY ===\n")
cat("Unique projects:", nrow(ifc_projects), "\n")
cat("Type distribution:\n")
print(table(ifc_projects$type_desc))
cat("Status:\n")
print(table(ifc_projects$status))
cat("Date type:\n")
print(table(ifc_projects$date_type))
cat("In-scope (1980-2025):", sum(ifc_projects$in_scope, na.rm = TRUE), "\n")
cat(
  "Pipeline (Pending, excluded from historical analysis):",
  sum(ifc_projects$is_pipeline),
  "\n"
)

# ---------------------------------------------------------------------------
# 4.  Known issues documentation
# ---------------------------------------------------------------------------

cat("\n=== KNOWN IFC DATA ISSUES ===\n")
cat(
  "
1. BAC CR Green (50864): Board approved $200M; description mentions a $30M unfunded
   guarantee line. IFC's own-account investment = $200M per the disclosure field.
   Do NOT report $230M as the investment amount.
   Projected board date 2025-06-26 — within the 1980-2025 study window (year = 2025).
   
2. Coopenae (30325): $15M IFC; another IFI contributed $15M for a $30M total package.
   IFC amount = $15M.

3. Aeropuerto Juan Santamaría (9967): $35M. Older project; disclosure format less detailed.
   Includes IFC A-loan; B-loan syndication separate. IFC own-account = $35M.

4. Corporacion Supermercados Unidos (9233): $40M (Loan $30M + Equity $10M). Multi-country
   scope uncertain; Costa Rica appears to be the primary market.

5. Insulet CR (51930): $97M, Status=Pending, Projected board 2026-02-26.
   EXCLUDED from 1980-2025 historical analysis. Mentioned as pipeline.

6. Reventazon HPP (31383): $100M. Appears as two separate rows in the file (HPP + Trust).
   The Trust project is a separate financing vehicle for the same physical project.
   Both rows use the same project number 31383; deduplicated to one project here.

7. Date caveats: Only 2 Advisory Services projects have actual Approval Date.
   All Investment projects show Projected Board Date only. Year assignments should
   be labelled as 'indicative' and not used for precise annual trend claims.
\n"
)

# Specific project checks
cat(
  "BAC CR Green (50864) amount:",
  ifc_projects$ifc_invest_usd_m[ifc_projects$project_number == "50864"],
  "M\n"
)
cat(
  "Coopenae (30325) amount:",
  ifc_projects$ifc_invest_usd_m[ifc_projects$project_number == "30325"],
  "M\n"
)
cat(
  "Reventazon HPP (31383) amount:",
  ifc_projects$ifc_invest_usd_m[ifc_projects$project_number == "31383"],
  "M\n"
)
cat(
  "Insulet CR (51930) in_scope:",
  ifc_projects$in_scope[ifc_projects$project_number == "51930"],
  "\n"
)

# ---------------------------------------------------------------------------
# 5.  In-scope investment summary
# ---------------------------------------------------------------------------

ifc_invest_scope <- ifc_projects |>
  filter(is_investment, in_scope, !is_pipeline)

cat("\n=== IN-SCOPE IFC INVESTMENTS (1980-2025, excl. pipeline) ===\n")
cat("Projects:", nrow(ifc_invest_scope), "\n")
cat("With parsed amount:", sum(!is.na(ifc_invest_scope$ifc_invest_usd_m)), "\n")
cat(
  "Total IFC own-account (indicative):",
  sum(ifc_invest_scope$ifc_invest_usd_m, na.rm = TRUE),
  "M USD\n"
)
cat(
  "Note: dates are projected/planned; do not use for precise annual time series.\n"
)

cat("\nIn-scope investments by sector:\n")
ifc_invest_scope |>
  group_by(sector_group) |>
  summarise(
    n = n(),
    total_m = sum(ifc_invest_usd_m, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(total_m)) |>
  print()

# ---------------------------------------------------------------------------
# 6.  Save
# ---------------------------------------------------------------------------

write_csv(ifc_projects, file.path(PROC, "ifc_projects.csv"))
write_csv(ifc_invest_scope, file.path(DER, "ifc_investments_scope.csv"))

cat("\nSaved:\n")
cat("  data/processed/ifc_projects.csv\n")
cat("  data/derived/ifc_investments_scope.csv\n")
cat("IFC preparation complete.\n")
