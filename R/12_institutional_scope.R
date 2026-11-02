# 12_institutional_scope.R
# Phase 1: Document WBG institutional scope; assess IDA, IFC, MIGA data availability.
# Retrieves US GDP deflator and assesses inflation adjustment feasibility.

library(here)
library(httr2)
library(jsonlite)
library(dplyr)
library(readr)
library(lubridate)

source(here("R", "functions.R"))

der_dir <- here("data", "derived")

# ---------------------------------------------------------------------------
# 1.  World Bank Group institutional structure
# ---------------------------------------------------------------------------

cat("=== WORLD BANK GROUP INSTITUTIONAL STRUCTURE ===\n")
cat("
The 'World Bank Group' comprises five institutions:

1. IBRD (International Bank for Reconstruction and Development)
   - Lends to creditworthy middle-income and upper-middle-income countries
   - Market-rate, long-maturity loans
   - Costa Rica is an IBRD borrower

2. IDA (International Development Association)
   - Concessional loans and grants to the poorest countries
   - Costa Rica graduated from IDA eligibility in the 1990s
   - IDA lending to Costa Rica is not expected in the modern period

3. IFC (International Finance Corporation)
   - Private-sector arm; investments in private companies and FIs
   - Does not lend to governments; operates separately from IBRD/IDA
   - Financial measures (investment amounts) not directly comparable to IBRD principal

4. MIGA (Multilateral Investment Guarantee Agency)
   - Political risk guarantees for private investors
   - Financial measures (guarantee exposure) not comparable to loan principal

5. ICSID (International Centre for Settlement of Investment Disputes)
   - Arbitration body; no financing operations

Core analysis uses IBRD loans only. IFC and MIGA are separate.
\n")

# ---------------------------------------------------------------------------
# 2.  IDA data availability for Costa Rica
# ---------------------------------------------------------------------------

cat("=== IDA DATA AVAILABILITY ===\n")

# Check IDA via WB API
url_ida <- "https://api.worldbank.org/v2/en/country/CRI/indicator/IDA.COMMIT?format=json&per_page=100&date=1960:2025"
ida_check <- tryCatch({
  resp <- request(url_ida) |> req_timeout(20) |> req_perform()
  fromJSON(resp_body_string(resp))
}, error = function(e) NULL)

# Try IDA terms/statement download (similar to IBRD)
url_ida2 <- "https://financesapp.worldbank.org/summaries/ids/cr/"
cat("WB IDA Statement URL (for reference):\n")
cat("  https://thedocs.worldbank.org/en/doc/statements/IDA-Statement-of-Credits-and-Grants\n\n")

# Direct IDA API check
url_ida_api <- "https://api.worldbank.org/v2/country/CR/indicator/IDA.COMMIT?format=json&per_page=5"
ida_r <- tryCatch({
  resp <- request(url_ida_api) |> req_timeout(20) |> req_perform()
  cat("IDA indicator API:", resp_status(resp), "\n")
  b <- resp_body_string(resp)
  cat("Response:", substr(b, 1, 300), "\n\n")
  b
}, error = function(e) { cat("IDA API error:", conditionMessage(e), "\n\n"); NULL })

cat("IDA ASSESSMENT:\n")
cat("Costa Rica graduated from IDA eligibility circa 1993-1994 as it crossed the IDA\n")
cat("income threshold. The IBRD dataset includes no IDA transactions for Costa Rica.\n")
cat("IDA lending to Costa Rica in the 1980s is possible but appears minimal.\n")
cat("The IDA Statement of Credits and Grants (World Bank Open Data) could be checked,\n")
cat("but this is low priority: Costa Rica is an IBRD country and its pre-1990 financing\n")
cat("is already documented through the IBRD statement.\n\n")

# ---------------------------------------------------------------------------
# 3.  IFC data availability
# ---------------------------------------------------------------------------

cat("=== IFC DATA AVAILABILITY ===\n")

# IFC projects API
url_ifc <- "https://ifcprojectapi.worldbank.org/api/v1.0/projects?country=Costa+Rica&format=json&rows=5"
ifc_r <- tryCatch({
  resp <- request(url_ifc) |> req_timeout(20) |> req_perform()
  cat("IFC API status:", resp_status(resp), "\n")
  b <- resp_body_string(resp)
  cat("IFC API response preview:", substr(b, 1, 500), "\n\n")
  b
}, error = function(e) { cat("IFC API error:", conditionMessage(e), "\n\n"); NULL })

# Try known IFC data portal
cat("IFC data portals:\n")
cat("  1. IFC Projects API: https://disclosures.ifc.org/project-detail\n")
cat("  2. IFC Annual Report datasets: https://www.ifc.org/en/about/annual-report\n")
cat("  3. World Bank Open Data - IFC: https://data.worldbank.org/topic/financial-sector\n\n")

cat("IFC ASSESSMENT:\n")
cat("IFC does operate in Costa Rica (private financial sector, agribusiness, tourism).\n")
cat("IFC investment data IS available from the IFC project portal but:\n")
cat("  - Financial measures differ from IBRD: IFC reports 'committed/invested' amounts,\n")
cat("    which include debt (loans), equity, and quasi-equity instruments.\n")
cat("  - Cannot be added to IBRD principal without methodology documentation.\n")
cat("  - Coverage: IFC data available from ~1950s; downloadable from ifc.org.\n")
cat("  - Recommended: Report IFC separately if included.\n\n")

# ---------------------------------------------------------------------------
# 4.  MIGA data availability
# ---------------------------------------------------------------------------

cat("=== MIGA DATA AVAILABILITY ===\n")

cat("MIGA guarantee data:\n")
cat("  - MIGA publishes project-level data at: https://www.miga.org/projects\n")
cat("  - MIGA guarantees protect private investors, not governments.\n")
cat("  - Financial measure: 'gross exposure' (guarantee face value), not loans.\n")
cat("  - Costa Rica MIGA exposure is likely limited.\n")
cat("  - Cannot be compared with IBRD original principal.\n")
cat("  - Recommended: Mention in scope limitations; do not include in financial totals.\n\n")

# ---------------------------------------------------------------------------
# 5.  US GDP deflator retrieval
# ---------------------------------------------------------------------------

cat("=== US GDP DEFLATOR RETRIEVAL ===\n")

url_defl <- "https://api.worldbank.org/v2/country/US/indicator/NY.GDP.DEFL.ZS?format=json&per_page=200&date=1960:2026"
cat("Fetching US GDP deflator (NY.GDP.DEFL.ZS) from World Bank API...\n")

defl_data <- tryCatch({
  resp <- request(url_defl) |> req_timeout(30) |> req_perform()
  raw <- fromJSON(resp_body_string(resp))
  df <- raw[[2]]
  tibble(
    year  = as.integer(df$date),
    defl  = as.numeric(df$value)
  ) |>
    filter(!is.na(defl)) |>
    arrange(year)
}, error = function(e) {
  cat("Error fetching deflator:", conditionMessage(e), "\n")
  NULL
})

if (!is.null(defl_data)) {
  cat("Deflator coverage:", min(defl_data$year), "to", max(defl_data$year), "\n")
  cat("Missing years:", sum(is.na(defl_data$defl)), "\n")
  cat("Base year (value = 100):", defl_data$year[which.min(abs(defl_data$defl - 100))], "\n\n")
  cat("Recent values:\n")
  print(tail(defl_data[defl_data$year >= 2018, ], 8))

  # Indicator note: NY.GDP.DEFL.ZS base year is 2015 for WB data
  cat("\nNote: WB NY.GDP.DEFL.ZS base year = 2020 (value=100 in 2020, per API response).\n")
  cat("To convert to 2025 USD, use: real_2025 = nominal * (defl_2025 / defl_year)\n")
  cat("2025 deflator:", defl_data$defl[defl_data$year == 2025], "\n")
  cat("1980 deflator:", defl_data$defl[defl_data$year == 1980], "\n")

  # Calculate a CPI comparison note
  cat("\nFor reference, to convert a 1980 USD to 2025 USD:\n")
  ratio <- defl_data$defl[defl_data$year == 2025] / defl_data$defl[defl_data$year == 1980]
  cat("  Multiplier (2025/1980):", round(ratio, 3), "\n")
  cat("  A $100M loan in 1980 = $", round(100 * ratio, 0), "M in 2025 USD\n\n")

  write_csv(defl_data, here("data", "processed", "us_gdp_deflator.csv"))
  cat("Saved: data/processed/us_gdp_deflator.csv\n\n")
}

# ---------------------------------------------------------------------------
# 6.  Deflator methodology note
# ---------------------------------------------------------------------------

cat("=== INFLATION ADJUSTMENT METHODOLOGY ===\n")
cat("
PROPOSED APPROACH: US GDP Deflator (NY.GDP.DEFL.ZS, base 2015 = 100)

WHY GDP DEFLATOR (not CPI):
  - CPI measures consumer price changes affecting US households.
  - GDP deflator measures price changes across ALL US-produced final goods and services,
    providing a broader and more appropriate measure of the purchasing-power value
    of dollar-denominated international transactions.
  - For comparing the real scale of US-dollar lending commitments, the GDP deflator
    is more appropriate than CPI.

INTENDED CALCULATION:
  real_amount_2025 = nominal_amount * (defl_2025 / defl_approval_year)
  where defl_x = NY.GDP.DEFL.ZS value for year x.

SOURCE: World Bank Open Data API, indicator NY.GDP.DEFL.ZS, country US.
  Retrieved from: https://api.worldbank.org/v2/country/US/indicator/NY.GDP.DEFL.ZS
  Coverage: 1960-2025. Annual frequency.

LIMITATIONS:
  1. Neither the US GDP deflator nor CPI measures the local purchasing power of
     lending in Costa Rica. A $100M loan in 1985 bought different quantities of
     Costa Rican infrastructure than $100M in 2020.
  - Source deflator base year is 2020 (value=100 in 2020 per WB API). Only ratios matter for conversion.
  3. 2025 value is preliminary; may be revised in later WB data vintages.
  4. This adjustment converts nominal commitment amounts for cross-period comparison
     only. It does not adjust for exchange-rate changes or local price levels.

IMPLEMENTATION PLAN (Phase 2):
  - Preserve original nominal amounts as primary field.
  - Create real_2025_usd as a derived field with clear documentation.
  - Report both nominal and real amounts in tables.
  - Label all real-amount figures explicitly as '2025 USD (US GDP deflator)'.
  - Note that 2025 deflator is preliminary.
\n")

# ---------------------------------------------------------------------------
# 7.  Disbursement data note
# ---------------------------------------------------------------------------

cat("=== ANNUAL DISBURSEMENT DATA ===\n")
cat("
The IBRD Statement of Loans and Guarantees provides CUMULATIVE snapshot balances as
of 09/30/2026. Specifically:
  - disbursed_amount_us = total cumulative drawdowns since loan inception
  - This CANNOT be assigned to the approval year as an annual flow.

To obtain annual disbursement flows (year-by-year drawdowns), one would need:
  - World Bank IDS (International Debt Statistics) transaction-level data
  - Or IMF BOP/Balance of Payments data for CR external debt disbursements
  - Or the WB financial portal transaction-level downloads

For Phase 2, the primary financial measure will be original_principal_amount (commitment
at approval), which correctly maps to the approval year. Annual disbursement trends
are a potential extension but require a separate transaction-level dataset.
\n")

# ---------------------------------------------------------------------------
# 8.  Save scope assessment
# ---------------------------------------------------------------------------

scope_df <- tribble(
  ~institution, ~role, ~cr_relevance, ~data_available, ~comparability, ~recommendation,
  "IBRD", "Middle-income country loans",
    "Primary lender to Costa Rica",
    "Yes — IBRD Statement of Loans (9,538 rows global; 71 CR loans)",
    "Definitive for this analysis",
    "Core dataset — use original_principal_amount",
  "IDA", "Concessional loans/grants to poorest countries",
    "Minimal — Costa Rica graduated ~1993-1994",
    "IDA Statement of Credits available; likely negligible for CR",
    "Concessional terms differ from IBRD",
    "Check IDA statement; report separately if any CR records found",
  "IFC", "Private-sector investments",
    "Active in CR private sector (banks, agribusiness, tourism)",
    "IFC project database available at ifc.org; not currently downloaded",
    "NOT comparable to IBRD loan principal",
    "Scope note only; do not include in IBRD financial totals",
  "MIGA", "Political risk guarantees",
    "Likely limited exposure in CR",
    "MIGA project portal at miga.org",
    "NOT comparable (exposure, not loans)",
    "Scope note only",
  "ICSID", "Investment dispute arbitration",
    "No direct financing",
    "N/A",
    "N/A",
    "Exclude"
)

write_csv(scope_df, file.path(der_dir, "wbg_scope_assessment.csv"))
cat("Saved: data/derived/wbg_scope_assessment.csv\n")
cat("Institutional scope and deflator assessment complete.\n")
