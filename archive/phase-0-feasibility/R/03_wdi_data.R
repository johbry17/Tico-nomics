# 03_wdi_data.R
# Retrieve and clean World Development Indicators for Costa Rica
# Indicators: GDP per capita, Gini, life expectancy, adult literacy

library(here)
library(dplyr)
library(tidyr)
library(readr)
library(wbstats)
library(lubridate)

source(here("R", "functions.R"))

# ---------------------------------------------------------------------------
# 1.  Retrieve WDI data
# ---------------------------------------------------------------------------

wdi_indicators <- c(
  "NY.GDP.PCAP.CD",  # GDP per capita (current US$)
  "SI.POV.GINI",     # Gini index
  "SP.DYN.LE00.IN",  # Life expectancy at birth
  "SE.ADT.LITR.ZS"   # Adult literacy rate (%)
)

cat("Retrieving WDI data from World Bank API...\n")
wdi_raw <- wbstats::wb_data(
  wdi_indicators,
  country   = "CR",
  start_date = 1960,
  end_date   = 2024
)

cat("Retrieved:", nrow(wdi_raw), "rows\n")
cat("Columns:", paste(names(wdi_raw), collapse = ", "), "\n\n")

# ---------------------------------------------------------------------------
# 2.  Clean and reshape
# ---------------------------------------------------------------------------

wdi_clean <- wdi_raw %>%
  rename(
    year            = date,
    gdp_per_capita  = NY.GDP.PCAP.CD,
    gini            = SI.POV.GINI,
    life_expectancy = SP.DYN.LE00.IN,
    adult_literacy  = SE.ADT.LITR.ZS
  ) %>%
  select(country, iso3c, year,
         gdp_per_capita, gini, life_expectancy, adult_literacy) %>%
  arrange(year)

# ---------------------------------------------------------------------------
# 3.  Coverage audit
# ---------------------------------------------------------------------------

cat("=== WDI COVERAGE AUDIT ===\n\n")

indicators_meta <- list(
  gdp_per_capita  = list(label = "GDP per capita (current US$)",  col = "gdp_per_capita"),
  gini            = list(label = "Gini index",                    col = "gini"),
  life_expectancy = list(label = "Life expectancy at birth",      col = "life_expectancy"),
  adult_literacy  = list(label = "Adult literacy rate (%)",       col = "adult_literacy")
)

coverage_table <- purrr::map_dfr(indicators_meta, function(ind) {
  vals       <- wdi_clean[[ind$col]]
  yrs        <- wdi_clean$year[!is.na(vals)]
  n_obs      <- length(yrs)
  n_total    <- nrow(wdi_clean)
  yr_min     <- if (n_obs > 0) min(yrs) else NA_integer_
  yr_max     <- if (n_obs > 0) max(yrs) else NA_integer_
  coverage_pct <- round(100 * n_obs / n_total, 0)
  # Check for gaps in the 1990-2024 window
  yrs_scope  <- yrs[yrs >= 1990 & yrs <= 2024]
  n_scope    <- length(yrs_scope)
  gaps       <- setdiff(1990:2024, yrs_scope)
  tibble(
    indicator     = ind$label,
    n_obs         = n_obs,
    coverage_pct  = coverage_pct,
    year_min      = yr_min,
    year_max      = yr_max,
    n_in_scope    = n_scope,
    n_gaps_scope  = length(gaps),
    feasibility   = dplyr::case_when(
      n_scope >= 25 ~ "Good",
      n_scope >= 10 ~ "Usable with limitations",
      n_scope >= 3  ~ "Poor",
      TRUE          ~ "Not usable"
    )
  )
})

print(coverage_table)

# Detailed gaps for sparse indicators
for (ind in indicators_meta) {
  vals <- wdi_clean[[ind$col]]
  yrs  <- wdi_clean$year[!is.na(vals) & wdi_clean$year >= 1990 & wdi_clean$year <= 2024]
  if (length(yrs) < 30) {
    cat("\n", ind$label, "— years with data (1990–2024):\n")
    cat(" ", paste(sort(yrs), collapse = ", "), "\n")
  }
}

# ---------------------------------------------------------------------------
# 4.  In-scope subset (1990–2024)
# ---------------------------------------------------------------------------

wdi_scope <- wdi_clean %>%
  filter(year >= 1990, year <= 2024)

cat("\n\nWDI in-scope (1990–2024):", nrow(wdi_scope), "rows\n")

# ---------------------------------------------------------------------------
# 5.  Save
# ---------------------------------------------------------------------------

write_csv(wdi_clean, here("data", "processed", "wdi_clean.csv"))
write_csv(wdi_scope, here("data", "processed", "wdi_scope.csv"))
write_csv(coverage_table, here("data", "derived", "wdi_coverage.csv"))

cat("\nSaved:\n")
cat("  data/processed/wdi_clean.csv\n")
cat("  data/processed/wdi_scope.csv\n")
cat("  data/derived/wdi_coverage.csv\n")
cat("\nWDI retrieval complete.\n")
