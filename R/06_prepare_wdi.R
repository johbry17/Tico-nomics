# 06_prepare_wdi.R
# Retrieve WDI contextual indicators and US GDP deflator.
# Saves: data/processed/wdi_clean.csv, data/processed/us_gdp_deflator.csv

source(here::here("R", "00_setup.R"))

cat("\n========== WDI AND DEFLATOR PREPARATION ==========\n\n")

# ---------------------------------------------------------------------------
# 1.  US GDP deflator (NY.GDP.DEFL.ZS)
# ---------------------------------------------------------------------------

cat("--- US GDP Deflator ---\n")

defl_url <- "https://api.worldbank.org/v2/country/US/indicator/NY.GDP.DEFL.ZS?format=json&per_page=200&date=1960:2026"

deflator <- tryCatch({
  resp <- request(defl_url) |> req_timeout(30) |> req_perform()
  raw  <- fromJSON(resp_body_string(resp))
  tibble(year = as.integer(raw[[2]]$date), defl = as.numeric(raw[[2]]$value)) |>
    filter(!is.na(defl)) |>
    arrange(year)
}, error = function(e) {
  cat("WARNING: Could not retrieve deflator from API:", conditionMessage(e), "\n")
  cat("Trying cached file if available...\n")
  if (file.exists(file.path(PROC, "us_gdp_deflator.csv")))
    read_csv(file.path(PROC, "us_gdp_deflator.csv"), show_col_types = FALSE)
  else
    stop("No deflator data available. Check internet connection.")
})

cat("Coverage:", min(deflator$year), "-", max(deflator$year), "\n")
cat("Base year (value=100):", deflator$year[which.min(abs(deflator$defl - 100))], "\n")
cat("Missing years:", sum(is.na(deflator$defl)), "\n")

defl_2025 <- deflator$defl[deflator$year == 2025]
defl_1980 <- deflator$defl[deflator$year == 1980]
cat("2025 deflator:", round(defl_2025, 2), "| 1980 deflator:", round(defl_1980, 2), "\n")
cat("1980->2025 multiplier:", round(defl_2025/defl_1980, 3),
    "| $100M in 1980 =", round(100 * defl_2025/defl_1980, 0), "M in 2025 USD\n")

# Validate: Phase 1 check was ~$328M; should still be approximately this
if (abs(100 * defl_2025/defl_1980 - 328) > 30) {
  warning("Deflator validation: unexpected 1980->2025 multiplier. Check NY.GDP.DEFL.ZS data.")
} else {
  cat("Deflator validation: 1980->2025 multiplier within expected range. OK.\n")
}

write_csv(deflator, file.path(PROC, "us_gdp_deflator.csv"))
cat("Saved: data/processed/us_gdp_deflator.csv\n\n")

# ---------------------------------------------------------------------------
# 2.  WDI contextual indicators for Costa Rica
# ---------------------------------------------------------------------------

cat("--- WDI Contextual Indicators ---\n")

wdi_indicators <- c(
  "NY.GDP.PCAP.CD",    # GDP per capita (current USD)
  "NY.GDP.MKTP.KD.ZG", # GDP growth (%)
  "SI.POV.GINI",       # Gini index
  "SP.DYN.LE00.IN",    # Life expectancy
  "BX.KLT.DINV.WD.GD.ZS", # FDI (% GDP)
  "GC.DOD.TOTL.GD.ZS"  # Central government debt (% GDP)
)

wdi_url <- function(indicator) {
  sprintf("https://api.worldbank.org/v2/country/CR/indicator/%s?format=json&per_page=200&date=1960:2026",
          indicator)
}

get_wdi <- function(indicator) {
  tryCatch({
    resp <- request(wdi_url(indicator)) |> req_timeout(20) |> req_perform()
    raw  <- fromJSON(resp_body_string(resp))
    if (length(raw) < 2 || !is.data.frame(raw[[2]])) return(NULL)
    tibble(
      year      = as.integer(raw[[2]]$date),
      value     = as.numeric(raw[[2]]$value),
      indicator = indicator
    ) |> filter(!is.na(value))
  }, error = function(e) {
    cat("Warning: could not retrieve", indicator, ":", conditionMessage(e), "\n")
    NULL
  })
}

cat("Retrieving WDI data for Costa Rica...\n")
wdi_list <- lapply(wdi_indicators, function(ind) {
  cat("  Fetching", ind, "...")
  d <- get_wdi(ind)
  if (!is.null(d)) cat(" OK (", nrow(d), "obs )\n") else cat(" FAILED\n")
  Sys.sleep(0.3)
  d
})

wdi_long <- bind_rows(wdi_list)

# Pivot to wide format
wdi_wide <- wdi_long |>
  pivot_wider(names_from = indicator, values_from = value) |>
  arrange(year) |>
  filter(year >= 1970, year <= 2025)

# Rename for clarity
wdi_clean <- wdi_wide |>
  rename(
    gdp_per_capita_usd     = any_of("NY.GDP.PCAP.CD"),
    gdp_growth_pct         = any_of("NY.GDP.MKTP.KD.ZG"),
    gini                   = any_of("SI.POV.GINI"),
    life_expectancy_years  = any_of("SP.DYN.LE00.IN"),
    fdi_pct_gdp            = any_of("BX.KLT.DINV.WD.GD.ZS"),
    govt_debt_pct_gdp      = any_of("GC.DOD.TOTL.GD.ZS")
  )

cat("\nWDI coverage summary:\n")
for (col in names(wdi_clean)[names(wdi_clean) != "year"]) {
  obs   <- sum(!is.na(wdi_clean[[col]]))
  yrs   <- wdi_clean$year[!is.na(wdi_clean[[col]])]
  scope <- sum(!is.na(wdi_clean[[col]]) & wdi_clean$year >= 1980 & wdi_clean$year <= 2025)
  cat(sprintf("  %-25s : %d obs (1980-2025 scope: %d)\n", col, obs, scope))
}

write_csv(wdi_clean, file.path(PROC, "wdi_clean.csv"))
cat("\nSaved: data/processed/wdi_clean.csv\n")
cat("WDI preparation complete.\n")
