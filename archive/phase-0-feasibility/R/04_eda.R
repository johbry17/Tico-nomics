# 04_eda.R
# Exploratory Data Analysis — World Bank lending in Costa Rica, 1990–2024
# Produces derived tables used in visualizations and reports.

library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(forcats)
library(readr)
library(lubridate)
library(tidytext)

source(here("R", "functions.R"))

# ---------------------------------------------------------------------------
# 0.  Load cleaned data
# ---------------------------------------------------------------------------

projects     <- read_csv(here("data", "processed", "projects_inscope.csv"),
                          show_col_types = FALSE)
sector_long  <- read_csv(here("data", "processed", "sector_long.csv"),
                          show_col_types = FALSE)
annual_sum   <- read_csv(here("data", "derived", "annual_summary.csv"),
                          show_col_types = FALSE)
wdi          <- read_csv(here("data", "processed", "wdi_scope.csv"),
                          show_col_types = FALSE)

cat("Loaded:\n")
cat("  projects (in-scope):", nrow(projects), "\n")
cat("  sector_long:        ", nrow(sector_long), "\n")
cat("  annual_summary:     ", nrow(annual_sum), "\n")
cat("  wdi (1990–2024):   ", nrow(wdi), "\n\n")

# ---------------------------------------------------------------------------
# A.  Portfolio overview
# ---------------------------------------------------------------------------

cat("=== A. PORTFOLIO OVERVIEW ===\n\n")

# Total portfolio stats
total_commitments <- sum(projects$totalamt[projects$has_financing == TRUE], na.rm = TRUE)
cat("Total IBRD commitments (1990–2024, nominal USD):", fmt_billions(total_commitments), "\n")
cat("Number of projects (all):", nrow(projects), "\n")
cat("Number with positive financing:", sum(projects$has_financing, na.rm = TRUE), "\n")

# Projects by period
by_period <- projects %>%
  filter(!is.na(period), period != "Pre-1990") %>%
  group_by(period) %>%
  summarise(
    n_projects    = n(),
    total_usd     = sum(totalamt[has_financing == TRUE], na.rm = TRUE),
    mean_usd      = mean(totalamt[has_financing == TRUE], na.rm = TRUE),
    median_usd    = median(totalamt[has_financing == TRUE], na.rm = TRUE),
    n_dpl         = sum(instrument_type == "Development Policy Loan"),
    n_ipf         = sum(instrument_type == "Investment Project Financing"),
    .groups = "drop"
  ) %>%
  mutate(
    pct_of_total = round(100 * total_usd / sum(total_usd), 1)
  )

cat("\nCommitments by period:\n")
print(by_period)

# Instrument mix
instrument_mix <- projects %>%
  filter(!is.na(instrument_type), instrument_type != "Unknown") %>%
  group_by(instrument_type) %>%
  summarise(
    n         = n(),
    total_usd = sum(totalamt[has_financing == TRUE], na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  mutate(pct_n   = round(100 * n / sum(n), 1),
         pct_usd = round(100 * total_usd / sum(total_usd), 1))

cat("\nInstrument mix:\n")
print(instrument_mix)

# Largest projects (top 10)
top_projects <- projects %>%
  filter(has_financing == TRUE) %>%
  arrange(desc(totalamt)) %>%
  select(id, project_name, approval_year, totalamt, instrument_type, status) %>%
  head(10) %>%
  mutate(totalamt_fmt = fmt_millions(totalamt))

cat("\nTop 10 projects by commitment:\n")
print(top_projects, n = 10)

# ---------------------------------------------------------------------------
# B.  Sectoral analysis
# ---------------------------------------------------------------------------

cat("\n=== B. SECTORAL ANALYSIS ===\n\n")

# Most common primary sectors
primary_sectors <- projects %>%
  filter(!is.na(sector1_name), has_financing == TRUE) %>%
  group_by(sector1_name) %>%
  summarise(
    n          = n(),
    total_usd  = sum(totalamt, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  mutate(
    sector_group = group_sector(sector1_name),
    pct_usd      = round(100 * total_usd / sum(total_usd), 1)
  ) %>%
  arrange(desc(total_usd))

cat("Primary sectors by total financing:\n")
print(primary_sectors, n = 20)

# Sector group summary using allocated amounts where available
sector_group_summary <- sector_long %>%
  group_by(sector_group, period) %>%
  summarise(
    n_allocations  = n(),
    # Prefer allocated amounts; fall back to counting projects
    total_alloc    = sum(alloc_amt, na.rm = TRUE),
    has_alloc_pct  = mean(!is.na(alloc_amt)),
    .groups        = "drop"
  )

# Also: sector group summary without period (totals)
sector_group_total <- sector_long %>%
  group_by(sector_group) %>%
  summarise(
    n_allocations  = n(),
    n_projects     = n_distinct(id),
    total_alloc    = sum(alloc_amt, na.rm = TRUE),
    has_alloc_pct  = round(mean(!is.na(alloc_amt)) * 100, 0),
    .groups        = "drop"
  ) %>%
  arrange(desc(total_alloc))

cat("\nSector group totals (allocated financing where available):\n")
print(sector_group_total, n = 20)

# For projects without pct allocation: count by sector_group as appearance only
sector_presence <- sector_long %>%
  group_by(id) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  count(sector_group, period, name = "n_projects") %>%
  arrange(period, desc(n_projects))

# Sector over time: projects per sector_group per year
sector_by_year <- sector_long %>%
  filter(!is.na(approval_year)) %>%
  group_by(approval_year, sector_group) %>%
  summarise(
    n             = n(),
    total_alloc   = sum(alloc_amt, na.rm = TRUE),
    .groups       = "drop"
  )

# Sector by period
sector_by_period <- sector_long %>%
  filter(!is.na(period), period != "Pre-1990") %>%
  group_by(period, sector_group) %>%
  summarise(
    n_allocs     = n(),
    total_alloc  = sum(alloc_amt, na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  group_by(period) %>%
  mutate(pct_of_period = round(100 * total_alloc / sum(total_alloc), 1)) %>%
  ungroup() %>%
  arrange(period, desc(total_alloc))

cat("\nSector allocations by period (top sectors):\n")
sector_by_period %>%
  filter(total_alloc > 0) %>%
  print(n = 40)

# ---------------------------------------------------------------------------
# C.  Text analysis of project abstracts
# ---------------------------------------------------------------------------

cat("\n=== C. TEXT ANALYSIS ===\n\n")

abstracts_df <- projects %>%
  filter(!is.na(project_abstract), nchar(trimws(project_abstract)) > 50) %>%
  select(id, project_name, approval_year, period, project_abstract) %>%
  mutate(period = factor(period, levels = c("1990–1999","2000–2009","2010–2019","2020–2024")))

cat("Projects with usable abstracts:", nrow(abstracts_df), "\n")

# Stop words
data("stop_words", package = "tidytext")
custom_stops <- tibble(word = c(
  "project", "costa", "rica", "bank", "world", "program", "programme",
  "component", "support", "including", "include", "provide", "provided",
  "improve", "percent", "million", "billion", "financing",
  "loan", "lending", "ibrd", "ida", "will", "also", "government",
  "ministry", "1", "2", "3", "4", "5", "i", "ii", "iii",
  "objective", "objectives", "subcomponent", "subcomponents"
))

# Tokenise
word_counts <- abstracts_df %>%
  unnest_tokens(word, project_abstract) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stops, by = "word") %>%
  filter(str_detect(word, "^[a-z]"), nchar(word) > 2) %>%
  count(word, sort = TRUE)

cat("\nTop 30 words across all abstracts:\n")
print(word_counts %>% head(30))

# Word counts by period
word_by_period <- abstracts_df %>%
  unnest_tokens(word, project_abstract) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(custom_stops, by = "word") %>%
  filter(str_detect(word, "^[a-z]"), nchar(word) > 2, !is.na(period)) %>%
  count(period, word, sort = TRUE) %>%
  group_by(period) %>%
  slice_max(n, n = 15) %>%
  ungroup()

# Theme keywords: test presence across abstracts over time
# (simple, transparent approach)
theme_keywords <- list(
  Infrastructure        = c("road", "highway", "transport", "infrastructure", "port",
                            "bridge", "power", "electricity", "grid", "energy"),
  Environment_Climate   = c("environment", "environmental", "climate", "biodiversity",
                            "forest", "conservation", "emissions", "carbon", "nature",
                            "ecosystem", "natural", "sustainable", "sustainability"),
  Social_Development    = c("social", "poverty", "health", "education", "welfare",
                            "vulnerable", "protection", "safety", "inequality"),
  Governance_Fiscal     = c("fiscal", "governance", "institutional", "reform",
                            "management", "tax", "revenue", "accountability",
                            "transparency", "decentralization", "municipal"),
  Competitiveness_Trade = c("competitiveness", "trade", "productivity", "private",
                            "investment", "market", "economic", "growth",
                            "innovation", "technology"),
  Disaster_Risk         = c("disaster", "risk", "resilience", "resilient",
                            "emergency", "catastrophe", "flood", "earthquake",
                            "pandemic", "covid")
)

theme_presence <- purrr::map_dfr(names(theme_keywords), function(theme) {
  kw <- theme_keywords[[theme]]
  abstracts_df %>%
    mutate(
      has_theme = str_detect(tolower(project_abstract),
                             paste(kw, collapse = "|")),
      theme     = theme
    ) %>%
    group_by(period, theme) %>%
    summarise(
      n_projects    = n(),
      n_with_theme  = sum(has_theme),
      pct_with_theme = round(100 * n_with_theme / n_projects, 0),
      .groups       = "drop"
    )
}) %>%
  arrange(period, desc(pct_with_theme))

cat("\nTheme presence by period (% of projects mentioning theme):\n")
print(theme_presence, n = 40)

# ---------------------------------------------------------------------------
# D.  Economic context (WDI)
# ---------------------------------------------------------------------------

cat("\n=== D. ECONOMIC CONTEXT ===\n\n")

cat("GDP per capita range:", round(min(wdi$gdp_per_capita, na.rm = TRUE)), "–",
    round(max(wdi$gdp_per_capita, na.rm = TRUE)), "USD\n")
cat("Life expectancy range:", round(min(wdi$life_expectancy, na.rm = TRUE), 1), "–",
    round(max(wdi$life_expectancy, na.rm = TRUE), 1), "years\n")

gini_obs <- wdi %>% filter(!is.na(gini))
cat("Gini observations:", nrow(gini_obs), "| Years:",
    paste(gini_obs$year, collapse = ", "), "\n")

lit_obs <- wdi %>% filter(!is.na(adult_literacy))
cat("Literacy observations:", nrow(lit_obs), "| Years:",
    paste(lit_obs$year, collapse = ", "), "\n")

# ---------------------------------------------------------------------------
# E.  Save derived tables
# ---------------------------------------------------------------------------

write_csv(by_period,          here("data", "derived", "portfolio_by_period.csv"))
write_csv(instrument_mix,     here("data", "derived", "instrument_mix.csv"))
write_csv(top_projects,       here("data", "derived", "top_projects.csv"))
write_csv(primary_sectors,    here("data", "derived", "primary_sectors.csv"))
write_csv(sector_group_total, here("data", "derived", "sector_group_total.csv"))
write_csv(sector_by_period,   here("data", "derived", "sector_by_period.csv"))
write_csv(sector_by_year,     here("data", "derived", "sector_by_year.csv"))
write_csv(word_counts,        here("data", "derived", "word_counts.csv"))
write_csv(word_by_period,     here("data", "derived", "word_by_period.csv"))
write_csv(theme_presence,     here("data", "derived", "theme_presence.csv"))

cat("\nSaved all EDA derived tables to data/derived/\n")
cat("EDA complete.\n")
