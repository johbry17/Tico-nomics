# 07_analysis.R
# Generate analytical summaries for the final publication.
# Reads from processed/ and derived/; writes additional tables to derived/.

source(here::here("R", "00_setup.R"))

cat("\n========== ANALYSIS ==========\n\n")

# ---------------------------------------------------------------------------
# 0.  Load inputs
# ---------------------------------------------------------------------------

ibrd_projects <- read_csv(
  file.path(PROC, "ibrd_projects.csv"),
  show_col_types = FALSE
)
ibrd_loans <- read_csv(
  file.path(PROC, "ibrd_loans_clean.csv"),
  show_col_types = FALSE
)
ibrd_annual <- read_csv(
  file.path(DER, "ibrd_annual.csv"),
  show_col_types = FALSE
)
ibrd_period <- read_csv(
  file.path(DER, "ibrd_by_period.csv"),
  show_col_types = FALSE
)
ifc_projects <- read_csv(
  file.path(PROC, "ifc_projects.csv"),
  show_col_types = FALSE
)
miga_raw_all <- read_csv(
  file.path(PROC, "miga_records_raw.csv"),
  show_col_types = FALSE
)
miga_projects <- read_csv(
  file.path(PROC, "miga_projects.csv"),
  show_col_types = FALSE
)
projects_univ <- read_csv(
  file.path(PROC, "projects_universe.csv"),
  show_col_types = FALSE
)
wdi <- read_csv(file.path(PROC, "wdi_clean.csv"), show_col_types = FALSE)
deflator <- read_csv(
  file.path(PROC, "us_gdp_deflator.csv"),
  show_col_types = FALSE
)

# In-scope IBRD projects (1980-2025 with loan records)
ibrd_scope <- ibrd_projects |>
  filter(!is.na(approval_year), approval_year >= 1980, approval_year <= 2025)

cat("IBRD in-scope projects:", nrow(ibrd_scope), "\n")
cat(
  "IFC in-scope investments:",
  nrow(ifc_projects |> filter(is_investment, in_scope, !is_pipeline)),
  "\n"
)
cat("MIGA deduplicated projects:", nrow(miga_projects), "\n\n")

# ---------------------------------------------------------------------------
# A.  IBRD portfolio overview
# ---------------------------------------------------------------------------

cat("=== A. IBRD PORTFOLIO OVERVIEW ===\n")

total_nominal <- sum(ibrd_scope$total_principal)
total_real <- sum(ibrd_scope$real_principal_2025, na.rm = TRUE)
total_net <- sum(ibrd_scope$net_principal)
total_cancelled <- sum(ibrd_scope$total_cancelled)
pct_2020s <- 100 *
  sum(ibrd_scope$total_principal[ibrd_scope$period == "2020–2025"]) /
  total_nominal

cat(
  "Total original principal (nominal, 1980-2025):",
  fmt_billions(total_nominal),
  "\n"
)
cat("Total real principal (2025 USD):", fmt_billions(total_real), "\n")
cat("Total net principal (orig - cancelled):", fmt_billions(total_net), "\n")
cat("Total cancelled:", fmt_millions(total_cancelled), "\n")
cat(
  "Cancellation rate:",
  round(100 * total_cancelled / total_nominal, 1),
  "%\n"
)
cat("Share in 2020-2025:", round(pct_2020s, 1), "%\n\n")

# Top 10 projects
top10 <- ibrd_scope |>
  arrange(desc(total_principal)) |>
  head(10) |>
  select(
    project_id,
    project_name,
    approval_year,
    total_principal,
    real_principal_2025,
    net_principal,
    total_cancelled,
    instrument_group,
    loan_status_summary
  )

cat("Top 10 projects (nominal):\n")
print(
  top10 |>
    mutate(
      total_principal = fmt_millions(total_principal),
      real_principal_2025 = fmt_millions(real_principal_2025),
      net_principal = fmt_millions(net_principal)
    ) |>
    as.data.frame()
)

write_csv(top10, file.path(DER, "ibrd_top10.csv"))

# ---------------------------------------------------------------------------
# B.  Instrument mix over time
# ---------------------------------------------------------------------------

cat("\n=== B. INSTRUMENT MIX ===\n")

instr_summary <- ibrd_scope |>
  group_by(period, instrument_group) |>
  summarise(
    n = n(),
    total_principal = sum(total_principal),
    real_principal = sum(real_principal_2025, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(period) |>
  mutate(
    pct_nominal = round(100 * total_principal / sum(total_principal), 1)
  ) |>
  ungroup()

cat("Instrument mix by period:\n")
print(instr_summary)

overall_instr <- ibrd_scope |>
  group_by(instrument_group) |>
  summarise(
    n = n(),
    total_principal = sum(total_principal),
    real_principal = sum(real_principal_2025, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    pct_n = round(100 * n / sum(n), 1),
    pct_nominal = round(100 * total_principal / sum(total_principal), 1)
  )
cat("\nOverall instrument mix:\n")
print(overall_instr)

write_csv(instr_summary, file.path(DER, "instrument_by_period.csv"))

# ---------------------------------------------------------------------------
# C.  Sector analysis (from Projects List sector fields)
# ---------------------------------------------------------------------------

cat("\n=== C. SECTOR ANALYSIS ===\n")

# P178320 (COVID-19 Vaccines, Terminated): The API reports $120M project amount
# and sector allocations summing to 100%, but the IBRD statement records
# original_principal = 0 (the loan was never signed). Using P178320's
# API-reported total_principal ($120M is what the API shows in totalamt, but
# ibrd_projects.total_principal = 0 from the financial statement) would
# overstate committed financing. We exclude it from dollar-allocation totals
# but retain it in project counts where it has sector names.
p178320_excluded_from_dollar_alloc <- TRUE
cat(
  "NOTE: P178320 (COVID Vaccines, Terminated) excluded from sector dollar allocations.\n",
  "  IBRD statement records original_principal = $0 (loan never became effective).\n",
  "  API sector metadata retained for project-count purposes only.\n"
)

# Build sector data: use total_principal from ibrd_projects (financial statement source)
# P178320 has total_principal = 0, so alloc_principal will be 0 even if pcts exist.
sector_data <- ibrd_scope |>
  select(
    project_id,
    project_name,
    approval_year,
    period,
    total_principal,
    real_principal_2025,
    sector1_xl,
    sector2_xl,
    sector3_xl
  )

# Get sector percentages from API data for projects in API
api_raw_main <- jsonlite::fromJSON(
  file.path(RAW, "world_bank_costa_rica.json"),
  simplifyVector = FALSE
)

sector_pcts <- purrr::map_dfr(names(api_raw_main$projects), function(id) {
  p <- api_raw_main$projects[[id]]
  get_sec <- function(slot) {
    s <- p[[slot]]
    if (is.null(s)) {
      return(list(name = NA_character_, pct = NA_real_))
    }
    nm <- if (is.null(s$Name) || s$Name == "") NA_character_ else s$Name
    pct <- if (!is.null(s$Percent) && s$Percent > 0) {
      as.numeric(s$Percent)
    } else {
      NA_real_
    }
    list(name = nm, pct = pct)
  }
  s1 <- get_sec("sector1")
  s2 <- get_sec("sector2")
  s3 <- get_sec("sector3")
  tibble(
    project_id = id,
    sec1_name = s1$name,
    sec1_pct = s1$pct,
    sec2_name = s2$name,
    sec2_pct = s2$pct,
    sec3_name = s3$name,
    sec3_pct = s3$pct
  )
})

# Merge sector pcts with in-scope projects
sector_full <- ibrd_scope |>
  left_join(sector_pcts, by = "project_id") |>
  # Use API sector names where available, else fall back to xlsx
  mutate(
    sector1_name = dplyr::coalesce(sec1_name, sector1_xl),
    sector2_name = dplyr::coalesce(sec2_name, sector2_xl),
    sector3_name = dplyr::coalesce(sec3_name, sector3_xl)
  )

# Sector long table: use bind_rows with three separate pivots (simplest approach)
make_sector_rows <- function(df, slot_num, name_col, pct_col) {
  df |>
    filter(!is.na(.data[[name_col]])) |>
    transmute(
      project_id,
      project_name,
      approval_year,
      period,
      total_principal,
      real_principal_2025,
      slot = slot_num,
      sector_name = .data[[name_col]],
      sector_pct = suppressWarnings(as.numeric(.data[[pct_col]]))
    )
}

sector_long2 <- bind_rows(
  make_sector_rows(sector_full, 1L, "sector1_name", "sec1_pct"),
  make_sector_rows(sector_full, 2L, "sector2_name", "sec2_pct"),
  make_sector_rows(sector_full, 3L, "sector3_name", "sec3_pct")
) |>
  mutate(
    sector_group = group_sector(sector_name),
    alloc_principal = if_else(
      !is.na(sector_pct) & sector_pct > 0,
      total_principal * sector_pct / 100,
      NA_real_
    ),
    alloc_real_2025 = if_else(
      !is.na(sector_pct) & sector_pct > 0,
      real_principal_2025 * sector_pct / 100,
      NA_real_
    )
  ) |>
  arrange(approval_year, project_id, slot)

cat("Sector long table:", nrow(sector_long2), "rows\n")
n_sec_proj <- n_distinct(sector_long2$project_id)
n_insco <- nrow(ibrd_scope)
cat("Projects with any sector metadata:", n_sec_proj, "/", n_insco, "\n")
cat("Projects WITHOUT sector data:     ", n_insco - n_sec_proj, "\n")

# Coverage: how much principal is represented by projects with sector data?
principal_with_sector <- sum(
  ibrd_scope$total_principal[
    ibrd_scope$project_id %in% sector_long2$project_id
  ],
  na.rm = TRUE
)
principal_total <- sum(ibrd_scope$total_principal, na.rm = TRUE)
cat(sprintf(
  "Principal covered by sector data: %s of %s (%.0f%%)\n",
  fmt_billions(principal_with_sector),
  fmt_billions(principal_total),
  100 * principal_with_sector / principal_total
))

# Of those, how many have percentages that allow dollar allocation?
alloc_proj <- sector_long2 |>
  filter(!is.na(alloc_principal)) |>
  pull(project_id) |>
  unique()
cat("Projects with reliable pct-based alloc:", length(alloc_proj), "\n")
cat("NOTE: Sector dollar allocations represent a partial, non-representative\n")
cat(
  "  share of the portfolio. The 2020-2025 DPL series lacks sector pct data.\n"
)

# Sector group totals (using allocated amounts where available, else count)
sec_totals <- sector_long2 |>
  group_by(sector_group) |>
  summarise(
    n_allocations = n(),
    n_projects = n_distinct(project_id),
    alloc_principal = sum(alloc_principal, na.rm = TRUE),
    alloc_real = sum(alloc_real_2025, na.rm = TRUE),
    pct_with_alloc = round(mean(!is.na(alloc_principal)) * 100, 0),
    .groups = "drop"
  ) |>
  arrange(desc(alloc_principal))

cat("\nSector group totals:\n")
print(sec_totals)

# Sector by period
sec_by_period <- sector_long2 |>
  filter(!is.na(period), period != "Pre-1980") |>
  group_by(period, sector_group) |>
  summarise(
    alloc_principal = sum(alloc_principal, na.rm = TRUE),
    alloc_real = sum(alloc_real_2025, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  group_by(period) |>
  mutate(
    pct = round(
      100 *
        alloc_principal /
        sum(alloc_principal) *
        if_else(sum(alloc_principal) > 0, 1, 0),
      1
    )
  ) |>
  ungroup() |>
  arrange(period, desc(alloc_principal))

write_csv(sector_long2, file.path(DER, "sector_long.csv"))
write_csv(sec_totals, file.path(DER, "sector_totals.csv"))
write_csv(sec_by_period, file.path(DER, "sector_by_period.csv"))
cat("Sector tables saved.\n")

# ---------------------------------------------------------------------------
# D.  Text analysis: project abstracts
# ---------------------------------------------------------------------------

cat("\n=== D. ABSTRACT THEMES ===\n")

# Get abstracts from filtered API file
api_filt <- jsonlite::fromJSON(
  file.path(RAW, "world_bank_costa_rica_filtered.json"),
  simplifyVector = FALSE
)
abstracts_raw <- purrr::map_dfr(names(api_filt$projects), function(id) {
  p <- api_filt$projects[[id]]
  ab <- p[["project_abstract"]]
  txt <- if (!is.null(ab)) {
    coalesce(ab[["cdata!"]], ab[["#cdata-section"]], as.character(ab[[1]]))
  } else {
    NA_character_
  }
  txt <- if (!is.null(txt) && !is.na(txt) && nchar(trimws(txt)) > 0) {
    trimws(txt)
  } else {
    NA_character_
  }
  tibble(project_id = id, abstract = txt)
})

# Join with in-scope IBRD projects
abstracts_scope <- ibrd_scope |>
  left_join(abstracts_raw, by = "project_id") |>
  filter(!is.na(abstract), nchar(abstract) > 100)

cat("In-scope projects with abstracts:", nrow(abstracts_scope), "\n")

# Keyword theme presence
theme_keywords <- list(
  Infrastructure = c(
    "road",
    "highway",
    "transport",
    "infrastructure",
    "port",
    "bridge",
    "power",
    "electricity",
    "grid",
    "energy"
  ),
  Environment_Climate = c(
    "environment",
    "environmental",
    "climate",
    "biodiversity",
    "forest",
    "conservation",
    "emissions",
    "carbon",
    "nature",
    "ecosystem",
    "sustainable",
    "decarbonization",
    "decarbon"
  ),
  Social_Development = c(
    "social",
    "poverty",
    "health",
    "education",
    "welfare",
    "vulnerable",
    "protection",
    "safety",
    "inequality"
  ),
  Governance_Fiscal = c(
    "fiscal",
    "governance",
    "institutional",
    "reform",
    "management",
    "tax",
    "revenue",
    "accountability",
    "transparency",
    "decentralization"
  ),
  Competitiveness_Trade = c(
    "competitiveness",
    "trade",
    "productivity",
    "private",
    "investment",
    "market",
    "economic",
    "growth",
    "innovation"
  ),
  Disaster_Risk = c(
    "disaster",
    "risk",
    "resilience",
    "resilient",
    "emergency",
    "catastrophe",
    "flood",
    "earthquake",
    "pandemic",
    "covid"
  )
)

period_levels <- c(
  "1980–1989",
  "1990–1999",
  "2000–2009",
  "2010–2019",
  "2020–2025"
)

theme_pres <- purrr::map_dfr(names(theme_keywords), function(theme_name) {
  kw <- theme_keywords[[theme_name]]
  # Build a single pattern for word-boundary-aware matching where feasible
  # Using paste with | for OR; str_detect does full-string search on each abstract
  pattern <- paste(kw, collapse = "|")
  abstracts_scope |>
    filter(!is.na(period)) |>
    mutate(has_theme = str_detect(tolower(abstract), pattern)) |>
    group_by(period) |>
    summarise(
      # Denominator = projects WITH abstracts in this period (not all in-scope projects)
      n_with_abstract = n(),
      n_with_theme = sum(has_theme),
      pct = round(100 * n_with_theme / n_with_abstract, 0),
      .groups = "drop"
    ) |>
    mutate(theme = theme_name)
})

# Add n_total_projects (all in-scope IBRD projects per period) for denominator transparency
period_totals <- ibrd_scope |>
  filter(!is.na(period), period %in% period_levels) |>
  count(period, name = "n_total_ibrd")

# Ensure all period_levels appear, even those with zero abstracts (e.g., 1990–1999)
all_period_theme <- tidyr::crossing(
  period = period_levels,
  theme = names(theme_keywords)
)

theme_pres <- all_period_theme |>
  left_join(theme_pres, by = c("period", "theme")) |>
  left_join(period_totals, by = "period") |>
  mutate(
    n_with_abstract = replace_na(n_with_abstract, 0L),
    n_with_theme = replace_na(n_with_theme, 0L),
    pct = replace_na(pct, NA_real_), # NA where no abstracts; do not show 0%
    has_abstracts = n_with_abstract > 0,
    period = factor(period, levels = period_levels)
  ) |>
  arrange(period, desc(pct))

cat("Theme presence computed.\n")
cat(
  "Note: The 1990–1999 period has 0 project abstracts in the API extract and is excluded\n"
)
cat(
  "  from theme analysis. Percentages are shares of projects WITH abstracts, not all projects.\n"
)
cat("Coverage check:\n")
theme_pres |>
  dplyr::distinct(period, n_with_abstract, n_total_ibrd) |>
  print()
write_csv(theme_pres, file.path(DER, "abstract_themes.csv"))

# Word counts
stop_words_custom <- tibble::tibble(
  word = c(
    "project",
    "costa",
    "rica",
    "bank",
    "world",
    "program",
    "programme",
    "component",
    "support",
    "including",
    "include",
    "provide",
    "provided",
    "improve",
    "percent",
    "million",
    "billion",
    "financing",
    "loan",
    "lending",
    "ibrd",
    "ida",
    "will",
    "also",
    "government",
    "ministry",
    "objective",
    "objectives",
    "subcomponent",
    "subcomponents",
    "1",
    "2",
    "3",
    "4",
    "5",
    "i",
    "ii",
    "iii"
  )
)
data("stop_words", package = "tidytext")

word_counts <- abstracts_scope |>
  tidytext::unnest_tokens(word, abstract) |>
  anti_join(stop_words, by = "word") |>
  anti_join(stop_words_custom, by = "word") |>
  filter(str_detect(word, "^[a-z]"), nchar(word) > 2) |>
  count(word, sort = TRUE)

write_csv(word_counts, file.path(DER, "word_counts.csv"))
cat("Word counts saved:", nrow(word_counts), "unique words\n")

# ---------------------------------------------------------------------------
# E.  IFC summary for charts
# ---------------------------------------------------------------------------

cat("\n=== E. IFC ANALYSIS ===\n")

ifc_scope <- ifc_projects |>
  filter(is_investment, in_scope, !is_pipeline) |>
  arrange(best_year)

cat("In-scope IFC investments:", nrow(ifc_scope), "\n")
cat(
  "Total IFC own-account (indicative, M USD):",
  sum(ifc_scope$ifc_invest_usd_m, na.rm = TRUE),
  "\n"
)

ifc_by_sector <- ifc_scope |>
  group_by(sector_group) |>
  summarise(
    n = n(),
    total_m = sum(ifc_invest_usd_m, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(total_m))
cat("IFC by sector:\n")
print(ifc_by_sector)

write_csv(ifc_by_sector, file.path(DER, "ifc_by_sector.csv"))

# ---------------------------------------------------------------------------
# F.  WDI context summary
# ---------------------------------------------------------------------------

cat("\n=== F. WDI CONTEXT ===\n")
wdi_scope <- wdi |> filter(year >= 1980, year <= 2025)
cat("WDI rows in scope:", nrow(wdi_scope), "\n")
for (col in c(
  "gdp_per_capita_usd",
  "gdp_growth_pct",
  "gini",
  "life_expectancy_years"
)) {
  if (col %in% names(wdi_scope)) {
    obs <- sum(!is.na(wdi_scope[[col]]))
    cat(" ", col, ":", obs, "obs in scope\n")
  }
}

write_csv(wdi_scope, file.path(DER, "wdi_scope.csv"))

cat("\nAnalysis complete. All tables saved to data/derived/\n")
