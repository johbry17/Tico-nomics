# 07_scenarios.R
# Three-scenario analysis using original, recovered, and financing-only dates
# Compares how the timeline and financing distribution change with recovered dates.

library(here)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

source(here("R", "functions.R"))

# ---------------------------------------------------------------------------
# 0.  Load data
# ---------------------------------------------------------------------------

projects_all <- read_csv(here("data", "processed", "projects_all.csv"),
                          show_col_types = FALSE)
enrichment   <- read_csv(here("data", "processed", "date_enrichment.csv"),
                          show_col_types = FALSE)

# ---------------------------------------------------------------------------
# 1.  Build enriched projects table
# ---------------------------------------------------------------------------

# Merge recovered dates into projects_all
projects_enriched <- projects_all |>
  left_join(
    enrichment |>
      select(id, recovered_date, recovered_year, confidence, in_scope_recovered),
    by = "id"
  ) |>
  mutate(
    # Enriched date: use original where available; fall back to recovered
    enriched_date = if_else(
      !is.na(approval_date),
      approval_date,
      recovered_date
    ),
    enriched_year = case_when(
      !is.na(approval_year)    ~ as.integer(approval_year),
      !is.na(recovered_year)   ~ as.integer(recovered_year),
      TRUE                     ~ NA_integer_
    ),
    date_source = case_when(
      !is.na(approval_year)                              ~ "Original API (V2)",
      !is.na(recovered_year) & grepl("High", confidence) ~ "Recovered (V3 API + Excel, high confidence)",
      !is.na(recovered_year) & grepl("Medium", confidence) ~ "Recovered (V3/Excel, medium — FY-end proxy)",
      TRUE                                               ~ "Not recovered"
    ),
    # Enriched in-scope flag
    in_scope_enriched = !is.na(enriched_year) &
                        enriched_year >= 1990 &
                        enriched_year <= 2024
  )

cat("=== ENRICHED PROJECTS TABLE ===\n")
cat("Projects with original dates:   ", sum(!is.na(projects_all$approval_year)), "\n")
cat("Projects with recovered dates:  ", sum(!is.na(projects_enriched$recovered_year) &
                                              is.na(projects_all$approval_year)), "\n")
cat("Still no date:                  ",
    sum(is.na(projects_enriched$enriched_year)), "\n\n")

cat("Date source breakdown:\n")
projects_enriched |>
  count(date_source, sort = TRUE) |>
  print()

cat("\nEnriched in-scope (1990-2024):", sum(projects_enriched$in_scope_enriched, na.rm = TRUE), "\n")

# ---------------------------------------------------------------------------
# 2.  Define the three scenarios
# ---------------------------------------------------------------------------

cat("\n=== THREE SCENARIO DEFINITIONS ===\n")

# Scenario A: Original data (V2 API confirmed dates only)
scen_a <- projects_enriched |>
  filter(!is.na(approval_year), approval_year >= 1990, approval_year <= 2024)

# Scenario B: Recovered dates included (all in-scope regardless of date source,
#             including medium-confidence FY-end proxy dates)
scen_b <- projects_enriched |>
  filter(in_scope_enriched == TRUE, enriched_year >= 1990, enriched_year <= 2024)

# Scenario C: Positive financing only, confirmed original dates
scen_c <- projects_enriched |>
  filter(!is.na(approval_year), approval_year >= 1990, approval_year <= 2024,
         !is.na(totalamt), totalamt > 0)

cat("Scenario A (original confirmed dates, 1990-2024):",
    nrow(scen_a), "projects\n")
cat("Scenario B (with recovered dates, 1990-2024):    ",
    nrow(scen_b), "projects\n")
cat("Scenario C (positive financing, confirmed dates): ",
    nrow(scen_c), "projects\n\n")

# ---------------------------------------------------------------------------
# 3.  Per-scenario metrics
# ---------------------------------------------------------------------------

scenario_summary <- function(df, name, year_col = "approval_year") {
  yr   <- df[[year_col]]
  fin  <- df$totalamt

  n_tot   <- nrow(df)
  n_pos   <- sum(!is.na(fin) & fin > 0)
  total   <- sum(fin[!is.na(fin) & fin > 0])
  pct2024 <- if (total > 0)
    sum(fin[!is.na(fin) & fin > 0 & !is.na(yr) & yr >= 2020]) / total * 100
  else NA

  gap_yrs <- setdiff(2013:2019, yr[!is.na(yr)])

  cat(sprintf("--- %s ---\n", name))
  cat(sprintf("  Projects included:            %d\n", n_tot))
  cat(sprintf("  With positive financing:      %d\n", n_pos))
  cat(sprintf("  Total commitments (nom. USD): %s\n", fmt_billions(total)))
  cat(sprintf("  %% of commitments in 2020–2024: %.1f%%\n", pct2024))
  cat(sprintf("  Projects in 2013–2019:        %d\n",
              sum(!is.na(yr) & yr >= 2013 & yr <= 2019)))
  cat(sprintf("  Years with no project in 2013–2019: %s\n",
              if (length(gap_yrs) == 0) "none" else paste(gap_yrs, collapse = ", ")))
  cat("\n")
}

cat("=== PER-SCENARIO METRICS ===\n\n")
scenario_summary(scen_a |> filter(has_financing),
                 "Scenario A — Original data (financing projects only)")
scenario_summary(scen_b |> filter(!is.na(totalamt) & totalamt > 0) |>
                   mutate(approval_year = enriched_year),
                 "Scenario B — Recovered dates (financing projects only)",
                 year_col = "enriched_year")
scenario_summary(scen_c, "Scenario C — Positive financing, confirmed dates")

# ---------------------------------------------------------------------------
# 4.  Annual financing by scenario
# ---------------------------------------------------------------------------

all_years <- tibble(year = 1990:2024)

annual_a <- scen_a |>
  filter(has_financing) |>
  group_by(year = approval_year) |>
  summarise(n = n(), total_usd = sum(totalamt, na.rm = TRUE), .groups = "drop") |>
  right_join(all_years, by = "year") |>
  mutate(n = coalesce(n, 0L), total_usd = coalesce(total_usd, 0), scenario = "A")

annual_b <- scen_b |>
  filter(!is.na(totalamt), totalamt > 0) |>
  group_by(year = enriched_year) |>
  summarise(n = n(), total_usd = sum(totalamt, na.rm = TRUE), .groups = "drop") |>
  right_join(all_years, by = "year") |>
  mutate(n = coalesce(n, 0L), total_usd = coalesce(total_usd, 0), scenario = "B")

annual_c <- scen_c |>
  group_by(year = approval_year) |>
  summarise(n = n(), total_usd = sum(totalamt, na.rm = TRUE), .groups = "drop") |>
  right_join(all_years, by = "year") |>
  mutate(n = coalesce(n, 0L), total_usd = coalesce(total_usd, 0), scenario = "C")

annual_scenarios <- bind_rows(annual_a, annual_b, annual_c)

cat("=== ANNUAL FINANCING BY SCENARIO (selected years) ===\n")
annual_scenarios |>
  pivot_wider(names_from = scenario,
              values_from = c(n, total_usd),
              names_glue = "{scenario}_{.value}") |>
  filter(year %in% c(1994, 2000, 2007, 2008, 2009, 2010, 2011, 2012,
                     2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024)) |>
  mutate(
    A_total_usd = fmt_millions(A_total_usd),
    B_total_usd = fmt_millions(B_total_usd),
    C_total_usd = fmt_millions(C_total_usd)
  ) |>
  print(n = 25)

# ---------------------------------------------------------------------------
# 5.  What the enrichment adds to 2013–2019
# ---------------------------------------------------------------------------

cat("\n=== 2013–2019 GAP ANALYSIS ===\n")

gap_added <- projects_enriched |>
  filter(
    enriched_year >= 2013, enriched_year <= 2019,
    is.na(approval_year),     # was missing in original
    !is.na(recovered_year)    # now recovered
  ) |>
  select(id, project_name, enriched_year, totalamt, confidence, status, lendinginstr)

cat("Projects now placed in 2013–2019 via date recovery:\n")
print(gap_added, n = 20)
cat("\n")

# Was any positive financing added to 2013-2019?
cat("Positive financing added to 2013-2019:\n")
gap_pos <- gap_added |> filter(!is.na(totalamt), totalamt > 0)
if (nrow(gap_pos) > 0) {
  cat("  Projects:", nrow(gap_pos), "\n")
  cat("  Total:", fmt_millions(sum(gap_pos$totalamt)), "\n")
  gap_pos |>
    select(id, project_name, enriched_year, totalamt, confidence) |>
    mutate(totalamt = fmt_millions(totalamt)) |>
    print()
} else {
  cat("  None — 2013-2019 gap has no recovered positive financing\n")
}

# What is the non-gap (2014) story?
cat("\nNew projects in 2014 specifically:\n")
projects_enriched |>
  filter(enriched_year == 2014) |>
  select(id, project_name, totalamt, date_source, status) |>
  print()

# ---------------------------------------------------------------------------
# 6.  Full project list for Scenario B (new additions only)
# ---------------------------------------------------------------------------

cat("\n=== SCENARIO B NEW ADDITIONS (projects with recovered in-scope dates) ===\n")
new_additions <- projects_enriched |>
  filter(in_scope_enriched, !is.na(approval_year) == FALSE,
         !is.na(recovered_year)) |>
  # Actually: filter to recovered in-scope projects
  filter(is.na(approval_year), !is.na(recovered_year),
         enriched_year >= 1990, enriched_year <= 2024) |>
  arrange(enriched_year) |>
  select(id, project_name, enriched_year, totalamt, confidence, status, lendinginstr)

print(new_additions, n = 20)

cat("\nNew additions financing summary:\n")
cat("  With positive financing:", sum(new_additions$totalamt > 0, na.rm = TRUE), "\n")
cat("  Total positive financing added:",
    fmt_millions(sum(new_additions$totalamt[new_additions$totalamt > 0], na.rm = TRUE)), "\n")
cat("  Year 2014 additions:", sum(new_additions$enriched_year == 2014, na.rm=TRUE),
    "($", sum(new_additions$totalamt[new_additions$enriched_year == 2014], na.rm=TRUE) / 1e6, "M)\n")

# ---------------------------------------------------------------------------
# 7.  Gap verdict
# ---------------------------------------------------------------------------

cat("\n=== GAP VERDICT ===\n\n")

in_scope_b_years <- projects_enriched |>
  filter(in_scope_enriched, !is.na(totalamt), totalamt > 0) |>
  pull(enriched_year)

gap_2015_2019 <- setdiff(2015:2019, in_scope_b_years)
cat("In Scenario B, years 2015-2019 with zero positive financing:\n")
cat("  ", paste(sort(gap_2015_2019), collapse = ", "), "\n\n")

cat("FINDING: The 2013-2019 gap is BOTH a missing-data artifact AND a real pattern.\n")
cat("  - P122265 ($200M Health Networks APL) was approved 2014-07-03 — fills 2014.\n")
cat("  - P163431 ($0 Household Surveys) was approved 2018-02-02 — fills 2018 with $0 financing.\n")
cat("  - P167926 ($0 Stats Capacity) was approved 2018-11-16 — fills 2018 further.\n")
cat("  - Years 2013, 2015, 2016, 2017, 2019 still have no confirmed in-scope financing.\n")
cat("  - P169724 ($100M Learning for All, Dropped) has NO recovered date — genuinely unknown.\n")
cat("  - The financing gap in 2015-2019 is real: only $0 projects confirmed there.\n\n")

cat("RECOMMENDATION: The gap is partially explainable by missing data (P122265 fills 2014)\n")
cat("but CANNOT be dismissed as entirely an artifact. The absence of positive IBRD\n")
cat("financing commitments in 2013 and 2015-2019 is a defensible observation,\n")
cat("subject to the caveat that P169724 ($100M) remains unresolved.\n")

# ---------------------------------------------------------------------------
# 8.  Save outputs
# ---------------------------------------------------------------------------

write_csv(projects_enriched, here("data", "processed", "projects_enriched.csv"))
write_csv(annual_scenarios,  here("data", "derived",   "annual_scenarios.csv"))
write_csv(new_additions,     here("data", "derived",   "recovered_projects.csv"))

cat("\nSaved:\n")
cat("  data/processed/projects_enriched.csv\n")
cat("  data/derived/annual_scenarios.csv\n")
cat("  data/derived/recovered_projects.csv\n")
cat("\nScenario analysis complete.\n")
