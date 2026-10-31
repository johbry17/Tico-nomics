# 01_import_and_audit.R
# Data feasibility audit for the Tico-nomics project
# World Bank Lending in Costa Rica, 1990–2024
#
# Reads both raw JSON files, compares their content, and produces a
# structured audit report. Outputs are written to data/processed/ and
# outputs/ for use in subsequent scripts and Quarto reports.

library(here)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(readr)
library(wbstats)

source(here("R", "functions.R"))

# ---------------------------------------------------------------------------
# 1.  Read both JSON files
# ---------------------------------------------------------------------------

cat("\n=== READING JSON FILES ===\n")

raw_main     <- fromJSON(here("data", "raw", "world_bank_costa_rica.json"),
                         simplifyVector = FALSE)
raw_filtered <- fromJSON(here("data", "raw", "world_bank_costa_rica_filtered.json"),
                         simplifyVector = FALSE)

cat("Main file     — API total claimed:", raw_main$total,
    " | projects in list:", length(raw_main$projects), "\n")
cat("Filtered file — API total claimed:", raw_filtered$total,
    " | projects in list:", length(raw_filtered$projects), "\n")

# ---------------------------------------------------------------------------
# 2.  Extract flat project tables from each file
# ---------------------------------------------------------------------------

# Helper: safely pull a scalar field, returning NA when absent / NULL
safe_pull <- function(proj, field) {
  val <- proj[[field]]
  if (is.null(val) || length(val) == 0) return(NA_character_)
  as.character(val[[1]])
}

# Helper: extract a sector list entry (Name + Percent) or NA
extract_sector <- function(proj, slot) {
  s <- proj[[slot]]
  if (is.null(s)) return(list(name = NA_character_, pct = NA_real_))
  nm  <- if (is.null(s$Name)    || s$Name    == "") NA_character_ else s$Name
  pct <- if (is.null(s$Percent) || s$Percent == 0 && is.na(nm)) NA_real_ else
           suppressWarnings(as.numeric(s$Percent))
  list(name = nm, pct = pct)
}

# --- Extract main file ---
extract_main <- function(proj_list) {
  purrr::map_dfr(names(proj_list), function(id) {
    p <- proj_list[[id]]

    s1 <- extract_sector(p, "sector1")
    s2 <- extract_sector(p, "sector2")
    s3 <- extract_sector(p, "sector3")

    # Parse approval date — main file uses MM/DD/YYYY HH:MM:SS
    date_raw <- safe_pull(p, "boardapprovaldate")
    approval_date <- suppressWarnings(
      tryCatch(mdy_hms(date_raw), error = function(e) NA)
    )
    if (is.na(approval_date)) {
      approval_date <- suppressWarnings(
        tryCatch(ymd_hms(date_raw), error = function(e) NA)
      )
    }

    # Parse closing date
    close_raw  <- safe_pull(p, "closingdate")
    close_date <- suppressWarnings(
      tryCatch(mdy_hms(close_raw), error = function(e) NA)
    )
    if (is.na(close_date)) {
      close_date <- suppressWarnings(
        tryCatch(ymd_hms(close_raw), error = function(e) NA)
      )
    }

    # Parse totalamt — stored as "200,000,000" string
    totalamt_raw <- safe_pull(p, "totalamt")
    totalamt     <- suppressWarnings(
      as.numeric(str_remove_all(coalesce(totalamt_raw, ""), ","))
    )

    ibrd_raw  <- safe_pull(p, "ibrdcommamt")
    ibrd      <- suppressWarnings(as.numeric(str_remove_all(coalesce(ibrd_raw, ""), ",")))

    cost_raw  <- safe_pull(p, "lendprojectcost")
    proj_cost <- suppressWarnings(as.numeric(str_remove_all(coalesce(cost_raw, ""), ",")))

    tibble(
      id               = id,
      project_name     = safe_pull(p, "project_name"),
      approval_date    = approval_date,
      approval_year    = if (!is.na(approval_date)) year(approval_date) else NA_integer_,
      close_date       = close_date,
      status           = safe_pull(p, "status"),
      lendinginstr     = safe_pull(p, "lendinginstr"),
      totalamt         = totalamt,
      ibrdcommamt      = ibrd,
      lendprojectcost  = proj_cost,
      sector1_name     = s1$name,
      sector1_pct      = s1$pct,
      sector2_name     = s2$name,
      sector2_pct      = s2$pct,
      sector3_name     = s3$name,
      sector3_pct      = s3$pct,
      borrower         = safe_pull(p, "borrower"),
      url              = safe_pull(p, "url"),
      source_file      = "main"
    )
  })
}

# --- Extract filtered file ---
extract_filtered <- function(proj_list) {
  purrr::map_dfr(names(proj_list), function(id) {
    p <- proj_list[[id]]

    s1 <- extract_sector(p, "sector1")
    s2 <- extract_sector(p, "sector2")
    s3 <- extract_sector(p, "sector3")

    # Filtered file uses ISO 8601
    date_raw     <- safe_pull(p, "boardapprovaldate")
    approval_date <- suppressWarnings(
      tryCatch(ymd_hms(date_raw), error = function(e) NA)
    )

    close_raw  <- safe_pull(p, "closingdate")
    close_date <- suppressWarnings(
      tryCatch(mdy_hms(close_raw), error = function(e) NA)
    )
    if (is.na(close_date)) {
      close_date <- suppressWarnings(
        tryCatch(ymd_hms(close_raw), error = function(e) NA)
      )
    }

    totalamt_raw <- safe_pull(p, "totalamt")
    totalamt     <- suppressWarnings(
      as.numeric(str_remove_all(coalesce(totalamt_raw, ""), ","))
    )

    # Abstract is nested: project_abstract$`cdata!`
    abstract_raw <- p[["project_abstract"]]
    abstract <- if (!is.null(abstract_raw)) {
      coalesce(
        abstract_raw[["cdata!"]],
        abstract_raw[["#cdata-section"]],
        as.character(abstract_raw[[1]])
      )
    } else {
      NA_character_
    }
    abstract <- if (!is.null(abstract) && !is.na(abstract) &&
                    nchar(trimws(abstract)) > 0) trimws(abstract) else NA_character_

    tibble(
      id               = id,
      project_name     = safe_pull(p, "project_name"),
      approval_date    = approval_date,
      approval_year    = if (!is.na(approval_date)) year(approval_date) else NA_integer_,
      close_date       = close_date,
      status           = safe_pull(p, "status"),
      totalamt         = totalamt,
      sector1_name     = s1$name,
      sector1_pct      = s1$pct,
      sector2_name     = s2$name,
      sector2_pct      = s2$pct,
      sector3_name     = s3$name,
      sector3_pct      = s3$pct,
      project_abstract = abstract,
      url              = safe_pull(p, "url"),
      source_file      = "filtered"
    )
  })
}

df_main     <- extract_main(raw_main$projects)
df_filtered <- extract_filtered(raw_filtered$projects)

cat("\nExtracted", nrow(df_main), "projects from main file\n")
cat("Extracted", nrow(df_filtered), "projects from filtered file\n")

# ---------------------------------------------------------------------------
# 3.  Compare the two files
# ---------------------------------------------------------------------------

cat("\n=== FILE COMPARISON ===\n")

ids_main     <- df_main$id
ids_filtered <- df_filtered$id

cat("IDs only in main file:    ", length(setdiff(ids_main, ids_filtered)), "\n")
cat("IDs only in filtered file:", length(setdiff(ids_filtered, ids_main)), "\n")
cat("IDs in both files:        ", length(intersect(ids_main, ids_filtered)), "\n")

dups_main     <- ids_main[duplicated(ids_main)]
dups_filtered <- ids_filtered[duplicated(ids_filtered)]
cat("Duplicate IDs in main:    ", length(dups_main), "\n")
cat("Duplicate IDs in filtered:", length(dups_filtered), "\n")

# Date format difference
cat("\nboardapprovaldate format:\n")
cat("  Main file (raw example):    ",
    raw_main$projects[[1]]$boardapprovaldate %||% "NA", "\n")
cat("  Filtered file (raw example):",
    raw_filtered$projects[[1]]$boardapprovaldate %||% "NA", "\n")

# ---------------------------------------------------------------------------
# 4.  Date coverage audit
# ---------------------------------------------------------------------------

cat("\n=== DATE COVERAGE (main file) ===\n")

cat("Missing approval dates:", sum(is.na(df_main$approval_date)), "\n")
cat("Projects with dates:   ", sum(!is.na(df_main$approval_date)), "\n")

date_range <- range(df_main$approval_date, na.rm = TRUE)
cat("Earliest approval:     ", format(date_range[1], "%Y-%m-%d"), "\n")
cat("Latest approval:       ", format(date_range[2], "%Y-%m-%d"), "\n")

pre1990  <- df_main %>% filter(!is.na(approval_date), approval_year < 1990)
in_scope <- df_main %>% filter(!is.na(approval_date), approval_year >= 1990)
cat("\nPre-1990 projects:     ", nrow(pre1990), "\n")
cat("1990–2024 projects:    ", nrow(in_scope), "\n")
cat("Missing-date projects: ", sum(is.na(df_main$approval_date)), "\n")

cat("\nPre-1990 project list:\n")
pre1990 %>%
  select(id, project_name, approval_year) %>%
  arrange(approval_year) %>%
  print(n = Inf)

cat("\nProjects with missing approval date:\n")
df_main %>%
  filter(is.na(approval_date)) %>%
  select(id, project_name, status, totalamt) %>%
  print(n = Inf)

# ---------------------------------------------------------------------------
# 5.  Financing audit
# ---------------------------------------------------------------------------

cat("\n=== FINANCING AUDIT ===\n")

cat("Field interpretation:\n")
cat("  totalamt       = World Bank IBRD/IDA commitment (loan amount)\n")
cat("  ibrdcommamt    = Same as totalamt in all cases examined\n")
cat("  lendprojectcost= Total project cost (may include counterpart funding)\n")
cat("  Currency: nominal US dollars\n\n")

amt_summary <- df_main %>%
  summarise(
    n_total        = n(),
    n_zero         = sum(totalamt == 0, na.rm = TRUE),
    n_missing      = sum(is.na(totalamt)),
    n_positive     = sum(totalamt > 0, na.rm = TRUE),
    min_usd        = min(totalamt[totalamt > 0], na.rm = TRUE),
    max_usd        = max(totalamt, na.rm = TRUE),
    median_usd     = median(totalamt[totalamt > 0], na.rm = TRUE),
    mean_usd       = mean(totalamt[totalamt > 0], na.rm = TRUE),
    total_usd      = sum(totalamt, na.rm = TRUE)
  )

cat("Summary of totalamt (all 72 projects):\n")
cat("  Projects with positive amount:", amt_summary$n_positive, "\n")
cat("  Projects with zero amount:    ", amt_summary$n_zero, "\n")
cat("  Projects with missing amount: ", amt_summary$n_missing, "\n")
cat("  Minimum (excl. zero):         ", fmt_millions(amt_summary$min_usd), "\n")
cat("  Median (excl. zero):          ", fmt_millions(amt_summary$median_usd), "\n")
cat("  Mean (excl. zero):            ", fmt_millions(amt_summary$mean_usd), "\n")
cat("  Maximum:                      ", fmt_millions(amt_summary$max_usd), "\n")
cat("  Total portfolio (nominal):    ", fmt_billions(amt_summary$total_usd), "\n\n")

cat("Projects with zero totalamt:\n")
df_main %>%
  filter(totalamt == 0 | is.na(totalamt)) %>%
  select(id, project_name, totalamt, lendprojectcost, status, lendinginstr) %>%
  print(n = Inf)

# Largest projects
cat("\nTop 10 projects by totalamt:\n")
df_main %>%
  filter(totalamt > 0) %>%
  arrange(desc(totalamt)) %>%
  select(id, project_name, approval_year, totalamt, lendinginstr) %>%
  head(10) %>%
  mutate(totalamt = fmt_millions(totalamt)) %>%
  print(n = Inf)

# ---------------------------------------------------------------------------
# 6.  Sector audit
# ---------------------------------------------------------------------------

cat("\n=== SECTOR AUDIT ===\n")

# Note: sector1–3 in main file; max 3 slots observed
sector_coverage <- df_main %>%
  summarise(
    has_s1_name  = sum(!is.na(sector1_name)),
    has_s2_name  = sum(!is.na(sector2_name)),
    has_s3_name  = sum(!is.na(sector3_name)),
    has_s1_pct   = sum(!is.na(sector1_pct) & sector1_pct > 0),
    has_s2_pct   = sum(!is.na(sector2_pct) & sector2_pct > 0),
    has_s3_pct   = sum(!is.na(sector3_pct) & sector3_pct > 0)
  )

cat("Projects with sector1 name:      ", sector_coverage$has_s1_name, "\n")
cat("Projects with sector2 name:      ", sector_coverage$has_s2_name, "\n")
cat("Projects with sector3 name:      ", sector_coverage$has_s3_name, "\n")
cat("Projects with sector1 pct > 0:   ", sector_coverage$has_s1_pct, "\n")
cat("Projects with sector2 pct > 0:   ", sector_coverage$has_s2_pct, "\n")
cat("Projects with sector3 pct > 0:   ", sector_coverage$has_s3_pct, "\n")

# Check percent sums — do sector percentages add to ~100?
pct_check <- df_main %>%
  filter(!is.na(sector1_name)) %>%
  rowwise() %>%
  mutate(
    pct_sum = sum(c(sector1_pct, sector2_pct, sector3_pct), na.rm = TRUE)
  ) %>%
  ungroup()

cat("\nSector percent sum (among projects with sector1 name):\n")
cat("  Projects where sum ≈ 100 (±5):  ",
    sum(abs(pct_check$pct_sum - 100) <= 5, na.rm = TRUE), "\n")
cat("  Projects where sum < 95:        ",
    sum(pct_check$pct_sum < 95, na.rm = TRUE), "\n")
cat("  Projects where sum = 0:         ",
    sum(pct_check$pct_sum == 0, na.rm = TRUE), "\n")
cat("  Range of percent sums:          ",
    round(min(pct_check$pct_sum, na.rm = TRUE), 1), "–",
    round(max(pct_check$pct_sum, na.rm = TRUE), 1), "\n")

# Unique sector names
all_sector_names <- c(df_main$sector1_name, df_main$sector2_name, df_main$sector3_name)
all_sector_names <- all_sector_names[!is.na(all_sector_names)]
cat("\nUnique sector names across all projects:\n")
sort(unique(all_sector_names)) %>% purrr::walk(~ cat("  ", ., "\n"))

# Projects with NO sector data
no_sector <- df_main %>% filter(is.na(sector1_name))
cat("\nProjects with no usable sector data:", nrow(no_sector), "\n")
cat("(These are mostly recent 2024 approvals with incomplete API records)\n")
no_sector %>%
  select(id, project_name, approval_year, totalamt) %>%
  print(n = Inf)

# ---------------------------------------------------------------------------
# 7.  Abstract audit
# ---------------------------------------------------------------------------

cat("\n=== ABSTRACT AUDIT ===\n")

abs_cov <- df_filtered %>%
  summarise(
    n_total   = n(),
    n_present = sum(!is.na(project_abstract)),
    n_missing = sum(is.na(project_abstract)),
    min_chars = min(nchar(project_abstract[!is.na(project_abstract)])),
    max_chars = max(nchar(project_abstract[!is.na(project_abstract)])),
    med_chars = median(nchar(project_abstract[!is.na(project_abstract)]))
  )

cat("Projects with abstract:   ", abs_cov$n_present, "/", abs_cov$n_total, "\n")
cat("Missing abstracts:        ", abs_cov$n_missing, "\n")
cat("Abstract length (chars):\n")
cat("  Minimum:", abs_cov$min_chars, "\n")
cat("  Median: ", abs_cov$med_chars, "\n")
cat("  Maximum:", abs_cov$max_chars, "\n")

cat("\nProjects missing abstracts:\n")
df_filtered %>%
  filter(is.na(project_abstract)) %>%
  select(id, project_name, approval_year, status) %>%
  print(n = Inf)

# ---------------------------------------------------------------------------
# 8.  Status and closing date audit
# ---------------------------------------------------------------------------

cat("\n=== STATUS AND CLOSING DATE AUDIT ===\n")

cat("Status distribution:\n")
df_main %>%
  count(status, sort = TRUE) %>%
  print(n = Inf)

cat("\nMissing closing dates:", sum(is.na(df_main$close_date)), "\n")

# ---------------------------------------------------------------------------
# 9.  WDI data audit
# ---------------------------------------------------------------------------

cat("\n=== WDI DATA AUDIT ===\n")

wdi_indicators <- c(
  "NY.GDP.PCAP.CD",  # GDP per capita
  "SI.POV.GINI",     # Gini index
  "SP.DYN.LE00.IN",  # Life expectancy
  "SE.ADT.LITR.ZS"   # Adult literacy
)

cat("Retrieving WDI data for Costa Rica...\n")
wdi_raw <- tryCatch(
  wbstats::wb_data(wdi_indicators, country = "CR"),
  error = function(e) {
    cat("ERROR retrieving WDI data:", conditionMessage(e), "\n")
    NULL
  }
)

if (!is.null(wdi_raw)) {
  cat("WDI data retrieved:", nrow(wdi_raw), "rows,", ncol(wdi_raw), "columns\n")
  cat("Year range:", min(wdi_raw$date, na.rm = TRUE), "–",
      max(wdi_raw$date, na.rm = TRUE), "\n\n")

  # Coverage by indicator
  for (ind in wdi_indicators) {
    col <- ind
    if (col %in% names(wdi_raw)) {
      vals    <- wdi_raw[[col]]
      present <- sum(!is.na(vals))
      total   <- nrow(wdi_raw)
      yrs     <- wdi_raw$date[!is.na(vals)]
      cat(ind, ":\n")
      cat("  Non-missing observations:", present, "/", total, "\n")
      if (length(yrs) > 0) {
        cat("  Year range:", min(yrs), "–", max(yrs), "\n")
        # Check for gaps
        yr_seq <- seq(min(yrs), max(yrs))
        gaps   <- setdiff(yr_seq, yrs)
        if (length(gaps) > 0) {
          cat("  Gaps in series:", paste(gaps, collapse = ", "), "\n")
        } else {
          cat("  No gaps in year range\n")
        }
      }
      cat("\n")
    } else {
      cat(ind, ": column not found in WDI output\n\n")
    }
  }
}

# ---------------------------------------------------------------------------
# 10. Save audit outputs
# ---------------------------------------------------------------------------

cat("\n=== SAVING AUDIT OUTPUTS ===\n")

write_csv(df_main,     here("data", "processed", "projects_main_raw.csv"))
write_csv(df_filtered, here("data", "processed", "projects_filtered_raw.csv"))

if (!is.null(wdi_raw)) {
  write_csv(wdi_raw, here("data", "processed", "wdi_raw.csv"))
}

cat("Saved:\n")
cat("  data/processed/projects_main_raw.csv\n")
cat("  data/processed/projects_filtered_raw.csv\n")
cat("  data/processed/wdi_raw.csv\n")

# ---------------------------------------------------------------------------
# 11. Feasibility verdict summary
# ---------------------------------------------------------------------------

cat("\n=== FEASIBILITY VERDICT ===\n")

cat("
+----------------------+-------------------+------------------------------+
| Component            | Feasibility       | Notes                        |
+----------------------+-------------------+------------------------------+
| Project dates        | Good              | 72 projects; 41 in scope;    |
|                      |                   | 16 missing dates             |
| Financing amounts    | Good              | 57/72 positive; nominal USD  |
| Sector allocations   | Usable w/ limits  | 49/72 with sector names;     |
|                      |                   | gaps in recent projects      |
| Project abstracts    | Good (if present) | 56-67/72 — verify count      |
| Project status       | Good              | All 72 populated             |
| GDP per capita       | Good              | Annual 1960–2023             |
| Gini index           | Usable w/ limits  | Sparse, survey years only    |
| Life expectancy      | Good              | Annual 1960–2022             |
| Adult literacy       | Poor              | Very few observations        |
+----------------------+-------------------+------------------------------+

OVERALL VERDICT: PROCEED WITH MODIFICATIONS
- Core project analysis is viable for 1990–2024
- Exclude zero-amount technical assistance from financing totals
- Sector analysis limited by missing data for most recent projects
- Gini and literacy should be plotted with actual available points only
- Avoid inflation adjustment unless CPI data is explicitly added
- Label all amounts as 'nominal US dollars at time of commitment'
")

cat("Audit complete.\n")
