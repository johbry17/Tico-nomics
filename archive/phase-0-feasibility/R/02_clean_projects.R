# 02_clean_projects.R
# Clean and prepare project-level datasets for analysis
# Depends on: 01_import_and_audit.R having been run first

library(here)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(readr)

source(here("R", "functions.R"))

# ---------------------------------------------------------------------------
# 1.  Re-read raw data (ensures reproducibility even if run standalone)
# ---------------------------------------------------------------------------

raw_main     <- fromJSON(here("data", "raw", "world_bank_costa_rica.json"),
                         simplifyVector = FALSE)
raw_filtered <- fromJSON(here("data", "raw", "world_bank_costa_rica_filtered.json"),
                         simplifyVector = FALSE)

# Pull processed CSVs if already available, otherwise re-extract
if (file.exists(here("data", "processed", "projects_main_raw.csv"))) {
  df_main     <- read_csv(here("data", "processed", "projects_main_raw.csv"),
                           show_col_types = FALSE)
  df_filtered <- read_csv(here("data", "processed", "projects_filtered_raw.csv"),
                           show_col_types = FALSE)
} else {
  source(here("R", "01_import_and_audit.R"))
  df_main     <- read_csv(here("data", "processed", "projects_main_raw.csv"),
                           show_col_types = FALSE)
  df_filtered <- read_csv(here("data", "processed", "projects_filtered_raw.csv"),
                           show_col_types = FALSE)
}

# Also extract lending instrument from main file (not in CSV yet)
# We need lendinginstr and mjtheme info from the raw JSON
instr_df <- purrr::map_dfr(names(raw_main$projects), function(id) {
  p <- raw_main$projects[[id]]

  # Major theme from theme_list (if available)
  theme_list <- p[["theme_list"]]
  mjtheme_name <- NA_character_
  if (!is.null(theme_list) && length(theme_list) > 0) {
    mjtheme_name <- theme_list[[1]][["name"]] %||% NA_character_
    if (is.null(mjtheme_name) || is.na(mjtheme_name) || nchar(trimws(mjtheme_name)) == 0)
      mjtheme_name <- NA_character_
  }
  # Fallback: mjtheme_namecode
  if (is.na(mjtheme_name)) {
    mnc <- p[["mjtheme_namecode"]]
    if (!is.null(mnc)) {
      nm <- if (is.list(mnc)) mnc[["name"]] else NA_character_
      if (!is.null(nm) && !is.na(nm) && nchar(trimws(nm)) > 0)
        mjtheme_name <- nm
    }
  }

  # Collect all sector names from sector_namecode (can capture more than sector1-3)
  all_sectors_nc <- p[["sector_namecode"]]
  all_sector_names <- if (!is.null(all_sectors_nc) && is.list(all_sectors_nc)) {
    nms <- purrr::map_chr(all_sectors_nc, ~ .x[["name"]] %||% NA_character_)
    nms[!is.na(nms) & nchar(trimws(nms)) > 0]
  } else {
    character(0)
  }

  # mjsector names (major sector categories)
  mjsector_nc <- p[["mjsector_namecode"]]
  mjsector_names <- if (!is.null(mjsector_nc) && is.list(mjsector_nc)) {
    nms <- purrr::map_chr(mjsector_nc, ~ .x[["name"]] %||% NA_character_)
    unique(nms[!is.na(nms) & nchar(trimws(nms)) > 0])
  } else {
    character(0)
  }

  tibble(
    id             = id,
    lendinginstr   = p[["lendinginstr"]] %||% NA_character_,
    mjtheme_name   = mjtheme_name,
    all_sectors    = paste(all_sector_names, collapse = " | "),
    mjsector_names = paste(mjsector_names, collapse = " | ")
  )
})

# ---------------------------------------------------------------------------
# 2.  Build the master project table
# ---------------------------------------------------------------------------

# Left join to get abstracts from filtered file
abstracts_df <- df_filtered %>%
  select(id, project_abstract)

projects <- df_main %>%
  left_join(abstracts_df, by = "id") %>%
  left_join(instr_df %>% select(id, mjtheme_name, all_sectors, mjsector_names),
            by = "id") %>%
  mutate(
    # Coerce dates (might have been read as character)
    approval_date = as_datetime(approval_date),
    close_date    = as_datetime(close_date),
    approval_year = if_else(!is.na(approval_date), year(approval_date), NA_integer_),

    # Instrument grouping
    instrument_type = case_when(
      str_detect(lendinginstr, regex("Development Policy", ignore_case = TRUE)) ~ "Development Policy Loan",
      str_detect(lendinginstr, regex("Investment Project",  ignore_case = TRUE)) ~ "Investment Project Financing",
      str_detect(lendinginstr, regex("Technical Assist",    ignore_case = TRUE)) ~ "Technical Assistance",
      str_detect(lendinginstr, regex("Emergency",           ignore_case = TRUE)) ~ "Emergency Recovery",
      !is.na(lendinginstr) ~ lendinginstr,
      TRUE ~ "Unknown"
    ),

    # Has non-zero financing?
    has_financing = !is.na(totalamt) & totalamt > 0,

    # Analysis period flag
    in_scope_1990 = !is.na(approval_year) & approval_year >= 1990 & approval_year <= 2024,

    # Decade / period grouping
    period = case_when(
      is.na(approval_year)                           ~ NA_character_,
      approval_year < 1990                           ~ "Pre-1990",
      approval_year >= 1990 & approval_year <= 1999  ~ "1990–1999",
      approval_year >= 2000 & approval_year <= 2009  ~ "2000–2009",
      approval_year >= 2010 & approval_year <= 2019  ~ "2010–2019",
      approval_year >= 2020                          ~ "2020–2024",
      TRUE                                           ~ NA_character_
    ),

    # Primary sector grouping (from sector1_name)
    sector_group = group_sector(coalesce(sector1_name, "")),

    # Missing flags
    missing_date     = is.na(approval_date),
    missing_amount   = is.na(totalamt) | totalamt == 0,
    missing_sector   = is.na(sector1_name),
    missing_abstract = is.na(project_abstract),
    abstract_chars   = if_else(!is.na(project_abstract),
                               as.integer(nchar(project_abstract)),
                               NA_integer_)
  )

cat("Master projects table:", nrow(projects), "rows\n")
cat("In-scope (1990-2024):", sum(projects$in_scope_1990, na.rm = TRUE), "\n")
cat("With financing > 0:", sum(projects$has_financing, na.rm = TRUE), "\n")

# ---------------------------------------------------------------------------
# 3.  In-scope analysis dataset (1990–2024, any financing status)
# ---------------------------------------------------------------------------

projects_inscope <- projects %>%
  filter(in_scope_1990 == TRUE)

cat("\nIn-scope projects (1990–2024):", nrow(projects_inscope), "\n")
cat("With positive financing:", sum(projects_inscope$has_financing), "\n")
cat("With zero/missing financing:", sum(!projects_inscope$has_financing), "\n")

# ---------------------------------------------------------------------------
# 4.  Sector allocation long table
# ---------------------------------------------------------------------------
# Pivot sector slots to long format, preserving percents.
# This allows sector-level analysis without double-counting when
# projects span multiple sectors.

sector_long <- projects_inscope %>%
  filter(has_financing) %>%
  select(id, project_name, approval_year, period, totalamt, instrument_type,
         sector1_name, sector1_pct,
         sector2_name, sector2_pct,
         sector3_name, sector3_pct) %>%
  pivot_longer(
    cols      = c(sector1_name:sector3_pct),
    names_to  = c("slot", ".value"),
    names_pattern = "sector(\\d)_(name|pct)"
  ) %>%
  filter(!is.na(name), name != "") %>%
  rename(sector_name = name, sector_pct = pct) %>%
  mutate(
    slot       = as.integer(slot),
    sector_pct = if_else(is.na(sector_pct) | sector_pct == 0, NA_real_, sector_pct),
    # Sector group (broad classification)
    sector_group = group_sector(sector_name),
    # Allocated amount = total commitment × (percent / 100)
    # Only when percent is available and project has complete pct data
    alloc_amt  = if_else(!is.na(sector_pct), totalamt * sector_pct / 100, NA_real_)
  )

# For projects where all sectors have pct, we can allocate financing
# For projects where pct is missing, flag as unallocated
pct_completeness <- sector_long %>%
  group_by(id) %>%
  summarise(
    has_any_pct  = any(!is.na(sector_pct)),
    pct_sum      = sum(sector_pct, na.rm = TRUE),
    pcts_add_up  = abs(pct_sum - 100) <= 5
  )

cat("\nSector percent completeness (in-scope, with financing):\n")
cat("  Projects with any pct:    ", sum(pct_completeness$has_any_pct), "\n")
cat("  Projects where pcts ~100: ", sum(pct_completeness$pcts_add_up, na.rm = TRUE), "\n")

# Flag in sector_long whether allocation is reliable
sector_long <- sector_long %>%
  left_join(pct_completeness %>% select(id, pcts_add_up), by = "id") %>%
  mutate(
    alloc_reliable = coalesce(pcts_add_up, FALSE),
    # For projects where pcts don't add to ~100, count sector only (no allocation)
    alloc_amt = if_else(alloc_reliable, alloc_amt, NA_real_)
  )

cat("\nSector_long table:", nrow(sector_long), "rows\n")

# ---------------------------------------------------------------------------
# 5.  Annual summary table
# ---------------------------------------------------------------------------

annual_summary <- projects_inscope %>%
  group_by(approval_year) %>%
  summarise(
    n_projects       = n(),
    n_with_financing = sum(has_financing),
    total_commitments_usd = sum(totalamt[has_financing], na.rm = TRUE),
    median_project_usd    = median(totalamt[has_financing], na.rm = TRUE),
    mean_project_usd      = mean(totalamt[has_financing], na.rm = TRUE),
    n_dpl                 = sum(instrument_type == "Development Policy Loan"),
    n_ipf                 = sum(instrument_type == "Investment Project Financing"),
    .groups = "drop"
  ) %>%
  arrange(approval_year)

cat("\nAnnual summary preview:\n")
print(annual_summary, n = 10)

# ---------------------------------------------------------------------------
# 6.  Save processed datasets
# ---------------------------------------------------------------------------

write_csv(projects,         here("data", "processed", "projects_all.csv"))
write_csv(projects_inscope, here("data", "processed", "projects_inscope.csv"))
write_csv(sector_long,      here("data", "processed", "sector_long.csv"))
write_csv(annual_summary,   here("data", "derived",   "annual_summary.csv"))

cat("\nSaved:\n")
cat("  data/processed/projects_all.csv\n")
cat("  data/processed/projects_inscope.csv\n")
cat("  data/processed/sector_long.csv\n")
cat("  data/derived/annual_summary.csv\n")
cat("\nData cleaning complete.\n")
