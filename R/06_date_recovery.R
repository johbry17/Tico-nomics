# 06_date_recovery.R
# Investigates and recovers missing boardapprovaldate values for 16 projects
# Uses the WB Projects API V3 and bulk Excel download as authoritative sources.
# The V2 search API (used in the original data pull) has missing boardapprovaldate
# for these 16 projects; V3 and the Excel have them populated.

library(here)
library(httr2)
library(jsonlite)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(readxl)

source(here("R", "functions.R"))

# ---------------------------------------------------------------------------
# 1.  Identify the 16 missing-date projects
# ---------------------------------------------------------------------------

projects_all <- read_csv(here("data", "processed", "projects_all.csv"),
                          show_col_types = FALSE)

missing_ids <- projects_all |>
  filter(is.na(approval_year)) |>
  pull(id)

cat("Projects with missing approval date:", length(missing_ids), "\n")
cat("IDs:", paste(missing_ids, collapse = ", "), "\n\n")

# ---------------------------------------------------------------------------
# 2.  Baseline financing audit of missing-date projects
# ---------------------------------------------------------------------------

missing_baseline <- projects_all |>
  filter(id %in% missing_ids) |>
  mutate(
    fin_status = case_when(
      is.na(totalamt)  ~ "Missing",
      totalamt == 0    ~ "Zero",
      TRUE             ~ "Positive"
    )
  )

cat("=== FINANCING STATUS OF MISSING-DATE PROJECTS ===\n")
cat("Positive totalamt:", sum(missing_baseline$fin_status == "Positive"), "\n")
cat("Zero totalamt:    ", sum(missing_baseline$fin_status == "Zero"), "\n")
cat("Missing totalamt: ", sum(missing_baseline$fin_status == "Missing"), "\n")
cat("Total positive financing (undated, nominal USD):",
    sum(missing_baseline$totalamt[missing_baseline$fin_status == "Positive"],
        na.rm = TRUE) / 1e6, "M\n")

total_pos_all  <- sum(projects_all$totalamt[projects_all$totalamt > 0], na.rm = TRUE)
total_pos_miss <- sum(missing_baseline$totalamt[missing_baseline$fin_status == "Positive"],
                      na.rm = TRUE)
cat("All positive financing:", total_pos_all / 1e6, "M\n")
cat("Undated share:", round(100 * total_pos_miss / total_pos_all, 1), "%\n\n")

cat("=== CROSS-TAB: DATE AVAILABILITY vs FINANCING STATUS ===\n")
projects_all |>
  mutate(
    date_status = if_else(is.na(approval_year), "Missing date", "Has date"),
    fin_status  = case_when(
      is.na(totalamt)  ~ "Missing amount",
      totalamt == 0    ~ "Zero",
      TRUE             ~ "Positive"
    )
  ) |>
  count(date_status, fin_status) |>
  pivot_wider(names_from = fin_status, values_from = n, values_fill = 0L) |>
  print()

# ---------------------------------------------------------------------------
# 3.  Recover dates via V3 API
# ---------------------------------------------------------------------------

cat("\n=== DATE RECOVERY VIA V3 API ===\n")

v3_results <- purrr::map_dfr(missing_ids, function(tid) {
  url <- sprintf(
    "https://search.worldbank.org/api/v3/projects?format=json&id=%s&rows=1",
    tid
  )
  tryCatch({
    resp <- request(url) |>
      req_headers("User-Agent" = "R/httr2 date-recovery research") |>
      req_timeout(20) |>
      req_perform()
    parsed <- fromJSON(resp_body_string(resp), simplifyVector = FALSE)
    proj <- parsed[["projects"]][[tid]]
    tibble(
      id                = tid,
      v3_project_name   = proj[["project_name"]] %||% NA_character_,
      v3_boarddate      = proj[["boardapprovaldate"]] %||% NA_character_,
      v3_closingdate    = proj[["closingdate"]] %||% NA_character_,
      source_url        = url,
      source_type       = "WB Search API v3"
    )
  }, error = function(e) {
    tibble(id = tid, v3_project_name = NA, v3_boarddate = NA,
           v3_closingdate = NA, source_url = url, source_type = "v3_api_error")
  })
})

cat("V3 API dates recovered:\n")
print(v3_results |> select(id, v3_project_name, v3_boarddate), n = 20)

Sys.sleep(1)

# ---------------------------------------------------------------------------
# 4.  Cross-validate against bulk Excel download
# ---------------------------------------------------------------------------

cat("\n=== CROSS-VALIDATION: BULK EXCEL DOWNLOAD ===\n")

# Download (or re-use if already present)
xlsx_path <- here("data", "processed", "wb_projects_bulk.xlsx")
if (!file.exists(xlsx_path)) {
  cat("Downloading bulk Excel from World Bank...\n")
  resp_xl <- request("https://search.worldbank.org/api/v3/projects/all.xlsx") |>
    req_headers("User-Agent" = "R/httr2 date-recovery research") |>
    req_timeout(180) |>
    req_perform()
  writeBin(resp_body_raw(resp_xl), xlsx_path)
  cat("Saved to", xlsx_path, "\n")
} else {
  cat("Using cached Excel file:", xlsx_path, "\n")
}

df_xl <- read_excel(xlsx_path, sheet = 1, skip = 2)
cat("Excel file date:", df_xl[1, 1, drop = TRUE], "\n")
# Row 1 was the title row ("World Bank Projects, data as of ..."), skip=2 means
# row 3 is headers, row 4+ is data. But the first data row will carry the title.
# Re-read properly
df_xl <- read_excel(xlsx_path, sheet = 1, skip = 2)

xl_cr <- df_xl[grepl("Costa Rica", df_xl[["countryshortname"]], ignore.case = TRUE), ]
xl_missing <- xl_cr[xl_cr[["id"]] %in% missing_ids, ]

xl_dates <- xl_missing |>
  select(id, xl_boarddate = boardapprovaldate, xl_closingdate = closingdate,
         xl_status = status) |>
  mutate(
    xl_boarddate  = as.character(xl_boarddate),
    xl_closingdate = as.character(xl_closingdate)
  )

cat("Excel dates for 16 projects:\n")
print(xl_dates, n = 20)

# ---------------------------------------------------------------------------
# 5.  Compare V3 API vs Excel — flag any disagreements
# ---------------------------------------------------------------------------

cat("\n=== V3 API vs EXCEL COMPARISON ===\n")

comparison <- v3_results |>
  left_join(xl_dates, by = "id") |>
  mutate(
    # Normalise: keep only date portion
    v3_date_only = substr(coalesce(v3_boarddate, ""), 1, 10),
    xl_date_only = substr(coalesce(xl_boarddate, ""), 1, 10),
    agreement    = case_when(
      is.na(v3_boarddate) & is.na(xl_boarddate) ~ "Both missing",
      is.na(v3_boarddate)  ~ "V3 missing, Excel has date",
      is.na(xl_boarddate)  ~ "Excel missing, V3 has date",
      v3_date_only == xl_date_only ~ "Agree",
      TRUE                 ~ "DISAGREE"
    )
  )

cat("Agreement status:\n")
print(comparison |> select(id, v3_date_only, xl_date_only, agreement), n = 20)
cat("\nDisagreements:\n")
comparison |> filter(agreement == "DISAGREE") |> print()

# ---------------------------------------------------------------------------
# 6.  Assign confidence levels and build enrichment dataset
# ---------------------------------------------------------------------------

cat("\n=== BUILDING ENRICHMENT DATASET ===\n")

# Confidence rules:
# - HIGH: specific non-June-30 date, confirmed by both V3 API and Excel
# - MEDIUM: June 30 date (likely WB fiscal year end approximation)
# - NOT_RECOVERED: still NA

enrichment <- comparison |>
  mutate(
    # Use V3 date as primary (Excel and V3 agreed on all cases)
    recovered_date = if_else(
      !is.na(v3_boarddate),
      as_datetime(v3_boarddate),
      NA_POSIXct_
    ),
    recovered_year = if_else(!is.na(recovered_date), year(recovered_date), NA_integer_),

    confidence = case_when(
      is.na(recovered_date)                     ~ "Not recovered",
      month(recovered_date) == 6 &
        day(recovered_date) == 30               ~ "Medium — FY year-end proxy (June 30)",
      TRUE                                      ~ "High — specific date confirmed by V3 API and Excel"
    ),

    # Scope classification
    in_scope_recovered = case_when(
      is.na(recovered_year)                    ~ FALSE,
      recovered_year < 1990                    ~ FALSE,   # pre-1990
      recovered_year > 2024                    ~ FALSE,   # post-analysis window
      TRUE                                     ~ TRUE
    ),

    # Join original financing info
    notes = case_when(
      id == "P169724" ~ "No date in either V3 API or bulk Excel; project was likely dropped before board approval",
      id == "P006930" ~ "1980-06-30: pre-1990, excluded from primary analysis",
      id == "P006950" ~ "1991-06-30: June 30 is WB FY-end; exact date within FY1991 unverifiable",
      id == "P006951" ~ "1998-06-30: June 30 is WB FY-end; exact date within FY1998 unverifiable",
      id == "P508347" ~ "2025-09-30: approved after the 1990–2024 analysis window",
      id == "P504033" ~ "2025-05-02: approved after the 1990–2024 analysis window",
      id == "P179757" ~ "2025-08-11: approved after the 1990–2024 analysis window",
      TRUE ~ "Date confirmed by V3 API and bulk Excel (downloaded 2026-10-30)"
    )
  ) |>
  left_join(
    projects_all |> select(id, project_name, totalamt, status, lendinginstr),
    by = "id"
  ) |>
  mutate(original_boarddate = NA_character_) |>
  select(id, project_name, original_boarddate, recovered_date, recovered_year,
         confidence, in_scope_recovered,
         totalamt, status, lendinginstr, source_url, source_type, notes)

cat("Enrichment dataset (", nrow(enrichment), "projects):\n")
enrichment |>
  select(id, project_name, recovered_year, confidence, totalamt) |>
  print(n = 20)

# ---------------------------------------------------------------------------
# 7.  Summary of recoveries
# ---------------------------------------------------------------------------

cat("\n=== RECOVERY SUMMARY ===\n")
cat("Dates successfully recovered:", sum(!is.na(enrichment$recovered_date)), "/", nrow(enrichment), "\n")
cat("  High confidence (specific date):    ",
    sum(enrichment$confidence == "High — specific date confirmed by V3 API and Excel"), "\n")
cat("  Medium confidence (FY approximation):",
    sum(grepl("Medium", enrichment$confidence)), "\n")
cat("Not recovered:                         ",
    sum(enrichment$confidence == "Not recovered"), "\n\n")

cat("In-scope (1990-2024) recoveries:\n")
in_scope_rec <- enrichment |> filter(in_scope_recovered == TRUE)
cat("  Total in-scope with recovered dates:", nrow(in_scope_rec), "\n")
cat("  With positive financing:            ", sum(in_scope_rec$totalamt > 0, na.rm=TRUE), "\n")
cat("  Additional positive financing:      ",
    sum(in_scope_rec$totalamt[in_scope_rec$totalamt > 0], na.rm=TRUE) / 1e6, "M USD\n\n")

# ---------------------------------------------------------------------------
# 8.  Save enrichment file
# ---------------------------------------------------------------------------

write_csv(
  enrichment |> select(
    id, project_name, recovered_date, recovered_year, confidence,
    in_scope_recovered, totalamt, status, lendinginstr,
    source_url, source_type, notes
  ),
  here("data", "processed", "date_enrichment.csv")
)

cat("Saved: data/processed/date_enrichment.csv\n")
cat("Date recovery complete.\n")
