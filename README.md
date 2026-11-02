# Tico-nomics

**The World Bank Group in Costa Rica: Development Finance, Shifting Priorities, and 45 Years of an Evolving Relationship**

An exploratory, descriptive analysis of how the World Bank Group's financing relationship with Costa Rica changed from 1980 to 2025.

---

## Overview

Between 1980 and 2025, the World Bank's IBRD approved 35 loans to Costa Rica with a total original principal of approximately $4.6 billion (nominal USD) — or roughly $6.2 billion in constant 2025 dollars. Across the same period, IFC made approximately $874 million in own-account investments in Costa Rican private enterprises, and MIGA issued historical guarantees with a total peak exposure of approximately $336 million.

**Central question:** How did the World Bank Group's financing relationship with Costa Rica evolve from 1980 to 2025, and what does that evolution reveal about changing development priorities, financing instruments, and the broader character of international development finance?

---

## Institutional scope

| Institution | Role | Covered? |
|---|---|---|
| **IBRD** | Sovereign loans to middle-income governments | Core analysis |
| **IDA** | Concessional financing | Historical note only (CR graduated FY1962) |
| **IFC** | Private-sector investment | Companion analysis |
| **MIGA** | Political-risk guarantees | Historical survey |

IDA graduates page: https://ida.worldbank.org/en/about/borrowing-countries/ida-graduates

These institutions' financial measures are NOT comparable and are never summed.

---

## Data sources

| File | Source | Description |
|---|---|---|
| `ibrd_statement_of_loans_and_guarantees_*.csv` | World Bank IBRD Statement DS00047 | Loan-level; snapshot 09/30/2026 |
| `Projects_and_Operations_List.xlsx` | World Bank Projects Portal | 115 Costa Rica projects; snapshot 11/01/2026 |
| `world_bank_costa_rica*.json` | WB Search API V2 | 72-project supplementary extract |
| `IFC_Projects_Costa Rica.xlsx` | IFC Project Disclosures | 30 projects |
| `World_Bank_Group_Guarantees_MIGA-projects.csv` | MIGA Project Briefs | 12 records (10 deduplicated) |

All raw files are immutable. Do not modify them.

---

## Repository structure

```
Tico-nomics/
├── _quarto.yml
├── README.md
├── index.qmd
├── data/
│   ├── raw/              # Unmodified source files
│   ├── processed/        # Cleaned datasets
│   └── derived/          # Summary tables
├── R/
│   ├── functions.R
│   ├── 00_setup.R
│   ├── 01_import_sources.R
│   ├── 02_reconcile_projects.R
│   ├── 03_prepare_ibrd.R
│   ├── 04_prepare_ifc.R
│   ├── 05_prepare_miga.R
│   ├── 06_prepare_wdi.R
│   ├── 07_analysis.R
│   ├── 08_visualizations.R
│   └── 09_validation.R
├── reports/
│   ├── field-note.qmd
│   ├── eda-analysis.qmd
│   ├── data-dictionary.qmd
│   └── methodology.qmd
├── figures/
├── archive/
│   ├── phase-0-feasibility/
│   ├── phase-1-date-recovery/
│   └── phase-1-final-audit/
└── docs/
```

---

## Rebuilding the analysis

```r
source("R/06_prepare_wdi.R")       # requires internet
source("R/01_import_sources.R")
source("R/02_reconcile_projects.R")
source("R/03_prepare_ibrd.R")
source("R/04_prepare_ifc.R")
source("R/05_prepare_miga.R")
source("R/07_analysis.R")
source("R/08_visualizations.R")
source("R/09_validation.R")
```

```bash
quarto render
```

**Requirements:** R >= 4.4, Quarto >= 1.4. Packages: `here`, `readr`, `readxl`, `dplyr`, `tidyr`, `purrr`, `stringr`, `lubridate`, `jsonlite`, `httr2`, `ggplot2`, `scales`, `patchwork`, `svglite`, `knitr`, `kableExtra`, `tidytext`, `forcats`, `glue`

---

## Key findings

1. **P148435 ($420M Health Insurance, 2016)** was missing from earlier API-based analyses. Its inclusion changes the picture of the 2010-2019 period substantially.
2. **Instrument mix shifts across four periods**: Policy-based lending dominated the 1980s (~80%), disappeared in the 2010s, and returned at scale in the 2020s tied to fiscal-climate policy.
3. **Real lending is more evenly distributed**: In 2025 USD, earlier periods (1980s SAL era, 2009 $500M DPL) are larger relative to recent lending than nominal figures suggest.
4. **Environmental themes appear in every period** but change character: from discrete PES/conservation projects to fiscal-climate integration in the 2020–2023 Decarbonization DPL series.
5. **IFC's CR portfolio is concentrated in financial services**, with Reventazon HPP ($100M, 2012) as the largest energy-sector investment.

---

## Data limitations

- WB Projects API V2 returned only 72 of 115 Costa Rica projects; the Projects & Operations List is authoritative.
- Sector data unavailable for 13 of 35 in-scope IBRD projects.
- Disbursements in the IBRD Statement are cumulative snapshot balances (09/30/2026), not annual flows.
- IFC investment dates are projected board dates, not confirmed approvals.
- MIGA gross exposure is maximum potential liability, not capital transferred.
- 2025 GDP deflator is preliminary.
