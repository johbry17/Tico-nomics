# Tico-nomics

The World Bank in Costa Rica: An exploratory analysis of development finance, sectoral priorities, and economic change, 1990–2024.

## Overview

This project uses the World Bank Projects API and World Development Indicators to trace 35 years of IBRD lending to Costa Rica — examining how sectoral priorities, instrument types, and development language have evolved across an era of structural adjustment, infrastructure investment, fiscal reform, and pandemic response.

**Working research question:** How has the sectoral and thematic distribution of World Bank lending to Costa Rica changed from 1990 to 2024, and what do these patterns reveal about evolving development priorities?

## Data

- [IBRD Loans Summary by Status](https://financesone.worldbank.org/ibrd-loans-summary-by-status/DS01535)
- [IBRD Statement of Loans and Guarantees - Latest Available Snapshot 10/31/26](https://financesone.worldbank.org/ibrd-statement-of-loans-and-guarantees-latest-available-snapshot/DS00047)
- [World Bank Group Projects & Operations](https://projects.worldbank.org/en/projects-operations/projects-summary) - List of projects in Costa Rica
- **World Bank Projects API:** 72 Costa Rica projects (all years); 25 confirmed in the 1990–2024 analysis window
  - Main: [all fields](https://search.worldbank.org/api/v2/projects?format=json&countrycode=CR&rows=100)
  - Filtered: [selected fields with abstracts](https://search.worldbank.org/api/v2/projects?format=json&countrycode=CR&rows=100&fl=id,project_name,totalamt,sector1,sector2,sector3,boardapprovaldate,closingdate,status,project_abstract)
- [International Finance Corporation Projects](https://disclosures.ifc.org/search?Country_Description=Costa+Rica&sortBy=Disclosed_Date&sortOrder=desc)
- [MIGA Projects](https://www.miga.org/projects?f%5B%5D=host_country%3A1882)
- **World Development Indicators:** GDP per capita, Gini index, life expectancy, adult literacy (via `wbstats`)

_[List of graduates from IDA](https://ida.worldbank.org/en/about/borrowing-countries/ida-graduates), with Costa Rica's last credit in FY62_

## Repository structure

```
Tico-nomics/
├── _quarto.yml               # Quarto website configuration
├── index.qmd                 # Site homepage
├── data/
│   ├── raw/                  # Original unmodified data files
│   ├── processed/            # Extracted and cleaned datasets
│   └── derived/              # Analytical summary tables
├── R/
│   ├── functions.R           # Reusable functions and ggplot2 theme
│   ├── 01_import_and_audit.R # Data inspection and feasibility audit
│   ├── 02_clean_projects.R   # Project data cleaning and preparation
│   ├── 03_wdi_data.R         # WDI retrieval and cleaning
│   ├── 04_eda.R              # Exploratory data analysis
│   └── 05_visualizations.R   # Publication-quality visualizations
├── reports/
│   ├── feasibility-audit.qmd # Data audit report
│   └── eda-report.qmd        # EDA findings report
├── figures/                  # Saved figures (PNG + SVG)
└── docs/                     # Rendered Quarto website output
```

## Rebuilding the analysis

Run scripts in order from the project root:

```r
source("R/01_import_and_audit.R")
source("R/02_clean_projects.R")
source("R/03_wdi_data.R")
source("R/04_eda.R")
source("R/05_visualizations.R")
```

Or render the full Quarto website:

```bash
quarto render
```

**Requirements:** R 4.4+, Quarto 1.4+, packages: `here`, `jsonlite`, `dplyr`, `tidyr`, `purrr`, `stringr`, `lubridate`, `ggplot2`, `scales`, `wbstats`, `readr`, `tidytext`, `forcats`, `glue`, `knitr`, `kableExtra`, `patchwork`, `ggtext`, `svglite`

## Key findings (preliminary)

1. **Temporal concentration:** Over 60% of confirmed 1990–2024 commitments were approved in 2020–2024, driven by large Development Policy Loans ($300–500M) for fiscal reform and decarbonization.
2. **Instrument shift:** The portfolio has moved from project-level Investment Project Financing toward budget-support Development Policy Loans — signaling a shift toward policy-conditioned budget support.
3. **Environmental continuity:** Environmental and climate themes appear in every lending period, evolving from standalone conservation projects to institutionalized fiscal policy (the Fiscal and Decarbonization DPL series, 2020–2023).
4. **A gap to investigate:** The 2013–2019 period appears underrepresented; 16 projects lack API approval dates and cannot be placed in the timeline.
5. **Independent development trajectory:** Costa Rica's strong development indicators appear largely independent of World Bank lending volume — consistent with the Bank's role as a supplemental financier in a middle-income country.

## Data limitations

- 16 of 72 projects lack approval dates in the API and cannot be placed in the time series
- Sector data absent for the 3 most recent 2024 approvals
- All amounts are nominal US dollars at time of commitment
- Adult literacy data has only 2 observations (2000, 2011); excluded from analysis

## Status

Exploratory analysis complete. See [Data Audit](reports/feasibility-audit.qmd) and [EDA Report](reports/eda-report.qmd) for details.