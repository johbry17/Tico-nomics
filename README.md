# Tico-nomics

**The World Bank Group in Costa Rica: Development Finance, Shifting Priorities, and 45 Years of an Evolving Relationship**

An exploratory, descriptive analysis of how the World Bank Group's financing relationship with Costa Rica changed from 1980 to 2025 — in scale, instruments, stated priorities, and institutional character.

---

## The central question

How did the World Bank Group's financing relationship with Costa Rica evolve from 1980 to 2025, and what does that evolution reveal about changing development priorities and the broader practice of international development finance?

The analysis is descriptive. It documents patterns in a 45-year lending and investment record. It does not establish that World Bank financing caused changes in Costa Rica's economic or social outcomes.

---

## Key findings

1. **The 2016 health insurance loan ($420M) was missing from earlier API-based analyses.** P148435, the Strengthening Universal Health Insurance project, was absent from the World Bank's V2 API extract. Its inclusion materially changes the picture of the 2010–2019 period.

2. **Policy-based lending defined the 1980s (~70% of that decade's principal), disappeared in the 2010s, and returned at scale in the 2020s.** The 1980s Structural Adjustment Loans (SAL I, II) and companion operations account for three development policy operations. The Fiscal and Decarbonization DPL series (2020–2023) accounts for six.

3. **In real 2025 dollars, earlier lending was larger than nominal figures suggest.** The 1980s operations (~$294M nominal) represent roughly $772M in 2025 dollars. The 2009 $500M DPL was approximately $728M in real terms.

4. **Abstract language analysis is limited by sparse coverage.** No project abstracts are available for the 1990s in the API extract. The 1980s and 2010s each have only 1–2 abstracts. Environment/climate keywords appear in the 2000s and 2020s but are absent in the 1980s — this reflects genuine project mix differences, but the small sample sizes limit interpretation.

5. **IFC's Costa Rica portfolio is concentrated in financial services**, with Reventazon HPP ($100M, 2012) as the largest non-financial investment.

---

## Institutional scope

| Institution | Role | Covered? |
|---|---|---|
| **IBRD** | Sovereign loans to middle-income governments | Core analysis |
| **IDA** | Concessional financing | Historical note only (CR graduated [FY1962](https://ida.worldbank.org/en/about/borrowing-countries/ida-graduates)) |
| **IFC** | Private-sector investment | Companion analysis |
| **MIGA** | Political-risk guarantees | Historical survey |

These institutions' financial measures are NOT comparable and are never summed.

---

## Published site

[https://johbry17.github.io/Tico-nomics/](https://johbry17.github.io/Tico-nomics/)

| Page | Contents |
|---|---|
| [Overview](reports/index.qmd) | The story — what happened across four decades |
| [Analysis](reports/eda-analysis.qmd) | Full tables, charts, and evidence |
| [Data](reports/data-dictionary.qmd) | Variable definitions and source documentation |
| [Methods](reports/methodology.qmd) | Research design, data decisions, and reproducibility |

---

## Data sources

| File | Source | Description |
|---|---|---|
| `ibrd_statement_of_loans_and_guarantees_*.csv` | [World Bank IBRD Statement DS00047](https://financesone.worldbank.org/ibrd-statement-of-loans-and-guarantees-latest-available-snapshot/DS00047) | Loan-level; snapshot 09/30/2026 |
| `Projects_and_Operations_List.xlsx` | [World Bank Projects Portal](https://projects.worldbank.org/en/projects-operations/projects-summary) | 115 CR projects; snapshot 11/01/2026 |
| `world_bank_costa_rica*.json` | [WB Search API V2](https://search.worldbank.org/api/v2/projects?format=json&countrycode=CR&rows=100) | 72-project extract (supplementary) |
| `IFC_Projects_Costa Rica.xlsx` | [IFC Project Disclosures](https://disclosures.ifc.org) | 30 unique projects |
| `World_Bank_Group_Guarantees_MIGA-projects.csv` | [MIGA Projects](https://www.miga.org/projects) | 12 records (10 deduplicated) |
| WDI indicators | [World Bank API](https://api.worldbank.org/v2/) | GDP, Gini, life expectancy, GDP growth |

All raw files are immutable and preserved unchanged.

---

## Repository structure

```
Tico-nomics/
├── _quarto.yml
├── README.md
├── data/
│   ├── raw/              # Unmodified source files
│   ├── processed/        # Cleaned analytical datasets
│   ├── derived/          # Summary tables
│   └── manual/           # Instrument classification overrides
├── R/
│   ├── functions.R       # Theme, palette, shared helpers
│   ├── 00_setup.R        # Packages and paths
│   ├── 01_import_sources.R
│   ├── 02_prepare_wdi.R  # Requires internet
│   ├── 03_reconcile_projects.R
│   ├── 04_prepare_ibrd.R
│   ├── 05_prepare_ifc.R
│   ├── 06_prepare_miga.R
│   ├── 07_analysis.R
│   ├── 08_visualizations.R
│   └── 09_validation.R
├── reports/              # Four public-facing Quarto pages
│   ├── index.qmd         # Overview
│   ├── eda-analysis.qmd  # Analysis
│   ├── data-dictionary.qmd
│   └── methodology.qmd
├── figures/              # Generated figures (PNG + SVG)
├── archive/              # Phase 0 and Phase 1 historical scripts
└── docs/                 # Rendered Quarto site
```

---

## Rebuilding the analysis

Run R scripts in order from the project root (R >= 4.4):

```r
source("R/02_prepare_wdi.R")       # requires internet connection
source("R/01_import_sources.R")    # source inspection, no outputs
source("R/03_reconcile_projects.R")
source("R/04_prepare_ibrd.R")
source("R/05_prepare_ifc.R")
source("R/06_prepare_miga.R")
source("R/07_analysis.R")
source("R/08_visualizations.R")
source("R/09_validation.R")        # all 55 checks should pass
```

Render the site (Quarto >= 1.4):

```bash
quarto render
```

**R packages:** `here`, `readr`, `readxl`, `dplyr`, `tidyr`, `purrr`, `stringr`, `lubridate`, `jsonlite`, `httr2`, `ggplot2`, `scales`, `patchwork`, `svglite`, `knitr`, `kableExtra`, `tidytext`, `forcats`, `glue`

---

## Data limitations

- The World Bank Projects API V2 returned only 72 of 115 Costa Rica projects. The Projects & Operations List (115 records) is the authoritative project universe for this analysis.
- Sector percentage data is unavailable for 13 of 35 in-scope IBRD projects. The 2020–2025 DPL series largely lacks sector data. Sector allocations represent approximately $2.2 billion of the $4.6 billion IBRD portfolio.
- Disbursement figures in the IBRD Statement are cumulative snapshot balances (09/30/2026), not annual flows.
- IFC investment dates are projected board dates, not confirmed approvals.
- MIGA gross exposure is maximum potential guarantee liability, not capital transferred.
- The 2025 GDP deflator and Gini values are preliminary; life expectancy is available only through 2024.

---

## Tools and technologies

- **R** — data preparation, analysis, and visualization
- **Quarto** — reproducible web publication
- **ggplot2** — custom figures
- **World Bank Open Data** — primary data sources
- **Git + GitHub** — version control and hosting

---

## License

[MIT](LICENSE)

Analysis and code by **Bryan C. Johns**