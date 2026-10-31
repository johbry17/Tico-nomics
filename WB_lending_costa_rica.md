
### Field Note F3: World Bank Lending in Costa Rica

**Working Title:** *Infrastructure, Reform, and Resilience: What the World Bank Has Financed in Costa Rica, 1990–2024*

**Research Question:**
- **Primary:** How has the sectoral and thematic distribution of World Bank lending to Costa Rica shifted across 35 years, and what do project patterns reveal about the country's changing development priorities and World Bank strategy?
- **Secondary:** How does Costa Rica's lending pattern compare to its macroeconomic trajectory (growth, debt, social indicators)?

---

**Significance**

Costa Rica is a genuine development success story — a country with strong biodiversity conservation, universal health coverage, and high human development relative to income — yet it has also relied significantly on external borrowing, including from the World Bank, particularly around fiscal reform periods. The World Bank's project-level data is publicly accessible and rich, covering project descriptions, sectors, amounts, and dates. Tracing 35 years of lending reveals which institutions and sectors the Bank (and by extension, Costa Rican governments) prioritized at each historical moment: SAPs in the 1990s, infrastructure in the 2000s, climate and education lending in the 2010s, COVID fiscal support in the 2020s.

Organizations that would value this: development institutions, Costa Rica policy researchers, civil society organizations, international development students and practitioners, researchers interested in the World Bank's country strategy evolution.

---

**Geographic and Temporal Scope**

- **Unit:** Project level (72 confirmed Costa Rica projects in the WB Projects API)
- **Geography:** Costa Rica
- **Years:** 1990–2024 (full confirmed WB portfolio)
- **Supplemental context:** WDI economic/social indicators for Costa Rica (confirmed via wbstats)

---

**Verified Data Inventory**

| Dataset | Direct URL | Key Variables | Coverage | Access | Notes |
|---------|-----------|---------------|----------|--------|-------|
| WB Projects API | `https://search.worldbank.org/api/v2/projects?format=json&countrycode=CR&rows=100` | Project name, ID, total amount, sector1–sector5, theme, board approval date, closing date, status, project description | 72 projects, 1944–2024 | Free, no auth | **Confirmed in session.** 72 projects returned. Rich text descriptions available. |
| WDI via wbstats | `wbstats::wb_data(c("NY.GDP.PCAP.CD","SI.POV.GINI","SP.DYN.LE00.IN","SE.ADT.LITR.ZS"), country="CR")` | GDP per capita, Gini, life expectancy, literacy | Costa Rica, 1960– | R package, CRAN confirmed | **Confirmed.** Supplemental context for the lending story. |

**Caveats:** The 72 projects include all Bank lending instruments (investment loans, development policy loans, CAT DDOs, technical assistance). Not all "projects" are equivalent in analytical weight — a $500M DPL is very different from a $5M technical assistance grant. The project description text is rich but in English (descriptions of projects from the 1990s may be sparse). Board approval dates are available; actual disbursement schedules are not in the Projects API.

---

**Proposed EDA**

**Language: R** (`httr`, `jsonlite`, `dplyr`, `ggplot2`, `lubridate`, `wbstats`, `stringr`, `scales`)

R is appropriate because the analysis starts with a JSON API call, requires date handling (`lubridate`), text-based sector classification (`stringr`), and visualization with ggplot2.

**Steps:**
1. **API pull (30 min):** Use `httr::GET()` or `jsonlite::fromJSON()` to fetch all 72 Costa Rica projects. Parse JSON to data frame. Clean sector columns (sector1–5 have `Name` and `Percent` subfields). Extract board approval year using `lubridate`.
2. **Data inspection (20 min):** Check sector completeness, identify null sectors, note large vs. small projects. Log-transform loan amounts for visualization.
3. **Timeline visualization (40 min):** Gantt-style chart or scatter plot of project amounts over time, colored by sector. Annotate key historical events (1994 financial crisis, 2008 global crisis, 2017–2020 fiscal consolidation, COVID 2020).
4. **Sector analysis (40 min):** Aggregate total lending by sector across three eras (pre-2010, 2010–2019, 2020–2024). Stacked bar. Has the Bank shifted from infrastructure to DPLs, or from social to climate?
5. **Instrument mix (30 min):** DPLs vs. investment loans vs. technical assistance. How has the balance shifted? DPLs (budget support) often imply macro-fiscal conditionality — their frequency tells a story.
6. **Supplemental macro context (30 min):** Pull 3–4 WDI indicators for Costa Rica. Two-panel plot: World Bank commitment volumes by year (left axis) vs. GDP growth rate or debt-to-GDP (right axis). Does lending peak during economic stress?
7. **Interpretation (60 min):** Narrative connecting lending patterns to Costa Rica's development story. Note the 5 most recent large projects (confirmed from API session): Education Results, Social Risk Management DPL, Climate Resilient Recovery, Fiscal DPL, CAT DDO — these tell a 2023–2024 policy moment story.

---

**Originality and Contribution**

Existing analyses: World Bank publishes a Costa Rica country page with portfolio summary. Academic papers analyze World Bank conditionality globally. Individual project evaluations exist. What is uncommon is a researcher doing their own data pull from the Projects API and constructing a visual narrative of a single country's 35-year lending trajectory.

**What this adds:** A reproducible, researcher-constructed EDA from the Projects API — demonstrating that the researcher can work with institutional data sources, not just pre-formatted teaching datasets. The Costa Rica connection is authentic (researcher has academic and research experience in country). The analysis differs entirely from the existing Floodlines or Capital Crashpad projects.

---

**Feasibility**

| Phase | Estimated Time |
|-------|---------------|
| API pull, parsing, initial inspection | 45 min |
| Data cleaning and sector classification | 60 min |
| Visualizations (4–5 charts) | 90 min |
| WDI supplemental pull and integration | 30 min |
| Interpretation and writing | 75 min |
| Quarto rendering and polish | 45 min |
| **Total** | **~6 hours** |

**Most likely obstacle:** Sector field parsing. The API returns sector data as JSON subobjects with Name and Percent fields; some projects have 5 sectors. Cleaning this into a flat format takes more time than expected if the JSON structure is irregular. Fallback: use only `sector1` (the primary sector) if sector5 data is sparse.

---

**Hiring Signal**

Demonstrates: API data acquisition (JSON), `lubridate` date handling, sector classification logic, multi-indicator context building, domain knowledge about World Bank instruments (DPLs, CAT DDOs, investment lending), Costa Rica-specific knowledge. 

Relevant roles: international development analyst, World Bank or IFC positions, development finance research, Central America-focused organizations, multilateral development bank support roles.

---

**Limitations**

- **72 projects is a small dataset.** Most visualizations will have N < 100. Statistical inference is not meaningful. This is descriptive storytelling, not regression analysis.
- **Project amounts vs. disbursements:** Board approval amounts differ from actual disbursements. DPLs (budget support) are typically drawn down quickly; investment loans may take years.
- **No project outcomes data.** The Projects API does not include project completion ratings or outcome assessments (those are in a separate ICR database, not easily accessible).
- **Sector classification is coarse.** The Bank's own sector taxonomy changed over time; comparing 1995 and 2024 sectors requires care.
- **Puerto Rico comparison is unavailable:** USVI and Puerto Rico are U.S. territories with no World Bank projects.

---


### F3: World Bank Lending in Costa Rica

**1. First action:**
- Open `https://search.worldbank.org/api/v2/projects?format=json&countrycode=CR&rows=100&fl=id,project_name,totalamt,sector1,sector2,sector3,boardapprovaldate,closingdate,status,project_abstract`
- This URL is confirmed working and returns 72 projects. Copy the JSON response into a text editor to inspect the structure before writing the R code.
- Secondary documentation: `https://projects.worldbank.org/en/projects-operations/projects-home?lang=en`

**2. First inspection:**
- Parse JSON: count total projects (confirmed 72). Check range of board approval dates (earliest and latest project).
- Inspect sector1 field — what values appear most frequently? Is sector data complete for all 72 projects?
- Check the distribution of `totalamt` — are there large outliers (e.g., $500M DPLs)?
- Note the 5 most recent projects from the confirmed API response: Education Results (2024), Social Risk Management DPL (2024), Climate Resilient Recovery (2024), Fiscal DPL (2023), CAT DDO (2023) — these are a ready-made set to profile.

**3. Initial EDA checks:**
1. Histogram of `totalamt` (log scale) — what's the typical project size?
2. Timeline scatter: projects over time colored by sector — any visible shift?
3. Count projects by decade (1990s, 2000s, 2010s, 2020s)
4. Total lending by decade — did the Bank lend more or less over time?
5. Pull WDI GDP growth for Costa Rica: `wb_data("NY.GDP.MKTP.KD.ZG", country="CR", start_date=1990, end_date=2024)` — annotate on lending timeline

**4. Technical setup:**
```r
library(jsonlite); library(dplyr); library(ggplot2); library(lubridate)
library(wbstats); library(scales); library(stringr)

url <- "https://search.worldbank.org/api/v2/projects?format=json&countrycode=CR&rows=100"
raw <- fromJSON(url)
projects <- raw$projects %>% bind_rows()  # flatten list of lists to tibble
projects$board_year <- year(as.Date(projects$boardapprovaldate))
projects$totalamt <- as.numeric(projects$totalamt)
```

**5. Feasibility test:**
- **GO** if: JSON parse works, sector1 column is populated for ≥50 of 72 projects, board approval dates span at least 1990–2023, and totalamt is non-null for the large projects.
- **NO-GO** (reframe question) if: Sector data is so sparse that sector analysis is impossible. Fallback: reframe as a lending timeline analysis only, supplemented by WDI context, without sector decomposition.

**6. Time budget:**
- Initial feasibility test: 20–30 minutes
- Complete Field Note: 5–7 hours

**7. Minimum viable deliverable:**
A Quarto HTML with: (a) timeline plot of WB project amounts by year, annotated with key Costa Rica events; (b) sector breakdown for the three most recent decades; (c) loan volume trend; (d) supplemental WDI macro context (GDP growth, debt); (e) 400-word interpretation connecting lending to development story. 72 projects is enough for a compelling narrative EDA even without deep statistical analysis.

**8. Principal failure mode:**
Sector field inconsistency. The WB sector taxonomy changed multiple times between 1990 and 2024. Projects approved in 1990 may use different sector codes than 2024 projects. The fallback is to group sectors into 5–6 broad categories manually, accepting some imprecision in the early years.

---
