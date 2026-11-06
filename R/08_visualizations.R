# 08_visualizations.R
# Publication-quality figures for the Tico-nomics project.
# Saves PNG + SVG to figures/. Uses theme_tico() from functions.R.

source(here::here("R", "00_setup.R"))

cat("\n========== VISUALIZATIONS ==========\n\n")

# ---------------------------------------------------------------------------
# 0.  Load data
# ---------------------------------------------------------------------------

ibrd_annual   <- read_csv(file.path(DER, "ibrd_annual.csv"),      show_col_types=FALSE)
ibrd_period   <- read_csv(file.path(DER, "ibrd_by_period.csv"),   show_col_types=FALSE)
ibrd_proj     <- read_csv(file.path(PROC,"ibrd_projects.csv"),    show_col_types=FALSE)
sec_period    <- read_csv(file.path(DER, "sector_by_period.csv"), show_col_types=FALSE)
instr_period  <- read_csv(file.path(DER, "instrument_by_period.csv"), show_col_types=FALSE)
theme_pres    <- read_csv(file.path(DER, "abstract_themes.csv"),  show_col_types=FALSE)
ifc_projects  <- read_csv(file.path(PROC,"ifc_projects.csv"),     show_col_types=FALSE)
miga_projects <- read_csv(file.path(PROC,"miga_projects.csv"),    show_col_types=FALSE)
wdi           <- read_csv(file.path(DER, "wdi_scope.csv"),        show_col_types=FALSE)
top10         <- read_csv(file.path(DER, "ibrd_top10.csv"),       show_col_types=FALSE)
word_counts   <- read_csv(file.path(DER, "word_counts.csv"),      show_col_types=FALSE)

period_levels <- c("1980–1989","1990–1999","2000–2009","2010–2019","2020–2025")
ibrd_proj_scope <- ibrd_proj |>
  filter(!is.na(approval_year), approval_year >= 1980, approval_year <= 2025) |>
  mutate(period = factor(period, levels=period_levels))

instr_period <- instr_period |>
  filter(period %in% period_levels) |>
  mutate(period = factor(period, levels=period_levels))

sec_period <- sec_period |>
  filter(period %in% period_levels) |>
  mutate(period = factor(period, levels=period_levels))

theme_pres <- theme_pres |>
  filter(period %in% period_levels) |>
  mutate(period = factor(period, levels=period_levels))

# ---------------------------------------------------------------------------
# FIG 1 — IBRD lending timeline: nominal and real commitments
# ---------------------------------------------------------------------------

cat("Rendering Fig 1 — IBRD timeline...\n")

fig1 <- ggplot(ibrd_annual, aes(x=approval_year)) +
  # Real 2025 USD (lighter fill)
  geom_col(aes(y=real_principal/1e6, fill="Real 2025 USD"), width=0.85, alpha=0.4) +
  # Nominal (solid bars in front)
  geom_col(aes(y=total_principal/1e6, fill="Nominal USD"), width=0.85, alpha=0.9) +
  scale_fill_manual(
    values = c("Nominal USD"="#1a6b72", "Real 2025 USD"="#c9a96e"),
    name   = "Amount",
    guide  = guide_legend(override.aes=list(alpha=c(0.9,0.4)))
  ) +
  scale_x_continuous(breaks=seq(1980,2025,5), expand=expansion(add=0.5)) +
  scale_y_continuous(labels=function(x) paste0("$",x,"M"), expand=expansion(mult=c(0,0.05))) +
  labs(
    title    = "IBRD Lending to Costa Rica, 1980–2025",
    subtitle = "Annual approved original principal — nominal and inflation-adjusted to 2025 USD",
    x        = "Year of board approval",
    y        = "Commitments (US$ millions)",
    caption  = caption_wb("Nominal = original principal at approval. Real = adjusted using US GDP deflator (NY.GDP.DEFL.ZS, base 2020=100).")
  ) +
  theme_tico() +
  theme(legend.position="top", legend.justification="left")

save_fig(fig1, "fig1_ibrd_timeline", width=11, height=6.5)
cat("  Saved fig1_ibrd_timeline\n")

# ---------------------------------------------------------------------------
# FIG 2 — Instrument mix by period
# ---------------------------------------------------------------------------

cat("Rendering Fig 2 — Instrument mix...\n")

instr_plot <- instr_period |>
  filter(instrument_group != "Other / Unknown") |>
  group_by(period) |>
  mutate(pct = round(100*total_principal/sum(total_principal), 0)) |>
  ungroup()

fig2 <- ggplot(instr_plot,
               aes(x=period, y=total_principal/1e6, fill=instrument_group)) +
  geom_col(width=0.72, colour=PALETTE$white, linewidth=0.25) +
  scale_fill_manual(
    values = c("Development Policy Loan"=PALETTE$amber,
               "Investment Project Financing"=PALETTE$teal),
    name   = "Instrument type"
  ) +
  scale_y_continuous(labels=function(x) paste0("$",x,"M"), expand=expansion(mult=c(0,0.05))) +
  labs(
    title    = "Instrument Mix: Investment Loans vs Development Policy Loans",
    subtitle = "IBRD commitments by period and instrument type (nominal USD)",
    x        = "Period", y = "Commitments (US$ millions, nominal)",
    caption  = caption_wb("DPLs provide budget support tied to policy conditions. IPF finances specific investment or programmatic activities.")
  ) +
  theme_tico() + theme(legend.position="bottom")

save_fig(fig2, "fig2_instrument_mix", width=10, height=6.5)
cat("  Saved fig2_instrument_mix\n")

# ---------------------------------------------------------------------------
# FIG 3 — Sectoral composition by period
# ---------------------------------------------------------------------------

cat("Rendering Fig 3 — Sector composition...\n")

TOP_SECTORS <- c("Transportation","Public Administration","Energy and Extractives",
                 "Finance","Education","Health","Water, Sanitation and Waste",
                 "Agriculture, Fishing & Forestry","Social Protection")

sec_plot <- sec_period |>
  filter(alloc_principal > 0) |>
  mutate(
    sg = if_else(sector_group %in% TOP_SECTORS, sector_group, "Other"),
    sg = factor(sg, levels=c(TOP_SECTORS,"Other","ICT / Trade"))
  ) |>
  group_by(period, sg) |>
  summarise(alloc_principal=sum(alloc_principal), .groups="drop") |>
  group_by(period) |> mutate(pct=alloc_principal/sum(alloc_principal)*100) |> ungroup()

sector_pal <- c(
  "Transportation"="1a6b72","Public Administration"="3d5166",
  "Energy and Extractives"="c87d2f","Finance"="c9a96e",
  "Education"="5a8a6a","Health"="b05060",
  "Water, Sanitation and Waste"="4e7fa0",
  "Agriculture, Fishing & Forestry"="7a9e5e",
  "Social Protection"="8b5c8c","ICT / Trade"="7a6244","Other"="c0bbb5"
)
sector_pal <- paste0("#", sector_pal)
names(sector_pal) <- names(c(
  "Transportation","Public Administration","Energy and Extractives","Finance",
  "Education","Health","Water, Sanitation and Waste",
  "Agriculture, Fishing & Forestry","Social Protection","ICT / Trade","Other"
))

fig3 <- ggplot(sec_plot, aes(x=period, y=pct, fill=sg)) +
  geom_col(width=0.72, colour=PALETTE$white, linewidth=0.3) +
  scale_fill_manual(values=SECTOR_COLOURS, name="Sector",
                    guide=guide_legend(ncol=2)) +
  scale_y_continuous(labels=label_percent(scale=1), expand=expansion(mult=c(0,0.02))) +
  labs(
    title    = "Where the Money Went: Sectoral Composition of IBRD Lending",
    subtitle = "Share of allocated commitments by sector group and approval period",
    x        = "Period", y = "Share of period commitments (%)",
    caption  = caption_wb("Based on sector percentages from Projects API. Projects without sector data excluded from allocation.")
  ) +
  theme_tico() + theme(legend.position="right")

save_fig(fig3, "fig3_sector_composition", width=11, height=7)
cat("  Saved fig3_sector_composition\n")

# ---------------------------------------------------------------------------
# FIG 4 — Top 10 projects
# ---------------------------------------------------------------------------

cat("Rendering Fig 4 — Top projects...\n")

top10_plot <- top10 |>
  mutate(
    label = str_trunc(project_name, 42),
    label = str_replace_all(label, c("Costa Rica "="","Strengthening "="Strengthng ")),
    total_principal_m = total_principal/1e6,
    real_principal_m  = real_principal_2025/1e6,
    period = factor(dplyr::case_when(
      approval_year <= 1989 ~ "1980–1989",
      approval_year <= 1999 ~ "1990–1999",
      approval_year <= 2009 ~ "2000–2009",
      approval_year <= 2019 ~ "2010–2019",
      TRUE                  ~ "2020–2025"
    ), levels=period_levels)
  ) |>
  arrange(total_principal_m) |>
  mutate(label=forcats::fct_inorder(label))

fig4 <- ggplot(top10_plot, aes(x=total_principal_m, y=label, fill=period)) +
  geom_col(width=0.7, alpha=0.9) +
  geom_text(aes(label=paste0("$",round(total_principal_m),"M  ",approval_year)),
            hjust=-0.05, size=3, colour=PALETTE$grey_dark) +
  scale_fill_manual(values=PERIOD_COLOURS, name="Period") +
  scale_x_continuous(labels=function(x) paste0("$",x,"M"), expand=expansion(mult=c(0,0.22))) +
  labs(
    title="The Ten Largest IBRD Projects in Costa Rica, 1980–2025",
    subtitle="By original principal amount at board approval (nominal USD)",
    x="IBRD original principal (US$ millions, nominal)", y=NULL,
    caption=caption_wb()
  ) +
  theme_tico() +
  theme(legend.position="bottom", panel.grid.major.y=element_blank(),
        axis.ticks.y=element_blank())

save_fig(fig4, "fig4_top_projects", width=11, height=7)
cat("  Saved fig4_top_projects\n")

# ---------------------------------------------------------------------------
# FIG 5 — Abstract themes over time
# ---------------------------------------------------------------------------

cat("Rendering Fig 5 — Abstract themes...\n")

theme_labels <- c(
  "Infrastructure"="Infrastructure",
  "Environment_Climate"="Environment & Climate",
  "Social_Development"="Social Development",
  "Governance_Fiscal"="Governance & Fiscal",
  "Competitiveness_Trade"="Competitiveness",
  "Disaster_Risk"="Disaster Risk"
)
theme_colours <- c(
  "Infrastructure"=PALETTE$teal,
  "Environment_Climate"="#5a8a6a",
  "Social_Development"=PALETTE$rose,
  "Governance_Fiscal"=PALETTE$slate,
  "Competitiveness_Trade"=PALETTE$amber,
  "Disaster_Risk"="#7a4488"
)

theme_plot <- theme_pres |>
  # Only plot rows where abstracts are available (pct is not NA)
  filter(has_abstracts) |>
  mutate(theme_label=theme_labels[theme])

fig5 <- ggplot(theme_plot, aes(x=period, y=pct, colour=theme, group=theme)) +
  geom_line(linewidth=1.1, alpha=0.9) +
  geom_point(aes(size=n_with_abstract), alpha=0.9) +
  scale_colour_manual(values=theme_colours,
                      labels=theme_labels, name="Thematic area",
                      guide=guide_legend(ncol=2, override.aes=list(size=3, linewidth=1.5))) +
  scale_size_continuous(range=c(3,7), guide="none") +
  scale_y_continuous(labels=label_percent(scale=1), limits=c(0,105), expand=expansion(mult=c(0,0))) +
  labs(
    title="Development Priorities in the Bank's Own Words",
    subtitle="% of projects with available abstracts that mention each thematic area\nNote: 1990–1999 excluded — no project abstracts available in the API extract for that period.",
    x="Period", y="Projects mentioning theme (% of abstracts available)",
    caption=caption_wb("Keyword search on project abstracts (WB Projects API). Point size = number of projects with abstracts in that period.")
  ) +
  theme_tico() + theme(legend.position="bottom")

save_fig(fig5, "fig5_abstract_themes", width=11, height=7)
cat("  Saved fig5_abstract_themes\n")

# ---------------------------------------------------------------------------
# FIG 6 — Costa Rica development trajectory
# ---------------------------------------------------------------------------

cat("Rendering Fig 6 — Development trajectory...\n")

p6a <- ggplot(wdi |> filter(!is.na(gdp_per_capita_usd)), aes(x=year,y=gdp_per_capita_usd)) +
  geom_area(fill=PALETTE$teal, alpha=0.15) +
  geom_line(colour=PALETTE$teal, linewidth=0.9) +
  scale_x_continuous(breaks=seq(1980,2025,5), expand=expansion(add=0.5)) +
  scale_y_continuous(labels=label_dollar(scale_cut=cut_short_scale()), expand=expansion(mult=c(0,0.08))) +
  labs(subtitle="GDP per capita (current US$ — not inflation-adjusted)", x=NULL, y=NULL) + theme_tico()

p6b <- ggplot(wdi |> filter(!is.na(life_expectancy_years)), aes(x=year,y=life_expectancy_years)) +
  geom_line(colour=PALETTE$slate, linewidth=0.9) +
  geom_point(colour=PALETTE$slate, size=1.5, shape=16) +
  scale_x_continuous(breaks=seq(1980,2025,5), expand=expansion(add=0.5)) +
  scale_y_continuous(labels=function(x) paste0(x," yrs"), limits=c(73,83),
                     expand=expansion(mult=c(0.02,0.05))) +
  labs(subtitle="Life expectancy at birth (years)", x=NULL, y=NULL) + theme_tico()

p6c <- ggplot(wdi |> filter(!is.na(gini)), aes(x=year,y=gini)) +
  geom_point(colour=PALETTE$amber, size=3, shape=18) +
  geom_line(colour=PALETTE$amber, linewidth=0.5, alpha=0.5) +
  scale_x_continuous(breaks=seq(1980,2025,5), expand=expansion(add=0.5)) +
  scale_y_continuous(limits=c(42,58), expand=expansion(mult=c(0,0.08))) +
  labs(subtitle="Gini index (income inequality)", x="Year", y=NULL) + theme_tico()

fig6 <- (p6a / p6b / p6c) +
  plot_annotation(
    title="Costa Rica's Development Trajectory, 1980–2025",
    subtitle="Context for the lending analysis — not evidence of causation.",
    caption=caption_wdi(),
    theme=theme_tico()
  )

save_fig(fig6, "fig6_development_trajectory", width=10, height=11)
cat("  Saved fig6_development_trajectory\n")

# ---------------------------------------------------------------------------
# FIG 7 — IFC investments by sector (companion)
# ---------------------------------------------------------------------------

cat("Rendering Fig 7 — IFC investments...\n")

ifc_scope <- ifc_projects |>
  filter(is_investment, in_scope, !is_pipeline, !is.na(ifc_invest_usd_m)) |>
  arrange(best_year)

fig7 <- ggplot(ifc_scope, aes(x=best_year, y=ifc_invest_usd_m, colour=sector_group)) +
  geom_segment(aes(xend=best_year, yend=0), linewidth=0.6, alpha=0.7) +
  geom_point(size=4, shape=16, alpha=0.9) +
  scale_colour_manual(values=c(
    "Financial Services"=PALETTE$slate,
    "Energy"=PALETTE$amber,
    "Tourism / Hospitality"=PALETTE$teal,
    "Agriculture / Food"="#5a8a6a",
    "Retail"=PALETTE$tan,
    "Transportation"="#4e7fa0",
    "Health"=PALETTE$rose,
    "Other"=PALETTE$grey_mid,
    "Unclassified"=PALETTE$grey_light
  ), name="Sector") +
  scale_x_continuous(breaks=seq(2000,2025,5), expand=expansion(add=0.5)) +
  scale_y_continuous(labels=function(x) paste0("$",x,"M"), expand=expansion(mult=c(0,0.08))) +
  labs(
    title="IFC Investment Portfolio in Costa Rica",
    subtitle="IFC own-account investment by sector and indicative board date (amounts in nominal USD millions)\nNot comparable to IBRD sovereign loan principal.",
    x="Indicative board approval year", y="IFC own-account investment (US$ millions)",
    caption=caption_ifc("Projected board dates used as approximate year indicator; actual approval years may differ. Excludes pipeline project Insulet CR (2026).")
  ) +
  theme_tico() + theme(legend.position="right")

save_fig(fig7, "fig7_ifc_investments", width=11, height=6.5)
cat("  Saved fig7_ifc_investments\n")

# ---------------------------------------------------------------------------
# FIG 8 — IBRD nominal vs real commitments by period (bar comparison)
# ---------------------------------------------------------------------------

cat("Rendering Fig 8 — Nominal vs real by period...\n")

period_plot <- ibrd_period |>
  filter(period %in% period_levels) |>
  mutate(period=factor(period, levels=period_levels)) |>
  select(period, nominal=total_principal, real=real_principal) |>
  pivot_longer(c(nominal,real), names_to="value_type", values_to="amount_m") |>
  mutate(amount_m = amount_m/1e6,
         value_type = if_else(value_type=="nominal","Nominal USD","Real 2025 USD"))

fig8 <- ggplot(period_plot, aes(x=period, y=amount_m, fill=value_type)) +
  geom_col(position=position_dodge(width=0.72), width=0.65, alpha=0.9) +
  scale_fill_manual(values=c("Nominal USD"=PALETTE$teal,"Real 2025 USD"=PALETTE$amber),
                    name=NULL) +
  scale_y_continuous(labels=function(x) paste0("$",x,"M"), expand=expansion(mult=c(0,0.06))) +
  labs(
    title="IBRD Commitments by Period: Nominal vs Real (2025 USD)",
    subtitle="The 2020–2025 surge is still the largest period in real terms, but earlier periods were larger in real value than nominal figures suggest.",
    x="Period", y="Total original principal (US$ millions)",
    caption=caption_wb("Real 2025 USD uses US GDP deflator NY.GDP.DEFL.ZS (base 2020=100). 2025 deflator is preliminary.")
  ) +
  theme_tico() + theme(legend.position="top", legend.justification="left")

save_fig(fig8, "fig8_nominal_vs_real", width=10, height=6.5)
cat("  Saved fig8_nominal_vs_real\n")

cat("\nAll figures saved to figures/\n")
cat("Visualizations complete.\n")
