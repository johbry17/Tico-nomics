# 05_visualizations.R
# Publication-quality visualizations for the Tico-nomics project
# All figures saved to figures/ as both PNG and SVG

library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(forcats)
library(readr)
library(lubridate)
library(ggplot2)
library(scales)
library(patchwork)
library(ggtext)
library(svglite)

source(here("R", "functions.R"))

# ---------------------------------------------------------------------------
# 0.  Load data
# ---------------------------------------------------------------------------

projects     <- read_csv(here("data", "processed", "projects_inscope.csv"),
                          show_col_types = FALSE)
sector_long  <- read_csv(here("data", "processed", "sector_long.csv"),
                          show_col_types = FALSE)
annual_sum   <- read_csv(here("data", "derived", "annual_summary.csv"),
                          show_col_types = FALSE)
sector_by_yr <- read_csv(here("data", "derived", "sector_by_year.csv"),
                          show_col_types = FALSE)
sec_period   <- read_csv(here("data", "derived", "sector_by_period.csv"),
                          show_col_types = FALSE)
theme_pres   <- read_csv(here("data", "derived", "theme_presence.csv"),
                          show_col_types = FALSE)
word_period  <- read_csv(here("data", "derived", "word_by_period.csv"),
                          show_col_types = FALSE)
wdi          <- read_csv(here("data", "processed", "wdi_scope.csv"),
                          show_col_types = FALSE)
top_proj     <- read_csv(here("data", "derived", "top_projects.csv"),
                          show_col_types = FALSE)

# Ensure period factor order throughout
period_levels <- c("1990–1999", "2000–2009", "2010–2019", "2020–2024")

projects <- projects %>%
  mutate(period = factor(period, levels = period_levels))

sec_period <- sec_period %>%
  filter(period %in% period_levels) %>%
  mutate(period = factor(period, levels = period_levels))

theme_pres <- theme_pres %>%
  filter(period %in% period_levels) %>%
  mutate(period = factor(period, levels = period_levels))

word_period <- word_period %>%
  filter(period %in% period_levels) %>%
  mutate(period = factor(period, levels = period_levels))

# Fill missing years (zero projects / zero financing)
all_years <- tibble(approval_year = 1990:2024)
annual_plot <- all_years %>%
  left_join(annual_sum, by = "approval_year") %>%
  mutate(
    n_projects             = coalesce(n_projects, 0L),
    n_with_financing       = coalesce(n_with_financing, 0L),
    total_commitments_usd  = coalesce(total_commitments_usd, 0)
  )

# ---------------------------------------------------------------------------
# Helper: unified x-axis for time-series plots
# ---------------------------------------------------------------------------

scale_x_years <- function(breaks = seq(1990, 2024, by = 5)) {
  scale_x_continuous(
    breaks = breaks,
    minor_breaks = seq(1990, 2024, by = 1),
    limits = c(1989, 2025),
    expand = expansion(add = c(0.5, 0.5))
  )
}

# ---------------------------------------------------------------------------
# FIG 1 — Portfolio over time: approvals and annual financing
# ---------------------------------------------------------------------------

cat("Rendering Figure 1 — Portfolio over time...\n")

# Top panel: annual financing as area
p1a <- ggplot(annual_plot, aes(x = approval_year, y = total_commitments_usd)) +
  geom_area(fill = PALETTE$teal, alpha = 0.18) +
  geom_line(colour = PALETTE$teal, linewidth = 0.9) +
  geom_point(
    data = annual_plot %>% filter(total_commitments_usd > 0),
    colour = PALETTE$teal, size = 2, shape = 16
  ) +
  scale_x_years() +
  scale_y_continuous(
    labels  = function(x) fmt_usd_axis(x),
    expand  = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title    = "World Bank Lending to Costa Rica, 1990–2024",
    subtitle = "Annual IBRD commitment volume, nominal US dollars",
    x        = NULL,
    y        = "Commitments (USD)",
    caption  = caption_wb("totalamt field; nominal USD at time of commitment. Three recent projects lack sector data but are included in totals.")
  ) +
  theme_tico()

# Bottom panel: project count as lollipop
p1b <- ggplot(annual_plot, aes(x = approval_year, y = n_projects)) +
  geom_segment(aes(xend = approval_year, y = 0, yend = n_projects),
               colour = PALETTE$slate, linewidth = 0.7, alpha = 0.7) +
  geom_point(colour = PALETTE$slate, size = 3, shape = 16) +
  scale_x_years() +
  scale_y_continuous(breaks = 0:6, expand = expansion(mult = c(0, 0.1))) +
  labs(
    x        = "Year of Board Approval",
    y        = "Projects approved",
    subtitle = "Number of projects approved per year"
  ) +
  theme_tico() +
  theme(plot.title = element_blank())

fig1 <- p1a / p1b + plot_layout(heights = c(2, 1))
save_fig(fig1, "fig1_portfolio_over_time", width = 10, height = 7.5)
cat("  Saved fig1_portfolio_over_time\n")

# ---------------------------------------------------------------------------
# FIG 2 — Distribution of project sizes
# ---------------------------------------------------------------------------

cat("Rendering Figure 2 — Project size distribution...\n")

size_data <- projects %>%
  filter(has_financing == TRUE) %>%
  mutate(
    totalamt_m = totalamt / 1e6,
    period     = factor(period, levels = period_levels)
  )

fig2 <- ggplot(size_data, aes(x = totalamt_m, fill = period, colour = period)) +
  geom_dotplot(
    binwidth    = 15,
    dotsize     = 0.85,
    stackgroups = FALSE,
    alpha       = 0.8,
    method      = "histodot"
  ) +
  scale_fill_manual(values = PERIOD_COLOURS, name = "Period") +
  scale_colour_manual(values = PERIOD_COLOURS, name = "Period") +
  scale_x_continuous(
    breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500),
    labels = function(x) paste0("$", x, "M")
  ) +
  scale_y_continuous(labels = NULL) +
  labs(
    title    = "Distribution of World Bank Project Sizes",
    subtitle = "Each dot represents one project; coloured by approval period",
    x        = "IBRD commitment at approval (US$ millions, nominal)",
    y        = NULL,
    caption  = caption_wb("Excludes 15 projects with zero reported commitment (grants, technical assistance, or cancelled projects).")
  ) +
  theme_tico() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())

save_fig(fig2, "fig2_project_size_distribution", width = 10, height = 5.5)
cat("  Saved fig2_project_size_distribution\n")

# ---------------------------------------------------------------------------
# FIG 3 — Sector composition over time (stacked bar by period)
# ---------------------------------------------------------------------------

cat("Rendering Figure 3 — Sectoral composition...\n")

# Prepare sector data: use allocated amounts where available; otherwise project counts
# Keep top N sector groups; merge remainder into "Other"
TOP_SECTORS <- c(
  "Transportation",
  "Public Administration",
  "Energy and Extractives",
  "Finance",
  "Education",
  "Health",
  "Water, Sanitation and Waste",
  "Agriculture, Fishing & Forestry",
  "Social Protection"
)

sec_plot <- sec_period %>%
  filter(total_alloc > 0) %>%
  mutate(
    sector_grp_clean = if_else(sector_group %in% TOP_SECTORS, sector_group, "Other"),
    sector_grp_clean = factor(sector_grp_clean,
                              levels = c(TOP_SECTORS, "Other", "ICT / Trade"))
  ) %>%
  group_by(period, sector_grp_clean) %>%
  summarise(total_alloc = sum(total_alloc), .groups = "drop") %>%
  group_by(period) %>%
  mutate(pct = total_alloc / sum(total_alloc) * 100) %>%
  ungroup()

sector_pal <- c(
  "Transportation"               = "#1a6b72",
  "Public Administration"        = "#3d5166",
  "Energy and Extractives"       = "#c87d2f",
  "Finance"                      = "#c9a96e",
  "Education"                    = "#5a8a6a",
  "Health"                       = "#b05060",
  "Water, Sanitation and Waste"  = "#4e7fa0",
  "Agriculture, Fishing & Forestry" = "#7a9e5e",
  "Social Protection"            = "#8b5c8c",
  "ICT / Trade"                  = "#7a6244",
  "Other"                        = "#c0bbb5"
)

fig3 <- ggplot(sec_plot,
               aes(x = period, y = pct,
                   fill = sector_grp_clean)) +
  geom_col(width = 0.72, colour = PALETTE$white, linewidth = 0.3) +
  scale_fill_manual(values = sector_pal,
                    name   = "Sector",
                    guide  = guide_legend(ncol = 2, reverse = FALSE)) +
  scale_y_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(
    title    = "Where the Money Went: Sectoral Composition of World Bank Lending",
    subtitle = "Share of allocated IBRD commitments by sector group and approval period",
    x        = "Approval period",
    y        = "Share of period commitments (%)",
    caption  = caption_wb("Based on sector percentage allocations from the Projects API. Projects lacking sector percentages are excluded from allocation calculations.")
  ) +
  theme_tico() +
  theme(legend.position = "right",
        legend.box      = "vertical")

save_fig(fig3, "fig3_sector_composition", width = 10, height = 6.5)
cat("  Saved fig3_sector_composition\n")

# ---------------------------------------------------------------------------
# FIG 4 — Top 12 projects (horizontal bar)
# ---------------------------------------------------------------------------

cat("Rendering Figure 4 — Top projects...\n")

top12 <- projects %>%
  filter(has_financing == TRUE) %>%
  arrange(desc(totalamt)) %>%
  head(12) %>%
  mutate(
    label  = str_wrap(project_name, 42),
    # Short label for chart
    label_short = str_wrap(
      str_replace_all(project_name,
        c("Costa Rica " = "", " Project" = "", " Loan" = "",
          "Management " = "Mgmt. ", "Development Policy" = "DPL")),
      38),
    totalamt_m = totalamt / 1e6,
    period_col = as.character(period)
  ) %>%
  arrange(totalamt) %>%
  mutate(label_short = fct_inorder(label_short))

fig4 <- ggplot(top12, aes(x = totalamt_m, y = label_short, fill = period)) +
  geom_col(width = 0.7, alpha = 0.9) +
  geom_text(aes(label = paste0("$", round(totalamt_m), "M")),
            hjust = -0.1, size = 3.1, colour = PALETTE$grey_dark, family = "sans") +
  scale_fill_manual(values = PERIOD_COLOURS, name = "Approval period") +
  scale_x_continuous(
    labels = function(x) paste0("$", x, "M"),
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title    = "The Largest World Bank Projects in Costa Rica",
    subtitle = "Top 12 projects by IBRD commitment at approval, 1990–2024",
    x        = "IBRD commitment (US$ millions, nominal)",
    y        = NULL,
    caption  = caption_wb()
  ) +
  theme_tico() +
  theme(
    legend.position  = "bottom",
    panel.grid.major.y = element_blank(),
    axis.ticks.y      = element_blank()
  )

save_fig(fig4, "fig4_top_projects", width = 11, height = 7)
cat("  Saved fig4_top_projects\n")

# ---------------------------------------------------------------------------
# FIG 5 — Development priorities in abstracts: theme presence over time
# ---------------------------------------------------------------------------

cat("Rendering Figure 5 — Abstract theme presence...\n")

theme_clean <- theme_pres %>%
  filter(!is.na(period)) %>%
  mutate(
    theme_label = str_replace_all(theme, "_", " & ") %>%
                  str_replace("Governance & Fiscal", "Governance / Fiscal") %>%
                  str_replace("Social & Development", "Social Development") %>%
                  str_replace("Competitiveness & Trade", "Competitiveness / Trade"),
    theme_label = case_when(
      theme == "Infrastructure"        ~ "Infrastructure",
      theme == "Environment_Climate"   ~ "Environment & Climate",
      theme == "Social_Development"    ~ "Social Development",
      theme == "Governance_Fiscal"     ~ "Governance & Fiscal",
      theme == "Competitiveness_Trade" ~ "Competitiveness & Trade",
      theme == "Disaster_Risk"         ~ "Disaster Risk & Resilience",
      TRUE ~ theme
    )
  )

theme_pal <- c(
  "Infrastructure"             = PALETTE$teal,
  "Environment & Climate"      = "#5a8a6a",
  "Social Development"         = "#b05060",
  "Governance & Fiscal"        = PALETTE$slate,
  "Competitiveness & Trade"    = PALETTE$amber,
  "Disaster Risk & Resilience" = "#7a4488"
)

fig5 <- ggplot(theme_clean,
               aes(x = period, y = pct_with_theme,
                   colour = theme_label, group = theme_label)) +
  geom_line(linewidth = 1.1, alpha = 0.85) +
  geom_point(aes(size = n_projects), alpha = 0.9) +
  scale_colour_manual(
    values = theme_pal,
    name   = "Thematic area",
    guide  = guide_legend(ncol = 2, override.aes = list(size = 3, linewidth = 1.5))
  ) +
  scale_y_continuous(labels = label_percent(scale = 1),
                     limits = c(0, 105),
                     expand = expansion(mult = c(0, 0))) +
  scale_size_continuous(range = c(3, 7), guide = "none") +
  labs(
    title    = "Development Priorities in the Bank's Own Words",
    subtitle = "Share of projects per period whose abstracts mention each thematic area",
    x        = "Approval period",
    y        = "Projects mentioning theme (%)",
    caption  = caption_wb("Keyword search on project abstracts. Point size proportional to number of projects in period.")
  ) +
  theme_tico() +
  theme(legend.position = "bottom")

save_fig(fig5, "fig5_abstract_themes", width = 10, height = 6.5)
cat("  Saved fig5_abstract_themes\n")

# ---------------------------------------------------------------------------
# FIG 6 — Costa Rica's development trajectory (dual-panel)
# ---------------------------------------------------------------------------

cat("Rendering Figure 6 — Development trajectory...\n")

# GDP per capita panel
p6a <- ggplot(wdi %>% filter(!is.na(gdp_per_capita)),
              aes(x = year, y = gdp_per_capita)) +
  geom_area(fill = PALETTE$teal, alpha = 0.15) +
  geom_line(colour = PALETTE$teal, linewidth = 0.9) +
  scale_x_continuous(breaks = seq(1990, 2024, by = 5),
                     limits = c(1990, 2024),
                     expand = expansion(add = 0.5)) +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale()),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(
    subtitle = "GDP per capita (current US$)",
    x        = NULL, y = NULL
  ) +
  theme_tico()

# Life expectancy panel
p6b <- ggplot(wdi %>% filter(!is.na(life_expectancy)),
              aes(x = year, y = life_expectancy)) +
  geom_line(colour = PALETTE$slate, linewidth = 0.9) +
  geom_point(colour = PALETTE$slate, size = 1.8, shape = 16) +
  scale_x_continuous(breaks = seq(1990, 2024, by = 5),
                     limits = c(1990, 2024),
                     expand = expansion(add = 0.5)) +
  scale_y_continuous(labels = function(x) paste0(x, " yrs"),
                     limits = c(74, 81),
                     expand = expansion(mult = c(0.02, 0.05))) +
  labs(
    subtitle = "Life expectancy at birth (years)",
    x        = NULL, y = NULL
  ) +
  theme_tico()

# Gini panel — sparse, use points only
p6c <- ggplot(wdi %>% filter(!is.na(gini)),
              aes(x = year, y = gini)) +
  geom_point(colour = PALETTE$amber, size = 3.5, shape = 18) +
  geom_segment(aes(xend = year, y = 42, yend = gini),
               colour = PALETTE$amber, linewidth = 0.6, alpha = 0.5) +
  scale_x_continuous(breaks = seq(1990, 2024, by = 5),
                     limits = c(1990, 2024),
                     expand = expansion(add = 0.5)) +
  scale_y_continuous(limits = c(42, 55),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(
    subtitle = "Gini index (survey years only — connect with caution)",
    x        = NULL, y = NULL
  ) +
  theme_tico() +
  theme(panel.grid.minor = element_blank())

fig6 <- (p6a / p6b / p6c) +
  plot_annotation(
    title    = "Costa Rica's Development Trajectory, 1990–2024",
    subtitle = "Key macroeconomic and social indicators. Gini shown only for available survey years.",
    caption  = caption_wdi("Gini index observations are sparse; do not interpolate or infer trends from single points."),
    theme    = theme_tico()
  )

save_fig(fig6, "fig6_development_trajectory", width = 10, height = 10)
cat("  Saved fig6_development_trajectory\n")

# ---------------------------------------------------------------------------
# FIG 7 — Instrument mix over time (bonus: DPL vs IPF)
# ---------------------------------------------------------------------------

cat("Rendering Figure 7 — Instrument mix over time...\n")

instr_annual <- projects %>%
  filter(has_financing == TRUE, !is.na(approval_year),
         instrument_type %in% c("Development Policy Loan",
                                  "Investment Project Financing")) %>%
  group_by(approval_year, instrument_type) %>%
  summarise(
    n             = n(),
    total_usd     = sum(totalamt, na.rm = TRUE),
    .groups       = "drop"
  )

fig7 <- ggplot(instr_annual,
               aes(x = approval_year, y = total_usd / 1e6,
                   fill = instrument_type)) +
  geom_col(width = 0.8, position = "stack", colour = PALETTE$grey_bg, linewidth = 0.2) +
  scale_fill_manual(
    values = c(
      "Development Policy Loan"        = PALETTE$amber,
      "Investment Project Financing"   = PALETTE$teal
    ),
    name = "Instrument type",
    labels = c("Development Policy Loan (budget support)",
               "Investment Project Financing")
  ) +
  scale_x_continuous(breaks = seq(1990, 2024, by = 5),
                     expand = expansion(add = 0.5)) +
  scale_y_continuous(labels = function(x) paste0("$", x, "M"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Lending Instrument Mix: Investment Loans vs. Development Policy Loans",
    subtitle = "Annual IBRD commitments by instrument type, 1990–2024 (nominal USD)",
    x        = "Year of Board Approval",
    y        = "Annual commitments (US$ millions, nominal)",
    caption  = caption_wb("Development Policy Loans provide budget support and often accompany policy reforms. Investment Project Financing supports specific capital or programmatic investments.")
  ) +
  theme_tico() +
  theme(legend.position = "bottom")

save_fig(fig7, "fig7_instrument_mix", width = 10, height = 6.5)
cat("  Saved fig7_instrument_mix\n")

# ---------------------------------------------------------------------------
# FIG 8 — Annual WB commitments + GDP per capita overlay
# ---------------------------------------------------------------------------

cat("Rendering Figure 8 — Commitments vs. GDP per capita...\n")

# Normalise each series to its range for visual overlay
wb_annual <- annual_plot %>%
  filter(total_commitments_usd > 0) %>%
  mutate(norm = (total_commitments_usd - min(total_commitments_usd)) /
                 (max(total_commitments_usd) - min(total_commitments_usd)))

gdp_data <- wdi %>%
  filter(!is.na(gdp_per_capita)) %>%
  mutate(norm = (gdp_per_capita - min(gdp_per_capita)) /
                 (max(gdp_per_capita) - min(gdp_per_capita)))

fig8 <- ggplot() +
  # GDP per capita line
  geom_area(data = gdp_data,
            aes(x = year, y = norm),
            fill = PALETTE$slate, alpha = 0.08) +
  geom_line(data = gdp_data,
            aes(x = year, y = norm, colour = "GDP per capita"),
            linewidth = 0.9) +
  # WB commitments as bars (normalised)
  geom_col(data = wb_annual,
           aes(x = approval_year, y = norm, fill = "WB commitments"),
           width = 0.7, alpha = 0.65) +
  scale_colour_manual(values = c("GDP per capita" = PALETTE$slate),
                      name = NULL) +
  scale_fill_manual(values = c("WB commitments" = PALETTE$teal),
                    name = NULL) +
  scale_x_continuous(breaks = seq(1990, 2024, by = 5),
                     limits = c(1989, 2025),
                     expand = expansion(add = 0.5)) +
  scale_y_continuous(labels = label_percent(), limits = c(0, 1.08),
                     expand = expansion(mult = c(0, 0))) +
  labs(
    title    = "World Bank Commitments and Costa Rica's GDP Trajectory",
    subtitle = "Both series indexed to 0–100% of their respective 1990–2024 range — not direct comparisons",
    x        = "Year",
    y        = "Index (0 = period min, 100% = period max)",
    caption  = paste0(
      caption_wb(), "\n",
      caption_wdi("Indexing allows visual comparison of trajectories only; values are not directly comparable.")
    )
  ) +
  theme_tico() +
  theme(legend.position = "top", legend.justification = "left")

save_fig(fig8, "fig8_commitments_vs_gdp", width = 10, height = 6.5)
cat("  Saved fig8_commitments_vs_gdp\n")

cat("\nAll figures saved to figures/\n")
cat("Visualizations complete.\n")
