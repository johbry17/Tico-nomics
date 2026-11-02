# 08_date_recovery_viz.R
# Visualization for the date recovery and scenario analysis
# Produces updated fig1 (scenarios overlay) and supplemental figures

library(here)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(scales)
library(patchwork)
library(svglite)

source(here("R", "functions.R"))

projects_enriched <- read_csv(here("data", "processed", "projects_enriched.csv"),
                               show_col_types = FALSE)
annual_scen       <- read_csv(here("data", "derived", "annual_scenarios.csv"),
                               show_col_types = FALSE)
enrichment        <- read_csv(here("data", "processed", "date_enrichment.csv"),
                               show_col_types = FALSE)

# ---------------------------------------------------------------------------
# FIG 9 — Scenario comparison: original vs recovered dates
# ---------------------------------------------------------------------------

cat("Rendering Figure 9 — Scenario comparison...\n")

scen_wide <- annual_scen |>
  filter(scenario %in% c("A", "B")) |>
  pivot_wider(names_from = scenario, values_from = c(n, total_usd))

# Long form for faceted plot
scen_long <- annual_scen |>
  filter(scenario %in% c("A", "B")) |>
  mutate(
    scenario_label = case_when(
      scenario == "A" ~ "A: Original data (V2 API confirmed dates only)",
      scenario == "B" ~ "B: Including recovered dates (V3 API + bulk Excel)"
    )
  )

# Commitments panel
p9a <- ggplot(scen_long, aes(x = year, y = total_usd / 1e6,
                              fill = scenario_label)) +
  geom_col(position = position_dodge2(width = 0.8, preserve = "single"),
           width = 0.7, alpha = 0.85) +
  scale_fill_manual(
    values = c(
      "A: Original data (V2 API confirmed dates only)"          = PALETTE$teal,
      "B: Including recovered dates (V3 API + bulk Excel)"      = PALETTE$amber
    ),
    name = NULL
  ) +
  scale_x_continuous(breaks = seq(1990, 2024, by = 5),
                     expand = expansion(add = 0.5)) +
  scale_y_continuous(labels = function(x) paste0("$", x, "M"),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "IBRD Commitment Timeline: Original vs Recovered Dates",
    subtitle = "Scenario A uses only dates available in the original V2 API pull.\nScenario B adds dates recovered from the WB V3 API and bulk Excel (confirmed by both sources).",
    x        = "Year of board approval",
    y        = "Commitments (US$ millions, nominal)",
    caption  = caption_wb("P122265 ($200M, 2014) is the only positive-financing project added in the 2013–2019 window. Its status is Dropped (APL).")
  ) +
  theme_tico() +
  theme(legend.position = "top", legend.justification = "left")

# Project count panel
p9b <- ggplot(scen_long, aes(x = year, y = n,
                              fill = scenario_label)) +
  geom_col(position = position_dodge2(width = 0.8, preserve = "single"),
           width = 0.7, alpha = 0.85) +
  scale_fill_manual(
    values = c(
      "A: Original data (V2 API confirmed dates only)"          = PALETTE$teal,
      "B: Including recovered dates (V3 API + bulk Excel)"      = PALETTE$amber
    ),
    name = NULL
  ) +
  scale_x_continuous(breaks = seq(1990, 2024, by = 5),
                     expand = expansion(add = 0.5)) +
  scale_y_continuous(breaks = 0:6, expand = expansion(mult = c(0, 0.1))) +
  labs(
    x        = "Year of board approval",
    y        = "Projects approved",
    subtitle = "Project counts per year"
  ) +
  theme_tico() +
  theme(legend.position = "none", plot.title = element_blank())

fig9 <- p9a / p9b + plot_layout(heights = c(2, 1))
save_fig(fig9, "fig9_scenario_comparison", width = 11, height = 8)
cat("  Saved fig9_scenario_comparison\n")

# ---------------------------------------------------------------------------
# FIG 10 — Recovery detail: the 16 projects
# ---------------------------------------------------------------------------

cat("Rendering Figure 10 — Recovery detail table visualization...\n")

recovery_detail <- enrichment |>
  mutate(
    scope_label = case_when(
      !is.na(recovered_year) & recovered_year < 1990    ~ "Pre-1990",
      !is.na(recovered_year) & recovered_year > 2024    ~ "Post-2024",
      !is.na(recovered_year) & recovered_year >= 1990 &
        recovered_year <= 2024                          ~ "In scope",
      is.na(recovered_year)                             ~ "Not recovered"
    ),
    fin_label = case_when(
      is.na(totalamt) | totalamt == 0  ~ "Zero / N/A",
      totalamt > 0                     ~ paste0("$", round(totalamt / 1e6, 0), "M")
    ),
    confidence_short = case_when(
      grepl("High", confidence)   ~ "High",
      grepl("Medium", confidence) ~ "Med.",
      TRUE                        ~ "N/R"
    ),
    # Short name for display
    name_short = stringr::str_trunc(project_name, 45),
    plot_label = paste0(name_short, "\n(", coalesce(as.character(recovered_year), "?"), ", ",
                        fin_label, ")")
  ) |>
  arrange(recovered_year)

scope_colours <- c(
  "In scope"      = PALETTE$teal,
  "Pre-1990"      = PALETTE$grey_mid,
  "Post-2024"     = PALETTE$amber,
  "Not recovered" = "#cc3333"
)

fig10 <- ggplot(recovery_detail,
                aes(y = reorder(name_short, coalesce(recovered_year, 0L)),
                    x = coalesce(as.numeric(recovered_year), 1985),
                    colour = scope_label, size = pmax(totalamt / 1e6, 1))) +
  geom_point(alpha = 0.85, shape = 16) +
  geom_vline(xintercept = c(1990, 2024), linetype = "dashed",
             colour = PALETTE$grey_mid, linewidth = 0.5) +
  scale_colour_manual(values = scope_colours, name = "Scope") +
  scale_size_continuous(
    range = c(2.5, 10),
    name  = "Commitment (US$M)",
    guide = guide_legend(override.aes = list(colour = PALETTE$slate))
  ) +
  scale_x_continuous(
    breaks = seq(1980, 2026, by = 5),
    limits = c(1978, 2027),
    expand = expansion(add = 0.5)
  ) +
  annotate("text", x = 1990, y = 0.5, label = "1990", hjust = -0.1,
           size = 3, colour = PALETTE$grey_mid) +
  annotate("text", x = 2024, y = 0.5, label = "2024", hjust = -0.1,
           size = 3, colour = PALETTE$grey_mid) +
  labs(
    title    = "Date Recovery: The 16 Missing-Date Projects",
    subtitle = "Recovered board approval dates from WB V3 API and bulk Excel (confirmed by both sources).\nVertical dashed lines mark the 1990–2024 analysis window. Point size = commitment amount.",
    x        = "Recovered board approval year",
    y        = NULL,
    caption  = caption_wb("Dates shown as recovered. P169724 (Learning for All, $100M) remains unrecovered in both V3 API and Excel. Three June-30 dates (P006930, P006950, P006951) are WB fiscal year-end proxies.")
  ) +
  theme_tico() +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position    = "right"
  )

save_fig(fig10, "fig10_recovery_detail", width = 12, height = 7)
cat("  Saved fig10_recovery_detail\n")

# ---------------------------------------------------------------------------
# FIG 1 updated — full timeline using Scenario B (best available data)
# ---------------------------------------------------------------------------

cat("Rendering Figure 1 updated — full timeline with recovered dates...\n")

annual_b <- annual_scen |>
  filter(scenario == "B") |>
  # Mark which years have recovered (not original) data
  left_join(
    projects_enriched |>
      filter(!is.na(enriched_year), enriched_year >= 1990,
             !is.na(totalamt), totalamt > 0,
             is.na(approval_year)) |>
      group_by(enriched_year) |>
      summarise(has_recovered = TRUE, .groups = "drop"),
    by = c("year" = "enriched_year")
  ) |>
  mutate(has_recovered = coalesce(has_recovered, FALSE))

p1a_upd <- ggplot(annual_b, aes(x = year, y = total_usd)) +
  geom_area(fill = PALETTE$teal, alpha = 0.15) +
  geom_col(aes(fill = has_recovered), width = 0.7, show.legend = TRUE, alpha = 0.85) +
  scale_fill_manual(
    values = c("FALSE" = PALETTE$teal, "TRUE" = PALETTE$amber),
    labels = c("FALSE" = "Original V2 date", "TRUE" = "Recovered date (V3/Excel)"),
    name   = "Date source"
  ) +
  scale_x_continuous(breaks = seq(1990, 2024, by = 5),
                     minor_breaks = 1990:2024,
                     limits = c(1989, 2025),
                     expand = expansion(add = 0.5)) +
  scale_y_continuous(labels = function(x) fmt_usd_axis(x),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(
    title    = "World Bank Lending to Costa Rica, 1990–2024 (Enriched Timeline)",
    subtitle = "Annual IBRD commitments using best available dates; amber bars show projects with dates recovered from V3 API",
    x        = NULL,
    y        = "Commitments (USD)",
    caption  = caption_wb("Scenario B: confirmed V2 dates + recovered V3 dates. P169724 ($100M, Dropped) excluded — date unrecoverable. All amounts nominal USD.")
  ) +
  theme_tico() +
  theme(legend.position = "top", legend.justification = "left")

p1b_upd <- ggplot(annual_b, aes(x = year, y = n)) +
  geom_segment(aes(xend = year, y = 0, yend = n),
               colour = PALETTE$slate, linewidth = 0.7, alpha = 0.7) +
  geom_point(aes(colour = has_recovered), size = 3, shape = 16) +
  scale_colour_manual(
    values = c("FALSE" = PALETTE$slate, "TRUE" = PALETTE$amber),
    guide = "none"
  ) +
  scale_x_continuous(breaks = seq(1990, 2024, by = 5),
                     limits = c(1989, 2025),
                     expand = expansion(add = 0.5)) +
  scale_y_continuous(breaks = 0:7, expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Year of board approval", y = "Projects approved",
       subtitle = "Number of projects approved per year (Scenario B)") +
  theme_tico() +
  theme(plot.title = element_blank())

fig1_updated <- p1a_upd / p1b_upd + plot_layout(heights = c(2, 1))
save_fig(fig1_updated, "fig1b_portfolio_enriched", width = 11, height = 8)
cat("  Saved fig1b_portfolio_enriched\n")

cat("\nAll recovery visualizations saved to figures/\n")
cat("Date recovery visualizations complete.\n")
