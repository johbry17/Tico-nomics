# functions.R
# Reusable functions and ggplot2 theme for the Tico-nomics project
# World Bank Group in Costa Rica, 1980–2025

library(ggplot2)
library(scales)
library(glue)
library(stringr)

# ---------------------------------------------------------------------------
# Colour palette
# ---------------------------------------------------------------------------

PALETTE <- list(
  teal = "#1a6b72", # primary / IBRD
  amber = "#c87d2f", # accent / highlight
  slate = "#3d5166", # secondary / IFC
  sage = "#5a8a6a", # third / MIGA
  rose = "#b05060", # fourth / alert
  tan = "#c9a96e", # fifth
  grey_dark = "#2d2d2d",
  grey_mid = "#6b6b6b",
  grey_light = "#d4d4d4",
  grey_bg = "#f7f5f2", # warm off-white background
  white = "#ffffff"
)

# Dark-mode plotting palette. Keep category identities while increasing
# contrast against the report's deep forest surfaces.
DARK_PALETTE <- list(
  teal = "#67AEB3",   # IBRD - brighter teal
  amber = "#F59E0B",  # ochre accent
  slate = "#93A7BA",  # IFC - brighter slate
  sage = "#74BA86",   # MIGA - bright sage
  rose = "#E58A98",
  tan = "#F0C68A",
  grey_dark = "#F2EDE2",
  grey_mid = "#A8B09F",
  grey_light = "#243029",
  grey_bg = "#111D15",
  white = "#F2EDE2",
  panel = "#182118",
  raised = "#1E2B1E",
  elevated = "#243029",
  border = "#2A3D30",
  border_mid = "#3A4E3E",
  grid = "#3A4E3E"
)

# Institution colours
INSTITUTION_COLOURS <- c(
  "IBRD" = "#1a6b72",
  "IFC" = "#3d5166",
  "MIGA" = "#5a8a6a",
  "IDA" = "#9e9e9e"
)

# Period colours
PERIOD_COLOURS <- c(
  "1980–1989" = "#7a6244",
  "1990–1999" = "#1a6b72",
  "2000–2009" = "#c87d2f",
  "2010–2019" = "#3d5166",
  "2020–2025" = "#5a8a6a"
)

# Sector colours
SECTOR_COLOURS <- c(
  "Transportation" = "#1a6b72",
  "Energy and Extractives" = "#c87d2f",
  "Public Administration" = "#3d5166",
  "Education" = "#5a8a6a",
  "Health" = "#b05060",
  "Finance" = "#c9a96e",
  "Water, Sanitation and Waste" = "#4e7fa0",
  "Agriculture, Fishing & Forestry" = "#7a9e5e",
  "Social Protection" = "#8b5c8c",
  "ICT / Trade" = "#7a6244",
  "Other" = "#9e9e9e"
)

# ---------------------------------------------------------------------------
# Base ggplot2 theme
# ---------------------------------------------------------------------------

theme_tico <- function(base_size = 11, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # --- Plot area ---
      plot.background = element_rect(fill = PALETTE$grey_bg, colour = NA),
      panel.background = element_rect(fill = PALETTE$grey_bg, colour = NA),
      panel.grid.major = element_line(colour = "#e0ddd8", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),

      # --- Axes ---
      axis.line = element_line(colour = "#c0bbb5", linewidth = 0.4),
      axis.ticks = element_line(colour = "#c0bbb5", linewidth = 0.4),
      axis.ticks.length = unit(3, "pt"),
      axis.text = element_text(colour = PALETTE$grey_mid, size = rel(0.85)),
      axis.title = element_text(
        colour = PALETTE$grey_dark,
        size = rel(0.95),
        margin = margin(t = 4, r = 4)
      ),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8), angle = 90),

      # --- Legend ---
      legend.background = element_rect(fill = PALETTE$grey_bg, colour = NA),
      legend.key = element_rect(fill = PALETTE$grey_bg, colour = NA),
      legend.text = element_text(colour = PALETTE$grey_mid, size = rel(0.85)),
      legend.title = element_text(
        colour = PALETTE$grey_dark,
        size = rel(0.9),
        face = "bold"
      ),
      legend.position = "bottom",
      legend.box.margin = margin(t = 4),

      # --- Text hierarchy ---
      plot.title = element_text(
        colour = PALETTE$grey_dark,
        size = rel(1.35),
        face = "bold",
        hjust = 0,
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        colour = PALETTE$grey_mid,
        size = rel(1.05),
        hjust = 0,
        lineheight = 1.3,
        margin = margin(b = 12)
      ),
      plot.caption = element_text(
        colour = "#909090",
        size = rel(0.82),
        hjust = 0,
        lineheight = 1.3,
        margin = margin(t = 10)
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",

      # --- Facets ---
      strip.background = element_rect(fill = "#e8e5e0", colour = NA),
      strip.text = element_text(
        colour = PALETTE$grey_dark,
        face = "bold",
        size = rel(0.85),
        margin = margin(4, 4, 4, 4)
      ),

      # --- Margins ---
      plot.margin = margin(t = 12, r = 16, b = 8, l = 8)
    )
}

theme_tico_dark <- function(base_size = 11, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.background = element_rect(fill = DARK_PALETTE$grey_bg, colour = NA),
      panel.background = element_rect(fill = DARK_PALETTE$grey_bg, colour = NA),
      panel.grid.major = element_line(colour = DARK_PALETTE$grid, linewidth = 0.4),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),

      axis.line = element_line(colour = DARK_PALETTE$border_mid, linewidth = 0.4),
      axis.ticks = element_line(colour = DARK_PALETTE$border_mid, linewidth = 0.4),
      axis.ticks.length = unit(3, "pt"),
      axis.text = element_text(colour = DARK_PALETTE$grey_mid, size = rel(0.85)),
      axis.title = element_text(
        colour = DARK_PALETTE$grey_dark,
        size = rel(0.95),
        margin = margin(t = 4, r = 4)
      ),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8), angle = 90),

      legend.background = element_rect(fill = DARK_PALETTE$panel, colour = NA),
      legend.key = element_rect(fill = DARK_PALETTE$panel, colour = NA),
      legend.text = element_text(colour = DARK_PALETTE$grey_mid, size = rel(0.85)),
      legend.title = element_text(
        colour = DARK_PALETTE$grey_dark,
        size = rel(0.9),
        face = "bold"
      ),
      legend.position = "bottom",
      legend.box.margin = margin(t = 4),

      plot.title = element_text(
        colour = DARK_PALETTE$white,
        size = rel(1.35),
        face = "bold",
        hjust = 0,
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        colour = DARK_PALETTE$grey_mid,
        size = rel(1.05),
        hjust = 0,
        lineheight = 1.3,
        margin = margin(b = 12)
      ),
      plot.caption = element_text(
        colour = "#72786F",
        size = rel(0.82),
        hjust = 0,
        lineheight = 1.3,
        margin = margin(t = 10)
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot",

      strip.background = element_rect(fill = DARK_PALETTE$raised, colour = NA),
      strip.text = element_text(
        colour = DARK_PALETTE$grey_dark,
        face = "bold",
        size = rel(0.85),
        margin = margin(4, 4, 4, 4)
      ),

      plot.margin = margin(t = 12, r = 16, b = 8, l = 8)
    )
}

tico_palette <- function(mode = c("light", "dark")) {
  mode <- match.arg(mode)
  if (mode == "dark") DARK_PALETTE else PALETTE
}

tico_theme <- function(mode = c("light", "dark"), base_size = 11, base_family = "sans") {
  mode <- match.arg(mode)
  if (mode == "dark") {
    theme_tico_dark(base_size, base_family)
  } else {
    theme_tico(base_size, base_family)
  }
}

tico_period_colours <- function(mode = c("light", "dark")) {
  mode <- match.arg(mode)
  if (mode == "dark") {
    c(
      "1980–1989" = "#D8C09A",
      "1990–1999" = "#67AEB3",
      "2000–2009" = "#F59E0B",
      "2010–2019" = "#93A7BA",
      "2020–2025" = "#74BA86"
    )
  } else {
    PERIOD_COLOURS
  }
}

tico_sector_colours <- function(mode = c("light", "dark")) {
  mode <- match.arg(mode)
  if (mode == "dark") {
    c(
      "Transportation" = "#67AEB3",
      "Energy and Extractives" = "#F59E0B",
      "Public Administration" = "#93A7BA",
      "Education" = "#74BA86",
      "Health" = "#E58A98",
      "Finance" = "#F0C68A",
      "Water, Sanitation and Waste" = "#79AFC9",
      "Agriculture, Fishing & Forestry" = "#A8C978",
      "Social Protection" = "#C59AC7",
      "ICT / Trade" = "#D8C09A",
      "Other" = "#A8B09F"
    )
  } else {
    SECTOR_COLOURS
  }
}

# Default theme for this project
theme_set(theme_tico())

# ---------------------------------------------------------------------------
# Standard caption text
# ---------------------------------------------------------------------------

wrap_caption <- function(text, width = 160) {
  stringr::str_wrap(text, width = width)
}

caption_wb <- function(extra = NULL) {
  base <- paste(
    "Source: World Bank Projects & Operations List;",
    "IBRD Statement of Loans and Guarantees (DS00047, snapshot 09/30/2026)"
  )
  wrap_caption(if (!is.null(extra)) paste(base, extra, sep = ". ") else base)
}

caption_ifc <- function(extra = NULL) {
  base <- "Source: IFC Project Disclosures (disclosures.ifc.org)"
  wrap_caption(if (!is.null(extra)) paste(base, extra, sep = ". ") else base)
}

caption_miga <- function(extra = NULL) {
  base <- "Source: MIGA Project Briefs (miga.org)"
  wrap_caption(if (!is.null(extra)) paste(base, extra, sep = ". ") else base)
}

caption_wdi <- function(extra = NULL) {
  base <- "Source: World Development Indicators via World Bank API"
  wrap_caption(if (!is.null(extra)) paste(base, extra, sep = ". ") else base)
}

# ---------------------------------------------------------------------------
# Formatting helpers
# ---------------------------------------------------------------------------

fmt_millions <- function(x) {
  scales::label_dollar(
    scale = 1e-6,
    suffix = "M",
    accuracy = 1
  )(x)
}

fmt_billions <- function(x) {
  scales::label_dollar(
    scale = 1e-9,
    suffix = "B",
    accuracy = 0.1
  )(x)
}

fmt_usd_axis <- function(x) {
  dplyr::case_when(
    abs(x) >= 1e9 ~ scales::dollar(x / 1e9, suffix = "B", accuracy = 0.1),
    abs(x) >= 1e6 ~ scales::dollar(x / 1e6, suffix = "M", accuracy = 1),
    TRUE ~ scales::dollar(x, accuracy = 1)
  )
}

# ---------------------------------------------------------------------------
# Sector grouping
# ---------------------------------------------------------------------------

# Map granular sector names to 10 major groupings for cleaner charts
group_sector <- function(sector_name) {
  s <- tolower(sector_name)
  dplyr::case_when(
    grepl("transport|highway|road|port|railway|aviation", s) ~ "Transportation",
    grepl(
      "energy|power|electr|oil|gas|extractive|petroleum|coal|renewab|solar|wind",
      s
    ) ~ "Energy and Extractives",
    grepl(
      "public admin|governance|central gov|sub-national|municipal",
      s
    ) ~ "Public Administration",
    grepl("education|school|learning|university|vocational", s) ~ "Education",
    grepl("health|hospital|medical|vaccine", s) ~ "Health",
    grepl(
      "water|sanitation|waste|sewage|drainage",
      s
    ) ~ "Water, Sanitation and Waste",
    grepl(
      "financial|capital market|banking|insurance|pension|fiscal",
      s
    ) ~ "Finance",
    grepl(
      "agricult|rural|fisheries|forestry|natural res|environ|biodiver|land|soil|forest",
      s
    ) ~ "Agriculture, Fishing & Forestry",
    grepl(
      "social protect|poverty|welfare|labor|labour|safety net",
      s
    ) ~ "Social Protection",
    grepl(
      "ict|telecom|information|technology|trade|commerce|competitiveness",
      s
    ) ~ "ICT / Trade",
    TRUE ~ "Other"
  )
}

# ---------------------------------------------------------------------------
# Save figures
# ---------------------------------------------------------------------------

save_fig <- function(
  plot,
  filename,
  width = 10,
  height = 6.5,
  dpi = 300,
  mode = c("light", "dark")
) {
  mode <- match.arg(mode)
  base <- here::here("figures")
  export_filename <- if (mode == "dark") paste0(filename, "-dark") else filename
  palette <- tico_palette(mode)
  # PNG (high-res)
  ggplot2::ggsave(
    filename = file.path(base, paste0(export_filename, ".png")),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = palette$grey_bg
  )
  # SVG (vector for web)
  ggplot2::ggsave(
    filename = file.path(base, paste0(export_filename, ".svg")),
    plot = plot,
    width = width,
    height = height,
    device = svglite::svglite,
    bg = palette$grey_bg
  )
  invisible(plot)
}
