# functions.R
# Reusable functions and ggplot2 theme for the Tico-nomics project
# World Bank Group in Costa Rica, 1980–2025

library(ggplot2)
library(scales)
library(glue)

# ---------------------------------------------------------------------------
# Colour palette
# ---------------------------------------------------------------------------

PALETTE <- list(
  teal        = "#1a6b72",   # primary / IBRD
  amber       = "#c87d2f",   # accent / highlight
  slate       = "#3d5166",   # secondary / IFC
  sage        = "#5a8a6a",   # third / MIGA
  rose        = "#b05060",   # fourth / alert
  tan         = "#c9a96e",   # fifth
  grey_dark   = "#2d2d2d",
  grey_mid    = "#6b6b6b",
  grey_light  = "#d4d4d4",
  grey_bg     = "#f7f5f2",   # warm off-white background
  white       = "#ffffff"
)

# Institution colours
INSTITUTION_COLOURS <- c(
  "IBRD" = "#1a6b72",
  "IFC"  = "#3d5166",
  "MIGA" = "#5a8a6a",
  "IDA"  = "#9e9e9e"
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
  "Transportation"                  = "#1a6b72",
  "Energy and Extractives"          = "#c87d2f",
  "Public Administration"           = "#3d5166",
  "Education"                       = "#5a8a6a",
  "Health"                          = "#b05060",
  "Finance"                         = "#c9a96e",
  "Water, Sanitation and Waste"     = "#4e7fa0",
  "Agriculture, Fishing & Forestry" = "#7a9e5e",
  "Social Protection"               = "#8b5c8c",
  "ICT / Trade"                     = "#7a6244",
  "Other"                           = "#9e9e9e"
)

# ---------------------------------------------------------------------------
# Base ggplot2 theme
# ---------------------------------------------------------------------------

theme_tico <- function(base_size = 11, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # --- Plot area ---
      plot.background   = element_rect(fill = PALETTE$grey_bg, colour = NA),
      panel.background  = element_rect(fill = PALETTE$grey_bg, colour = NA),
      panel.grid.major  = element_line(colour = "#e0ddd8", linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      panel.border      = element_blank(),

      # --- Axes ---
      axis.line         = element_line(colour = "#c0bbb5", linewidth = 0.4),
      axis.ticks        = element_line(colour = "#c0bbb5", linewidth = 0.4),
      axis.ticks.length = unit(3, "pt"),
      axis.text         = element_text(colour = PALETTE$grey_mid, size = rel(0.85)),
      axis.title        = element_text(colour = PALETTE$grey_dark, size = rel(0.9),
                                       margin = margin(t = 4, r = 4)),
      axis.title.x      = element_text(margin = margin(t = 8)),
      axis.title.y      = element_text(margin = margin(r = 8), angle = 90),

      # --- Legend ---
      legend.background = element_rect(fill = PALETTE$grey_bg, colour = NA),
      legend.key        = element_rect(fill = PALETTE$grey_bg, colour = NA),
      legend.text       = element_text(colour = PALETTE$grey_mid, size = rel(0.8)),
      legend.title      = element_text(colour = PALETTE$grey_dark, size = rel(0.85),
                                       face = "bold"),
      legend.position   = "bottom",
      legend.box.margin = margin(t = 4),

      # --- Text hierarchy ---
      plot.title        = element_text(
        colour   = PALETTE$grey_dark,
        size     = rel(1.25),
        face     = "bold",
        hjust    = 0,
        margin   = margin(b = 4)
      ),
      plot.subtitle     = element_text(
        colour   = PALETTE$grey_mid,
        size     = rel(0.95),
        hjust    = 0,
        lineheight = 1.3,
        margin   = margin(b = 12)
      ),
      plot.caption      = element_text(
        colour   = "#909090",
        size     = rel(0.72),
        hjust    = 0,
        lineheight = 1.3,
        margin   = margin(t = 10)
      ),
      plot.title.position   = "plot",
      plot.caption.position = "plot",

      # --- Facets ---
      strip.background  = element_rect(fill = "#e8e5e0", colour = NA),
      strip.text        = element_text(colour = PALETTE$grey_dark, face = "bold",
                                       size = rel(0.85), margin = margin(4, 4, 4, 4)),

      # --- Margins ---
      plot.margin = margin(t = 12, r = 16, b = 8, l = 8)
    )
}

# Default theme for this project
theme_set(theme_tico())

# ---------------------------------------------------------------------------
# Standard caption text
# ---------------------------------------------------------------------------

caption_wb <- function(extra = NULL) {
  base <- "Source: World Bank Projects & Operations List; IBRD Statement of Loans and Guarantees (DS00047, snapshot 09/30/2026)"
  if (!is.null(extra)) glue("{base}. {extra}") else base
}

caption_ifc <- function(extra = NULL) {
  base <- "Source: IFC Project Disclosures (disclosures.ifc.org)"
  if (!is.null(extra)) glue("{base}. {extra}") else base
}

caption_miga <- function(extra = NULL) {
  base <- "Source: MIGA Project Briefs (miga.org)"
  if (!is.null(extra)) glue("{base}. {extra}") else base
}

caption_wdi <- function(extra = NULL) {
  base <- "Source: World Development Indicators via World Bank API"
  if (!is.null(extra)) glue("{base}. {extra}") else base
}

# ---------------------------------------------------------------------------
# Formatting helpers
# ---------------------------------------------------------------------------

fmt_millions <- function(x) {
  scales::label_dollar(
    scale  = 1e-6,
    suffix = "M",
    accuracy = 1
  )(x)
}

fmt_billions <- function(x) {
  scales::label_dollar(
    scale  = 1e-9,
    suffix = "B",
    accuracy = 0.1
  )(x)
}

fmt_usd_axis <- function(x) {
  dplyr::case_when(
    abs(x) >= 1e9  ~ scales::dollar(x / 1e9, suffix = "B", accuracy = 0.1),
    abs(x) >= 1e6  ~ scales::dollar(x / 1e6, suffix = "M", accuracy = 1),
    TRUE           ~ scales::dollar(x, accuracy = 1)
  )
}

# ---------------------------------------------------------------------------
# Sector grouping
# ---------------------------------------------------------------------------

# Map granular sector names to 10 major groupings for cleaner charts
group_sector <- function(sector_name) {
  s <- tolower(sector_name)
  dplyr::case_when(
    grepl("transport|highway|road|port|railway|aviation", s)                       ~ "Transportation",
    grepl("energy|power|electr|oil|gas|extractive|petroleum|coal|renewab|solar|wind", s) ~ "Energy and Extractives",
    grepl("public admin|governance|central gov|sub-national|municipal", s)         ~ "Public Administration",
    grepl("education|school|learning|university|vocational", s)                    ~ "Education",
    grepl("health|hospital|medical|vaccine", s)                                    ~ "Health",
    grepl("water|sanitation|waste|sewage|drainage", s)                             ~ "Water, Sanitation and Waste",
    grepl("financial|capital market|banking|insurance|pension|fiscal", s)          ~ "Finance",
    grepl("agricult|rural|fisheries|forestry|natural res|environ|biodiver|land|soil|forest", s) ~ "Agriculture, Fishing & Forestry",
    grepl("social protect|poverty|welfare|labor|labour|safety net", s)             ~ "Social Protection",
    grepl("ict|telecom|information|technology|trade|commerce|competitiveness", s)  ~ "ICT / Trade",
    TRUE                                                                            ~ "Other"
  )
}

# ---------------------------------------------------------------------------
# Save figures
# ---------------------------------------------------------------------------

save_fig <- function(plot, filename, width = 10, height = 6.5, dpi = 300) {
  base <- here::here("figures")
  # PNG (high-res)
  ggplot2::ggsave(
    filename = file.path(base, paste0(filename, ".png")),
    plot     = plot,
    width    = width,
    height   = height,
    dpi      = dpi,
    bg       = PALETTE$grey_bg
  )
  # SVG (vector for web)
  ggplot2::ggsave(
    filename = file.path(base, paste0(filename, ".svg")),
    plot     = plot,
    width    = width,
    height   = height,
    device   = svglite::svglite,
    bg       = PALETTE$grey_bg
  )
  invisible(plot)
}
