# ===========================
# Libraries & Font Setup
# ===========================
# install.packages(c("ggplot2","dplyr","tibble","forcats","showtext","readxl","tidyr","stringr","purrr"), quiet = TRUE)

library(ggplot2)
library(dplyr)
library(tibble)
library(forcats)
library(showtext)
library(readxl)
library(tidyr)
library(stringr)
library(purrr)

# --- CMU font (adjust paths if yours differ) ---
font_add(
  family    = "CMU Serif",
  regular   = "cm-unicode/cmunrm.otf",
  bold      = "cm-unicode/cmunbx.otf",
  italic    = "cm-unicode/cmunti.otf",
  bolditalic= "cm-unicode/cmunbi.otf"
)
showtext_auto()

# ===========================
# Global theme & palette
# ===========================
theme_thesis <- theme_bw(base_size = 12, base_family = "CMU Serif") +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text       = element_text(face = "bold", size = 16, family = "CMU Serif"),
    axis.text        = element_text(size = 14, family = "CMU Serif"),
    axis.title       = element_text(size = 16, family = "CMU Serif"),
    legend.position  = "bottom",
    legend.text      = element_text(size = 14, family = "CMU Serif"),
    legend.title     = element_text(size = 16, family = "CMU Serif"),
    plot.title       = element_text(size = 16, family = "CMU Serif"),
    plot.subtitle    = element_text(size = 14, family = "CMU Serif")
  )

# Canonical attribute order + colors
attr_levels <- c("Business Model","Location","Synergy","Budget")
attr_cols <- c(
  "Business Model" = "#1f77b4",
  "Location"       = "#2ca02c",
  "Synergy"        = "#d62728",
  "Budget"         = "#9467bd"
)

# ===========================
# HB Average (Forest) example data
# ===========================
coef_df <- tribble(
  ~group,           ~label,                ~utility,  ~std,    ~lower,    ~upper,
  "Business Model", "Certificate NGO",     25.00,     50.81,   15.98,     34.02,
  "Business Model", "Certificate Company", -54.64,    36.01,  -61.03,    -48.25,
  "Business Model", "Direct Investment",      29.64,     64.47,   18.20,     41.08,
  "Location",       "Regional",            14.28,     23.79,   10.06,     18.50,
  "Location",       "National",            -0.08,     21.69,   -3.93,      3.77,
  "Location",       "International",       -14.20,    35.47,  -20.50,     -7.91,
  "Synergy",        "No",                  -43.97,    34.96,  -50.18,    -37.77,
  "Synergy",        "Yes",                  43.97,    34.96,   37.77,     50.18,
  "Budget",         "Uni",                 -17.53,    40.36,  -24.69,    -10.37,
  "Budget",         "User",                 34.93,    49.34,   26.18,     43.69,
  "Budget",         "Funding",             -17.40,    48.56,  -26.02,     -8.78
) %>%
  mutate(
    sig   = ifelse(lower > 0 | upper < 0, "Significant", "Not significant"),
    group = factor(group, levels = attr_levels)
  ) %>%
  group_by(group) %>%
  arrange(utility, .by_group = TRUE) %>%
  mutate(label = fct_inorder(label)) %>%
  ungroup()

p_forest <- ggplot(coef_df, aes(x = label, y = utility, ymin = lower, ymax = upper, shape = sig)) +
  geom_hline(yintercept = 0, linewidth = 0.4, linetype = "dashed") +
  geom_errorbar(width = 0.15, linewidth = 0.5) +
  geom_point(size = 2.8) +
  coord_flip() +
  facet_wrap(~ group, scales = "free_y", ncol = 1, drop = FALSE) +
  labs(x = NULL, y = "Utility (HB units)", shape = "Significance") +
  theme_thesis

dir.create("hb_analysis", showWarnings = FALSE)
ggsave("hb_analysis/hb_utilities_forest.pdf", p_forest, width = 7.5, height = 8.5, device = cairo_pdf)

# ===========================
# Read Excel + Prep Long Data
# ===========================
xlsx <- "data/Excel Report - CBC HB.xlsx"  # <-- adjust if needed
sheets <- readxl::excel_sheets(xlsx)

sheet_util_zc  <- sheets[ str_detect(sheets, regex("Individual Utilities.*ZC", ignore_case = TRUE)) ]
sheet_util_raw <- sheets[ str_detect(sheets, regex("Individual Utilities.*Raw", ignore_case = TRUE)) ]
sheet_imps     <- sheets[ str_detect(sheets, regex("Individual Importances",  ignore_case = TRUE)) ]

if (length(sheet_util_zc) != 1) stop("Couldn't uniquely find 'Individual Utilities (ZC Diffs)' sheet.")
if (length(sheet_imps)    != 1) stop("Couldn't uniquely find 'Individual Importances' sheet.")
has_raw <- length(sheet_util_raw) == 1

util_zc  <- read_excel(xlsx, sheet = sheet_util_zc,  col_names = TRUE)
util_raw <- if (has_raw) read_excel(xlsx, sheet = sheet_util_raw, col_names = TRUE) else NULL
imps_ind <- read_excel(xlsx, sheet = sheet_imps,    col_names = TRUE)

# Helpers
ensure_id <- function(df) {
  cn <- names(df)
  id_col <- which(str_detect(tolower(cn), "id|resp|respondent"))
  if (length(id_col) == 0) {
    df <- df %>% mutate(RespondentID = row_number())
    id_name <- "RespondentID"
  } else {
    id_name <- cn[id_col[1]]
  }
  list(df = df, id = id_name)
}

level_to_group <- function(lbl) {
  dplyr::case_when(
    lbl %in% c("Certificate NGO","Certificate Company","Direct Investment") ~ "Business Model",
    lbl %in% c("Regional","National","International") ~ "Location",
    lbl %in% c("No","Yes") ~ "Synergy",
    lbl %in% c("Uni","User","Funding") ~ "Budget",
    TRUE ~ "Other"
  )
}

pivot_util_long <- function(df) {
  info <- ensure_id(df)
  df <- info$df; idcol <- info$id
  df %>%
    pivot_longer(
      cols = setdiff(names(.), idcol),
      names_to = "label",
      values_to = "utility"
    ) %>%
    mutate(group = level_to_group(label),
           label = as.factor(label)) %>%
    filter(!is.na(utility), group != "Other")
}

pivot_imps_long <- function(df) {
  info <- ensure_id(df)
  df <- info$df; idcol <- info$id
  df %>%
    pivot_longer(
      cols = setdiff(names(.), idcol),
      names_to = "attribute",
      values_to = "importance"
    ) %>%
    filter(!is.na(importance)) %>%
    mutate(attribute = factor(attribute, levels = attr_levels))
}

# Long data + enforced order
util_zc_long  <- pivot_util_long(util_zc)  %>% mutate(group = factor(group, levels = attr_levels))
util_raw_long <- if (has_raw) pivot_util_long(util_raw) %>% mutate(group = factor(group, levels = attr_levels)) else NULL
imps_long     <- pivot_imps_long(imps_ind) %>% mutate(attribute = factor(attribute, levels = attr_levels))

# ===========================
# 3a) Utilities (ZC) – violin/box
# ===========================
p_util_zc <- util_zc_long %>%
  group_by(group) %>%
  mutate(label = fct_reorder(label, utility, .fun = median, .desc = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = label, y = utility)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_violin(trim = FALSE, alpha = 0.25) +
  geom_boxplot(width = 0.35, outlier.size = 0.8) +
  coord_flip() +
  facet_wrap(~ group, scales = "free_y", ncol = 1, drop = FALSE) +
  labs(x = NULL, y = "Individual Utilities (ZC Diffs)") +
  theme_thesis

ggsave("hb_analysis/hb_individual_utilities_ZC_boxplots.pdf", p_util_zc, width = 7.5, height = 9.0, device = cairo_pdf)

# ===========================
# 3b) Utilities (Raw) – optional
# ===========================
if (!is.null(util_raw_long)) {
  p_util_raw <- util_raw_long %>%
    group_by(group) %>%
    mutate(label = fct_reorder(label, utility, .fun = median, .desc = TRUE)) %>%
    ungroup() %>%
    ggplot(aes(x = label, y = utility)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
    geom_violin(trim = FALSE, alpha = 0.25) +
    geom_boxplot(width = 0.35, outlier.size = 0.8) +
    coord_flip() +
    facet_wrap(~ group, scales = "free_y", ncol = 1, drop = FALSE) +
    labs(x = NULL, y = "Individual Utilities (Raw Units)") +
    theme_thesis
  
  ggsave("hb_analysis/hb_individual_utilities_RAW_boxplots.pdf", p_util_raw, width = 7.5, height = 9.0, device = cairo_pdf)
}

# ===========================
# 3c) Individual Importances — histograms
# ===========================
p_imps_hist <- imps_long %>%
  ggplot(aes(x = importance, fill = attribute)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 20,
    alpha = 0.2,           # very light bars
    color = "black",
    position = "identity"
  ) +
  geom_density(
    aes(color = attribute),
    linewidth = 0.8,
    fill = NA              # line only
  ) +
  facet_wrap(~ attribute, scales = "free", nrow = 2, drop = FALSE) +
  labs(x = "Importance (%)", y = "Density") +
  scale_fill_manual(values = attr_cols, breaks = attr_levels, limits = attr_levels) +
  scale_color_manual(values = attr_cols, breaks = attr_levels, limits = attr_levels) +
  theme_thesis +
  theme(legend.position = "none")

ggsave("hb_analysis/hb_individual_importances_hist.pdf", p_imps_hist, width = 7.5, height = 5.5, device = cairo_pdf)

# ===========================
# 4) Pie chart for average importances
# ===========================
avg_imps <- imps_long %>%
  group_by(attribute) %>%
  summarise(mean_importance = mean(importance, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    attribute = factor(attribute, levels = attr_levels),
    prop = mean_importance / sum(mean_importance)
  )

p_imps_pie <- ggplot(avg_imps, aes(x = "", y = prop, fill = attribute)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  labs(fill = "Attribute") +
  scale_fill_manual(values = attr_cols, breaks = attr_levels, limits = attr_levels) +
  theme_void(base_size = 12, base_family = "CMU Serif") +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold", size = 16, family = "CMU Serif"),
    legend.position = "right",
    legend.title    = element_text(face = "bold", size = 16, family = "CMU Serif"),
    legend.text     = element_text(size = 14, family = "CMU Serif")
  )

ggsave("hb_analysis/hb_average_importances_pie.pdf", p_imps_pie, width = 6.5, height = 6.0, device = cairo_pdf)

# ===========================
# End (no per-level histogram exports)
# ===========================


# ===========================
# Packages
# ===========================
# install.packages(c("tidyverse","showtext","posterior","scales"))
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(forcats)
library(ggplot2)
library(showtext)
library(posterior)
library(scales)

# ===========================
# Font + global theme (CMU Serif)
# ===========================
font_add(
  family = "CMU Serif",
  regular    = "C:/Users/peers/working/friends/luise/masterarbeit_luise/cm-unicode/cmunrm.otf",
  bold       = "C:/Users/peers/working/friends/luise/masterarbeit_luise/cm-unicode/cmunbx.otf",
  italic     = "C:/Users/peers/working/friends/luise/masterarbeit_luise/cm-unicode/cmunti.otf",
  bolditalic = "C:/Users/peers/working/friends/luise/masterarbeit_luise/cm-unicode/cmunbi.otf"
)
showtext_auto()

theme_thesis <- theme_bw(base_size = 12, base_family = "CMU Serif") +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text       = element_text(face = "bold", size = 16, family = "CMU Serif"),
    axis.text        = element_text(size = 14, family = "CMU Serif"),
    axis.title       = element_text(size = 16, family = "CMU Serif"),
    legend.position  = "none",   # keep figures clean
    plot.title       = element_text(size = 16, family = "CMU Serif"),
    plot.subtitle    = element_text(size = 14, family = "CMU Serif")
  )

# ===========================
# Attribute order + palettes
# ===========================
attr_levels <- c("Business Model","Location","Synergy","Budget")
# Attribute colors (used across all plots)
attr_cols <- c(
  "Business Model" = "#1f77b4",
  "Location"       = "#2ca02c",
  "Synergy"        = "#d62728",
  "Budget"         = "#9467bd"
)

# For convergence facets: distinct per-level colors *within* each attribute
# (repeat palette per facet so lines are easy to tell apart)
make_param_palette <- function(n) scales::hue_pal()(n)

# ===========================
# Load Lighthouse wide draws
# ===========================
draws_file <- "data/hb/CBC HB (3)-2025-15-8--15-49-16_draws.csv"  # <-- set your path
df_wide <- read_csv(draws_file, show_col_types = FALSE)

# Rename Carbon Removal to Direct Investment
df_wide <- df_wide %>%
  rename(`Direct Investment` = `Carbon Removal`)

# Identify non-parameter columns
non_param <- intersect(names(df_wide), c("Respondent", "Draw", "Fit (RLH)"))

# WIDE -> LONG
draws_df <- df_wide %>%
  pivot_longer(
    cols = setdiff(names(df_wide), non_param),
    names_to = "Parameter",
    values_to = "DrawValue"
  )

# Map level -> Attribute (keep your fixed order)
param_to_attr <- function(x) {
  dplyr::case_when(
    x %in% c("Certificate NGO", "Certificate Company", "Direct Investment") ~ "Business Model",
    x %in% c("Regional", "National", "International") ~ "Location",
    x %in% c("No", "Yes") ~ "Synergy",
    x %in% c("Uni", "User", "Funding") ~ "Budget",
    TRUE ~ "Other"
  )
}

draws_df <- draws_df %>%
  mutate(
    Attribute = factor(param_to_attr(Parameter), levels = c(attr_levels, "Other"))
  ) %>%
  filter(Attribute != "Other") %>%
  mutate(Attribute = factor(Attribute, levels = attr_levels))

# ===========================
# Global ordering: Attribute first, then level by posterior mean
# ===========================
param_means <- draws_df %>%
  group_by(Attribute, Parameter) %>%
  summarise(mu = mean(DrawValue, na.rm = TRUE), .groups = "drop") %>%
  group_by(Attribute) %>%
  arrange(desc(mu), .by_group = TRUE) %>%
  mutate(order_within = row_number()) %>%
  ungroup()

draws_df <- draws_df %>%
  left_join(param_means, by = c("Attribute","Parameter")) %>%
  mutate(
    attr_index = as.integer(Attribute),
    # one global factor that sorts by attribute (fixed) then by within-attribute order
    Parameter_global = fct_reorder(Parameter, attr_index + order_within/100, .desc = FALSE)
  )

# ===========================
# Output dir
# ===========================
out_dir <- "hb_analysis"

# ===========================
# 1) Posterior densities (facet by Attribute; colored by Attribute)
# ===========================
p_density_attr <- ggplot(draws_df, aes(x = DrawValue, fill = Attribute)) +
  geom_density(alpha = 0.35, color = NA) +
  facet_wrap(~ Attribute, ncol = 2, scales = "free") +
  scale_fill_manual(values = attr_cols, breaks = attr_levels) +
  labs(x = "Utility", y = "Density") +
  theme_thesis

ggsave(file.path(out_dir, "posterior_densities_by_attribute.pdf"), p_density_attr, width = 10, height = 8, device = cairo_pdf)
ggsave(file.path(out_dir, "posterior_densities_by_attribute.png"), p_density_attr, width = 10, height = 8, dpi = 300)

# ===========================
# 2) Violin + box (single panel), colored by Attribute, grouped by Attribute on x
# ===========================
p_violin_grouped <- ggplot(draws_df, aes(x = Parameter_global, y = DrawValue, fill = Attribute, color = Attribute)) +
  geom_violin(alpha = 0.18, scale = "width") +
  geom_boxplot(width = 0.12, outlier.size = 0.4, fill = "white") +
  coord_flip() +
  scale_fill_manual(values = attr_cols, breaks = attr_levels) +
  scale_color_manual(values = attr_cols, breaks = attr_levels) +
  labs(x = NULL, y = "Utility") +
  theme_thesis +
  # light dotted separators between attribute blocks
  geom_vline(
    data = param_means %>%
      group_by(Attribute) %>%
      summarise(last_pos = max(order_within), .groups = "drop") %>%
      mutate(cut_after = cumsum(last_pos) + 0.5),
    aes(xintercept = cut_after),
    linetype = "dotted",
    linewidth = 0.4,
    inherit.aes = FALSE
  )

ggsave(file.path(out_dir, "posterior_violin_box_grouped.pdf"), p_violin_grouped, width = 11, height = 7.5, device = cairo_pdf)
ggsave(file.path(out_dir, "posterior_violin_box_grouped.png"), p_violin_grouped, width = 11, height = 7.5, dpi = 300)

# ===========================
# 3) 95% Credible intervals (single panel), colored by Attribute
# ===========================
ci_df <- draws_df %>%
  group_by(Parameter_global, Attribute) %>%
  summarise(
    mean  = mean(DrawValue, na.rm = TRUE),
    lower = quantile(DrawValue, 0.025, na.rm = TRUE),
    upper = quantile(DrawValue, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # for separators later
  left_join(
    draws_df %>% distinct(Parameter_global, Parameter, Attribute, order_within, attr_index),
    by = c("Parameter_global","Attribute")
  )

p_forest_grouped <- ggplot(ci_df, aes(x = Parameter_global, y = mean, ymin = lower, ymax = upper,
                                      color = Attribute)) +
  geom_pointrange() +
  coord_flip() +
  scale_color_manual(values = attr_cols, breaks = attr_levels) +
  labs(x = NULL, y = "Utility") +
  theme_thesis +
  geom_vline(
    data = param_means %>%
      group_by(Attribute) %>%
      summarise(last_pos = max(order_within), .groups = "drop") %>%
      mutate(cut_after = cumsum(last_pos) + 0.5),
    aes(xintercept = cut_after),
    linetype = "dotted",
    linewidth = 0.4,
    inherit.aes = FALSE
  )

ggsave(file.path(out_dir, "credible_intervals_grouped.pdf"), p_forest_grouped, width = 10, height = 7.5, device = cairo_pdf)
ggsave(file.path(out_dir, "credible_intervals_grouped.png"), p_forest_grouped, width = 10, height = 7.5, dpi = 300)

# ===========================
# 4) Convergence plots
# ===========================
# Long table with Attribute tags for convergence
df_long <- df_wide %>%
  pivot_longer(
    cols = setdiff(names(df_wide), non_param),
    names_to = "Parameter",
    values_to = "Value"
  ) %>%
  mutate(
    Attribute = param_to_attr(Parameter),
    Attribute = factor(Attribute, levels = c(attr_levels, "Other"))
  ) %>%
  filter(Attribute != "Other") %>%
  mutate(Attribute = factor(Attribute, levels = attr_levels))

# Mean beta per parameter per iteration (across respondents)
iter_means <- df_long %>%
  group_by(Draw, Parameter, Attribute) %>%
  summarise(mean_beta = mean(Value, na.rm = TRUE), .groups = "drop")

burn_in <- 20000      # set to your preliminary iterations
x_max   <- 7000      # set to 7000 if you want to zoom in to 7,000

# ---- 4a) Single panel (color by Attribute) ----
p_conv <- ggplot(iter_means, aes(x = Draw, y = mean_beta, group = Parameter, color = Attribute)) +
  annotate("rect", xmin = 0, xmax = burn_in, ymin = -Inf, ymax = Inf,
           alpha = 0.12, fill = "grey60") +
  geom_line(linewidth = 0.55) +
  scale_x_continuous(limits = c(0, x_max), labels = comma) +
  scale_color_manual(values = attr_cols, breaks = attr_levels) +
  labs(x = "Iterations", y = "Mean Beta") +
  theme_thesis

ggsave(file.path(out_dir, "convergence_parameters_by_attribute_color.pdf"), p_conv, width = 10, height = 6, device = cairo_pdf)
ggsave(file.path(out_dir, "convergence_parameters_by_attribute_color.png"), p_conv, width = 10, height = 6, dpi = 300)

# ---- 4b) Faceted by Attribute (improved readability & color per level) ----
# Build a per-attribute color for each PARAMETER (so lines are distinguishable within a facet)
param_colors <- iter_means %>%
  distinct(Attribute, Parameter) %>%
  arrange(Attribute, Parameter) %>%
  group_by(Attribute) %>%
  mutate(col = make_param_palette(n()) ) %>%
  ungroup()

iter_means_col <- iter_means %>% left_join(param_colors, by = c("Attribute","Parameter"))

p_conv_attr <- ggplot(iter_means_col, aes(x = Draw, y = mean_beta, group = Parameter, color = col)) +
  annotate("rect", xmin = 0, xmax = burn_in, ymin = -Inf, ymax = Inf,
           alpha = 0.12, fill = "grey60") +
  geom_line(linewidth = 0.55, show.legend = FALSE) +
  facet_wrap(~ Attribute, ncol = 2, scales = "free_y") +
  scale_x_continuous(limits = c(0, x_max), labels = comma) +
  labs(x = "Iterations", y = "Mean Beta") +
  theme_thesis

ggsave(file.path(out_dir, "convergence_by_attribute_faceted.pdf"), p_conv_attr, width = 12, height = 8, device = cairo_pdf)
ggsave(file.path(out_dir, "convergence_by_attribute_faceted.png"), p_conv_attr, width = 12, height = 8, dpi = 300)


message("All plots saved in: ", normalizePath(out_dir),
        "\nTip: set x_max <- 7000 if you want to zoom in to 7,000 iterations.")


# =========================================================
# ================ SAVE CSV TABLES =========================
# =========================================================
dir.create(file.path(out_dir, "tables"), showWarnings = FALSE, recursive = TRUE)

# ---- Core tables from HB analysis ----
# 1. HB Average Coefficients (Forest Plot)
write_csv(coef_df, file.path(out_dir, "tables/hb_forest_coefficients.csv"))

# 2. Individual Utilities (Zero-Centered, long format)
write_csv(util_zc_long, file.path(out_dir, "tables/hb_individual_utilities_ZC_long.csv"))

# 3. Individual Utilities (Raw, if present)
if (!is.null(util_raw_long)) {
  write_csv(util_raw_long, file.path(out_dir, "tables/hb_individual_utilities_RAW_long.csv"))
}

# 4. Individual Importances (long format)
write_csv(imps_long, file.path(out_dir, "tables/hb_individual_importances_long.csv"))

# 5. Average Importances (for pie chart)
write_csv(avg_imps, file.path(out_dir, "tables/hb_average_importances.csv"))

# ---- Posterior draws and summaries ----
# 6. Posterior draws (long format)
write_csv(draws_df, file.path(out_dir, "tables/hb_posterior_draws_long.csv"))

# 7. Posterior means per parameter (used for ordering)
write_csv(param_means, file.path(out_dir, "tables/hb_posterior_means.csv"))

# 8. Credible intervals (95% CIs, for forest plot)
write_csv(ci_df, file.path(out_dir, "tables/hb_credible_intervals.csv"))

# 9. Convergence iteration means
write_csv(iter_means, file.path(out_dir, "tables/hb_convergence_iteration_means.csv"))

# 10. Colored convergence table (used for faceted convergence plots)
write_csv(iter_means_col, file.path(out_dir, "tables/hb_convergence_iteration_means_colored.csv"))


message("✅ All CSV tables saved in: ", normalizePath(file.path(out_dir, "tables")))
