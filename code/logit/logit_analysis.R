# =========================================================
# Libraries & Font Setup
# =========================================================
library(ggplot2)
library(dplyr)
library(tibble)
library(forcats)
library(showtext)

# ---- Font setup (CMU Serif) ----
font_add(
  family    = "CMU Serif",
  regular   = "cm-unicode/cmunrm.otf",
  bold      = "cm-unicode/cmunbx.otf",
  italic    = "cm-unicode/cmunti.otf",
  bolditalic= "cm-unicode/cmunbi.otf"
)
showtext_auto()

# ---- Global theme ----
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

# =========================================================
# Canonical attribute order & colors
# =========================================================
attr_levels <- c("Business Model", "Location", "Synergy", "Budget")
attr_cols <- c(
  "Business Model" = "#1f77b4",
  "Location"       = "#2ca02c",
  "Synergy"        = "#d62728",
  "Budget"         = "#9467bd"
)

# =========================================================
# ====================== FOREST PLOT ======================
# =========================================================
coef_df <- tribble(
  ~group,            ~label,               ~utility,  ~se,      ~t,
  "Business Model",  "Certificate NGO",     0.23189,  0.05579,  4.15655,
  "Business Model",  "Certificate Company", -0.55276, 0.06144, -8.99609,
  "Business Model",  "Direct Investment",      0.32086,  0.06372,  5.03582,
  "Location",        "Regional",            0.13305,  0.06454,  2.06148,
  "Location",        "National",            0.03098,  0.05537,  0.55947,
  "Location",        "International",      -0.16403,  0.05717, -2.86897,
  "Synergy",         "No",                 -0.44551,  0.03928, -11.34067,
  "Synergy",         "Yes",                 0.44551,  0.03928, 11.34067,
  "Budget",          "Uni",                -0.15512,  0.05471, -2.83518,
  "Budget",          "User",                0.35345,  0.05311,  6.65456,
  "Budget",          "Funding",            -0.19833,  0.05603, -3.53954
) %>%
  mutate(
    group = factor(group, levels = attr_levels),
    lower = utility - 1.96*se,
    upper = utility + 1.96*se,
    sig   = factor(ifelse(abs(t) >= 1.96, "p < .05", "n.s."),
                   levels = c("p < .05","n.s."))
  ) %>%
  group_by(group) %>%
  arrange(utility, .by_group = TRUE) %>%
  mutate(label = fct_inorder(label)) %>%
  ungroup()

p_forest <- ggplot(
  coef_df,
  aes(x = label, y = utility, ymin = lower, ymax = upper, shape = sig)
) +
  geom_hline(yintercept = 0, linewidth = 0.4, linetype = "dashed") +
  geom_errorbar(width = 0.15, linewidth = 0.5) +
  geom_point(size = 2.8) +
  coord_flip() +
  facet_wrap(~ group, scales = "free_y", ncol = 1, drop = FALSE) +
  labs(x = NULL, y = "Utility (logit units)", shape = "Significance") +
  theme_thesis

dir.create("logit_analysis", showWarnings = FALSE)
ggsave("logit_analysis/logit_coefficients_forest.pdf", p_forest, width = 7.5, height = 8.5, device = cairo_pdf)

# =========================================================
# ================= UTILITIES & IMPORTANCES ===============
# =========================================================
utils <- tribble(
  ~group,          ~label,               ~utility,
  "Business Model","Certificate NGO",     35.49,
  "Business Model","Certificate Company", -84.60,
  "Business Model","Direct Investment",       49.11,
  "Location",      "Regional",             20.36,
  "Location",      "National",              4.74,
  "Location",      "International",       -25.10,
  "Synergy",       "No",                  -68.19,
  "Synergy",       "Yes",                  68.19,
  "Budget",        "Uni",                 -23.74,
  "Budget",        "User",                 54.10,
  "Budget",        "Funding",             -30.35
) %>%
  mutate(group = factor(group, levels = attr_levels)) %>%
  group_by(group) %>%
  arrange(desc(utility), .by_group = TRUE) %>%
  mutate(label = fct_inorder(label)) %>%
  ungroup()

imps <- tribble(
  ~attribute,       ~importance,
  "Business Model",  33.43,
  "Location",        11.37,
  "Synergy",         34.09,
  "Budget",          21.11
) %>%
  mutate(attribute = factor(attribute, levels = attr_levels))

# ---- Plot 1: Average Utilities (greys only) ----
p_utils <- ggplot(utils, aes(x = label, y = utility, fill = utility > 0)) +
  geom_hline(yintercept = 0, linewidth = 0.5, linetype = "dashed") +
  geom_col(width = 0.65) +
  coord_flip() +
  facet_wrap(~ group, scales = "free_y", ncol = 1, drop = FALSE) +
  scale_fill_manual(values = c("FALSE" = "grey70", "TRUE" = "grey30"), guide = "none") +
  labs(x = NULL, y = "Average Utility (Zero-Centered)") +
  theme_thesis

# ---- Plot 2: Average Importances (greys only) ----
p_imps <- ggplot(imps, aes(x = attribute, y = importance)) +
  geom_col(width = 0.65, fill = "grey50") +
  coord_flip() +
  labs(x = NULL, y = "Average Importance (%)") +
  theme_thesis

print(p_utils)
print(p_imps)

ggsave("logit_analysis/average_utilities_diverging.pdf", p_utils, width = 7.5, height = 8.5, device = cairo_pdf)
ggsave("logit_analysis/average_importances.pdf",       p_imps,  width = 6.5, height = 3.8, device = cairo_pdf)

# =========================================================
# ========================= PIE ===========================
# =========================================================
imps_pie <- imps %>%
  mutate(frac = importance / sum(importance),
         lab  = paste0(attribute, " (", sprintf("%.1f", importance), "%)"))

p_pie_imps <- ggplot(imps_pie, aes(x = "", y = frac, fill = attribute)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = attr_cols, limits = attr_levels) +
  labs(fill = "Attribute", x = NULL, y = NULL) +
  theme_void(base_size = 12, base_family = "CMU Serif") +
  theme(
    legend.position = "right",
    legend.text     = element_text(size = 14, family = "CMU Serif"),
    legend.title    = element_text(size = 16, family = "CMU Serif"),
    plot.title      = element_text(size = 16, family = "CMU Serif")
  )

print(p_pie_imps)
ggsave("logit_analysis/pie_overall_importances.pdf", p_pie_imps, width = 6.5, height = 4.8, device = cairo_pdf)

# Create folder if it doesn’t exist
dir.create("logit_analysis/tables", showWarnings = FALSE, recursive = TRUE)

# 1. Coefficients table (logit estimates with SE, CI, etc.)
write_csv(coef_df, "logit_analysis/tables/logit_coefficients.csv")

# 2. Utilities table (zero-centered)
write_csv(utils, "logit_analysis/tables/average_utilities.csv")

# 3. Importances table (percentages)

write_csv(imps, "logit_analysis/tables/average_importances.csv")
