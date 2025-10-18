# ============================================================
# CBC/HB CORRELATIONS — population, attributes, and levels
# with 95% CIs saved to CSV (heatmaps unchanged)
# ============================================================

# ---------- Packages ----------
# install.packages(c("readr","dplyr","stringr","tidyr","ggplot2","showtext"))
library(readr); library(dplyr); library(stringr); library(tidyr)
library(ggplot2); library(showtext)

# ---------- INPUTS ----------
cov_path      <- "data/hb/CBC HB (2)-2025-16-7--13-56-31_covariances.csv"
meanbeta_path <- "data/hb/CBC HB (2)-2025-16-7--13-56-31_meanbeta.csv"

out_dir <- "hb_analysis"
dir.create(out_dir, showWarnings = FALSE)

# Bootstrap reps for respondent-level CIs
B <- 1000  # increase to 1000+ for publication, time permitting

# ---------- Styling ----------
try({
  font_add(
    family    = "CMU Serif",
    regular   = "cm-unicode/cmunrm.otf",
    bold      = "cm-unicode/cmunbx.otf",
    italic    = "cm-unicode/cmunti.otf",
    bolditalic= "cm-unicode/cmunbi.otf"
  )
}, silent = TRUE)
showtext_auto()

theme_thesis <- theme_bw(base_size = 12, base_family = "CMU Serif") +
  theme(panel.grid.minor = element_blank())

# ============================================================
# 0) Mapping & fixed order (edit here if your naming differs)
# ============================================================
rename_map <- c("Carbon Removal" = "Direct Investment")  # harmonize label

level_to_attr <- function(lbl) {
  dplyr::case_when(
    lbl %in% c("Direct Investment","Certificate NGO","Certificate Company") ~ "Business Model",
    lbl %in% c("Regional","National","International")                      ~ "Location",
    lbl %in% c("Yes","No")                                                 ~ "Synergy",
    lbl %in% c("User","Funding","Uni")                                     ~ "Budget",
    TRUE ~ NA_character_
  )
}

attr_levels <- c("Business Model","Location","Synergy","Budget")
order_list <- list(
  "Business Model" = c("Direct Investment","Certificate NGO","Certificate Company"),
  "Location"       = c("Regional","National","International"),
  "Synergy"        = c("Yes","No"),
  "Budget"         = c("User","Funding","Uni")
)
global_levels_order <- unlist(order_list[attr_levels])

# ---------------- Helpers ----------------
# Reconstruct a covariance matrix from a vectorized row
reconstruct_cov_from_vector_row <- function(row_named, param_order = NULL) {
  cov_cols  <- names(row_named)
  variances <- cov_cols[!str_detect(cov_cols, "\\s+x\\s+")]
  cov_terms <- cov_cols[ str_detect(cov_cols, "\\s+x\\s+") ]
  if (length(cov_terms) > 0) {
    parts <- str_match(cov_terms, '^"?(.*?)"?\\s+x\\s+"?(.*?)"?$')[, 2:3, drop = FALSE]
    all_levels <- sort(unique(c(variances, as.vector(parts))))
  } else {
    all_levels <- sort(unique(variances))
  }
  if (!is.null(param_order)) {
    lvls   <- param_order[param_order %in% all_levels]
    extras <- setdiff(all_levels, lvls)
    all_levels <- c(lvls, extras)
  }
  K <- length(all_levels)
  C <- matrix(0, nrow = K, ncol = K, dimnames = list(all_levels, all_levels))
  for (v in variances) if (v %in% all_levels) C[v, v] <- as.numeric(row_named[[v]])
  for (cc in cov_terms) {
    m <- str_match(cc, '^"?(.*?)"?\\s+x\\s+"?(.*?)"?$')[, 2:3]
    a <- m[1]; b <- m[2]
    if (a %in% all_levels && b %in% all_levels) {
      val <- as.numeric(row_named[[cc]])
      C[a, b] <- val; C[b, a] <- val
    }
  }
  C
}

# Save a correlation matrix as long CSV with CIs
write_corr_with_ci <- function(R_point, R_lwr = NULL, R_upr = NULL, path) {
  rn <- rownames(R_point); cn <- colnames(R_point)
  long <- as.data.frame(R_point) |>
    mutate(row = rn) |>
    pivot_longer(-row, names_to = "col", values_to = "r") |>
    select(row, col, r)
  if (!is.null(R_lwr) && !is.null(R_upr)) {
    long_lwr <- as.data.frame(R_lwr) |>
      mutate(row = rn) |>
      pivot_longer(-row, names_to = "col", values_to = "lwr") |>
      select(row, col, lwr)
    long_upr <- as.data.frame(R_upr) |>
      mutate(row = rn) |>
      pivot_longer(-row, names_to = "col", values_to = "upr") |>
      select(row, col, upr)
    long <- long |> left_join(long_lwr, by = c("row","col")) |>
      left_join(long_upr, by = c("row","col"))
  } else {
    long$lwr <- NA_real_; long$upr <- NA_real_
  }
  write.csv(long, path, row.names = FALSE)
}

# ============================================================
# 1) POPULATION correlation (levels) from HB covariance
#    If multiple iterations present, compute CI across them
# ============================================================
# Read meanbeta header for ordering
param_order <- NULL
if (!is.null(meanbeta_path)) {
  mb_hdr <- read_csv(meanbeta_path, n_max = 1, show_col_types = FALSE)
  nm <- names(mb_hdr)
  nm[nm %in% names(rename_map)] <- rename_map[nm[nm %in% names(rename_map)]]
  names(mb_hdr) <- nm
  param_order <- names(mb_hdr)[-1]
}

cov_raw <- suppressWarnings(read_csv(cov_path, col_names = FALSE, show_col_types = FALSE))
is_square    <- nrow(cov_raw) == ncol(cov_raw) && nrow(cov_raw) > 1
looks_numeric<- all(sapply(cov_raw, function(col) all(suppressWarnings(!is.na(as.numeric(col))))))

if (is_square && looks_numeric) {
  cov_mat <- as.matrix(sapply(cov_raw, as.numeric))
  if (!is.null(param_order) && length(param_order) == nrow(cov_mat)) {
    dimnames(cov_mat) <- list(param_order, param_order)
  }
  corr_pop_levels <- cov2cor(cov_mat)
  # No iteration series → no CI available
  corr_pop_lwr <- corr_pop_upr <- NULL
} else {
  cov_df <- read_csv(cov_path, show_col_types = FALSE)
  # Harmonize headers if needed
  nms <- names(cov_df)
  nms[nms %in% names(rename_map)] <- rename_map[nms[nms %in% names(rename_map)]]
  names(cov_df) <- nms
  
  if ("Iteration" %in% names(cov_df)) {
    # Build correlation per iteration
    corr_list <- vector("list", nrow(cov_df))
    for (i in seq_len(nrow(cov_df))) {
      row_i <- cov_df[i, setdiff(names(cov_df), "Iteration"), drop = FALSE]
      C_i <- reconstruct_cov_from_vector_row(row_i, param_order = param_order)
      corr_list[[i]] <- cov2cor(C_i)
    }
    # Point = last iteration (or mean); here we use last row as in your earlier code
    corr_pop_levels <- corr_list[[length(corr_list)]]
    # CI across iterations (component-wise quantiles)
    arr <- simplify2array(corr_list)  # dims: p x p x T
    corr_pop_lwr <- apply(arr, c(1,2), function(x) quantile(x, 0.025, na.rm = TRUE))
    corr_pop_upr <- apply(arr, c(1,2), function(x) quantile(x, 0.975, na.rm = TRUE))
    dimnames(corr_pop_lwr) <- dimnames(corr_pop_levels)
    dimnames(corr_pop_upr) <- dimnames(corr_pop_levels)
  } else {
    # Single row → point only
    cov_row <- cov_df |> slice_tail(n = 1)
    cov_mat <- reconstruct_cov_from_vector_row(cov_row, param_order = param_order)
    corr_pop_levels <- cov2cor(cov_mat)
    corr_pop_lwr <- corr_pop_upr <- NULL
  }
}

# Save point matrix and long+CI
write.csv(corr_pop_levels, file.path(out_dir, "corr_levels_population.csv"), row.names = TRUE)
write_corr_with_ci(
  R_point = corr_pop_levels,
  R_lwr   = corr_pop_lwr,
  R_upr   = corr_pop_upr,
  path    = file.path(out_dir, "corr_levels_population_long_ci.csv")
)

# Heatmap (unchanged)
corr_long <- as.data.frame(corr_pop_levels) |>
  mutate(.row = rownames(corr_pop_levels)) |>
  pivot_longer(-.row, names_to = ".col", values_to = "r")
p_heat <- ggplot(corr_long, aes(x = .col, y = .row, fill = r)) +
  geom_tile() +
  scale_fill_gradient2(limits = c(-1, 1), midpoint = 0,
                       low = "#b2182b", mid = "white", high = "#2166ac") +
  coord_equal() +
  labs(x = NULL, y = NULL, fill = "r", title = "HB Population Correlations (Levels)") +
  theme_thesis +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(out_dir, "corr_levels_population_heatmap.pdf"), p_heat, width = 8.5, height = 7)
ggsave(file.path(out_dir, "corr_levels_population_heatmap.png"), p_heat, width = 8.5, height = 7, dpi = 300)

# ============================================================
# 2) ATTRIBUTES — correlation across respondents (with bootstrap CIs)
# ============================================================
mb <- read_csv(meanbeta_path, show_col_types = FALSE)
nm <- names(mb); nm[nm %in% names(rename_map)] <- rename_map[nm[nm %in% names(rename_map)]]
names(mb) <- nm
respondent_col <- names(mb)[1]

long <- mb |>
  pivot_longer(-all_of(respondent_col), names_to = "Level", values_to = "Beta") |>
  mutate(Level = factor(Level),
         Attribute = level_to_attr(as.character(Level))) |>
  filter(!is.na(Attribute))

# Percent importance per respondent
attr_imp <- long |>
  group_by(.data[[respondent_col]], Attribute) |>
  summarise(Importance = max(Beta, na.rm = TRUE) - min(Beta, na.rm = TRUE), .groups = "drop") |>
  group_by(.data[[respondent_col]]) |>
  mutate(PctImportance = 100 * Importance / sum(Importance)) |>
  ungroup()

wide_attr <- attr_imp |>
  select(Respondent = all_of(respondent_col), Attribute, PctImportance) |>
  mutate(Attribute = factor(Attribute, levels = attr_levels)) |>
  pivot_wider(names_from = Attribute, values_from = PctImportance)

# Point correlation
corr_attr <- cor(wide_attr |> select(-Respondent), use = "pairwise.complete.obs")
write.csv(corr_attr, file.path(out_dir, "corr_attributes_from_pctimportance.csv"), row.names = TRUE)

# Bootstrap CIs over respondents
set.seed(42)
Rlist <- vector("list", B)
X <- wide_attr |> select(-Respondent)
n <- nrow(X)
for (b in seq_len(B)) {
  idx <- sample.int(n, n, replace = TRUE)
  Rlist[[b]] <- suppressWarnings(cor(X[idx, , drop = FALSE], use = "pairwise.complete.obs"))
}
arr <- simplify2array(Rlist)  # p x p x B
attr_lwr <- apply(arr, c(1,2), function(x) quantile(x, 0.025, na.rm = TRUE))
attr_upr <- apply(arr, c(1,2), function(x) quantile(x, 0.975, na.rm = TRUE))
dimnames(attr_lwr) <- dimnames(corr_attr)
dimnames(attr_upr) <- dimnames(corr_attr)
write_corr_with_ci(corr_attr, attr_lwr, attr_upr,
                   file.path(out_dir, "corr_attributes_from_pctimportance_long_ci.csv"))

# Heatmap (unchanged)
attr_long <- as.data.frame(corr_attr) |>
  mutate(.row = rownames(corr_attr)) |>
  pivot_longer(-.row, names_to = ".col", values_to = "r")
p_attr <- ggplot(attr_long, aes(x = .col, y = .row, fill = r)) +
  geom_tile() +
  scale_fill_gradient2(limits = c(-1, 1), midpoint = 0,
                       low = "#b2182b", mid = "white", high = "#2166ac") +
  coord_equal() +
  labs(x = NULL, y = NULL, fill = "r") +
  theme_thesis +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(out_dir, "corr_attributes_heatmap.pdf"), p_attr, width = 5.5, height = 5)
ggsave(file.path(out_dir, "corr_attributes_heatmap.png"), p_attr, width = 5.5, height = 5, dpi = 300)

# ============================================================
# 3) LEVELS — correlation across respondents (with bootstrap CIs)
# ============================================================
wide_levels <- long |>
  select(Respondent = all_of(respondent_col), Level, Beta) |>
  filter(!is.na(level_to_attr(as.character(Level)))) |>
  mutate(Level = factor(Level,
                        levels = if (all(unique(Level) %in% global_levels_order))
                          global_levels_order else unique(Level))) |>
  arrange(Level) |>
  pivot_wider(names_from = Level, values_from = Beta)

corr_levels <- cor(wide_levels |> select(-Respondent), use = "pairwise.complete.obs")
write.csv(corr_levels, file.path(out_dir, "corr_levels.csv"), row.names = TRUE)

# Order rows/cols for plotting
lvl_names <- colnames(corr_levels)
lvl_df <- tibble(Level = lvl_names, Attribute = level_to_attr(lvl_names)) |>
  mutate(
    Attribute = factor(Attribute, levels = attr_levels),
    Level     = factor(Level,
                       levels = if (all(lvl_names %in% global_levels_order))
                         global_levels_order else lvl_names)
  ) |>
  arrange(Attribute, Level)
ord      <- as.character(lvl_df$Level)
corr_ord <- corr_levels[ord, ord, drop = FALSE]

# Bootstrap CIs for levels correlation
set.seed(43)
Xl <- wide_levels |> select(-Respondent)
nl <- nrow(Xl)
RlistL <- vector("list", B)
for (b in seq_len(B)) {
  idx <- sample.int(nl, nl, replace = TRUE)
  RlistL[[b]] <- suppressWarnings(cor(Xl[idx, , drop = FALSE], use = "pairwise.complete.obs"))
}
arrL <- simplify2array(RlistL)  # L x L x B
levels_lwr <- apply(arrL, c(1,2), function(x) quantile(x, 0.025, na.rm = TRUE))
levels_upr <- apply(arrL, c(1,2), function(x) quantile(x, 0.975, na.rm = TRUE))
dimnames(levels_lwr) <- dimnames(corr_levels)
dimnames(levels_upr) <- dimnames(corr_levels)
# Save long+CI in the plotting order (Attribute -> Level)
write_corr_with_ci(corr_ord[ord, ord, drop = FALSE],
                   levels_lwr[ord, ord, drop = FALSE],
                   levels_upr[ord, ord, drop = FALSE],
                   file.path(out_dir, "corr_levels_long_ci.csv"))

# Plot (unchanged)
sep_df <- lvl_df |>
  count(Attribute, name = "n_levels") |>
  mutate(cut_after = cumsum(n_levels) + 0.5)

corr_long_levels <- as.data.frame(corr_ord) |>
  mutate(.row = factor(rownames(corr_ord), levels = ord)) |>
  pivot_longer(-.row, names_to = ".col", values_to = "r") |>
  mutate(.col = factor(.col, levels = ord))

p_levels <- ggplot(corr_long_levels, aes(x = .col, y = .row, fill = r)) +
  geom_tile() +
  scale_fill_gradient2(limits = c(-1, 1), midpoint = 0,
                       low = "#b2182b", mid = "white", high = "#2166ac") +
  coord_equal() +
  labs(x = NULL, y = NULL, fill = "r") +
  theme_thesis +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1,
                               family = "CMU Serif", size = 12),
    axis.text.y = element_text(family = "CMU Serif", size = 12),
    plot.margin = margin(10, 20, 10, 10)
  ) +
  geom_hline(data = sep_df, aes(yintercept = cut_after), color = "grey30", linewidth = 0.4) +
  geom_vline(data = sep_df, aes(xintercept = cut_after), color = "grey30", linewidth = 0.4)

ggsave(file.path(out_dir, "corr_levels_heatmap.pdf"), p_levels, width = 8.5, height = 7.5)
ggsave(file.path(out_dir, "corr_levels_heatmap.png"), p_levels, width = 8.5, height = 7.5, dpi = 300)

# ============================================================
# 4) Optional: Attribute correlation implied by levels (block averages) + CIs
# ============================================================
attr_of <- setNames(level_to_attr(lvl_names), lvl_names)
attrs   <- attr_levels
agg_attr_corr <- matrix(NA_real_, nrow = length(attrs), ncol = length(attrs),
                        dimnames = list(attrs, attrs))
for (i in seq_along(attrs)) {
  for (j in seq_along(attrs)) {
    Li <- names(attr_of)[attr_of == attrs[i]]
    Lj <- names(attr_of)[attr_of == attrs[j]]
    if (length(Li) > 0 && length(Lj) > 0) {
      agg_attr_corr[i, j] <- mean(corr_levels[Li, Lj, drop = FALSE], na.rm = TRUE)
    }
  }
}
write.csv(agg_attr_corr, file.path(out_dir, "corr_attributes_from_levels.csv"), row.names = TRUE)

# CIs for block-averaged attribute correlation (reuse level bootstraps)
agg_list <- vector("list", B)
for (b in seq_len(B)) {
  Rb <- RlistL[[b]]
  A <- matrix(NA_real_, nrow = length(attrs), ncol = length(attrs),
              dimnames = list(attrs, attrs))
  for (i in seq_along(attrs)) {
    for (j in seq_along(attrs)) {
      Li <- names(attr_of)[attr_of == attrs[i]]
      Lj <- names(attr_of)[attr_of == attrs[j]]
      if (length(Li) > 0 && length(Lj) > 0) {
        A[i, j] <- mean(Rb[Li, Lj, drop = FALSE], na.rm = TRUE)
      }
    }
  }
  agg_list[[b]] <- A
}
arrA <- simplify2array(agg_list)
agg_lwr <- apply(arrA, c(1,2), function(x) quantile(x, 0.025, na.rm = TRUE))
agg_upr <- apply(arrA, c(1,2), function(x) quantile(x, 0.975, na.rm = TRUE))
dimnames(agg_lwr) <- dimnames(agg_attr_corr)
dimnames(agg_upr) <- dimnames(agg_attr_corr)
write_corr_with_ci(agg_attr_corr, agg_lwr, agg_upr,
                   file.path(out_dir, "corr_attributes_from_levels_long_ci.csv"))

# Optional heatmap (unchanged)
agg_long <- as.data.frame(agg_attr_corr) |>
  mutate(.row = rownames(agg_attr_corr)) |>
  pivot_longer(-.row, names_to = ".col", values_to = "r")
p_attr_from_levels <- ggplot(agg_long, aes(x = .col, y = .row, fill = r)) +
  geom_tile() +
  scale_fill_gradient2(limits = c(-1, 1), midpoint = 0,
                       low = "#b2182b", mid = "white", high = "#2166ac") +
  coord_equal() +
  labs(x = NULL, y = NULL, fill = "r", title = "Attribute Correlation (Averaged over Level–Level Correlations)") +
  theme_thesis +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(out_dir, "corr_attributes_from_levels_heatmap.pdf"), p_attr_from_levels, width = 5.5, height = 5)
ggsave(file.path(out_dir, "corr_attributes_from_levels_heatmap.png"), p_attr_from_levels, width = 5.5, height = 5, dpi = 300)

message(
  "Done. Wrote to ", normalizePath(out_dir), ":\n",
  " - corr_levels_population.csv & corr_levels_population_long_ci.csv (+ heatmap)\n",
  " - corr_attributes_from_pctimportance.csv & corr_attributes_from_pctimportance_long_ci.csv (+ heatmap)\n",
  " - corr_levels.csv & corr_levels_long_ci.csv (+ heatmap with attribute blocks)\n",
  " - corr_attributes_from_levels.csv & corr_attributes_from_levels_long_ci.csv (+ heatmap)\n"
)

