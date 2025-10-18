# make_count_analysis_outputs.R
# Creates LaTeX tables + plots for every CSV in count_analysis/

# ---- packages ----
need <- c("tidyverse", "kableExtra", "glue", "readr","showtext")

to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)
suppressPackageStartupMessages({
  library(tidyverse)
  library(kableExtra)
  library(glue)
  library(readr)
})

library(showtext)

font_add(
  family = "CMU Serif",
  regular    = "cm-unicode/cmunrm.otf",
  bold       = "cm-unicode/cmunbx.otf",
  italic     = "cm-unicode/cmunti.otf",
  bolditalic = "cm-unicode/cmunbi.otf"
)

showtext_auto()





# ---- paths ----
in_dir  <- "count_analysis"
tab_dir <- file.path(in_dir, "tables")
plt_dir <- file.path(in_dir, "plots")
dir.create(tab_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plt_dir, showWarnings = FALSE, recursive = TRUE)

# ---- helpers ----

# Clean numeric values that may use comma decimal or include "*"
parse_val <- function(x) {
  x <- trimws(as.character(x))
  x[x == "*"] <- NA
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

is_stat_row <- function(x) {
  x %in% c("Within Att. Chi-Square", "Interaction Chi-Square",
           "D.F.", "Significance", "Warning")
}

# Make a nice footnote text from stats
stats_as_note <- function(stats_df, N) {
  chi <- stats_df %>%
    filter(`Category_1` %in% c("Within Att. Chi-Square", "Interaction Chi-Square")) %>%
    pull(Value) %>% first()
  
  df  <- stats_df %>% filter(`Category_1` == "D.F.") %>% pull(Value) %>% first()
  sig <- stats_df %>% filter(`Category_1` == "Significance") %>% pull(`Value`) %>% first()
  warn <- stats_df %>% filter(`Category_1` == "Warning") %>% pull(`Value`) %>% first()
  
  note <- glue("N = {N}; $\\chi^2$ = {chi %||% 'NA'}; df = {df %||% 'NA'}; sig = {sig %||% 'NA'}")
  if (!is.null(warn) && !is.na(warn) && nzchar(warn)) {
    note <- glue("{note}. Warning: {warn}")
  }
  note
}

`%||%` <- function(a, b) if (is.null(a) || (length(a)==0) || is.na(a)) b else a

# Style kable for LaTeX
style_kable <- function(kbl_obj, note_txt) {
  kbl_obj %>%
    kableExtra::kable_styling(latex_options = c("HOLD_position", "scale_down")) %>%
    kableExtra::footnote(general = note_txt, threeparttable = TRUE, escape = TRUE)
}
# ---- main loop ----
csvs <- list.files("data/count_results", pattern = "\\.csv$", full.names = TRUE)

for (f in csvs) {
  message("Processing: ", basename(f))
  
  raw <- read_csv(f, show_col_types = FALSE)
  stopifnot(all(c("Category_1", "Category_2", "Value") %in% names(raw)))
  
  # Extract N and stats
  N <- raw %>%
    filter(Category_1 == "Total Respondents") %>%
    pull(Value) %>%
    suppressWarnings(parse_val(.)) %>%
    first() %||% NA_real_
  
  stats_df <- raw %>% filter(is_stat_row(Category_1))
  
  # Data rows only + numeric
  dat <- raw %>%
    filter(!Category_1 %in% c("Total Respondents") & !is_stat_row(Category_1)) %>%
    mutate(Value_num = parse_val(Value))
  
  is_interaction <- dat %>%
    filter(!is.na(Category_2) & Category_2 != "") %>%
    nrow() > 0
  
  # -------- LaTeX table (unchanged) --------
  out_base <- tools::file_path_sans_ext(basename(f))
  tex_path <- file.path(tab_dir, paste0(out_base, ".tex"))
  
  if (!is_interaction) {
    tbl_df <- dat %>% select(Category = Category_1, Value = Value_num)
    
    kbl_obj <- knitr::kable(
      tbl_df,
      format = "latex",
      booktabs = TRUE,
      digits = 3,
      caption = gsub("_", " ", out_base),
      col.names = c("Category", "Value")
    )
    
    note_txt <- stats_as_note(stats_df, N)
    sty <- style_kable(kbl_obj, note_txt)
    writeLines(as.character(sty), tex_path)
    
  } else {
    mat_df <- dat %>%
      mutate(Category_2 = ifelse(is.na(Category_2) | Category_2 == "", "(Total)", Category_2)) %>%
      select(Category_1, Category_2, Value_num) %>%
      pivot_wider(names_from = Category_2, values_from = Value_num) %>%
      arrange(Category_1)
    
    kbl_obj <- knitr::kable(
      mat_df,
      format = "latex",
      booktabs = TRUE,
      digits = 3,
      caption = gsub("_", " ", out_base),
      col.names = c("Category", names(mat_df)[-1])
    )
    
    note_txt <- stats_as_note(stats_df, N)
    sty <- style_kable(kbl_obj, note_txt)
    writeLines(as.character(sty), tex_path)
  }
  
  # -------- Plots --------
  plt_base <- file.path(plt_dir, out_base) # per-file base name
  
  if (!is_interaction) {
    p <- dat %>%
      ggplot(aes(x = reorder(Category_1, Value_num), y = Value_num)) +
      geom_col(fill = "seagreen", color = "black") +
      coord_flip() +
      labs(
        x = NULL,
        y = "Value",
        subtitle = glue("N = {N} | chi-square & sig in table footnote")
      ) +
      theme_minimal(base_family = "CMU Serif") +
      theme(
        axis.text = element_text(size = 14, family = "CMU Serif"),
        axis.title = element_text(size = 16, family = "CMU Serif"),
        title = element_text(size = 16, family = "CMU Serif"),
        plot.subtitle = element_text(size = 14, family = "CMU Serif")
      )
    
  } else {
    p <- dat %>%
      mutate(Category_2 = ifelse(is.na(Category_2) | Category_2 == "", "(Total)", Category_2)) %>%
      ggplot(aes(x = Category_1, y = Value_num, fill = Category_2)) +
      geom_col(position = position_dodge(width = 0.9), color = "black") +
      labs(
        x = NULL,
        y = "Value",
        fill = "",
        subtitle = glue("N = {N} | chi-square & sig in table footnote")
      ) +
      theme_minimal(base_family = "CMU Serif") +
      theme(
        axis.text.x = element_text(size = 14, angle = 30, hjust = 1, family = "CMU Serif"),
        axis.text.y = element_text(size = 14, family = "CMU Serif"),
        axis.title = element_text(size = 16, family = "CMU Serif"),
        legend.text = element_text(size = 14, family = "CMU Serif"),
        legend.title = element_text(size = 16, family = "CMU Serif"),
        title = element_text(size = 16, family = "CMU Serif"),
        plot.subtitle = element_text(size = 14, family = "CMU Serif")
      )
  }
  
  # Save with Cairo (embeds CMU TTF)
  ggsave(paste0(plt_base, ".pdf"), p, width = 8, height = 5, device = cairo_pdf)
}

message("Done. LaTeX tables in: ", normalizePath(tab_dir))
message("Plots in: ", normalizePath(plt_dir))

