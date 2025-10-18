library(ggplot2)
library(readr)
library(dplyr)
library(gridExtra) 
library(extrafont)
library(knitr)
library(kableExtra)
library(tidyr)
library(showtext)

font_add(
  family = "CMU Serif",
  regular    = "cm-unicode/cmunrm.otf",
  bold       = "cm-unicode/cmunbx.otf",
  italic     = "cm-unicode/cmunti.otf",
  bolditalic = "cm-unicode/cmunbi.otf"
)

showtext_auto()



df <- read.csv("data/CsvAllFields.csv", stringsAsFactors = FALSE)


# --- Mapping dictionaries (English only) ---
gender_map <- c(
  "1" = "Male",
  "2" = "Female",
  "3" = "Diverse",
  "4" = "Not specified"
)

sustainability_map <- c(
  "1" = "Not at all interested",
  "2" = "Rather not interested",
  "3" = "Neutral",
  "4" = "Rather interested",
  "5" = "Very interested"
)

experience_map <- c(
  "1" = "Yes, actively involved",
  "2" = "Yes, passively informed",
  "3" = "No, no points of contact"
)

education_map <- c(
  "1" = "No degree",
  "2" = "School degree",
  "3" = "Apprenticeship",
  "4" = "Bachelor's degree",
  "5" = "Master's degree",
  "6" = "State examination",
  "7" = "Doctorate",
  "8" = "Other"
)

university_map <- c(
  "1" = "University of St.Gallen",
  "2" = "University of Konstanz"
)

stakeholder_map <- c(
  "1" = "University management",
  "2" = "Sustainability Officer",
  "3" = "Academic staff",
  "4" = "Administrative staff",
  "5" = "Students",
  "6" = "Other"
)

certificate_map <- c(
  "1" = "Yes",
  "2" = "No"
)

degree_map <- c(
  "1" = "Assessment (HSG only)",
  "2" = "Bachelor",
  "3" = "Master",
  "4" = "PhD",
  "5" = "State examination"
)

tenure_map <- c(
  "1" = "Less than 5 years",
  "2" = "5 to 10 years",
  "3" = "More than 10 years"
)

# --- Apply mapping as factors with all possible levels ---
df$Gender         <- factor(gender_map[as.character(df$Gender)], levels = gender_map)
df$Sustainability <- factor(sustainability_map[as.character(df$Sustainability)], levels = sustainability_map)
df$Experience     <- factor(experience_map[as.character(df$Experience)], levels = experience_map)
df$Education      <- factor(education_map[as.character(df$Education)], levels = education_map)
df$University     <- factor(university_map[as.character(df$University)], levels = university_map)
df$Stakeholder    <- factor(stakeholder_map[as.character(df$Stakeholder)], levels = stakeholder_map)
df$Certificate    <- factor(certificate_map[as.character(df$Certificate)], levels = certificate_map)
df$Degree         <- factor(degree_map[as.character(df$Degree)], levels = degree_map)
df$Tenure         <- factor(tenure_map[as.character(df$Tenure)], levels = tenure_map)


# --- Plot Age overall ---
p_age <- ggplot(df, aes(x = Age)) +
  geom_histogram(bins = 20, fill = "seagreen", color = "black", na.rm = TRUE) +
  labs(x = "Age", y = "Count") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14, family = "CMU Serif"),      # tick labels
    axis.title = element_text(size = 16, family = "CMU Serif")      # axis labels
  )

ggsave("respondents/dist_Age.pdf", plot = p_age, width = 7, height = 5, device = cairo_pdf)


# --- Plot Age by University in 1x2 layout ---
p_age_uni <- ggplot(df, aes(x = Age)) +
  geom_histogram(bins = 20, fill = "seagreen", color = "black", na.rm = TRUE) +
  facet_wrap(~ University, ncol = 2) +
  labs(x = "Age", y = "Count") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14, family = "CMU Serif"),
    axis.title = element_text(size = 16, family = "CMU Serif"),
    strip.text = element_text(size = 16, family = "CMU Serif")  # facet labels (university names)
  )

ggsave("respondents/dist_Age_by_University.pdf", plot = p_age_uni, width = 10, height = 5, device = cairo_pdf)

#Latex Tables

cat_vars <- c(
  "University", "Degree", "Certificate", "Stakeholder",
  "Education", "Experience", "Sustainability", "Tenure"
)

# ---- Get counts and percentages ----
get_counts <- function(data, varname) {
  var_data <- factor(data[[varname]], levels = levels(df[[varname]]))
  tab <- as.data.frame(table(var_data))
  names(tab) <- c("Category", "Count")
  tab$Percent <- if (sum(tab$Count) > 0) round(tab$Count / sum(tab$Count) * 100, 1) else 0
  tab$Variable <- varname
  tab <- tab[, c("Variable", "Category", "Count", "Percent")] # Variable first
  tab
}

# ---- Create table with \addlinespace between variables ----
make_desc_table <- function(data, caption_text) {
  desc_table <- bind_rows(lapply(cat_vars, function(v) get_counts(data, v)))
  
  # Identify last row per variable for spacing
  desc_table <- desc_table %>%
    group_by(Variable) %>%
    mutate(addspace = row_number() == n()) %>%
    ungroup()
  
  kable(
    desc_table %>% select(-addspace),
    format = "latex",
    booktabs = TRUE,
    caption = caption_text,
    col.names = c("Variable", "Category", "Count", "Percent")
  ) %>%
    row_spec(which(desc_table$addspace), extra_latex_after = "\\addlinespace")
}

# ---- Generate tables ----
table_overall  <- make_desc_table(df, "Descriptive statistics for all respondents")


cat_vars <- c(
  "Degree", "Certificate", "Stakeholder",
  "Education", "Experience", "Sustainability", "Tenure"
)

table_stgallen <- make_desc_table(df %>% filter(University == "University of St.Gallen"),
                                  "Descriptive statistics for University of St.Gallen")
table_konstanz <- make_desc_table(df %>% filter(University == "University of Konstanz"),
                                  "Descriptive statistics for University of Konstanz")

# ---- Output ----
table_overall
table_stgallen
table_konstanz


# Table Vizualisation
cat_vars <- c(
  "University", "Degree", "Certificate", "Stakeholder",
  "Education", "Experience", "Sustainability", "Tenure"
)

# Convert to long format & set factor order
df_long <- df %>%
  select(all_of(cat_vars)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = factor(Variable, levels = cat_vars))

p_facets <- ggplot(df_long, aes(x = Value)) +
  geom_bar(fill = "seagreen", color = "black") +
  facet_wrap(~ Variable, ncol = 2, scales = "free_x") +
  labs(x = NULL, y = "Count") +
  theme_minimal(base_family = "CMU Serif") +
  theme(
    axis.text.x  = element_text(size = 14, angle = 45, hjust = 1, family = "CMU Serif"),
    axis.text.y  = element_text(size = 14, family = "CMU Serif"),
    axis.title.y = element_text(size = 16, family = "CMU Serif", vjust = 2),  # adjust inward
    strip.text   = element_text(size = 16, family = "CMU Serif"),
    plot.margin  = margin(t = 10, r = 10, b = 10, l = 40),
    panel.spacing.x = unit(2, "cm")   # extra space between the two columns
  )

  

ggsave(
  "respondents/categorical_facets_4x2.pdf",
  plot = p_facets,
  width = 10,
  height = 14,
  device = cairo_pdf
)

# ---- Save descriptive tables as CSV files ----
# Helper to convert LaTeX kable to data frame again
extract_table_df <- function(data, vars) {
  bind_rows(lapply(vars, function(v) get_counts(data, v)))
}

cat_vars_all <- c(
  "University", "Degree", "Certificate", "Stakeholder",
  "Education", "Experience", "Sustainability", "Tenure"
)

cat_vars_uni <- c(
  "Degree", "Certificate", "Stakeholder",
  "Education", "Experience", "Sustainability", "Tenure"
)

# Create data frames for export
df_overall   <- extract_table_df(df, cat_vars_all)
df_stgallen  <- extract_table_df(df %>% filter(University == "University of St.Gallen"), cat_vars_uni)
df_konstanz  <- extract_table_df(df %>% filter(University == "University of Konstanz"), cat_vars_uni)

# Save as CSVs
write_csv(df_overall,  "respondents/table_overall.csv")
write_csv(df_stgallen, "respondents/table_stgallen.csv")
write_csv(df_konstanz, "respondents/table_konstanz.csv")

