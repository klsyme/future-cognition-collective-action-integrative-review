library(stringr)
library(ggplot2)
library(ggmosaic)
library(tidyr)
library(dplyr)
library(gt)
library(scales)
library(writexl)

### -----------------------------------------------------
### Helper: clean results field
### -----------------------------------------------------
clean_results <- function(x) {
  x %>%
    sub(":.*", "", .) %>%      # remove everything after colon
    trimws() %>%               # remove whitespace
    str_to_title()             # standardize capitalization
}

### -----------------------------------------------------
### t1: Anticipated Emotion
### -----------------------------------------------------
t1 <- Anticipated_Emotion_w_K_annotation_updated

t1_clean <- t1 %>%
  filter(`Article Title` !=
           "Does Fear of the New Coronavirus Lead to Low-Carbon Behaviors: The Moderating Effect of Outcome Framing") %>%
  mutate(
    Results_type_final = clean_results(AE_Results_type_updated),
    Set = "AE"
  )

### -----------------------------------------------------
### t2: CFC
### -----------------------------------------------------
t2 <- Consideration_of_Future_Consequences_w_K_annotation_updated %>%
  filter(DOI != "10.1016/j.futures.2021.102711") %>%
  mutate(
    Results_type_final = clean_results(CFC_Results_type_updated),
    Set = "CFC"
  )

### -----------------------------------------------------
### t3: Construal Level Theory
### -----------------------------------------------------
t3_clean <- Construal_Level_Theory_w_K_annotation_updated %>%
  filter(CLT_Results_types_updated != "na") %>%
  filter(`Article Title` !=
           "The impact of uncertainty on tourists? controllability, mood state and the persuasiveness of message framing in the pandemic era") %>%
  mutate(
    Results_type_final = clean_results(CLT_Results_types_updated),
    Set = "CLT"
  )

### -----------------------------------------------------
### t4: Episodic Future Thinking
### -----------------------------------------------------
t4 <- Episodic_Future_Thinking_w_K_annotation_updated %>%
  mutate(
    Results_type_final = clean_results(EFT_Results_type_updated),
    Set = "EFT"
  )

### -----------------------------------------------------
### t5: Temporal Discounting
### -----------------------------------------------------
t5 <- Temporal_Discounting_w_K_annotation_updated %>%
  filter(!`Article Title` %in% c(
    "Accounting for the importance of psychological distance in assessing public preferences for air quality improvement policies: an application of the integrated choice and latent variable model",
    "Moral Future-Thinking: Does the Moral Circle Stand the Test of Time?"
  )) %>% 
  mutate(
    Results_type_final = clean_results(TD_Results_type_updated),
    Set = "TD"
  )

### -----------------------------------------------------
### t6: Utopian Thinking
### -----------------------------------------------------
t6 <- Utopian_Thinking_w_K_annotation_updated %>%
  mutate(
    Results_type_final = clean_results(UT_Results_type_updated),
    Set = "UT"
  )

### -----------------------------------------------------
### t7: Cognitive Alternatives
### -----------------------------------------------------
t7 <- Cognitive_Alternatives_w_K_annotation_updated %>%
  filter(DOI != "10.5964/gep.11105") %>%        # <-- remove row here
  mutate(
    Results_type_final = clean_results(CA_Results_type_updated),
    Set = "CA"
  )

### -----------------------------------------------------
### Standardize selection AFTER results are created
### -----------------------------------------------------
select_cols <- c(
  "DOI", "Study order within article", "Article Title",
  "Type of collective-action problem", "Data Collection Methods",
  "Results_type_final", "Set"
)

t1_selected <- t1_clean[, select_cols]
t2_selected <- t2[, select_cols]
t3_selected <- t3_clean[, select_cols]
t4_selected <- t4[, select_cols]
t5_selected <- t5[, select_cols]
t6_selected <- t6[, select_cols]
t7_selected <- t7[, select_cols]

### -----------------------------------------------------
### Combine datasets
### -----------------------------------------------------
t_combined <- bind_rows(
  t1_selected, t2_selected, t3_selected, t4_selected,
  t5_selected, t6_selected, t7_selected
)

### -----------------------------------------------------
### Recode Set names
### -----------------------------------------------------
t_combined <- t_combined %>%
  mutate(Set = recode(Set,
                      AE  = "Anticipated Emotion",
                      CFC = "Cons. Future Conseq.",
                      CLT = "Construal Level",
                      EFT = "Episodic Future",
                      TD  = "Temporal Discounting",
                      UT  = "Utopian Thinking",
                      CA  = "Cognitive Alternatives"
  ))


### -----------------------------------------------------
### Build counts
### -----------------------------------------------------

# Safer method classification (covers more cases)
classify_method <- function(x) {
  case_when(
    str_detect(x, regex("survey|questionnaire|self-report|self report", ignore_case = TRUE)) ~ "Survey",
    str_detect(x, regex("experiment|experimental|lab study|laboratory|field experiment|survey experiment|online study", ignore_case = TRUE)) ~ "Experiments",
    TRUE ~ "Other"
  )
}

table_by_set <- t_combined %>%
  mutate(Method = classify_method(`Data Collection Methods`)) %>%
  filter(Method != "Other") %>%
  count(Set, Method, Results_type_final) %>%
  pivot_wider(
    names_from = c(Method, Results_type_final),
    values_from = n,
    names_sep = "_",
    values_fill = 0
  ) %>%
  # Guarantee missing categories do not break the table
  mutate(
    Survey_Yes        = Survey_Yes        %||% 0,
    Survey_Mixed      = Survey_Mixed      %||% 0,
    Survey_No         = Survey_No         %||% 0,
    Experiments_Yes   = Experiments_Yes   %||% 0,
    Experiments_Mixed = Experiments_Mixed %||% 0,
    Experiments_No    = Experiments_No    %||% 0
  ) %>%
  select(
    Set,
    Survey_Yes, Survey_Mixed, Survey_No,
    Experiments_Yes, Experiments_Mixed, Experiments_No
  )

table_by_set_no_totals <- table_by_set

### -----------------------------------------------------
### Compute totals and percentages
### -----------------------------------------------------
survey_cols     <- c("Survey_Yes", "Survey_Mixed", "Survey_No")
experiment_cols <- c("Experiments_Yes", "Experiments_Mixed", "Experiments_No")

table_by_set_with_pct <- table_by_set_no_totals %>%
  mutate(
    Survey_Total      = rowSums(across(all_of(survey_cols)), na.rm = TRUE),
    Experiments_Total = rowSums(across(all_of(experiment_cols)), na.rm = TRUE)
  ) %>%
  mutate(
    across(
      all_of(survey_cols),
      ~ ifelse(Survey_Total == 0, 0, .x / Survey_Total),
      .names = "{.col}_pct"
    ),
    across(
      all_of(experiment_cols),
      ~ ifelse(Experiments_Total == 0, 0, .x / Experiments_Total),
      .names = "{.col}_pct"
    )
  )
### Build GT table
### -----------------------------------------------------
table_by_set_with_pct <- table_by_set_with_pct %>%
  mutate(Category = dplyr::case_when(
    Set == "Construal Level" ~ "Perceptions of Temporal Proximity",
    Set %in% c("Temporal Discounting", "Cons. Future Conseq.") ~
      "Individual Tendencies in Evaluating the Future",
    Set %in% c("Episodic Future", "Cognitive Alternatives", "Anticipated Emotion") ~
      "Mental & Affective Simulations",
    Set == "Utopian Thinking" ~ "Collective Frameworks",
    TRUE ~ "Other"
  ))


# 1) Set factor levels (as you already do)
table_by_set_with_pct <- table_by_set_with_pct %>%
  mutate(
    Category = factor(
      Category,
      levels = c(
        "Perceptions of Temporal Proximity",
        "Individual Tendencies in Evaluating the Future",
        "Mental & Affective Simulations",
        "Collective Frameworks"
      )
    )
  ) %>%
  # 2) Arrange BEFORE gt()
  arrange(Category, Set)

# 3) Build gt table from the already-ordered data
table_by_set_gt <- table_by_set_with_pct %>%
  gt(rowname_col = "Set", groupname_col = "Category") %>%
  tab_header(
    title = md("**Results by Theory**"),
    subtitle = "Counts split by Data Collection Method"
  ) %>%
  tab_spanner(label = "Survey",
              columns = c(Survey_Yes, Survey_Mixed, Survey_No, Survey_Total)) %>%
  tab_spanner(label = "Experiments",
              columns = c(Experiments_Yes, Experiments_Mixed, Experiments_No, Experiments_Total)) %>%
  
  cols_label(
    Survey_Yes        = "Supportive",
    Survey_Mixed      = "Mixed",
    Survey_No         = "Non-supportive",
    Survey_Total      = "Total",
    Experiments_Yes   = "Supportive",
    Experiments_Mixed = "Mixed",
    Experiments_No    = "Non-supportive",
    Experiments_Total = "Total"
  ) %>%
  cols_move(columns = Survey_Total,      after = Survey_No) %>%
  cols_move(columns = Experiments_Total, after = Experiments_No) %>%
  fmt_percent(columns = ends_with("_pct"), decimals = 0) %>%
  fmt_number(
    columns = c(all_of(survey_cols),
                all_of(experiment_cols),
                Survey_Total, Experiments_Total),
    decimals = 0
  ) %>%
  cols_merge(columns = c(Survey_Yes,        Survey_Yes_pct),        pattern = "{1} ({2})") %>%
  cols_merge(columns = c(Survey_Mixed,      Survey_Mixed_pct),      pattern = "{1} ({2})") %>%
  cols_merge(columns = c(Survey_No,         Survey_No_pct),         pattern = "{1} ({2})") %>%
  cols_merge(columns = c(Experiments_Yes,   Experiments_Yes_pct),   pattern = "{1} ({2})") %>%
  cols_merge(columns = c(Experiments_Mixed, Experiments_Mixed_pct), pattern = "{1} ({2})") %>%
  cols_merge(columns = c(Experiments_No,    Experiments_No_pct),    pattern = "{1} ({2})") %>%
  cols_hide(columns = ends_with("_pct")) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(
    table.font.size = px(13),
    column_labels.font.weight = "bold"
  )

table_by_set_gt

# Matching year by 'DOI'

t_combined2 <- t_combined %>%
  left_join(
    Studies_with_years %>% select(DOI, Year) %>% distinct(DOI, Year),
    by = "DOI"
  ) %>%
  rename(Matched_Year = Year)

ggplot(t_combined2, aes(x = Matched_Year, fill = Results_type_final)) +
  geom_bar() +
  facet_wrap(~ Set) +
  labs(x = "Year", y = "Frequency", fill = "Results type") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14),
    axis.text.x = element_text(size = 12, color = "black"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13)
  ) +
  scale_x_continuous(breaks = seq(2009, 2025, by = 2))

# Preregistered ONLY

select_cols <- c(
  "DOI", "Study order within article", "Article Title",
  "Type of collective-action problem", "Data Collection Methods",
  "Results_type_final", "Set",
  "Preregistration"
)

# -----------------------------------------------------
# Combine datasets
# -----------------------------------------------------
t1_selected <- t1_clean[, select_cols]
t2_selected <- t2[, select_cols]
t3_selected <- t3_clean[, select_cols]
t4_selected <- t4[, select_cols]
t5_selected <- t5[, select_cols]
t6_selected <- t6[, select_cols]
t7_selected <- t7[, select_cols]

t_combined <- bind_rows(
  t1_selected, t2_selected, t3_selected, t4_selected,
  t5_selected, t6_selected, t7_selected
)

# -----------------------------------------------------
# Filter preregistered only
# -----------------------------------------------------
t_combined <- t_combined %>%
  filter(str_trim(tolower(Preregistration)) == "yes")
# -----------------------------------------------------
# Method classifier
# -----------------------------------------------------
classify_method <- function(x) {
  case_when(
    str_detect(x, regex("survey|questionnaire|self-report|self report", ignore_case = TRUE)) ~ "Survey",
    str_detect(x, regex("experiment|experimental|lab study|laboratory|field experiment|survey experiment|online study", ignore_case = TRUE)) ~ "Experiments",
    TRUE ~ "Other"
  )
}

# -----------------------------------------------------
# Build counts
# -----------------------------------------------------
table_by_set_prereg <- t_combined %>%
  mutate(Method = classify_method(`Data Collection Methods`)) %>%
  filter(Method != "Other") %>%
  count(Set, Method, Results_type_final) %>%
  pivot_wider(
    names_from = c(Method, Results_type_final),
    values_from = n,
    names_sep = "_",
    values_fill = list(n = 0)
  ) %>%
  {
    df <- .
    for (col in c(
      "Survey_Yes", "Survey_Mixed", "Survey_No",
      "Experiments_Yes", "Experiments_Mixed", "Experiments_No"
    )) {
      if (!col %in% names(df)) df[[col]] <- 0
    }
    df
  } %>%
  select(
    Set,
    Survey_Yes, Survey_Mixed, Survey_No,
    Experiments_Yes, Experiments_Mixed, Experiments_No
  )

# -----------------------------------------------------
# Totals + percentages
# -----------------------------------------------------
survey_cols     <- c("Survey_Yes", "Survey_Mixed", "Survey_No")
experiment_cols <- c("Experiments_Yes", "Experiments_Mixed", "Experiments_No")

table_by_set_prereg <- table_by_set_prereg %>%
  mutate(
    Survey_Total      = rowSums(across(all_of(survey_cols)), na.rm = TRUE),
    Experiments_Total = rowSums(across(all_of(experiment_cols)), na.rm = TRUE)
  ) %>%
  mutate(
    across(
      all_of(survey_cols),
      ~ ifelse(Survey_Total == 0, 0, .x / Survey_Total),
      .names = "{.col}_pct"
    ),
    across(
      all_of(experiment_cols),
      ~ ifelse(Experiments_Total == 0, 0, .x / Experiments_Total),
      .names = "{.col}_pct"
    )
  )

# -----------------------------------------------------
# Add categories + ordering
# -----------------------------------------------------
table_by_set_prereg <- table_by_set_prereg %>%
  mutate(Category = case_when(
    Set == "Construal Level" ~ "Perceptions of Temporal Proximity",
    Set %in% c("Temporal Discounting", "Cons. Future Conseq.") ~
      "Individual Tendencies in Evaluating the Future",
    Set %in% c("Episodic Future", "Cognitive Alternatives", "Anticipated Emotion") ~
      "Mental & Affective Simulations",
    Set == "Utopian Thinking" ~ "Collective Frameworks",
    TRUE ~ "Other"
  )) %>%
  mutate(
    Category = factor(
      Category,
      levels = c(
        "Perceptions of Temporal Proximity",
        "Individual Tendencies in Evaluating the Future",
        "Mental & Affective Simulations",
        "Collective Frameworks"
      )
    )
  ) %>%
  arrange(Category, Set)

# -----------------------------------------------------
# Build GT table (no Category)
# -----------------------------------------------------
table_by_set_prereg_gt <- table_by_set_prereg %>%
  select(-Category) %>%   # ✅ REMOVE COLUMN HERE
  gt(rowname_col = "Set") %>%
  # ✅ removed groupname_col
  tab_header(
    title = md("**Results by Theory (Preregistered Only)**"),
    subtitle = "Counts split by Data Collection Method"
  ) %>%
  tab_spanner(
    label = "Survey",
    columns = c(Survey_Yes, Survey_Mixed, Survey_No, Survey_Total)
  ) %>%
  tab_spanner(
    label = "Experiments",
    columns = c(Experiments_Yes, Experiments_Mixed, Experiments_No, Experiments_Total)
  ) %>%
  cols_label(
    Survey_Yes = "Supportive",
    Survey_Mixed = "Mixed",
    Survey_No = "Non-supportive",
    Survey_Total = "Total",
    Experiments_Yes = "Supportive",
    Experiments_Mixed = "Mixed",
    Experiments_No = "Non-supportive",
    Experiments_Total = "Total"
  ) %>%
  cols_move(Survey_Total, after = Survey_No) %>%
  cols_move(Experiments_Total, after = Experiments_No) %>%
  fmt_percent(columns = ends_with("_pct"), decimals = 0) %>%
  fmt_number(
    columns = c(all_of(survey_cols),
                all_of(experiment_cols),
                Survey_Total, Experiments_Total),
    decimals = 0
  ) %>%
  cols_merge(c(Survey_Yes, Survey_Yes_pct), pattern = "{1} ({2})") %>%
  cols_merge(c(Survey_Mixed, Survey_Mixed_pct), pattern = "{1} ({2})") %>%
  cols_merge(c(Survey_No, Survey_No_pct), pattern = "{1} ({2})") %>%
  cols_merge(c(Experiments_Yes, Experiments_Yes_pct), pattern = "{1} ({2})") %>%
  cols_merge(c(Experiments_Mixed, Experiments_Mixed_pct), pattern = "{1} ({2})") %>%
  cols_merge(c(Experiments_No, Experiments_No_pct), pattern = "{1} ({2})") %>%
  cols_hide(ends_with("_pct")) %>%
  cols_hide(matches("_NA$")) %>%
  cols_align("center", everything())

# Display table
table_by_set_prereg_gt

