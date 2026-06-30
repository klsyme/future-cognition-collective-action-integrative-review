library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(viridis)
library(writexl)
library(stringr)

# Studies not consolidated yet

# Use your dataframe

t <- subset(
  prelim_data2,
  !(DOI == "10.1177/01461672241284324" &
      `Study order within article` %in% c("1(5)", "3(5)"))
)


# Do studies have different DOIs?
# 1) Define the columns to check
cols_to_check <- c(
  "Temporal Discounting",
  "Utopian Thinking",
  "Episodic Future Thinking",
  "Hope",
  "Construal Level Theory",
  "Consideration Future Consequences",
  "Cognitive Alternatives",
  "Anxiety",
  "Anticipated Emotion"
)

# 2) Per-DOI, per-column: how many distinct values?
#    (NA counts as distinct; set na.rm = TRUE to ignore NA differences)
distinct_counts <- t %>%
  group_by(DOI) %>%
  summarize(
    across(all_of(cols_to_check), ~ n_distinct(.x, na.rm = FALSE)),
    .groups = "drop"
  )

# 3) Is every column consistent (≤ 1 unique) for every DOI?
all_consistent <- distinct_counts %>%
  mutate(`__all_ok__` = if_all(all_of(cols_to_check), ~ .x <= 1)) %>%
  summarize(ok = all(`__all_ok__`)) %>%
  pull(ok)

all_consistent  # TRUE if everything matches per DOI across all specified columns

# 4) Which DOIs and columns are inconsistent?
inconsistency_map <- distinct_counts %>%
  mutate(`__any_bad__` = if_any(all_of(cols_to_check), ~ .x > 1)) %>%
  filter(`__any_bad__`) %>%
  select(-`__any_bad__`) %>%
  tidyr::pivot_longer(
    cols = all_of(cols_to_check),
    names_to = "column",
    values_to = "n_unique"
  ) %>%
  filter(n_unique > 1) %>%
  arrange(DOI, column)

inconsistency_map

bad_doi <- unique(inconsistency_map$DOI)

t %>%
  filter(DOI %in% bad_doi) %>%
  select(DOI, all_of(cols_to_check)) %>%
  arrange(DOI)


# The above justifies looking at studies rather than articles by year

# List of theory columns
theory_columns <- c(
  "Temporal Discounting", "Utopian Thinking", "Episodic Future Thinking",
  "Hope", "Construal Level Theory", "Consideration Future Consequences", # <-- no "of"
  "Cognitive Alternatives", "Anxiety", "Anticipated Emotion"
)


# Reshape to long format
t_long <- t %>%
  select(Year, all_of(theory_columns)) %>%
  pivot_longer(cols = all_of(theory_columns), names_to = "Theory", values_to = "Presence") %>%
  filter(Presence == 1)

# Count occurrences per year and theory
t_summary <- t_long %>%
  group_by(Year, Theory) %>%
  summarise(Count = n(), .groups = "drop")

# Create a custom stacking order by sorting Theory within each Year
t_summary <- t_summary %>%
  arrange(Year, Count) %>%
  group_by(Year) %>%
  mutate(Theory_ordered = factor(Theory, levels = Theory[order(Count)])) %>%
  ungroup()

# Plot with custom stacking order
ggplot(t_summary, aes(x = as.factor(Year), y = Count, fill = Theory_ordered)) +
  geom_bar(stat = "identity") +
  labs(title = "Presence of Psychological Theories by Year",
       x = "Year",
       y = "Count",
       fill = "Theory") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Try a heatmap 1

# Define theory columns
theory_columns <- c("Temporal Discounting", "Utopian Thinking", "Episodic Future Thinking", 
                    "Hope", "Construal Level Theory", "Consideration of Future Consequences", 
                    "Cognitive Alternatives", "Anxiety", "Anticipated Emotion")

# Reshape only theory columns
t_long <- prelim_data %>%
  select(Year, all_of(theory_columns)) %>%
  pivot_longer(cols = all_of(theory_columns), names_to = "Theory", values_to = "Presence") %>%
  filter(Presence == 1) %>%
  count(Year, Theory)

# Example: Heatmap
ggplot(t_long, aes(x = as.factor(Year), y = Theory, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Heatmap of Theory Presence by Year", x = "Year", y = "Theory", fill = "Count") +
  theme_minimal()



# Define theory columns

# Prepare data
t_long <- prelim_data %>%
  select(Year, all_of(theory_columns)) %>%
  pivot_longer(cols = all_of(theory_columns), names_to = "Theory", values_to = "Presence") %>%
  filter(Presence == 1) %>%
  count(Year, Theory)

# Plot heatmap with viridis color scale
ggplot(t_long, aes(x = as.factor(Year), y = Theory, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_viridis(name = "Count", option = "D") +
  labs(title = "Heatmap of Theory Presence by Year", x = "Year", y = "Theory") +
  theme_minimal()


# Consolidate studies into one row per article
theory_df <- prelim_data %>%
  group_by(DOI, `Article Title`) %>%
  summarise(
    Year = min(Year, na.rm = TRUE),
    `Type of collective-action problem` = paste(unique(na.omit(`Type of collective-action problem`)), collapse = "; "),
    across(all_of(theory_columns), ~ max(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# write_xlsx(theory_df, "C:/Users/kls52/OneDrive - University of Leicester/Existential Threats Syme-Krockow/Integrative Review R Project/Analyses Data/Leeds Prelim/prelim2.xlsx")


# Try a heatmap 2--Studies within articles not consolidated

# Prepare data
t_long2 <- theory_df %>%
  select(Year, all_of(theory_columns)) %>%
  pivot_longer(cols = all_of(theory_columns), names_to = "Theory", values_to = "Presence") %>%
  filter(Presence == 1) %>%
  count(Year, Theory)

# Plot heatmap with viridis color scale
ggplot(t_long2, aes(x = as.factor(Year), y = Theory, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_viridis(name = "Count", option = "D") +
  labs(title = "Heatmap of Theory Presence by Year", x = "Year", y = "Theory") +
  theme_minimal()

# Stacked bar chart

# Prepare data for plotting
t_long2 <- theory_df %>%
  select(Year, all_of(theory_columns)) %>%
  pivot_longer(cols = all_of(theory_columns), names_to = "Theory", values_to = "Presence") %>%
  filter(Presence == 1) %>%
  count(Year, Theory)

# Plot stacked bar chart
ggplot(t_long2, aes(x = as.factor(Year), y = n, fill = Theory)) +
  geom_bar(stat = "identity") +
  labs(title = "Stacked Bar Chart of Theory Presence by Year",
       x = "Year",
       y = "Number of Articles",
       fill = "Theory") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





# Prepare data
t_long3 <- theory_df %>%
  select(Year, all_of(theory_columns)) %>%
  pivot_longer(cols = all_of(theory_columns), names_to = "Theory", values_to = "Presence") %>%
  filter(Presence == 1) %>%
  count(Year, Theory)

# Reorder Theory globally by total frequency (most frequent at base)
t_long3 <- t_long3 %>%
  group_by(Theory) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(Theory = fct_reorder(Theory, total))

# Plot stacked bar chart
ggplot(t_long3, aes(x = factor(Year), y = n, fill = Theory)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Theory Presence by Year (Most Frequent at Base)",
    x = "Year",
    y = "Number of Articles",
    fill = "Theory"
  ) +
  scale_fill_manual(values = muted_colors) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),               # base text size
    axis.text.x = element_text(size = 12,         # x-axis tick labels
                               angle = 0, 
                               hjust = 0.5),
    axis.title.x = element_text(size = 14),       # x-axis title
    legend.title = element_text(size = 14),       # legend title "Theory"
    legend.text  = element_text(size = 12)        # legend item labels
  )







