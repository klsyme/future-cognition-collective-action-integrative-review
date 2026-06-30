library(dplyr)
library(stringr)
library(forcats)
library(ggplot2)
library(scales)   # for hue_pal

# 1) Filter rows (your original step)
d <- subset(
  prelim_data2,
  !(DOI == "10.1177/01461672241284324" &
      `Study order within article` %in% c("1(5)", "3(5)"))
)

# 2) Normalize the raw text and classify into consolidated categories
d <- d %>%
  mutate(
    Type_raw  = as.character(`Type of collective-action problem`),
    Type_norm = str_squish(Type_raw),  # trim & collapse whitespace
    # Make separators consistent (replace ; with , and ensure no weird spaces)
    Type_norm = str_replace_all(Type_norm, ";", ","),
    Type_norm = str_replace_all(Type_norm, "\\s*,\\s*", ","),
    # Consolidate via pattern rules (case-insensitive)
    Type_cons = case_when(
      # COVID-19
      str_detect(Type_norm, regex("\\bCOVID[- ]?19\\b|^COVID", ignore_case = TRUE)) ~ "COVID-19",
      
      # Vaccination programs
      str_detect(Type_norm, regex("vaccin", ignore_case = TRUE)) ~ "Vaccination",
      
      # AMR
      str_detect(Type_norm, regex("\\bAMR\\b|Antimicrobial\\s+resistance", ignore_case = TRUE)) ~ "Antimicrobial resistance",
      
      # Infectious disease (incl. charity variants)
      str_detect(Type_norm, regex("Infectious\\s+disease|malaria|deworm|zika", ignore_case = TRUE)) ~ "Infectious disease",
      
      # Climate change & related (energy/electricity conservation, air/particulate, biodiversity, sustainability, carbon capture)
      str_detect(Type_norm, regex("Climate\\s+change", ignore_case = TRUE)) ~ "Climate change",
      str_detect(Type_norm, regex("Electricity\\s+conservation|Energy\\s+conservation", ignore_case = TRUE)) ~ "Climate change",
      str_detect(Type_norm, regex("Air\\s+quality|Particulate\\s+Matter|Sustainability|Carbon\\s+capture|Biodiver|Plastic\\s+pollution|Protection\\s+of\\s+endangered\\s+animals", ignore_case = TRUE)) ~ "Climate change",
      
      # General collective action
      str_detect(Type_norm, regex("^General\\s+collective\\s+action$", ignore_case = TRUE)) ~ "General collective action",
      
      # Socially responsible investing
      str_detect(Type_norm, regex("Socially\\s+responsible\\s+invest", ignore_case = TRUE)) ~ "Socially responsible investing",
      
      # Multiple CAPs (composite choice sets)
      str_detect(Type_norm, regex("Multiple\\s+collective-action\\s+problems", ignore_case = TRUE)) ~ "Multiple collective-action problems",
      str_detect(Type_norm, regex("Environmental\\s+quality.*Healthcare.*Safety.*Natural\\s+disaster", ignore_case = TRUE)) ~ "Multiple collective-action problems",
      
      # Social issues (social change, equality)
      str_detect(Type_norm, regex("Social\\s+issues|Social\\s+change|equality", ignore_case = TRUE)) ~ "Social issues",
      
      # Fallback: keep the normalized original so you can audit it
      TRUE ~ Type_norm
    ),
    # Make consolidated a factor & drop unused levels
    Type_cons = fct_drop(factor(Type_cons))
  )

# 3) See what didn’t match (should be empty or expected exact carry-overs)
unmapped <- d %>%
  filter(!(Type_cons %in% c(
    "Climate change", "COVID-19", "Vaccination",
    "Antimicrobial resistance", "Infectious disease",
    "General collective action", "Socially responsible investing",
    "Multiple collective-action problems", "Social issues"
  ))) %>%
  distinct(Type_norm, Type_cons)

print(unmapped)    # If any rows appear, we’ll add a rule for them.

# 4) Count and order for plotting with the consolidated column
d_counts <- d %>%
  count(Year, Type_cons, name = "n") %>%
  group_by(Type_cons) %>%
  mutate(total = sum(n, na.rm = TRUE)) %>%
  ungroup()

# Order stacks so largest is at the bottom
lvl_order <- d_counts %>%
  distinct(Type_cons, total) %>%
  arrange(desc(total)) %>%
  pull(Type_cons)

d_counts <- d_counts %>%
  mutate(Type_cons = factor(Type_cons, levels = lvl_order))

# 5) Build a palette that matches the number of consolidated levels exactly


# Your fixed palette
muted_colors <- c("#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6",
                  "#ffff99", "#1f78b4", "#33a02c", "#e31a1c")

# Match palette length to the number of levels
n_levels <- nlevels(d_counts$Type_cons)
palette_fit <- if (n_levels <= length(muted_colors)) {
  muted_colors[seq_len(n_levels)]
} else {
  # Repeat colors to reach n_levels (simple approach)
  rep(muted_colors, length.out = n_levels)
}

# ⬇️ Remove this line (it overwrites your custom palette)
# muted_colors <- hue_pal(l = 60, c = 60)(n_levels)

# Plot using your palette_fit
ggplot(d_counts, aes(x = factor(Year), y = n, fill = Type_cons)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  labs(
    title = "Collective-Action Problems by Year (Largest Stack at Bottom)",
    x = "Year", y = "Count",
    fill = "Collective-Action Problem"
  ) +
  scale_fill_manual(values = palette_fit, guide = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 12)
  )
