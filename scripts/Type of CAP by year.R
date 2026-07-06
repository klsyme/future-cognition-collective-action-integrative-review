library(dplyr)
library(stringr)
library(forcats)
library(ggplot2)
library(scales)   # for hue_pal

# =====================================================
# DATA
# =====================================================

d <- Studies_with_years

# 2) Normalize the raw text and classify into consolidated categories
d <- d %>%
  mutate(
    Type_raw  = as.character(`Type of collective-action problem`),
    Type_norm = str_squish(Type_raw),
    
    # Make separators consistent
    Type_norm = str_replace_all(Type_norm, ";", ","),
    Type_norm = str_replace_all(Type_norm, "\\s*,\\s*", ","),
    
    # Consolidate via pattern rules
    Type_cons = case_when(
      
      # COVID-19
      str_detect(Type_norm,
                 regex("\\bCOVID[- ]?19\\b|^COVID", ignore_case = TRUE)) ~
        "COVID-19",
      
      # Vaccination
      str_detect(Type_norm,
                 regex("vaccin", ignore_case = TRUE)) ~
        "Vaccination",
      
      # Antimicrobial resistance
      str_detect(Type_norm,
                 regex("\\bAMR\\b|Antimicrobial\\s+resistance",
                       ignore_case = TRUE)) ~
        "Antimicrobial resistance",
      
      # Infectious disease
      str_detect(Type_norm,
                 regex("Infectious\\s+disease|malaria|deworm|zika",
                       ignore_case = TRUE)) ~
        "Infectious disease",
      
      # Climate change and related
      str_detect(Type_norm,
                 regex("Climate\\s+change",
                       ignore_case = TRUE)) ~
        "Climate change",
      
      str_detect(Type_norm,
                 regex("Electricity\\s+conservation|Energy\\s+conservation",
                       ignore_case = TRUE)) ~
        "Climate change",
      
      str_detect(Type_norm,
                 regex(
                   "Air\\s+quality|Particulate\\s+Matter|Sustainability|Carbon\\s+capture|Biodiver|Plastic\\s+pollution|Protection\\s+of\\s+endangered\\s+animals",
                   ignore_case = TRUE
                 )) ~
        "Climate change",
      
      # General collective action
      str_detect(Type_norm,
                 regex("^General\\s+collective\\s+action$",
                       ignore_case = TRUE)) ~
        "General collective action",
      
      # NEW: General long-term collective action
      str_detect(Type_norm,
                 regex("General\\s+Long-Term\\s+Collective-Action",
                       ignore_case = TRUE)) ~
        "General Long-Term Collective Action",
      
      # Socially responsible investing
      str_detect(Type_norm,
                 regex("Socially\\s+responsible\\s+invest",
                       ignore_case = TRUE)) ~
        "Socially responsible investing",
      
      # Multiple collective-action problems
      str_detect(Type_norm,
                 regex("Multiple\\s+collective-action\\s+problems",
                       ignore_case = TRUE)) ~
        "Multiple collective-action problems",
      
      str_detect(Type_norm,
                 regex(
                   "Environmental\\s+quality.*Healthcare.*Safety.*Natural\\s+disaster",
                   ignore_case = TRUE
                 )) ~
        "Multiple collective-action problems",
      
      # Social issues
      str_detect(Type_norm,
                 regex("Social\\s+issues|Social\\s+change|equality",
                       ignore_case = TRUE)) ~
        "Social issues",
      
      # Fallback
      TRUE ~ Type_norm
    ),
    Type_cons = fct_drop(factor(Type_cons))
  )

# =====================================================
# 3 CHECK FOR UNMAPPED CATEGORIES
# =====================================================

unmapped <- d %>%
  filter(
    !(Type_cons %in% c(
      "Climate change",
      "COVID-19",
      "Vaccination",
      "Antimicrobial resistance",
      "Infectious disease",
      "General collective action",
      "General Long-Term Collective Action",
      "Socially responsible investing",
      "Multiple collective-action problems",
      "Social issues"
    ))
  ) %>%
  distinct(Type_norm, Type_cons)

print(unmapped)

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


# Plot using your palette_fit
ggplot(d_counts, aes(x = factor(Year), y = n, fill = Type_cons)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  labs(
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

ggsave(
  "CAPs_by_year.png",
  width = 11,
  height = 7,
  dpi = 300
)

# =====================================================
# PROPORTIONAL (100%) STACKED BAR CHART
# =====================================================

ggplot(
  d_counts,
  aes(
    x = factor(Year),
    y = n,
    fill = Type_cons
  )
) +
  geom_col(
    position = position_fill(reverse = TRUE),
    width = 0.9
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_fill_manual(
    values = palette_fit,
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    x = "Year",
    y = "Percentage of Studies",
    fill = "Collective-Action Problem"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5),
    axis.title.x = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

ggsave(
  "CAPs_by_year_proportions.png",
  width = 11,
  height = 7,
  dpi = 300
)
