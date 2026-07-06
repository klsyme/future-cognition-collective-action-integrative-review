library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(stringr)
library(viridis)

# =====================================================
# DATA
# =====================================================

t <- Studies_with_years

# Theories of interest
theory_columns <- c(
  "Temporal Discounting",
  "Utopian Thinking",
  "Episodic Future Thinking",
  "Hope",
  "Construal Level Theory",
  "Consideration of Future Consequences",
  "Cognitive Alternatives",
  "Anxiety",
  "Anticipated Emotion"
)

# =====================================================
# CREATE BINARY THEORY COLUMNS
# =====================================================

for(th in theory_columns){
  
  t[[th]] <- ifelse(
    str_detect(
      coalesce(t$`Theoretical Frameworks`, ""),
      fixed(th)
    ),
    1,
    0
  )
  
}

# =====================================================
# CONSOLIDATE TO ONE ROW PER ARTICLE
# =====================================================

theory_df <- t %>%
  group_by(DOI, `Article Title`) %>%
  summarise(
    Year = min(Year, na.rm = TRUE),
    across(all_of(theory_columns), max, na.rm = TRUE),
    .groups = "drop"
  )

# =====================================================
# PREPARE DATA FOR STACKED BAR CHART
# =====================================================

t_long2 <- theory_df %>%
  select(Year, all_of(theory_columns)) %>%
  pivot_longer(
    cols = all_of(theory_columns),
    names_to = "Theory",
    values_to = "Presence"
  ) %>%
  filter(Presence == 1) %>%
  count(Year, Theory)

# =====================================================
# ORDER THEORIES BY OVERALL FREQUENCY
# (MOST FREQUENT AT BASE)
# =====================================================

theory_freq <- t_long2 %>%
  group_by(Theory) %>%
  summarise(total = sum(n), .groups = "drop")

t_long2 <- t_long2 %>%
  left_join(theory_freq, by = "Theory") %>%
  mutate(
    Theory = factor(
      Theory,
      levels = theory_freq %>%
        arrange(desc(total)) %>%   # most frequent first
        pull(Theory)
    )
  )

# =====================================================
# CUSTOM COLORS
# =====================================================

muted_colors <- c(
  "Construal Level Theory"                = "#F41117",
  "Temporal Discounting"                  = "#37A629",
  "Anticipated Emotion"                   = "#2C7BB6",
  "Utopian Thinking"                      = "#E6E88B",
  "Episodic Future Thinking"              = "#B9A6C9",
  "Consideration of Future Consequences"  = "#F0B666",
  "Cognitive Alternatives"                = "#EA8D8D",
  "Anxiety"                               = "#A8D17F",
  "Hope"                                  = "#97B9CF"
)

# =====================================================
# PLOT
# =====================================================

ggplot(
  t_long2,
  aes(
    x = factor(Year),
    y = n,
    fill = Theory
  )
) +
  geom_col(width = 0.9, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = muted_colors) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    x = "Year",
    y = "Number of Articles",
    fill = "Theory"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      size = 12
    ),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 16)
  )

ggsave(
  "Theory_by_year.png",
  width = 11,
  height = 7,
  dpi = 300
)

# =====================================================
# PREPARE PROPORTIONAL DATA (100% STACKED)
# =====================================================

t_long_prop <- t_long2 %>%
  group_by(Year) %>%
  mutate(
    prop = n / sum(n)
  ) %>%
  ungroup()

# =====================================================
# 100% STACKED BAR CHART
# =====================================================

ggplot(
  t_long_prop,
  aes(
    x = factor(Year),
    y = prop,
    fill = Theory
  )
) +
  geom_col(
    width = 0.9,
    position = position_stack(reverse = TRUE)
  ) +
  scale_fill_manual(values = muted_colors) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    x = "Year",
    y = "Percentage of Theory Mentions",
    fill = "Theory"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5,
      size = 12
    ),
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

ggsave(
  "Theory_by_year_proportions.png",
  width = 11,
  height = 7,
  dpi = 300
)






