library(dplyr)
library(ggplot2)
library(stringr)
library(writexl)
library(scales)

f <- Final_Extraction_Form_ALL

# ============================================================
# BASIC COUNTS
# ============================================================

length(unique(f$DOI))
length(unique(f$`Article Title`))

nrow(unique(f[, c("DOI", "Study order within article")]))
nrow(unique(f[, c("Article Title", "Study order within article")]))

f %>%
  distinct(DOI, `Article Title`) %>%
  count(DOI) %>%
  filter(n > 1)

# ============================================================
# QUANTITATIVE STUDIES
# ============================================================

quant <- f %>%
  filter(`Data Collection Methods` %in% c(
    "Experiment",
    "Field experiment/Survey",
    "Survey"
  ))

table(table(quant$DOI))

length(unique(quant$DOI))
length(unique(quant$`Article Title`))

nrow(unique(quant[, c("DOI", "Study order within article")]))
nrow(unique(quant[, c("Article Title", "Study order within article")]))

# ============================================================
# METHOD TYPE
# ============================================================

table(quant$`Data Collection Methods`)

# ============================================================
# PREREGISTRATION
# ============================================================

quant$Preregistration <- tolower(trimws(quant$Preregistration))
quant$Preregistration[quant$Preregistration %in% c("not applicable", "no?")] <- "no"

table(quant$Preregistration)

# ============================================================
# SAMPLE SIZE CLEANING
# ============================================================

quant$SampleSize_num <- sapply(quant$`Sample Size`, function(x) {
  nums <- as.numeric(unlist(regmatches(x, gregexpr("\\d+", x))))
  if (length(nums) == 0) NA else sum(nums)
})

quant$`Sample Size` <- trimws(quant$`Sample Size`)

quant$SampleSize_clean <- quant$SampleSize_num

quant$SampleSize_clean[
  quant$`Sample Size` ==
    "Pre = 317, Post = 193, matched = 165"
] <- 341

quant$SampleSize_clean[
  quant$`Sample Size` ==
    "n= 800, vignette n = 658"
] <- 800

quant$SampleSize_clean[
  quant$`Sample Size` ==
    "284 (US), 310 (China)"
] <- 594

quant$SampleSize_clean[
  quant$`Sample Size` ==
    "160 (households)"
] <- 160

quant[
  is.na(quant$SampleSize_clean),
  c("Sample Size", "SampleSize_clean")
]

mean(quant$SampleSize_clean, na.rm = TRUE)
median(quant$SampleSize_clean, na.rm = TRUE)
range(quant$SampleSize_clean, na.rm = TRUE)
sd(quant$SampleSize_clean, na.rm = TRUE)

# ============================================================
# LOLLIPOP PLOT
# ============================================================

d_lolli <- quant %>%
  filter(!is.na(SampleSize_clean)) %>%
  arrange(SampleSize_clean) %>%
  mutate(idx = factor(row_number()))

p_lolli <- ggplot(d_lolli, aes(x = SampleSize_clean, y = idx)) +
  geom_segment(
    aes(x = 0,
        xend = SampleSize_clean,
        y = idx,
        yend = idx),
    color = "grey75",
    linewidth = 0.6
  ) +
  geom_point(
    color = "steelblue",
    size = 2.5
  ) +
  labs(
    title = "Sample Size by Study (sorted)",
    x = "Sample Size",
    y = "Study (sorted)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(size = 16),
    axis.title.y = element_text(size = 16)
  ) +
  scale_y_discrete(breaks = NULL)

p_lolli

# ============================================================
# SAMPLING METHODS
# ============================================================

quant$Sampling_clean <- tolower(trimws(quant$`Sampling Methods`))

quant$Sampling_clean <- ifelse(
  grepl("convenience", quant$Sampling_clean), "Convenience",
  ifelse(
    grepl("purposive", quant$Sampling_clean), "Purposive",
    ifelse(
      grepl("quota", quant$Sampling_clean), "Quota",
      ifelse(
        grepl("strat", quant$Sampling_clean), "Stratified random",
        ifelse(
          grepl("random", quant$Sampling_clean), "Random",
          ifelse(
            grepl("cluster|multistage", quant$Sampling_clean),
            "Cluster/multistage",
            ifelse(
              grepl("snowball", quant$Sampling_clean),
              "Snowball",
              ifelse(
                grepl("panel", quant$Sampling_clean),
                "Panel",
                ifelse(
                  grepl("not described|not specified",
                         quant$Sampling_clean),
                  "Not reported",
                  "Other"
                )
              )
            )
          )
        )
      )
    )
  )
)

quant$Sampling_clean[
  quant$`Sampling Methods` ==
    "Convenience (sample 1),Quota sampling (sample 2)"
] <- "Mixed"

quant$Sampling_clean[
  quant$`Sampling Methods` ==
    "Purposive,Convenience"
] <- "Mixed"

quant$Sampling_clean[
  quant$`Sampling Methods` ==
    "Purposive,Snowball"
] <- "Mixed"

table(quant$Sampling_clean)

prop.table(table(quant$Sampling_clean))["Convenience"] * 100

# ============================================================
# COUNTRY OF STUDY
# ============================================================

f2 <- f %>%
  mutate(
    `Country of Study` = str_trim(`Country of Study`),
    Country2 = case_when(
      str_detect(
        `Country of Study`,
        regex("^not\\s*specified$", ignore_case = TRUE)
      ) |
        str_detect(
          `Country of Study`,
          regex("^not\\s*stated\\s*\\(online\\)$",
                ignore_case = TRUE)
        ) |
        str_detect(
          `Country of Study`,
          regex("\\bonline\\b", ignore_case = TRUE)
        ) ~ "Online",
      TRUE ~ `Country of Study`
    )
  )

f2 <- f2 %>%
  mutate(
    Country_clean = tolower(trimws(Country2))
  ) %>%
  mutate(
    Country_clean = case_when(

      Country_clean == "online" ~ "Online",

      str_detect(Country_clean,
                 "not applicable") ~ "Not Applicable",

      str_detect(Country_clean, ",") ~ "Cross-national",
      str_detect(Country_clean,
                 "cross-national") ~ "Cross-national",
      str_detect(Country_clean,
                 "countries") ~ "Cross-national",

      str_detect(Country_clean,
                 "columbia") ~ "Colombia",
      str_detect(Country_clean,
                 "netherlands") ~ "Netherlands",

      str_detect(Country_clean,
                 "united states|usa|u\\.s\\.") ~ "United States",
      str_detect(Country_clean,
                 "united kingdom") ~ "United Kingdom",
      str_detect(Country_clean, "china") ~ "China",
      str_detect(Country_clean, "australia") ~ "Australia",
      str_detect(Country_clean, "france") ~ "France",
      str_detect(Country_clean, "germany") ~ "Germany",
      str_detect(Country_clean, "italy") ~ "Italy",
      str_detect(Country_clean, "japan") ~ "Japan",
      str_detect(Country_clean, "south korea") ~ "South Korea",
      str_detect(Country_clean, "new zealand") ~ "New Zealand",
      str_detect(Country_clean, "bangladesh") ~ "Bangladesh",
      str_detect(Country_clean, "canada") ~ "Canada",
      str_detect(Country_clean, "chile") ~ "Chile",
      str_detect(Country_clean, "denmark") ~ "Denmark",
      str_detect(Country_clean, "ecuador") ~ "Ecuador",
      str_detect(Country_clean, "finland") ~ "Finland",
      str_detect(Country_clean, "hong kong") ~ "Hong Kong",
      str_detect(Country_clean, "ireland") ~ "Ireland",
      str_detect(Country_clean, "israel") ~ "Israel",
      str_detect(Country_clean, "mexico") ~ "Mexico",
      str_detect(Country_clean, "norway") ~ "Norway",
      str_detect(Country_clean, "uganda") ~ "Uganda",
      str_detect(Country_clean, "vietnam") ~ "Vietnam",

      TRUE ~ "Other"
    )
  )

top_countries <- f2 %>%
  filter(
    !is.na(Country_clean),
    Country_clean != "Not Applicable"
  ) %>%
  count(Country_clean, sort = TRUE) %>%
  slice_head(n = 10)

ggplot(top_countries,
       aes(x = reorder(Country_clean, n), y = n)) +
  geom_bar(stat = "identity",
           fill = "skyblue") +
  geom_text(
    aes(label = n),
    hjust = -0.1,
    size = 4.5
  ) +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Top 10 Most Frequent Countries of Study",
    x = "Country",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 16)

country_to_region <- c(
  "United States" = "North America",
  "Canada" = "North America",
  "Mexico" = "North America",
  "Chile" = "South America",
  "Colombia" = "South America",
  "Ecuador" = "South America",
  "Germany" = "Europe",
  "France" = "Europe",
  "Italy" = "Europe",
  "United Kingdom" = "Europe",
  "Netherlands" = "Europe",
  "Norway" = "Europe",
  "Finland" = "Europe",
  "Ireland" = "Europe",
  "Denmark" = "Europe",
  "Israel" = "Middle East",
  "Uganda" = "Africa",
  "Bangladesh" = "Asia",
  "China" = "Asia",
  "Vietnam" = "Asia",
  "South Korea" = "Asia",
  "Japan" = "Asia",
  "Hong Kong" = "Asia",
  "Australia" = "Oceania",
  "New Zealand" = "Oceania",
  "Cross-national" = "Cross-national",
  "Online" = "Online"
)

f2$Region <- country_to_region[f2$Country_clean]

f2_plot <- f2 %>%
  filter(
    Country_clean != "Not Applicable",
    Region != "Other"
  )

ord <- names(sort(table(f2_plot$Region)))

ggplot(f2_plot,
       aes(x = factor(Region, levels = ord))) +
  geom_bar(fill = "steelblue") +
  coord_flip() +
  stat_count(
    aes(label = after_stat(count)),
    geom = "text",
    hjust = -0.1,
    size = 4.5
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Region Frequency",
    x = NULL,
    y = "Frequency"
  ) +
  theme_minimal(base_size = 16)

# ============================================================
# ROUND 1/2 DATA
# ============================================================

d <- Round_1_2_similarity_score

d2 <- d %>%
  filter(
    !`Data Collection Methods` %in% c(
      "Semi-structured interviews",
      "Open source text extraction",
      "Focus group,Semi-structured interviews"
    )
  )

length(unique(d2$DOI))

# ============================================================
# PREREGISTRATION FREQUENCIES
# ============================================================

d$Preregistration[d$Preregistration == "no?"] <- "no"
d$Preregistration[d$Preregistration == "not applicable"] <- "no"

freq <- table(d$Preregistration)
percent <- prop.table(freq) * 100

result <- data.frame(
  Preregistration = names(freq),
  Count = as.vector(freq),
  Percentage = round(as.vector(percent), 1)
)

print(result)

# ============================================================
# SAMPLE2 CLEANING
# ============================================================

d2$Sample2 <- d2$`Sample Size`

d2$Sample2[
  d2$Sample2 ==
    "Pre-questionnaire = 317,Post-questionnaire = 193,Pre–Post matched sample = 165,6-month follow-up = 34,Universities involved = 39 coaching weekends at German universities"
] <- "165"

d2$Sample2[d2$Sample2 == "160 (households)"] <- "160"
d2$Sample2[d2$Sample2 == "n= 800, vignette n = 658"] <- "1458"
d2$Sample2[d2$Sample2 == "284 (US), 310 (China)"] <- "594"

d2$Sample2[
  d2$Sample2 ==
    "not specified (4 wechat groups and 2 interviews)"
] <- "na"

d2$Sample2 <- as.numeric(d2$Sample2)

summary(d2$Sample2)
sd(d2$Sample2)

table(d2$`Data Collection Methods`)

# ============================================================
# SAMPLE2 VISUALIZATIONS
# ============================================================

p_hist <- d2 %>%
  filter(!is.na(Sample2)) %>%
  ggplot(aes(x = Sample2)) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "white"
  ) +
  geom_vline(
    xintercept = median(d2$Sample2, na.rm = TRUE),
    linetype = "dashed",
    color = "orange"
  ) +
  geom_vline(
    xintercept = mean(d2$Sample2, na.rm = TRUE),
    linetype = "dotted",
    color = "firebrick"
  ) +
  theme_minimal()

p_hist

p_hist_log <- d2 %>%
  filter(!is.na(Sample2), Sample2 > 0) %>%
  ggplot(aes(x = Sample2)) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "white"
  ) +
  scale_x_log10(
    labels = label_number(big.mark = ",")
  ) +
  theme_minimal()

p_hist_log

p_box <- d2 %>%
  filter(!is.na(Sample2)) %>%
  ggplot(aes(x = "", y = Sample2)) +
  geom_boxplot(fill = "grey90") +
  stat_summary(
    fun = mean,
    geom = "point",
    shape = 23,
    size = 3,
    fill = "firebrick"
  ) +
  theme_minimal()

p_box

p_violin <- d2 %>%
  filter(!is.na(Sample2)) %>%
  ggplot(aes(x = "", y = Sample2)) +
  geom_violin(
    fill = "lightblue",
    color = "grey40",
    trim = FALSE
  ) +
  geom_jitter(
    width = 0.08,
    alpha = 0.35,
    size = 1.6
  ) +
  theme_minimal()

p_violin

p_ecdf <- d2 %>%
  filter(!is.na(Sample2)) %>%
  ggplot(aes(x = Sample2)) +
  stat_ecdf(
    geom = "step",
    color = "steelblue"
  ) +
  theme_minimal()

p_ecdf

# ============================================================
# SAMPLE SIZE SUMMARY TABLE
# ============================================================

Mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

summary_table <- d2 %>%
  summarize(
    Min = min(Sample2, na.rm = TRUE),
    Max = max(Sample2, na.rm = TRUE),
    Mean = mean(Sample2, na.rm = TRUE),
    Median = median(Sample2, na.rm = TRUE),
    Mode = Mode(Sample2)
  )

print(summary_table)

# ============================================================
# COUNTRY SUMMARYS
# ============================================================

summary_by_country <- d %>%
  filter(!is.na(Country2)) %>%
  group_by(Country2) %>%
  summarize(
    N = sum(!is.na(Sample2)),
    Mean = mean(Sample2, na.rm = TRUE),
    Median = median(Sample2, na.rm = TRUE),
    Min = min(Sample2, na.rm = TRUE),
    Max = max(Sample2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Range = Max - Min,
    Range_str = paste0(Min, "–", Max)
  ) %>%
  arrange(desc(N))

summary_by_country |>
  mutate(across(
    c(Mean, Median, Min, Max, Range),
    ~ round(.x, 1)
  ))

# ============================================================
# CONVENIENCE SAMPLING %
# ============================================================

total <- sum(!is.na(d$`Sampling Methods`))

convenience <- sum(
  d$`Sampling Methods` %in%
    c("Convenience sampling", "Convenience"),
  na.rm = TRUE
)

percentage <- (convenience / total) * 100

percentage

# ============================================================
# COUNTRY NAME CLEANING
# ============================================================

f2$Country2_clean <- trimws(f2$Country2)
f2$Country2_clean <- gsub(" +", " ", f2$Country2_clean)
f2$Country2_clean <- stringr::str_to_title(f2$Country2_clean)

f2$Country2_clean[f2$Country2_clean == "Usa"] <- "United States"
f2$Country2_clean[f2$Country2_clean == "Not Specified"] <- "Not specified"
f2$Country2_clean[f2$Country2_clean == "Not Stated (Online)"] <- "Not stated (online)"
