library(dplyr)
library(tidyr)
library(stringr)
library(readr)

efthm <- eft_authors_combined

# Remove observations where the dv is not applicable
efthm <- efthm[!efthm$EFTid %in% c("eft35", "eft36", "eft37", "eft38", "eft39", "eft40", "eft41", "eft42"), ]


# ---- assumptions ----
# efthm: your data.frame / tibble
# EFTid : unique per-row analysis identifier (required)
stopifnot("EFTid" %in% names(efthm))

# ---- parsing rules ----
# Split ONLY on commas to preserve punctuation like "(-)" and labels like "Hope/Enthusiasm"
.split_regex <- "\\s*,\\s*"
# Treat these tokens as "no labels"
.none_tokens <- c("none", "no", "na")

# ---- one-hot helper (as you had) ----
one_hot_single_col <- function(df, id_col = "EFTid", field) {
  stopifnot(field %in% names(df))
  df %>%
    select(all_of(id_col), all_of(field)) %>%
    mutate(
      raw   = coalesce(as.character(.data[[field]]), ""),
      raw   = str_squish(raw),
      raw   = if_else(str_to_lower(raw) %in% .none_tokens, "", raw),
      label = if_else(raw == "", list(character()), str_split(raw, .split_regex))
    ) %>%
    select(-raw) %>%
    unnest_longer(label, keep_empty = TRUE) %>%
    mutate(label = str_squish(coalesce(label, ""))) %>%
    filter(label != "") %>%
    mutate(value = 1L) %>%
    distinct(!!sym(id_col), label, .keep_all = TRUE) %>%
    pivot_wider(
      id_cols     = all_of(id_col),
      names_from  = label,    # label text becomes column name (backticks when selecting later)
      values_from = value,
      values_fill = 0
    )
}

# ---- build per-field one-hots ----
oh_mediators   <- one_hot_single_col(efthm, field = "Mediators")
oh_moderators  <- one_hot_single_col(efthm, field = "Moderators")
oh_predictors  <- one_hot_single_col(efthm, field = "Predictors")
oh_dvk         <- one_hot_single_col(efthm, field = "dv.k2")

# NEW (optional): one-hot for Data Collection Methods
oh_dcm         <- one_hot_single_col(efthm, field = "Data Collection Methods")

# ---- helper: safe join that OR-collapses duplicate indicator columns ----
safe_join_or <- function(x, y, key = "EFTid") {
  common <- setdiff(intersect(names(x), names(y)), key)
  out <- dplyr::left_join(x, y, by = key, suffix = c(".x", ".y"))
  for (nm in common) {
    lhs <- if (paste0(nm, ".x") %in% names(out)) paste0(nm, ".x") else nm
    rhs <- paste0(nm, ".y")
    lx  <- ifelse(is.na(out[[lhs]]), 0L, out[[lhs]])
    rx  <- ifelse(is.na(out[[rhs]]), 0L, out[[rhs]])
    out[[nm]] <- pmax(as.integer(lx), as.integer(rx))
    if (lhs != nm && lhs %in% names(out)) out[[lhs]] <- NULL
    if (rhs %in% names(out)) out[[rhs]] <- NULL
  }
  out
}

# ---- combine with OR-collapse (prevents fear.x / fear.y) ----
onehot_all <- efthm %>% select(EFTid)

# Join in any order; duplicates will be collapsed to a single column name
onehot_all <- safe_join_or(onehot_all, oh_moderators, key = "EFTid")
onehot_all <- safe_join_or(onehot_all, oh_mediators,  key = "EFTid")
onehot_all <- safe_join_or(onehot_all, oh_predictors, key = "EFTid")
onehot_all <- safe_join_or(onehot_all, oh_dvk,        key = "EFTid")

# NEW (optional): include the one-hot expansion of Data Collection Methods
onehot_all <- safe_join_or(onehot_all, oh_dcm,        key = "EFTid")

# ---- replace any remaining NAs with 0 across indicator columns ----
indicator_cols <- setdiff(names(onehot_all), "EFTid")
onehot_all[indicator_cols] <- lapply(onehot_all[indicator_cols], function(x) {
  x[is.na(x)] <- 0L
  as.integer(x)
})

# ---- add metadata columns (raw strings) for DOI and Data Collection Methods ----
# This ensures both appear in the final file and are usable for annotations.
onehot_all <- onehot_all %>%
  left_join(
    efthm %>% select(EFTid, DOI, `Data Collection Methods`),
    by = "EFTid"
  ) %>%
  mutate(
    DOI = coalesce(DOI, ""),
    `Data Collection Methods` = coalesce(`Data Collection Methods`, "")
  ) %>%
  relocate(DOI, `Data Collection Methods`, .after = EFTid)

# ---- quick verification for an example row (edit "eft11" if needed) ----
# onehot_all %>%
#   filter(EFTid == "eft11") %>%
#   select(EFTid, DOI, `Data Collection Methods`)

# ---- OPTIONAL: export ----
# readr::write_csv(onehot_all, "onehot_all.csv")

# If you want Excel export:
# writexl::write_xlsx(list(onehot_all = onehot_all), "onehot_all.xlsx")

# ---- OPTIONAL: frequency table of labels (overall counts) ----
# freqs <- colSums(onehot_all[indicator_cols], na.rm = TRUE)
# sort(freqs, decreasing = TRUE)

summary <- efthm %>%
  group_by(`Type of collective-action problem`) %>%
  summarise(Study_Count = n_distinct(DOI)) %>%
  arrange(desc(Study_Count))

print(summary)

summary <- efthm %>%
  group_by(`Type of collective-action problem`) %>%
  summarise(Study_Count = n_distinct(DOI)) %>%
  arrange(desc(Study_Count))

print(summary)


efthm %>%
  summarise(
    Unique_DOI = n_distinct(DOI),
    Unique_StudyOrder = n_distinct(`Study order within article`),
    Unique_Combination = n_distinct(paste(DOI, `Study order within article`))
  )


summary <- efthm %>%
  group_by(DOI) %>%
  summarise(Theoretical_Frameworks = paste(unique(`Theoretical Frameworks`), collapse = "; ")) %>%
  arrange(DOI)


print(summary, width = Inf)
View(summary)


