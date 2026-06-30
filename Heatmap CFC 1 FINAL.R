library(dplyr)
library(tidyr)
library(stringr)
library(readr)

cfchm <- cfc_authors_combined

# ---- assumptions ----
# cfchm: your data.frame / tibble
# CFCid : unique per-row analysis identifier (required)
stopifnot("CFCid" %in% names(cfchm))

# (Optional) If not already filtered earlier:
# cfchm <- cfchm %>% filter(!CFCid %in% c("cfc41", "cfc44", "cfc46"))

# ---- parsing rules ----
# Split ONLY on commas to preserve punctuation like "(-)" and labels like "Hope/Enthusiasm"
.split_regex <- "\\s*,\\s*"
# Treat these tokens as "no labels"
.none_tokens <- c("none", "no", "na")

# ---- one-hot helper (as you had) ----
one_hot_single_col <- function(df, id_col = "CFCid", field) {
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
oh_mediators   <- one_hot_single_col(cfchm, field = "Mediators")
oh_moderators  <- one_hot_single_col(cfchm, field = "Moderators")
oh_predictors  <- one_hot_single_col(cfchm, field = "Predictors")
oh_dvk         <- one_hot_single_col(cfchm, field = "dv.k2")

# NEW (optional): one-hot for Data Collection Methods
oh_dcm         <- one_hot_single_col(cfchm, field = "Data Collection Methods")

# ---- helper: safe join that OR-collapses duplicate indicator columns ----
safe_join_or <- function(x, y, key = "CFCid") {
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
onehot_all <- cfchm %>% select(CFCid)

# Join in any order; duplicates will be collapsed to a single column name
onehot_all <- safe_join_or(onehot_all, oh_moderators, key = "CFCid")
onehot_all <- safe_join_or(onehot_all, oh_mediators,  key = "CFCid")
onehot_all <- safe_join_or(onehot_all, oh_predictors, key = "CFCid")
onehot_all <- safe_join_or(onehot_all, oh_dvk,        key = "CFCid")

# NEW (optional): include the one-hot expansion of Data Collection Methods
onehot_all <- safe_join_or(onehot_all, oh_dcm,        key = "CFCid")

# ---- replace any remaining NAs with 0 across indicator columns ----
indicator_cols <- setdiff(names(onehot_all), "CFCid")
onehot_all[indicator_cols] <- lapply(onehot_all[indicator_cols], function(x) {
  x[is.na(x)] <- 0L
  as.integer(x)
})

# ---- add metadata columns (raw strings) for Citation and Data Collection Methods ----
# This ensures both appear in the final file and are usable for annotations.
onehot_all <- onehot_all %>%
  left_join(
    cfchm %>% select(CFCid, Citation, `Data Collection Methods`),
    by = "CFCid"
  ) %>%
  mutate(
    Citation = coalesce(Citation, ""),
    `Data Collection Methods` = coalesce(`Data Collection Methods`, "")
  ) %>%
  relocate(Citation, `Data Collection Methods`, .after = CFCid)

# ---- quick verification for an example row (edit "cfc11" if needed) ----
# onehot_all %>%
#   filter(CFCid == "cfc11") %>%
#   select(CFCid, Citation, `Data Collection Methods`)

# ---- OPTIONAL: export ----
# readr::write_csv(onehot_all, "onehot_all.csv")

# If you want Excel export:
# writexl::write_xlsx(list(onehot_all = onehot_all), "onehot_all.xlsx")

# ---- OPTIONAL: frequency table of labels (overall counts) ----
# freqs <- colSums(onehot_all[indicator_cols], na.rm = TRUE)
# sort(freqs, decreasing = TRUE)


