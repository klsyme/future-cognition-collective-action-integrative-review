library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(writexl)

aehm <- ae_authors_combined

# ---- assumptions ----
stopifnot("AEid" %in% names(aehm))

# ---- parsing rules ----
.split_regex <- "\\s*,\\s*"
.none_tokens <- c("none", "no", "na")

# ---- one-hot helper ----
one_hot_single_col <- function(df, id_col = "AEid", field) {
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
      names_from  = label,
      values_from = value,
      values_fill = 0
    )
}

# ---- build per-field one-hots ----
oh_mediators   <- one_hot_single_col(aehm, field = "Mediators")
oh_moderators  <- one_hot_single_col(aehm, field = "Moderators")
oh_predictors  <- one_hot_single_col(aehm, field = "Predictors")
oh_dvk         <- one_hot_single_col(aehm, field = "dv.k2")
oh_dcm         <- one_hot_single_col(aehm, field = "Data Collection Methods")

# ---- safe join with OR-collapse ----
safe_join_or <- function(x, y, key = "AEid") {
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

# ---- combine one-hot tables ----
onehot_all <- aehm %>% select(AEid)
onehot_all <- safe_join_or(onehot_all, oh_moderators, key = "AEid")
onehot_all <- safe_join_or(onehot_all, oh_mediators,  key = "AEid")
onehot_all <- safe_join_or(onehot_all, oh_predictors, key = "AEid")
onehot_all <- safe_join_or(onehot_all, oh_dvk,        key = "AEid")
onehot_all <- safe_join_or(onehot_all, oh_dcm,        key = "AEid")

# ---- replace NAs with 0 ----
indicator_cols <- setdiff(names(onehot_all), "AEid")
onehot_all[indicator_cols] <- lapply(onehot_all[indicator_cols], function(x) {
  x[is.na(x)] <- 0L
  as.integer(x)
})

# ---- add metadata columns (without Type of collective-action problem) ----
onehot_all <- onehot_all %>%
  left_join(
    aehm %>% select(AEid, Citation, `Data Collection Methods`),
    by = "AEid"
  ) %>%
  mutate(
    Citation = coalesce(Citation, ""),
    `Data Collection Methods` = coalesce(`Data Collection Methods`, "")
  ) %>%
  relocate(Citation, `Data Collection Methods`, .after = AEid)

#write_xlsx(onehot_all, "C:/Users/kls52/OneDrive - University of Leicester/Existential Threats Syme-Krockow/Integrative Review R Project/AE heatmaps/ae onehot_all_final.xlsx")

