library(dplyr)
library(tidyr)
library(stringr)
library(readr)

clthm <- clt_authors_combined

# Remove article with specific title
clthm <- clthm %>%
  filter(`Article Title` != "Discounting environmental policy: The effects of psychological distance over time and space")

# Check that it was removed
clthm %>%
  filter(`Article Title` == "Discounting environmental policy: The effects of psychological distance over time and space") %>%
  nrow()  # Should return 0

# Normalise text

clthm2 <- clthm %>%
  mutate(
    type_clean = `Type of collective-action problem` %>%
      str_to_lower() %>%
      str_replace_all("[\u00A0]", " ") %>%   # non‑breaking spaces
      str_replace_all("[^a-z0-9 ]", " ") %>% # strip punctuation
      str_squish()
  )


# Assuming your dataframe is called clthm and the column is literally named:
# `Type of collective-action problem` with spaces and punctuation.

clthm2 <- clthm2 %>%
  mutate(
    cap2 = case_when(
      # --- Public health ---
      str_detect(
        type_clean,
        "covid\\s*19|covid|vaccin|monkeypox|zika|transmission|healthcare|prevention|containment|travel"
      ) ~ "public health",
      
      # --- Climate change ---
      str_detect(
        type_clean,
        "climate|biodiversity|carbon capture|plastic|air quality|particulate|environmental|sustainab|endangered|pesticid"
      ) ~ "climate change",
      
      TRUE ~ NA_character_
    )
  )


# ---- assumptions ----
# clthm2: your data.frame / tibble
# CLTid : unique per-row analysis identifier (required)
stopifnot("CLTid" %in% names(clthm2))

# ---- parsing rules ----
# Split ONLY on commas to preserve punctuation like "(-)" and labels like "Hope/Enthusiasm"
.split_regex <- "\\s*,\\s*"
# Treat these tokens as "no labels"
.none_tokens <- c("none", "no", "na")

# ---- one-hot helper (as you had) ----
one_hot_single_col <- function(df, id_col = "CLTid", field) {
  stopifnot(field %in% names(df))
  df %>%
    dplyr::select(dplyr::all_of(id_col), dplyr::all_of(field)) %>%
    dplyr::mutate(
      raw   = dplyr::coalesce(as.character(.data[[field]]), ""),
      raw   = stringr::str_squish(raw),
      raw   = dplyr::if_else(stringr::str_to_lower(raw) %in% .none_tokens, "", raw),
      label = dplyr::if_else(raw == "", list(character()), stringr::str_split(raw, .split_regex))
    ) %>%
    dplyr::select(-raw) %>%
    tidyr::unnest_longer(label, keep_empty = TRUE) %>%
    dplyr::mutate(label = stringr::str_squish(dplyr::coalesce(label, ""))) %>%
    dplyr::filter(label != "") %>%
    dplyr::mutate(value = 1L) %>%
    dplyr::distinct(!!rlang::sym(id_col), label, .keep_all = TRUE) %>%
    tidyr::pivot_wider(
      id_cols     = dplyr::all_of(id_col),
      names_from  = label,    # label text becomes column name (backticks when selecting later)
      values_from = value,
      values_fill = 0
    )
}

# ---- build per-field one-hots ----
oh_mediators   <- one_hot_single_col(clthm2, field = "Mediators")
oh_moderators  <- one_hot_single_col(clthm2, field = "Moderators")
oh_predictors  <- one_hot_single_col(clthm2, field = "Predictors")
oh_dvk         <- one_hot_single_col(clthm2, field = "dv.k2")

# NEW (optional): one-hot for Data Collection Methods
oh_dcm         <- one_hot_single_col(clthm2, field = "Data Collection Methods")

# NEW: one-hot for your new variable cap2 (replacing "Type of collective-action problem")
oh_cap2        <- one_hot_single_col(clthm2, field = "cap2")

# ---- helper: safe join that OR-collapses duplicate indicator columns ----
safe_join_or <- function(x, y, key = "CLTid") {
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
onehot_all <- clthm2 %>% dplyr::select(CLTid)

# Join in any order; duplicates will be collapsed to a single column name
onehot_all <- safe_join_or(onehot_all, oh_moderators, key = "CLTid")
onehot_all <- safe_join_or(onehot_all, oh_mediators,  key = "CLTid")
onehot_all <- safe_join_or(onehot_all, oh_predictors, key = "CLTid")
onehot_all <- safe_join_or(onehot_all, oh_dvk,        key = "CLTid")

# Include the one-hot expansion of Data Collection Methods
onehot_all <- safe_join_or(onehot_all, oh_dcm,        key = "CLTid")

# Include the one-hot expansion for cap2
onehot_all <- safe_join_or(onehot_all, oh_cap2,       key = "CLTid")

# ---- replace any remaining NAs with 0 across indicator columns ----
indicator_cols <- setdiff(names(onehot_all), "CLTid")
onehot_all[indicator_cols] <- lapply(onehot_all[indicator_cols], function(x) {
  x[is.na(x)] <- 0L
  as.integer(x)
})

# ---- add metadata columns (raw strings) for Citation and Data Collection Methods ----
# This ensures both appear in the final file and are usable for annotations.
onehot_all <- onehot_all %>%
  dplyr::left_join(
    clthm2 %>% dplyr::select(CLTid, Citation, `Data Collection Methods`, cap2),
    by = "CLTid"
  ) %>%
  dplyr::mutate(
    Citation = dplyr::coalesce(Citation, ""),
    `Data Collection Methods` = dplyr::coalesce(`Data Collection Methods`, ""),
    cap2 = dplyr::coalesce(cap2, "")
  ) %>%
  dplyr::relocate(Citation, `Data Collection Methods`, cap2, .after = CLTid)

# ---- quick verification for an example row (edit "11" if needed) ----
# onehot_all %>%
#   dplyr::filter(CLTid == "11") %>%
#   dplyr::select(CLTid, Citation, `Data Collection Methods`, cap2)

# ---- OPTIONAL: export ----
# readr::write_csv(onehot_all, "onehot_all.csv")

# If you want Excel export:
# writexl::write_xlsx(list(onehot_all = onehot_all), "clt onehot_all.xlsx")

# ---- OPTIONAL: frequency table of labels (overall counts) ----
# freqs <- colSums(onehot_all[indicator_cols], na.rm = TRUE)
# sort(freqs, decreasing = TRUE)
