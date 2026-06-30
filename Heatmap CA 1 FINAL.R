library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(readr)   # uncomment if you want CSV export later
library(writexl) # uncomment if you want Excel export later

# ============== 0) Load data ==============
canmf <- ca_authors_combined

# Guarantee CAid exists
stopifnot("CAid" %in% names(canmf))

# Removed section 1

# ============== 2) Helper: parsing rules for one-hot expansion ==============
.split_regex <- "\\s*,\\s*"   # split on commas only
.none_tokens <- c("none", "no", "na")

# One column -> wide one-hot (exact labels become column names)
one_hot_single_col <- function(df, id_col = "CAid", field) {
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
      names_from  = label,    # exact label becomes column name
      values_from = value,
      values_fill = 0
    )
}

# ============== 3) Build per-field one-hots ==============
oh_mediators  <- one_hot_single_col(canmf, field = "Mediators")
oh_moderators <- one_hot_single_col(canmf, field = "Moderators")
oh_predictors <- one_hot_single_col(canmf, field = "Predictors")
oh_dvk2 <- one_hot_single_col(canmf, field = "dv.k2")

# ============== 4) Helper: safe join that OR-collapses duplicate indicator cols ==============
safe_join_or <- function(x, y, key = "CAid") {
  common <- setdiff(intersect(names(x), names(y)), key)
  out <- dplyr::left_join(x, y, by = key, suffix = c(".x", ".y"))
  
  for (nm in common) {
    lhs <- if (paste0(nm, ".x") %in% names(out)) paste0(nm, ".x") else nm
    rhs <- paste0(nm, ".y")
    
    lx <- out[[lhs]]; rx <- out[[rhs]]
    lx <- ifelse(is.na(lx), 0L, lx)
    rx <- ifelse(is.na(rx), 0L, rx)
    
    out[[nm]] <- pmax(as.integer(lx), as.integer(rx))
    
    if (lhs != nm && lhs %in% names(out)) out[[lhs]] <- NULL
    if (rhs %in% names(out)) out[[rhs]] <- NULL
  }
  out
}

# ============== 5) Combine all one-hots with OR-collapse ==============
onehot_all <- canmf %>% select(CAid)
onehot_all <- safe_join_or(onehot_all, oh_moderators, key = "CAid")
onehot_all <- safe_join_or(onehot_all, oh_mediators,  key = "CAid")
onehot_all <- safe_join_or(onehot_all, oh_predictors, key = "CAid")
onehot_all <- safe_join_or(onehot_all, oh_dvk2, key = "CAid")

# Replace any remaining NAs across indicator columns with 0
indicator_cols <- setdiff(names(onehot_all), "CAid")
onehot_all[indicator_cols] <- lapply(onehot_all[indicator_cols], function(x) {
  x[is.na(x)] <- 0L
  as.integer(x)
})

# ============== 6) Add Citation + Data Collection Methods (safe) ==============
# If canmf might have multiple rows per CAid, deduplicate first:
canmf_keys <- canmf %>%
  arrange(CAid) %>%
  group_by(CAid) %>%
  summarise(
    Citation = dplyr::first(na.omit(Citation)),
    `Data Collection Methods` = dplyr::first(na.omit(`Data Collection Methods`)),
    .groups = "drop"
  )

# Warn if CAid is non-unique in canmf (many-to-many risk)
if (anyDuplicated(canmf$CAid) > 0) {
  dup_keys <- canmf$CAid[duplicated(canmf$CAid) | duplicated(canmf$CAid, fromLast = TRUE)]
  warning("CAid not unique in canmf. Using first non-missing Citation/Method per CAid for join. Duplicates: ",
          paste(unique(dup_keys), collapse = ", "))
}

onehot_all <- onehot_all %>%
  left_join(canmf_keys, by = "CAid")

# ============== 7) Relocate (guarded) ==============
if ("Citation" %in% names(onehot_all)) {
  onehot_all <- relocate(onehot_all, Citation, .after = CAid)
}
if ("Data Collection Methods" %in% names(onehot_all)) {
  if ("Citation" %in% names(onehot_all)) {
    onehot_all <- relocate(onehot_all, `Data Collection Methods`, .after = Citation)
  } else {
    onehot_all <- relocate(onehot_all, `Data Collection Methods`, .after = CAid)
  }
}

# ============== 8) Quick diagnostics (optional) ==============
message("Rows with no Citation match: ", sum(is.na(onehot_all$Citation)))
# names(onehot_all)              # inspect columns
# freqs <- colSums(onehot_all[indicator_cols], na.rm = TRUE); sort(freqs, TRUE)

# ============== 9) Export (optional) ==============
# readr::write_csv(onehot_all, "onehot_all.csv")
#writexl::write_xlsx(list(onehot_all = onehot_all), "ca onehot_all.xlsx")



# 1. Summarize study count by type of collective-action problem
summary <- canmf %>%
  group_by(`Type of collective-action problem`) %>%
  summarise(Study_Count = n_distinct(Citation)) %>%
  arrange(desc(Study_Count))

print(summary)

# 2. Summarize unique counts across key identifiers
canmf %>%
  summarise(
    Unique_Citation = n_distinct(Citation),
    Unique_StudyOrder = n_distinct(`Study order within article`),
    Unique_Combination = n_distinct(paste(Citation, `Study order within article`))
  )

# 3. Summarize theoretical frameworks by Citation
summary <- canmf %>%
  group_by(Citation) %>%
  summarise(Theoretical_Frameworks = paste(unique(`Theoretical Frameworks`), collapse = "; ")) %>%
  arrange(Citation)

print(summary, width = Inf)
View(summary)
