library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(RColorBrewer)
library(pheatmap)
library(vegan)
library(writexl)
library(cluster)
library(grid)

## -----------------------------------------------------------------------------
## 0) Load
## -----------------------------------------------------------------------------

stopifnot(
  "CFCid" %in% names(onehot_all),
  "Citation" %in% names(onehot_all),
  "Data Collection Methods" %in% names(onehot_all)
)

stopifnot(!anyDuplicated(onehot_all$CFCid))

## -----------------------------------------------------------------------------
## 1) Build matrix
## -----------------------------------------------------------------------------

meta_cols <- c("CFCid", "Citation", "Data Collection Methods")
feature_cols <- setdiff(names(onehot_all), meta_cols)

exclude_features <- c("Survey", "Experiment")
feature_cols_filtered <- setdiff(feature_cols, exclude_features)

M <- as.matrix(onehot_all[, feature_cols_filtered, drop = FALSE])
rownames(M) <- onehot_all$CFCid
X <- t(M)

storage.mode(X) <- "numeric"
X <- ifelse(is.na(X), NA_real_, ifelse(X >= 0.5, 1, 0))

sub_df <- onehot_all[, c("CFCid", "Data Collection Methods")]

## -----------------------------------------------------------------------------
## 2) Jaccard ordering
## -----------------------------------------------------------------------------

Xd <- X
Xd[is.na(Xd)] <- 0

empty_rows <- which(rowSums(Xd) == 0)
empty_cols <- which(colSums(Xd) == 0)

rows_keep <- setdiff(seq_len(nrow(Xd)), empty_rows)
cols_keep <- setdiff(seq_len(ncol(Xd)), empty_cols)

row_hc <- if (length(rows_keep) >= 2) {
  hclust(vegdist(Xd[rows_keep, ], method = "jaccard", binary = TRUE), method = "average")
} else NULL

col_hc <- if (length(cols_keep) >= 2) {
  hclust(vegdist(t(Xd[, cols_keep]), method = "jaccard", binary = TRUE), method = "average")
} else NULL

row_order <- if (!is.null(row_hc)) rownames(Xd)[rows_keep][row_hc$order] else rownames(Xd)[rows_keep]
row_order <- c(row_order, rownames(Xd)[empty_rows])

col_order <- if (!is.null(col_hc)) colnames(Xd)[cols_keep][col_hc$order] else colnames(Xd)[cols_keep]
col_order <- c(col_order, colnames(Xd)[empty_cols])

M_plot <- X[row_order, col_order, drop = FALSE]

ann_col <- sub_df %>%
  select(CFCid, `Data Collection Methods`) %>%
  column_to_rownames("CFCid")

ann_plot <- ann_col[col_order, , drop = FALSE]

ann_colors <- list(
  `Data Collection Methods` = c(Experiment = "#ff6d00", Survey = "#00acc1")
)

gap_col_empty <- if (length(empty_cols)) length(col_order) - length(empty_cols) else NULL

## -----------------------------------------------------------------------------
## 3) Row clustering (for gaps only)
## -----------------------------------------------------------------------------

k_clusters <- 11

row_clusters <- rep(NA_integer_, length(row_order))
names(row_clusters) <- row_order

if (!is.null(row_hc)) {
  cut_labels <- cutree(row_hc, k = min(k_clusters, length(rows_keep)))
  row_clusters[names(cut_labels)] <- cut_labels
}

## -----------------------------------------------------------------------------
## 3b) Bundle detection
## -----------------------------------------------------------------------------

annotation_row <- data.frame(row.names = row_order)

X_bin <- M_plot
X_bin[is.na(X_bin)] <- 0
X_bin <- ifelse(X_bin >= 0.5, 1L, 0L)

row_key <- apply(X_bin, 1, paste0, collapse = "")
bundle_groups <- split(rownames(X_bin), row_key)

bundles <- Filter(function(v) {
  length(v) > 1 && sum(X_bin[v[1], ]) >= 2
}, bundle_groups)

BundleLegend_levels <- character(0)
BundleLegend_map <- setNames(rep(NA_character_, nrow(X_bin)), rownames(X_bin))

if (length(bundles) > 0) {
  
  for (i in seq_along(bundles)) {
    fs <- bundles[[i]]
    sup <- sum(X_bin[fs[1], ])
    
    lab <- sprintf("B%d (n=%d, support=%d)", i, length(fs), sup)
    
    BundleLegend_levels <- c(BundleLegend_levels, lab)
    BundleLegend_map[fs] <- lab
  }
  
  annotation_row$Bundle <- factor(BundleLegend_map, levels = BundleLegend_levels)
  
  nb <- length(BundleLegend_levels)
  base_cols <- brewer.pal(max(3, min(8, nb)), "Set2")
  
  if (nb > length(base_cols)) {
    base_cols <- colorRampPalette(base_cols)(nb)
  } else {
    base_cols <- base_cols[seq_len(nb)]
  }
  
  ann_colors$Bundle <- setNames(base_cols, BundleLegend_levels)
  
} else {
  
  annotation_row <- NULL
  ann_colors$Bundle <- NULL
}

## -----------------------------------------------------------------------------
## 4) Row gaps
## -----------------------------------------------------------------------------

cl_lab <- ifelse(is.na(row_clusters), -999L, row_clusters)
chg_idx <- which(head(cl_lab, -1) != tail(cl_lab, -1))

if (any(is.na(row_clusters))) {
  first_na <- which(is.na(row_clusters))[1]
  chg_idx <- unique(c(chg_idx[chg_idx < first_na], first_na - 1))
}

gaps_row_final <- if (length(chg_idx)) chg_idx else NULL

## -----------------------------------------------------------------------------
## 5) Heatmap
## -----------------------------------------------------------------------------

binary_colors <- c("#f0f0f0", "#2166ac")
binary_breaks <- c(-0.5, 0.5, 1.5)

ph <- pheatmap(
  M_plot,
  annotation_row = annotation_row,
  annotation_col = ann_plot,
  annotation_colors = ann_colors,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  gaps_row = gaps_row_final,
  gaps_col = gap_col_empty,
  fontsize = 10,
  main = sprintf("Feature × CFCid — Jaccard‑ordered (k = %d)", k_clusters),
  color = binary_colors,
  breaks = binary_breaks,
  legend_breaks = c(0, 1),
  legend_labels = c("0", "1"),
  na_col = "#bdbdbd",
  border_color = "black"
)

png("cfcheatmap.png", width = 5000, height = 6000, res = 450)

grid::grid.newpage()
grid::grid.draw(ph$gtable)

dev.off()


