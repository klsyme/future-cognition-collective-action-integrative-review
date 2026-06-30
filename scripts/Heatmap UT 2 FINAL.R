library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(RColorBrewer)
library(pheatmap)
library(vegan)
library(writexl)
library(cluster)

## -----------------------------------------------------------------------------
## 0) Load
## -----------------------------------------------------------------------------

stopifnot(
  "UTid" %in% names(onehot_all),
  "Data Collection Methods" %in% names(onehot_all)
)

stopifnot(!anyDuplicated(onehot_all$UTid))

# Remove Citation to align with AE script
onehot_all2 <- onehot_all %>%
  select(-any_of("Citation"))

## -----------------------------------------------------------------------------
## 1) Build matrix
## -----------------------------------------------------------------------------

meta_cols <- c("UTid", "Data Collection Methods")
feature_cols <- setdiff(names(onehot_all2), meta_cols)

exclude_features <- c("Survey", "Experiment")
feature_cols_filtered <- setdiff(feature_cols, exclude_features)

M <- as.matrix(onehot_all2[, feature_cols_filtered, drop = FALSE])
rownames(M) <- onehot_all2$UTid
X <- t(M)

storage.mode(X) <- "numeric"
X <- ifelse(is.na(X), NA_real_, ifelse(X >= 0.5, 1, 0))

sub_df <- onehot_all2[, c("UTid", "Data Collection Methods")]

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
  hclust(vegdist(Xd[rows_keep, ], method = "jaccard", binary = TRUE),
         method = "average")
} else NULL

col_hc <- if (length(cols_keep) >= 2) {
  hclust(vegdist(t(Xd[, cols_keep]), method = "jaccard", binary = TRUE),
         method = "average")
} else NULL

row_order <- if (!is.null(row_hc)) rownames(Xd)[rows_keep][row_hc$order] else rownames(Xd)[rows_keep]
row_order <- c(row_order, rownames(Xd)[empty_rows])

col_order <- if (!is.null(col_hc)) colnames(Xd)[cols_keep][col_hc$order] else colnames(Xd)[cols_keep]
col_order <- c(col_order, colnames(Xd)[empty_cols])

M_plot <- X[row_order, col_order, drop = FALSE]

ann_col <- sub_df %>%
  select(UTid, `Data Collection Methods`) %>%
  column_to_rownames("UTid")

ann_plot <- ann_col[col_order, , drop = FALSE]

ann_colors <- list(
  `Data Collection Methods` = c(Experiment = "#ff6d00", Survey = "#00acc1")
)

gap_col_empty <- if (length(empty_cols)) length(col_order) - length(empty_cols) else NULL

## -----------------------------------------------------------------------------
## 3) Row clustering + silhouette (optional)
## -----------------------------------------------------------------------------

if (!is.null(row_hc)) {
  row_dist <- vegdist(Xd[rows_keep, ], method = "jaccard", binary = TRUE)
  
  sil_widths <- sapply(2:15, function(k) {
    cl <- cutree(row_hc, k = k)
    tryCatch(mean(silhouette(cl, row_dist)[, 3]), error = function(e) NA)
  })
  
  plot(2:15, sil_widths, type = "b", pch = 19,
       xlab = "k", ylab = "Avg silhouette width",
       main = "Row clustering silhouette")
  
  optimal_k <- which.max(sil_widths) + 1
  cat("Optimal k:", optimal_k, "\n")
}

k_clusters <- 14

## -----------------------------------------------------------------------------
## 4) Cluster assignment (NO annotation, gaps only)
## -----------------------------------------------------------------------------

row_clusters <- rep(NA_integer_, length(row_order))
names(row_clusters) <- row_order

if (!is.null(row_hc)) {
  cut_labels <- cutree(row_hc, k = min(k_clusters, length(rows_keep)))
  row_clusters[names(cut_labels)] <- cut_labels
}

## -----------------------------------------------------------------------------
## 5) Row gaps ONLY (visual cluster boundaries)
## -----------------------------------------------------------------------------

cl_lab <- ifelse(is.na(row_clusters), -999L, row_clusters)
chg_idx <- which(head(cl_lab, -1) != tail(cl_lab, -1))

if (any(is.na(row_clusters))) {
  first_na <- which(is.na(row_clusters))[1]
  chg_idx <- unique(c(chg_idx[chg_idx < first_na], first_na - 1))
}

gaps_row_final <- if (length(chg_idx)) chg_idx else NULL

## -----------------------------------------------------------------------------
## 6) Heatmap (NO cluster annotation, NO bundle legend)
## -----------------------------------------------------------------------------

binary_colors <- c("#f0f0f0", "#2166ac")
binary_breaks <- c(-0.5, 0.5, 1.5)

pheatmap(
  M_plot,
  annotation_row = NULL,      # ← removed clusters + bundles completely
  annotation_col = ann_plot,
  annotation_colors = ann_colors,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  gaps_row = gaps_row_final,
  gaps_col = gap_col_empty,
  fontsize = 10,
  main = sprintf("Feature × UTid — Jaccard-ordered (k = %d)", k_clusters),
  color = binary_colors,
  breaks = binary_breaks,
  legend_breaks = c(0, 1),
  legend_labels = c("Absent", "Present"),
  na_col = "#DDDDDD",
  border_color = "black"
)

## -----------------------------------------------------------------------------
## End
## -----------------------------------------------------------------------------
