library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(pheatmap)
library(vegan)
library(cluster)
library(writexl)

## -----------------------------------------------------------------------------
## 0) Load
## -----------------------------------------------------------------------------

stopifnot("EFTid" %in% names(onehot_all),
          "Data Collection Methods" %in% names(onehot_all))

stopifnot(!anyDuplicated(onehot_all$EFTid))

# Remove DOI like previous script
onehot_all2 <- onehot_all %>%
  select(-any_of("DOI"))

## -----------------------------------------------------------------------------
## 1) Build matrix
## -----------------------------------------------------------------------------

meta_cols <- c("EFTid", "Data Collection Methods")
feature_cols <- setdiff(names(onehot_all2), meta_cols)

exclude_features <- c("Survey", "Experiment")
feature_cols_filtered <- setdiff(feature_cols, exclude_features)

M <- as.matrix(onehot_all2[, feature_cols_filtered, drop = FALSE])
rownames(M) <- onehot_all2$EFTid
X <- t(M)

storage.mode(X) <- "numeric"
X <- ifelse(is.na(X), NA_real_, ifelse(X >= 0.5, 1, 0))

# Column annotations (no DOI)
ann_col <- onehot_all2 %>%
  select(EFTid, `Data Collection Methods`) %>%
  column_to_rownames("EFTid")

ann_colors <- list(
  `Data Collection Methods` = c(Experiment = "#ff6d00", Survey = "#00acc1")
)

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
  hclust(vegdist(Xd[rows_keep, , drop = FALSE], method = "jaccard", binary = TRUE),
         method = "average")
} else NULL

col_hc <- if (length(cols_keep) >= 2) {
  hclust(vegdist(t(Xd[, cols_keep, drop = FALSE]), method = "jaccard", binary = TRUE),
         method = "average")
} else NULL

row_order <- if (!is.null(row_hc)) rownames(Xd)[rows_keep][row_hc$order] else rownames(Xd)[rows_keep]
row_order <- c(row_order, rownames(Xd)[empty_rows])

col_order <- if (!is.null(col_hc)) colnames(Xd)[cols_keep][col_hc$order] else colnames(Xd)[cols_keep]
col_order <- c(col_order, colnames(Xd)[empty_cols])

M_plot <- X[row_order, col_order, drop = FALSE]

# Align annotations
common_cols <- intersect(rownames(ann_col), colnames(M_plot))
ann_plot <- ann_col[common_cols, , drop = FALSE]
M_plot   <- M_plot[, common_cols, drop = FALSE]

gap_col_empty <- if (length(empty_cols)) ncol(M_plot) - length(empty_cols) else NULL

## -----------------------------------------------------------------------------
## 3) Silhouette analysis (diagnostic only)
## -----------------------------------------------------------------------------

if (!is.null(row_hc) && length(rows_keep) >= 2) {
  
  row_dist <- vegdist(Xd[rows_keep, , drop = FALSE],
                      method = "jaccard", binary = TRUE)
  
  ks <- 2:min(15, length(rows_keep) - 1)
  
  sil_widths <- sapply(ks, function(k) {
    cl <- cutree(row_hc, k = k)
    out <- tryCatch(silhouette(cl, row_dist), error = function(e) NA)
    if (is.matrix(out)) mean(out[, 3]) else NA_real_
  })
  
  plot(ks, sil_widths, type = "b", pch = 19,
       xlab = "Number of clusters (k)",
       ylab = "Average silhouette width",
       main = "Silhouette analysis (rows)")
  
  optimal_k <- ks[which.max(sil_widths)]
  cat("Optimal k:", optimal_k, "\n")
}

## -----------------------------------------------------------------------------
## 4) Clusters → gaps ONLY
## -----------------------------------------------------------------------------

k_clusters <- if (exists("optimal_k")) optimal_k else 10

row_clusters <- rep(NA_integer_, length(row_order))
names(row_clusters) <- row_order

if (!is.null(row_hc) && length(rows_keep) >= 2) {
  cut_labels <- cutree(row_hc, k = min(k_clusters, length(rows_keep)))
  row_clusters[names(cut_labels)] <- cut_labels
}

## Convert clusters to gaps
cl_seq <- row_clusters
is_na  <- is.na(cl_seq)

cl_lab <- ifelse(is_na, -999L, cl_seq)
chg_idx <- which(head(cl_lab, -1) != tail(cl_lab, -1))

if (any(is_na)) {
  first_na <- which(is_na)[1]
  chg_idx <- unique(c(chg_idx[chg_idx < first_na], first_na - 1))
}

gaps_row_final <- if (length(chg_idx)) chg_idx else NULL

## -----------------------------------------------------------------------------
## 5) Heatmap (clean)
## -----------------------------------------------------------------------------

binary_colors <- c("#f0f0f0", "#2166ac")
binary_breaks <- c(-0.5, 0.5, 1.5)

pheatmap(
  M_plot,
  annotation_col    = ann_plot,
  annotation_colors = ann_colors,
  cluster_rows      = FALSE,
  cluster_cols      = FALSE,
  gaps_row          = gaps_row_final,
  gaps_col          = gap_col_empty,
  show_rownames     = TRUE,
  show_colnames     = TRUE,
  fontsize          = 10,
  main = sprintf("Feature × EFTid — Jaccard‑ordered (k = %d)", k_clusters),
  color             = binary_colors,
  breaks            = binary_breaks,
  legend_breaks     = c(0, 1),
  legend_labels     = c("Absent", "Present"),
  na_col            = "#bdbdbd"
)
