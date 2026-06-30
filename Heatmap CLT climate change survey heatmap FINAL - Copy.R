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

stopifnot("CLTid" %in% names(onehot_all),
          "Citation" %in% names(onehot_all),
          "Data Collection Methods" %in% names(onehot_all),
          "cap2" %in% names(onehot_all))

stopifnot(!anyDuplicated(onehot_all$CLTid))

## -----------------------------------------------------------------------------
## 1) Subset data
## -----------------------------------------------------------------------------

sub_df <- subset(onehot_all,
                 `Data Collection Methods` == "Survey" &
                   cap2 == "climate change")

## -----------------------------------------------------------------------------
## 2) Build matrix
## -----------------------------------------------------------------------------

meta_cols <- c("CLTid", "Citation", "Data Collection Methods", "cap2")
feature_cols <- setdiff(names(sub_df), meta_cols)

exclude_features <- c("Survey", "Experiment", "climate change")
feature_cols_filtered <- setdiff(feature_cols, exclude_features)

M <- as.matrix(sub_df[, feature_cols_filtered, drop = FALSE])
rownames(M) <- sub_df$CLTid
X <- t(M)

storage.mode(X) <- "numeric"
X <- ifelse(is.na(X), NA_real_, ifelse(X >= 0.5, 1, 0))

# ------------------------------------------------------------------
# ✅ Remove rows that are completely absent (all zeros / NA)
# ------------------------------------------------------------------
row_totals <- rowSums(ifelse(is.na(X), 0, X))

keep_rows <- row_totals > 0
X <- X[keep_rows, , drop = FALSE]

## -----------------------------------------------------------------------------
## 3) Jaccard ordering
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

# ✅ Correct ordering (keeps structure stable)
row_order <- if (!is.null(row_hc)) {
  rownames(Xd)[rows_keep][row_hc$order]
} else {
  rownames(Xd)[rows_keep]
}
row_order <- c(row_order, rownames(Xd)[empty_rows])

col_order <- if (!is.null(col_hc)) {
  colnames(Xd)[cols_keep][col_hc$order]
} else {
  colnames(Xd)[cols_keep]
}
col_order <- c(col_order, colnames(Xd)[empty_cols])

## ✅ FIX: remove NA for plotting (prevents “missing present ticks”)
M_plot <- X[row_order, col_order, drop = FALSE]
M_plot[is.na(M_plot)] <- 0

gap_col_empty <- if (length(empty_cols)) length(col_order) - length(empty_cols) else NULL

## -----------------------------------------------------------------------------
## 4) Row clustering + silhouette (optional)
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

k_clusters <- 15

## -----------------------------------------------------------------------------
## 5) Cluster assignment (for gaps only)
## -----------------------------------------------------------------------------

row_clusters <- rep(NA_integer_, length(row_order))
names(row_clusters) <- row_order

if (!is.null(row_hc)) {
  cut_labels <- cutree(row_hc, k = min(k_clusters, length(rows_keep)))
  row_clusters[names(cut_labels)] <- cut_labels
}

## -----------------------------------------------------------------------------
## 6) Row gaps ONLY
## -----------------------------------------------------------------------------

cl_lab <- ifelse(is.na(row_clusters), -999L, row_clusters)
chg_idx <- which(head(cl_lab, -1) != tail(cl_lab, -1))

if (any(is.na(row_clusters))) {
  first_na <- which(is.na(row_clusters))[1]
  chg_idx <- unique(c(chg_idx[chg_idx < first_na], first_na - 1))
}

gaps_row_final <- if (length(chg_idx)) chg_idx else NULL

## -----------------------------------------------------------------------------
## 7) Heatmap (clean)
## -----------------------------------------------------------------------------

binary_colors <- c("#f0f0f0", "#2166ac")
binary_breaks <- c(-0.5, 0.5, 1.5)

ph <- pheatmap(
  M_plot,
  annotation_row = NULL,
  annotation_col = NULL,     # ✅ removes top annotation bar
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  gaps_row = gaps_row_final,
  gaps_col = gap_col_empty,
  fontsize = 10,
  show_rownames = TRUE,
  show_colnames = TRUE,
  main = sprintf("Feature × CLTid — Jaccard-ordered (k = %d)", k_clusters),
  color = binary_colors,
  breaks = binary_breaks,
  legend_breaks = c(0, 1),
  legend_labels = c("Absent", "Present"),
  na_col = "#DDDDDD",
  border_color = "black"
)

png("cltsuccheatmap.png", width = 5000, height = 6000, res = 450)
grid.newpage()
grid.draw(ph$gtable)
dev.off()

## -----------------------------------------------------------------------------
## End
## -----------------------------------------------------------------------------
