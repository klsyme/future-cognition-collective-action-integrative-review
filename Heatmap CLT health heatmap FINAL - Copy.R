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

sub_df <- subset(onehot_all, cap2 == "public health")

## -----------------------------------------------------------------------------
## 2) Build matrix
## -----------------------------------------------------------------------------

meta_cols <- c("CLTid", "Citation", "Data Collection Methods", "cap2")
feature_cols <- setdiff(names(sub_df), meta_cols)

exclude_features <- c("Survey", "Experiment", "public health")
feature_cols_filtered <- setdiff(feature_cols, exclude_features)

M <- as.matrix(sub_df[, feature_cols_filtered, drop = FALSE])
rownames(M) <- sub_df$CLTid
X <- t(M)

storage.mode(X) <- "numeric"
X <- ifelse(is.na(X), NA_real_, ifelse(X >= 0.5, 1, 0))

# ✅ Remove truly empty rows (fix grey block issue)
row_totals <- rowSums(ifelse(is.na(X), 0, X))
X <- X[row_totals > 0, , drop = FALSE]

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

M_plot <- X[row_order, col_order, drop = FALSE]
M_plot[is.na(M_plot)] <- 0

gap_col_empty <- if (length(empty_cols)) length(col_order) - length(empty_cols) else NULL

## -----------------------------------------------------------------------------
## 4) Row clustering (gaps only)
## -----------------------------------------------------------------------------

k_clusters <- 15

row_clusters <- rep(NA_integer_, length(row_order))
names(row_clusters) <- row_order

if (!is.null(row_hc)) {
  cut_labels <- cutree(row_hc, k = min(k_clusters, length(rows_keep)))
  row_clusters[names(cut_labels)] <- cut_labels
}

## -----------------------------------------------------------------------------
## 5) Bundles (RETAINED)
## -----------------------------------------------------------------------------

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
}

annotation_row <- data.frame(row.names = row_order)

if (length(BundleLegend_levels) > 0) {
  annotation_row$Bundle <- factor(BundleLegend_map[row_order],
                                  levels = BundleLegend_levels)
  
  nb <- length(BundleLegend_levels)
  cols <- brewer.pal(max(3, min(8, nb)), "Set2")
  
  if (nb > length(cols)) {
    cols <- colorRampPalette(cols)(nb)
  } else {
    cols <- cols[seq_len(nb)]
  }
  
  ann_colors <- list(Bundle = setNames(cols, BundleLegend_levels))
} else {
  annotation_row <- NULL
  ann_colors <- NULL
}

## -----------------------------------------------------------------------------
## 6) Row gaps
## -----------------------------------------------------------------------------

cl_lab <- ifelse(is.na(row_clusters), -999L, row_clusters)
chg_idx <- which(head(cl_lab, -1) != tail(cl_lab, -1))

if (any(is.na(row_clusters))) {
  first_na <- which(is.na(row_clusters))[1]
  chg_idx <- unique(c(chg_idx[chg_idx < first_na], first_na - 1))
}

gaps_row_final <- if (length(chg_idx)) chg_idx else NULL

## -----------------------------------------------------------------------------
## 7) Heatmap (Bundles only)
## -----------------------------------------------------------------------------

binary_colors <- c("#f0f0f0", "#2166ac")
binary_breaks <- c(-0.5, 0.5, 1.5)

ph <- pheatmap(
  M_plot,
  annotation_row = annotation_row,
  annotation_col = NULL,   # ✅ no top bar
  annotation_colors = ann_colors,
  
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  
  gaps_row = gaps_row_final,
  gaps_col = gap_col_empty,
  
  fontsize = 14,
  fontsize_row = 11,
  fontsize_col = 13,
  
  main = sprintf("Feature × CLTid — Jaccard-ordered (k = %d, bundles shown)", k_clusters),
  
  color = binary_colors,
  breaks = binary_breaks,
  legend_breaks = c(0, 1),
  legend_labels = c("Absent", "Present"),
  na_col = "#DDDDDD",
  border_color = "black"
)

png("healthclt_heatmap_bundles.png", width = 5000, height = 6000, res = 450)
grid.newpage()
grid.draw(ph$gtable)
dev.off()

## -----------------------------------------------------------------------------
## End
## -----------------------------------------------------------------------------


