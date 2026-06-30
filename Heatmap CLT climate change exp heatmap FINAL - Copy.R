library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(RColorBrewer)
library(pheatmap)
library(vegan)      # Jaccard distance
library(writexl)
library(cluster)  # (optional) if you later run silhouette()
library(grid)

## -----------------------------------------------------------------------------
## 0) Load
## -----------------------------------------------------------------------------
# EITHER: already in the session:
# onehot_all <- onehot_all

# OR: read from Excel (uncomment & set path):
# onehot_all <- readxl::read_xlsx("ca onehot_all.xlsx")

## Basic sanity
stopifnot("CLTid" %in% names(onehot_all),
          "Citation" %in% names(onehot_all),
          "Data Collection Methods" %in% names(onehot_all),
          "cap2" %in% names(onehot_all))
stopifnot(!anyDuplicated(onehot_all$CLTid))  # unique IDs required

## -----------------------------------------------------------------------------
## 1) Subset data: Survey + climate change
## -----------------------------------------------------------------------------
sub_df <- subset(onehot_all,
                 `Data Collection Methods` == "Experiment" & 
                   cap2 == "climate change" &
                   CLTid != 69
)


## -----------------------------------------------------------------------------
## 2) Build (features × CLTid) and column annotations
## -----------------------------------------------------------------------------
meta_cols <- c("CLTid", "Citation", "Data Collection Methods", "cap2")
feature_cols <- setdiff(names(sub_df), meta_cols)

exclude_features <- c("Survey", "Experiment", "climate change")
feature_cols_filtered <- setdiff(feature_cols, exclude_features)

M <- as.matrix(sub_df[, feature_cols_filtered, drop = FALSE])
rownames(M) <- sub_df$CLTid
X <- t(M)

suppressWarnings(storage.mode(X) <- "numeric")
X <- ifelse(is.na(X), NA_real_, ifelse(X >= 0.5, 1, 0))

ann_col <- sub_df %>%
  dplyr::select(CLTid, Citation) %>%
  tibble::column_to_rownames("CLTid")

## -----------------------------------------------------------------------------
## 2) External ordering via Jaccard + average
## -----------------------------------------------------------------------------
Xd <- X
Xd[is.na(Xd)] <- 0

empty_rows <- which(rowSums(Xd) == 0)
empty_cols <- which(colSums(Xd) == 0)
rows_keep  <- setdiff(seq_len(nrow(Xd)), empty_rows)
cols_keep  <- setdiff(seq_len(ncol(Xd)), empty_cols)

row_hc <- NULL
if (length(rows_keep) >= 2) {
  row_dist <- vegan::vegdist(Xd[rows_keep, , drop = FALSE],
                             method = "jaccard", binary = TRUE)
  row_hc <- hclust(row_dist, method = "average")
}

col_hc <- NULL
if (length(cols_keep) >= 2) {
  col_dist <- vegan::vegdist(t(Xd[, cols_keep, drop = FALSE]),
                             method = "jaccard", binary = TRUE)
  col_hc <- hclust(col_dist, method = "average")
}

# ✅ FIXED: no overwrite bug
row_order <- if (!is.null(row_hc)) {
  rownames(Xd)[rows_keep][row_hc$order]
} else {
  rownames(Xd)[rows_keep]
}

col_order <- if (!is.null(col_hc)) {
  colnames(Xd)[cols_keep][col_hc$order]
} else {
  colnames(Xd)[cols_keep]
}
col_order <- c(col_order, colnames(Xd)[empty_cols])

M_plot  <- X[row_order, col_order, drop = FALSE]
ann_plot <- ann_col[col_order, , drop = FALSE]

gap_col_empty <- if (length(empty_cols)) {
  length(col_order) - length(empty_cols)
} else NULL

## -----------------------------------------------------------------------------
## 3) Row clusters
## -----------------------------------------------------------------------------
if (!is.null(row_hc) && length(rows_keep) >= 2) {
  
  # Compute Jaccard distance on the same data used for clustering
  row_dist <- vegdist(Xd[rows_keep, ], method = "jaccard", binary = TRUE)
  
  k_range <- 2:15
  
  sil_widths <- sapply(k_range, function(k) {
    cl <- cutree(row_hc, k = k)
    
    tryCatch(
      mean(silhouette(cl, row_dist)[, 3]),
      error = function(e) NA
    )
  })
  
  # Plot silhouette profile
  plot(k_range, sil_widths,
       type = "b", pch = 19,
       xlab = "k", ylab = "Avg silhouette width",
       main = "Row clustering silhouette")
  
  # Identify optimal k (handle NA safely)
  optimal_k <- k_range[which.max(sil_widths)]
  
  cat("Optimal k:", optimal_k, "\n")
}


k_clusters <- 15

row_clusters <- rep(NA_integer_, length(row_order))
names(row_clusters) <- row_order

if (!is.null(row_hc) && length(rows_keep) >= 2) {
  cut_labels <- cutree(row_hc, k = min(k_clusters, length(rows_keep)))
  present <- names(row_clusters) %in% names(cut_labels)
  row_clusters[present] <- cut_labels[names(row_clusters)[present]]
}

cluster_levels <- sort(unique(row_clusters[!is.na(row_clusters)]))

annotation_row <- data.frame(
  Cluster = factor(
    ifelse(is.na(row_clusters), "Empty/NA", paste0("C", row_clusters)),
    levels = c(paste0("C", cluster_levels), "Empty/NA")
  )
)
rownames(annotation_row) <- names(row_clusters)

## -----------------------------------------------------------------------------
## 4) Bundles (matched exactly to main script)
## -----------------------------------------------------------------------------
X_bin <- M_plot
X_bin[is.na(X_bin)] <- 0
storage.mode(X_bin) <- "numeric"
X_bin <- ifelse(X_bin >= 0.5, 1L, 0L)

row_key <- apply(X_bin, 1, paste0, collapse = "")
bundle_groups <- split(rownames(X_bin), row_key)

bundles <- Filter(function(v) {
  length(v) > 1 && sum(X_bin[v[1], ]) >= 2
}, bundle_groups)

pretty_feats <- function(v, max_chars = 120) {
  s <- paste(v, collapse = " / ")
  if (nchar(s) > max_chars) paste0(substr(s, 1, max_chars - 1), "…") else s
}

BundleLegend_levels <- character(0)
BundleLegend_map    <- setNames(rep(NA_character_, nrow(X_bin)), rownames(X_bin))
bundle_meta         <- list()

if (length(bundles)) {
  for (i in seq_along(bundles)) {
    fs  <- bundles[[i]]
    sup <- sum(X_bin[fs[1], ])
    lab <- sprintf("Bundle (support=%d): %s", sup, pretty_feats(fs))
    
    BundleLegend_levels <- c(BundleLegend_levels, lab)
    BundleLegend_map[fs] <- lab
    
    bundle_meta[[i]] <- list(
      ID       = paste0("B", i),
      support  = sup,
      size     = length(fs),
      features = fs,
      CLTids   = colnames(X_bin)[X_bin[fs[1], ] == 1]
    )
  }
}

Bundle <- factor(BundleLegend_map, levels = BundleLegend_levels)
annotation_row$Bundle <- droplevels(Bundle)

# ✅ FIXED: proper fallback (was broken in your script)
nb <- length(BundleLegend_levels)
if (nb > 0) {
  base_cols <- RColorBrewer::brewer.pal(max(3, min(8, nb)), "Set2")
  if (nb > length(base_cols)) {
    base_cols <- grDevices::colorRampPalette(base_cols)(nb)
  } else {
    base_cols <- base_cols[seq_len(nb)]
  }
  bundle_cols <- setNames(base_cols, BundleLegend_levels)
} else {
  annotation_row$Bundle <- factor(rep("No bundles", nrow(annotation_row)))
  bundle_cols <- c("No bundles" = "#d9d9d9")
}

## -----------------------------------------------------------------------------
## 5) Row gaps
## -----------------------------------------------------------------------------
stopifnot(identical(rownames(M_plot), names(row_clusters)))

cl_seq <- row_clusters
is_na  <- is.na(cl_seq)
cl_lab <- ifelse(is_na, -999L, cl_seq)

chg_idx <- which(head(cl_lab, -1) != tail(cl_lab, -1))

if (any(is_na)) {
  first_na <- which(is_na)[1]
  chg_idx <- chg_idx[chg_idx < first_na]
  chg_idx <- unique(c(chg_idx, first_na - 1L))
}
gaps_row_final <- if (length(chg_idx)) chg_idx else NULL

## -----------------------------------------------------------------------------
## 6) Heatmap (IDENTICAL FORMAT)
## -----------------------------------------------------------------------------
# Keep only Bundle annotation
annotation_row <- annotation_row[, "Bundle", drop = FALSE]

# Ensure Bundle is a factor (recommended for clean legend)
annotation_row$Bundle <- factor(annotation_row$Bundle)

binary_colors <- c("#f0f0f0", "#2166ac")
binary_breaks <- c(-0.5, 0.5, 1.5)

ph <- pheatmap::pheatmap(
  M_plot,
  annotation_row    = annotation_row,
  annotation_col    = NULL,
  annotation_colors = list(Bundle = bundle_cols),
  annotation_legend = FALSE,
  cluster_rows      = FALSE,
  cluster_cols      = FALSE,
  gaps_row          = gaps_row_final,
  gaps_col          = gap_col_empty,
  show_rownames     = TRUE,
  show_colnames     = TRUE,
  fontsize          = 8,
  fontsize_row      = 9,   # ✅ Larger y-axis labels
  main = sprintf(
    "Feature × CLTid — Jaccard‑ordered (k = %d; bundles with ≥2 variables & duplicates)",
    k_clusters
  ),
  color         = binary_colors,
  breaks        = binary_breaks,
  legend_breaks = c(0, 1),
  border_color  = "grey80"
)

## -----------------------------------------------------------------------------
## Separate bundle legend (MATCHED)
## -----------------------------------------------------------------------------
legend_labels <- BundleLegend_levels
legend_colors <- bundle_cols[legend_labels]

legend_grob <- grid::legendGrob(
  labels = legend_labels,
  pch = 15,
  gp = grid::gpar(col = legend_colors, fill = legend_colors),
  ncol = 1
)

png("bundle_legendcltexcc.png", width = 2000, height = 3000, res = 300)
grid::grid.newpage()
grid::grid.draw(legend_grob)
dev.off()
