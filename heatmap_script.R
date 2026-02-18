#!/usr/bin/env Rscript
# heatmap_script.R
# Simple, reusable heatmap script for expression/abundance matrices
# Usage:
#   Rscript heatmap_script.R [input.csv] [output.png] [scale]
#   input.csv: CSV with rownames in first column (genes/features) and samples in columns
#   output.png: optional (default: heatmap.png)
#   scale: "row", "column", or "none" (default: row)

suppressPackageStartupMessages({
  if (!requireNamespace("pheatmap", quietly=TRUE)) {
    stop("Please install pheatmap: install.packages('pheatmap')")
  }
  if (!requireNamespace("RColorBrewer", quietly=TRUE)) {
    stop("Please install RColorBrewer: install.packages('RColorBrewer')")
  }
})

library(pheatmap)
library(RColorBrewer)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  message("No input file provided — generating example matrix.")
  mat <- matrix(rnorm(10*8, mean=0, sd=1), nrow=10)
  rownames(mat) <- paste0("Feature", seq_len(nrow(mat)))
  colnames(mat) <- paste0("Sample", seq_len(ncol(mat)))
  outfile <- "heatmap_example.png"
  scale.opt <- "row"
} else {
  infile <- args[1]
  outfile <- ifelse(length(args) >= 2, args[2], "heatmap.png")
  scale.opt <- ifelse(length(args) >= 3, args[3], "row")

  # read CSV: first column contains rownames
  mat <- as.matrix(read.csv(infile, row.names = 1, check.names = FALSE))
}

# optional scaling
if (tolower(scale.opt) %in% c("row", "column")) {
  if (tolower(scale.opt) == "row") {
    mat <- t(scale(t(mat)))
  } else {
    mat <- scale(mat)
  }
}

# color palette (blue-white-red)
cols <- colorRampPalette(rev(brewer.pal(n = 11, name = "RdBu")))(100)

# draw heatmap and save to file (pheatmap auto-detects PNG/PDF by filename extension)
pheatmap(
  mat,
  color = cols,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  show_rownames = TRUE,
  show_colnames = TRUE,
  fontsize = 10,
  filename = outfile,
  width = 8,
  height = 6
)

message(sprintf("Wrote heatmap to %s", outfile))
