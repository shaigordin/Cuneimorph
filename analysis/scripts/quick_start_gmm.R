# ============================================================================
# QUICK START: Cuneiform Geometric Morphometrics
# Simplified analysis script for beginners
# ============================================================================

# Load packages
library(StereoMorph)
library(geomorph)

# Set working directory
setwd("/home/user/Cuneimorph")

# ===== STEP 1: Read Data =====
cat("Reading landmark data...\n")
shape_files <- list.files("data/shapes", pattern = "\\.txt$", full.names = TRUE)
shapes <- readShapes(shape_files)

# ===== STEP 2: Prepare Array =====
cat("Preparing data array...\n")
# Extract landmarks
landmarks <- shapes$landmarks.scaled

# Convert to array (p x k x n format: landmarks x dimensions x specimens)
n_lm <- nrow(landmarks[[1]])   # number of landmarks
n_dim <- ncol(landmarks[[1]])  # dimensions (2 for 2D)
n_spec <- length(landmarks)    # number of specimens

coords <- array(NA, dim = c(n_lm, n_dim, n_spec))
for (i in 1:n_spec) {
  coords[, , i] <- as.matrix(landmarks[[i]])
}

# Name the specimens
dimnames(coords)[[3]] <- gsub(".txt", "", basename(shape_files))

cat(paste("Data loaded:", n_spec, "specimens with", n_lm, "landmarks\n\n"))

# ===== STEP 3: Procrustes Analysis =====
cat("Performing Generalized Procrustes Analysis...\n")
gpa <- gpagen(coords, print.progress = FALSE)
cat("GPA complete!\n\n")

# ===== STEP 4: Principal Components Analysis =====
cat("Performing PCA...\n")
pca <- plotTangentSpace(gpa$coords, label = TRUE, verbose = TRUE)

# ===== STEP 5: Visualizations =====
cat("\nCreating plots...\n")

# Create output directory
dir.create("analysis/output", showWarnings = FALSE, recursive = TRUE)

# Plot 1: Raw vs Aligned Landmarks
pdf("analysis/output/quick_alignment.pdf", width = 10, height = 5)
par(mfrow = c(1, 2))
plotAllSpecimens(coords, mean = FALSE)
title("Raw Landmarks")
plotAllSpecimens(gpa$coords, mean = TRUE)
title("Aligned Landmarks")
dev.off()

# Plot 2: Shape variation
pdf("analysis/output/quick_shape_variation.pdf", width = 8, height = 8)
consensus <- mshape(gpa$coords)
plotRefToTarget(consensus,
                gpa$coords[, , 1],
                method = "TPS")
title("Shape Deformation (Specimen 1 vs Consensus)")
dev.off()

# ===== STEP 6: Save Results =====
cat("Saving results...\n")

results <- data.frame(
  Specimen = dimnames(coords)[[3]],
  CentroidSize = gpa$Csize,
  PC1 = pca$pc.scores[, 1],
  PC2 = pca$pc.scores[, 2]
)

write.csv(results, "analysis/output/quick_results.csv", row.names = FALSE)

# ===== SUMMARY =====
cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Results saved to analysis/output/\n")
cat("\nPC1 variance:", round(pca$pc.summary$importance[2, 1] * 100, 1), "%\n")
cat("PC2 variance:", round(pca$pc.summary$importance[2, 2] * 100, 1), "%\n")
cat("\nResults table:\n")
print(results)
