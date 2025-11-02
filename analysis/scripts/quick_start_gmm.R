# ============================================================================
# QUICK START: Cuneiform Geometric Morphometrics
# Simplified analysis script for beginners
# ============================================================================

# Suppress rgl warnings (for headless/non-X11 environments)
options(rgl.useNULL = TRUE)

# Load packages
library(StereoMorph)
library(geomorph)

# Automatically detect and set working directory
script_dir <- tryCatch({
  # Try to get script location if run from RStudio
  dirname(rstudioapi::getActiveDocumentContext()$path)
}, error = function(e) {
  # Otherwise use current working directory
  getwd()
})

# Navigate to project root if we're in the scripts directory
if (basename(script_dir) == "scripts") {
  project_root <- normalizePath(file.path(script_dir, "..", ".."))
  setwd(project_root)
  cat("Working from project root:", getwd(), "\n")
} else {
  cat("Working from:", getwd(), "\n")
  cat("NOTE: If this isn't the Cuneimorph project root, please cd there first\n")
}
cat("\n")

# ===== STEP 1: Read Data =====
cat("Reading landmark data...\n")
shape_files <- list.files("data/shapes", pattern = "\\.txt$", full.names = TRUE)
shapes <- readShapes(shape_files)

# ===== STEP 2: Prepare Array =====
cat("Preparing data array...\n")
# Extract landmarks (use pixel if scaled not available)
if (!is.null(shapes$landmarks.scaled) && length(shapes$landmarks.scaled) > 0) {
  landmarks <- shapes$landmarks.scaled
  cat("Using scaled landmark coordinates\n")
} else if (!is.null(shapes$landmarks.pixel) && length(shapes$landmarks.pixel) > 0) {
  landmarks <- shapes$landmarks.pixel
  cat("Using pixel landmark coordinates (no scaling info available)\n")
} else {
  stop("No landmark data found in shape files!")
}

# Check if landmarks is already an array (which is the case with readShapes output)
if (is.array(landmarks) && length(dim(landmarks)) == 3) {
  # Already in correct format (landmarks x dimensions x specimens)
  coords <- landmarks
  n_lm <- dim(coords)[1]
  n_dim <- dim(coords)[2]
  n_spec <- dim(coords)[3]
  cat("Data already in array format:", n_lm, "landmarks x", n_dim, "dimensions x", n_spec, "specimens\n")

  # Name the specimens
  dimnames(coords)[[3]] <- gsub(".txt", "", basename(shape_files))

} else {
  # landmarks is a list - need to convert to array
  cat("Converting landmark list to array format...\n")

  n_spec <- length(landmarks)

  # Ensure each landmark set is a proper matrix
  landmarks <- lapply(landmarks, function(x) {
    if (!is.matrix(x)) {
      x <- as.matrix(x)
    }
    return(x)
  })

  # Get dimensions from first specimen
  if (n_spec > 0 && !is.null(landmarks[[1]])) {
    n_lm <- nrow(landmarks[[1]])
    n_dim <- ncol(landmarks[[1]])
  } else {
    stop("No valid landmark data found!")
  }

  cat("Array dimensions:", n_lm, "landmarks x", n_dim, "dimensions x", n_spec, "specimens\n")

  # Create array
  coords <- array(NA, dim = c(n_lm, n_dim, n_spec))

  # Fill array
  for (i in 1:n_spec) {
    lm_matrix <- as.matrix(landmarks[[i]])

    # Check dimensions match
    if (nrow(lm_matrix) != n_lm || ncol(lm_matrix) != n_dim) {
      stop(paste("Dimension mismatch for specimen", i,
                 ": expected", n_lm, "x", n_dim,
                 "but got", nrow(lm_matrix), "x", ncol(lm_matrix)))
    }

    coords[, , i] <- lm_matrix
  }

  # Name the specimens
  dimnames(coords)[[3]] <- gsub(".txt", "", basename(shape_files))
}

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
