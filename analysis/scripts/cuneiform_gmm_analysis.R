# ============================================================================
# CUNEIFORM GEOMETRIC MORPHOMETRICS ANALYSIS
# Complete workflow for analyzing cuneiform landmark data
# ============================================================================

# Clear workspace
rm(list = ls())

# Suppress rgl warnings (for headless environments)
options(rgl.useNULL = TRUE)

# Load required packages
library(StereoMorph)
library(geomorph)
library(ggplot2)
library(dplyr)

# ============================================================================
# 1. DATA IMPORT
# ============================================================================

cat("Step 1: Importing landmark data...\n")

# Automatically detect project root directory
# This works whether you source the file or run from command line
script_dir <- tryCatch({
  # If sourced in RStudio or R console
  dirname(rstudioapi::getActiveDocumentContext()$path)
}, error = function(e) {
  # If run via Rscript
  getSrcDirectory(function(x) {x})
})

# If still can't detect, use current working directory
if (is.null(script_dir) || script_dir == "") {
  script_dir <- getwd()
  cat("Using current working directory:", script_dir, "\n")
} else {
  cat("Detected script directory:", script_dir, "\n")
}

# Navigate to project root (assuming script is in analysis/scripts/)
project_root <- normalizePath(file.path(script_dir, "..", ".."))
setwd(project_root)
cat("Project root set to:", getwd(), "\n\n")

# Define paths
shapes_dir <- file.path(getwd(), "data", "shapes")
output_dir <- file.path(getwd(), "analysis", "output")

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# List all shape files
shape_files <- list.files(shapes_dir, pattern = "\\.txt$", full.names = TRUE)
cat(paste("Found", length(shape_files), "specimen files\n"))
cat("Files:", basename(shape_files), "\n\n")

# Read all shapes using StereoMorph
shapes_data <- readShapes(shape_files)

# Extract landmark coordinates (scaled or pixel version)
if (!is.null(shapes_data$landmarks.scaled) && length(shapes_data$landmarks.scaled) > 0) {
  landmarks_list <- shapes_data$landmarks.scaled
  cat("Using scaled landmark coordinates\n")
} else if (!is.null(shapes_data$landmarks.pixel) && length(shapes_data$landmarks.pixel) > 0) {
  landmarks_list <- shapes_data$landmarks.pixel
  cat("Using pixel landmark coordinates (no scaling information available)\n")
  cat("Note: Procrustes analysis will remove scale anyway\n")
} else {
  stop("ERROR: No landmark data found in shape files!\n",
       "Check that your .txt files contain <landmarks.pixel> or <landmarks.scaled> sections.")
}

# Check dimensions
cat("Landmarks per specimen:", sapply(landmarks_list, nrow), "\n")
cat("Coordinates per landmark: 2 (X, Y)\n\n")

# ============================================================================
# 2. DATA PREPARATION
# ============================================================================

cat("Step 2: Preparing data for analysis...\n")

# Ensure all landmark sets are proper matrices
landmarks_list <- lapply(landmarks_list, function(x) {
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  return(x)
})

# Determine number of landmarks (should be consistent)
n_landmarks <- nrow(landmarks_list[[1]])
n_dim <- 2  # 2D data (X, Y)
n_specimens <- length(landmarks_list)

# Create specimen names from file names
specimen_names <- basename(shape_files)
specimen_names <- gsub(".txt$", "", specimen_names)

cat(paste("Number of specimens:", n_specimens, "\n"))
cat(paste("Number of landmarks:", n_landmarks, "\n"))
cat(paste("Dimensions:", n_dim, "(2D)\n\n"))

# Convert list to 3D array for geomorph
# Array format: (landmarks x dimensions x specimens)
landmark_array <- array(NA, dim = c(n_landmarks, n_dim, n_specimens),
                       dimnames = list(
                         rownames(landmarks_list[[1]]),  # landmark names
                         c("X", "Y"),                     # coordinate names
                         specimen_names                   # specimen names
                       ))

# Fill the array with error checking
for (i in 1:n_specimens) {
  lm_matrix <- as.matrix(landmarks_list[[i]])

  # Verify dimensions
  if (nrow(lm_matrix) != n_landmarks || ncol(lm_matrix) != n_dim) {
    stop(paste("ERROR: Dimension mismatch for specimen", i, "(",specimen_names[i], ")",
               "\nExpected:", n_landmarks, "landmarks x", n_dim, "dimensions",
               "\nGot:", nrow(lm_matrix), "x", ncol(lm_matrix)))
  }

  landmark_array[, , i] <- lm_matrix
}

cat("Landmark array created successfully\n")
cat("Array dimensions:", dim(landmark_array), "\n\n")

# ============================================================================
# 3. GENERALIZED PROCRUSTES ANALYSIS (GPA)
# ============================================================================

cat("Step 3: Performing Generalized Procrustes Analysis...\n")

# Perform GPA to remove effects of translation, rotation, and scale
gpa_result <- gpagen(landmark_array, print.progress = TRUE)

cat("\nGPA completed successfully\n")
cat("Consensus shape calculated\n")
cat("Centroid sizes:", round(gpa_result$Csize, 2), "\n\n")

# ============================================================================
# 4. PRINCIPAL COMPONENTS ANALYSIS (PCA)
# ============================================================================

cat("Step 4: Performing Principal Components Analysis...\n")

# Perform PCA on Procrustes-aligned coordinates
pca_result <- plotTangentSpace(gpa_result$coords,
                               label = TRUE,
                               verbose = TRUE)

# Extract variance explained
variance_explained <- pca_result$pc.summary$importance[2, 1:min(5, ncol(pca_result$pc.summary$importance))]
cat("\nVariance explained by first PCs:\n")
print(round(variance_explained * 100, 2))

# Save PCA plot
pdf(file.path(output_dir, "pca_plot.pdf"), width = 10, height = 8)
plotTangentSpace(gpa_result$coords,
                label = TRUE,
                verbose = FALSE)
dev.off()
cat("\nPCA plot saved to:", file.path(output_dir, "pca_plot.pdf"), "\n\n")

# ============================================================================
# 5. SHAPE VARIATION VISUALIZATION
# ============================================================================

cat("Step 5: Creating shape variation visualizations...\n")

# Plot all specimens before and after alignment
pdf(file.path(output_dir, "alignment_comparison.pdf"), width = 12, height = 6)
par(mfrow = c(1, 2))

# Before alignment
plotAllSpecimens(landmark_array, mean = FALSE,
                links = NULL)
title("Raw Landmarks")

# After alignment
plotAllSpecimens(gpa_result$coords, mean = TRUE,
                links = NULL)
title("Procrustes-Aligned Landmarks")

dev.off()
cat("Alignment comparison saved\n")

# Visualize shape changes along PC1
pdf(file.path(output_dir, "shape_variation_PC1.pdf"), width = 10, height = 8)
# Get consensus shape
consensus <- mshape(gpa_result$coords)

# Create shapes representing extremes of PC1
pc1_min <- min(pca_result$pc.scores[, 1])
pc1_max <- max(pca_result$pc.scores[, 1])

# Plot shape deformation along PC1
plotRefToTarget(consensus,
                gpa_result$coords[, , which.max(pca_result$pc.scores[, 1])],
                method = "TPS",
                mag = 2)
title("Shape Variation along PC1 (maximum)")

dev.off()
cat("Shape variation plot saved\n\n")

# ============================================================================
# 6. MORPHOLOGICAL DISPARITY
# ============================================================================

cat("Step 6: Calculating morphological disparity...\n")

# Calculate Procrustes distances from consensus
consensus_shape <- mshape(gpa_result$coords)

procrustes_distances <- numeric(n_specimens)
for (i in 1:n_specimens) {
  procrustes_distances[i] <- sqrt(sum((gpa_result$coords[, , i] - consensus_shape)^2))
}

names(procrustes_distances) <- specimen_names

cat("\nProcrustes distances from consensus:\n")
print(round(procrustes_distances, 4))

# Calculate mean shape disparity
mean_disparity <- mean(procrustes_distances)
cat("\nMean morphological disparity:", round(mean_disparity, 4), "\n\n")

# ============================================================================
# 7. SUMMARY STATISTICS AND RESULTS
# ============================================================================

cat("Step 7: Compiling summary statistics...\n")

# Create results data frame
results_df <- data.frame(
  Specimen = specimen_names,
  CentroidSize = gpa_result$Csize,
  PC1 = pca_result$pc.scores[, 1],
  PC2 = pca_result$pc.scores[, 2],
  ProcrustesDistance = procrustes_distances,
  stringsAsFactors = FALSE
)

# Save results
write.csv(results_df,
         file.path(output_dir, "gmm_results.csv"),
         row.names = FALSE)

cat("\nResults saved to:", file.path(output_dir, "gmm_results.csv"), "\n")

# Print summary table
cat("\nSummary Results:\n")
print(results_df)

# ============================================================================
# 8. SAVE R OBJECTS FOR FUTURE ANALYSIS
# ============================================================================

cat("\nStep 8: Saving R workspace...\n")

# Save key objects
save(landmark_array,
     gpa_result,
     pca_result,
     consensus_shape,
     results_df,
     file = file.path(output_dir, "gmm_workspace.RData"))

cat("Workspace saved to:", file.path(output_dir, "gmm_workspace.RData"), "\n")

# ============================================================================
# 9. ANALYSIS SUMMARY
# ============================================================================

cat("\n" , paste(rep("=", 70), collapse = ""), "\n")
cat("GEOMETRIC MORPHOMETRICS ANALYSIS COMPLETED\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("Summary:\n")
cat("--------\n")
cat(paste("Number of specimens analyzed:", n_specimens, "\n"))
cat(paste("Number of landmarks:", n_landmarks, "\n"))
cat(paste("Variance explained by PC1:", round(variance_explained[1] * 100, 2), "%\n"))
cat(paste("Variance explained by PC2:", round(variance_explained[2] * 100, 2), "%\n"))
cat(paste("Mean morphological disparity:", round(mean_disparity, 4), "\n"))
cat("\nOutput files created:\n")
cat("  - pca_plot.pdf\n")
cat("  - alignment_comparison.pdf\n")
cat("  - shape_variation_PC1.pdf\n")
cat("  - gmm_results.csv\n")
cat("  - gmm_workspace.RData\n")
cat("\nAll outputs saved to:", output_dir, "\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
