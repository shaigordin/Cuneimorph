# ============================================================================
# CUNEIFORM GMM ANALYSIS WITH SEMILANDMARKS
# Incorporates both fixed landmarks and curve semilandmarks
# ============================================================================

# Suppress rgl warnings
options(rgl.useNULL = TRUE)

# Load required packages
library(StereoMorph)
library(geomorph)

# ============================================================================
# SETUP
# ============================================================================

# Automatically detect and set working directory
script_dir <- tryCatch({
  dirname(rstudioapi::getActiveDocumentContext()$path)
}, error = function(e) {
  getwd()
})

if (basename(script_dir) == "scripts") {
  project_root <- normalizePath(file.path(script_dir, "..", ".."))
  setwd(project_root)
  cat("Working from project root:", getwd(), "\n\n")
}

# Output directory
output_dir <- "analysis/output/gmm_with_curves"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# CONFIGURATION
# ============================================================================

# Number of semilandmarks to sample from each curve (evenly spaced)
N_SEMILANDMARKS_PER_CURVE <- 10

cat("Configuration:\n")
cat("  - Semilandmarks per curve:", N_SEMILANDMARKS_PER_CURVE, "\n\n")

# ============================================================================
# LOAD DATA
# ============================================================================

cat("Loading shape data...\n")
shape_files <- list.files("data/shapes", pattern = "\\.txt$", full.names = TRUE)
shapes_data <- readShapes(shape_files)

specimen_names <- gsub(".txt$", "", basename(shape_files))

# Extract fixed landmarks
if (!is.null(shapes_data$landmarks.pixel) && is.array(shapes_data$landmarks.pixel)) {
  landmarks_fixed <- shapes_data$landmarks.pixel
  n_fixed <- dim(landmarks_fixed)[1]
  n_spec <- dim(landmarks_fixed)[3]
  cat("Loaded", n_spec, "specimens with", n_fixed, "fixed landmarks\n")
} else {
  stop("No landmark data found!")
}

# Extract curves
curves_data <- shapes_data$curves.pixel
n_curves <- length(curves_data[[1]])
cat("Found", n_curves, "curves per specimen\n\n")

# ============================================================================
# SAMPLE SEMILANDMARKS FROM CURVES
# ============================================================================

cat("Sampling semilandmarks from curves...\n")

# Function to sample n evenly-spaced points from a curve
sample_curve_points <- function(curve_matrix, n_points) {
  n_total <- nrow(curve_matrix)
  if (n_total <= n_points) {
    return(curve_matrix)
  }

  # Calculate indices for evenly spaced points
  indices <- round(seq(1, n_total, length.out = n_points))
  return(curve_matrix[indices, ])
}

# Create combined array: fixed landmarks + semilandmarks
n_semilandmarks <- n_curves * N_SEMILANDMARKS_PER_CURVE
n_total_landmarks <- n_fixed + n_semilandmarks

cat("  - Fixed landmarks:", n_fixed, "\n")
cat("  - Semilandmarks:", n_semilandmarks, "(", N_SEMILANDMARKS_PER_CURVE, "per curve x", n_curves, "curves)\n")
cat("  - Total landmarks:", n_total_landmarks, "\n\n")

# Initialize combined array
combined_coords <- array(NA, dim = c(n_total_landmarks, 2, n_spec))

# Fill array with fixed landmarks and sampled semilandmarks
for (i in 1:n_spec) {
  # Add fixed landmarks
  combined_coords[1:n_fixed, , i] <- landmarks_fixed[, , i]

  # Add semilandmarks from curves
  current_idx <- n_fixed + 1
  for (curve_name in names(curves_data[[i]])) {
    curve_pts <- curves_data[[i]][[curve_name]]
    sampled_pts <- sample_curve_points(curve_pts, N_SEMILANDMARKS_PER_CURVE)

    end_idx <- current_idx + nrow(sampled_pts) - 1
    combined_coords[current_idx:end_idx, , i] <- sampled_pts
    current_idx <- end_idx + 1
  }
}

# Set dimension names
dimnames(combined_coords)[[3]] <- specimen_names

cat("Combined coordinate array created: dim =", dim(combined_coords), "\n\n")

# ============================================================================
# SPECIFY CURVE STRUCTURE FOR GEOMORPH
# ============================================================================

# Tell geomorph which landmarks are semilandmarks on curves
# gpagen expects `curves` as an m x 3 matrix: start_index, end_index, curve_type
# where curve_type is 1 for open curves (or 0 for closed). We'll construct that.
start_indices <- integer(n_curves)
end_indices <- integer(n_curves)

current_idx <- n_fixed + 1
for (curve_idx in 1:n_curves) {
  end_idx <- current_idx + N_SEMILANDMARKS_PER_CURVE - 1
  start_indices[curve_idx] <- current_idx
  end_indices[curve_idx] <- end_idx
  current_idx <- end_idx + 1
}

# Use 1 for open curves (adjust if some curves are closed)
curve_type <- rep(1, n_curves)
curves_matrix <- cbind(start_indices, end_indices, curve_type)

cat("Curve indices for geomorph (start, end, type):\n")
print(curves_matrix)
cat("\n")

# ============================================================================
# GMM ANALYSIS
# ============================================================================

cat("Running Generalized Procrustes Analysis with semilandmarks...\n")

# Procrustes alignment with sliding semilandmarks
# The curves parameter tells gpagen to slide the semilandmarks along the curves
gpa_full <- gpagen(combined_coords, curves = curves_matrix, print.progress = TRUE)

cat("\nGPA complete!\n\n")

# PCA
cat("Performing PCA...\n")
pca_full <- gm.prcomp(gpa_full$coords)

var_explained <- (pca_full$sdev^2 / sum(pca_full$sdev^2))[1:min(5, length(pca_full$sdev))]
cat("\nVariance explained:\n")
for (i in 1:length(var_explained)) {
  cat(sprintf("  PC%d: %.2f%%\n", i, var_explained[i] * 100))
}
cat("\n")

# ============================================================================
# VISUALIZATIONS
# ============================================================================

cat("Creating visualizations...\n")

# Plot 1: PCA (custom scatter with legend)
pdf(file.path(output_dir, "pca_with_semilandmarks.pdf"), width = 10, height = 8)
pcx <- pca_full$x[,1]
pcy <- pca_full$x[,2]
cols <- rep("#2b8cbe", n_spec)
pch_vals <- rep(19, n_spec)
plot(pcx, pcy, pch = pch_vals, col = cols,
  xlab = paste0("PC1 (", round(var_explained[1]*100,1), "%)"),
  ylab = paste0("PC2 (", round(var_explained[2]*100,1), "%)"),
  main = "PCA with Fixed Landmarks + Semilandmarks")
text(pcx, pcy, labels = specimen_names, pos = 3, cex = 0.7)
legend("topright", legend = "Specimens", pch = 19, col = "#2b8cbe", bg = "white")
dev.off()
cat("  - PCA plot saved\n")

# Plot 2: Alignment comparison
pdf(file.path(output_dir, "alignment_comparison.pdf"), width = 15, height = 5)
par(mfrow = c(1, 3))

plotAllSpecimens(combined_coords, mean = FALSE)
title(paste("Raw Coordinates\n(", n_total_landmarks, "total landmarks)"))

plotAllSpecimens(gpa_full$coords, mean = FALSE)
title("Procrustes Aligned")

plotAllSpecimens(gpa_full$coords, mean = TRUE)
title("Aligned with Consensus")

dev.off()
cat("  - Alignment comparison saved\n")

# Plot 3: Shape variation
pdf(file.path(output_dir, "shape_variation.pdf"), width = 12, height = 8)

consensus <- mshape(gpa_full$coords)

par(mfrow = c(2, 2))
for (i in 1:min(n_spec, 4)) {
  plotRefToTarget(consensus, gpa_full$coords[, , i],
                 method = "TPS", mag = 2)
  title(paste("Shape Variation:", specimen_names[i]))
}

dev.off()
cat("  - Shape variation plots saved\n")

# Plot 4: Detailed specimen plot showing fixed vs semi landmarks
pdf(file.path(output_dir, "landmark_types.pdf"), width = 10, height = 10)
par(mfrow = c(2, 2))

for (i in 1:min(n_spec, 4)) {
  coords <- combined_coords[, , i]

  plot(coords, pch = 19, col = "gray", asp = 1,
       main = specimen_names[i], xlab = "", ylab = "")

  # Highlight fixed landmarks in red
  points(coords[1:n_fixed, 1], coords[1:n_fixed, 2],
         pch = 19, col = "red", cex = 1.5)

  # Highlight semilandmarks in blue
  points(coords[(n_fixed+1):n_total_landmarks, 1],
         coords[(n_fixed+1):n_total_landmarks, 2],
         pch = 19, col = "blue", cex = 0.8)

    legend("topright",
      legend = c("Fixed landmarks", "Semilandmarks"),
      col = c("red", "blue"), pch = 19, pt.cex = c(1.4, 0.9), cex = 0.8,
      bg = "white")
}

dev.off()
cat("  - Landmark types plot saved\n")

# ============================================================================
# SAVE RESULTS
# ============================================================================

cat("\nSaving results...\n")

# Results table
results <- data.frame(
  Specimen = specimen_names,
  CentroidSize = gpa_full$Csize,
  PC1 = pca_full$x[, 1],
  PC2 = pca_full$x[, 2],
  PC1_variance = var_explained[1] * 100,
  PC2_variance = var_explained[2] * 100
)

write.csv(results, file.path(output_dir, "gmm_results_full.csv"), row.names = FALSE)

# Save R workspace
save(gpa_full, pca_full, combined_coords, curves_matrix,
     landmarks_fixed, specimen_names,
     file = file.path(output_dir, "gmm_workspace.RData"))

cat("  - Results table saved\n")
cat("  - R workspace saved\n")

# ============================================================================
# COMPARISON WITH FIXED-LANDMARKS-ONLY ANALYSIS
# ============================================================================

cat("\nRunning comparison with fixed landmarks only...\n")

gpa_fixed <- gpagen(landmarks_fixed, print.progress = FALSE)
pca_fixed <- gm.prcomp(gpa_fixed$coords)

var_fixed <- (pca_fixed$sdev^2 / sum(pca_fixed$sdev^2))[1:2]

# Comparison table
comparison <- data.frame(
  Analysis = c("Fixed only", "Fixed + Semilandmarks"),
  N_Landmarks = c(n_fixed, n_total_landmarks),
  PC1_variance = c(var_fixed[1] * 100, var_explained[1] * 100),
  PC2_variance = c(var_fixed[2] * 100, var_explained[2] * 100)
)

cat("\n=== COMPARISON ===\n")
print(comparison)

write.csv(comparison, file.path(output_dir, "comparison.csv"), row.names = FALSE)

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Output directory:", output_dir, "\n\n")

cat("Generated files:\n")
cat("  - pca_with_semilandmarks.pdf\n")
cat("  - alignment_comparison.pdf\n")
cat("  - shape_variation.pdf\n")
cat("  - landmark_types.pdf\n")
cat("  - gmm_results_full.csv\n")
cat("  - comparison.csv\n")
cat("  - gmm_workspace.RData\n\n")

cat("Results:\n")
print(results)
