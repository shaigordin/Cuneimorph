# ============================================================================
# CUNEIFORM SIGN SHAPE VISUALIZATION
# Visualize individual signs with landmarks, curves, and GMM results
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

# Create output directory
output_dir <- "analysis/output/sign_visualizations"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ============================================================================
# LOAD DATA
# ============================================================================

cat("Loading shape data...\n")
shape_files <- list.files("data/shapes", pattern = "\\.txt$", full.names = TRUE)
shapes_data <- readShapes(shape_files)

# Extract specimen names
specimen_names <- gsub(".txt$", "", basename(shape_files))

# Extract landmarks
if (!is.null(shapes_data$landmarks.pixel) && is.array(shapes_data$landmarks.pixel)) {
  landmarks <- shapes_data$landmarks.pixel
  cat("Loaded", dim(landmarks)[3], "specimens with", dim(landmarks)[1], "landmarks\n")
} else {
  stop("No landmark data found!")
}

# Extract curves
curves_data <- shapes_data$curves.pixel
cat("Loaded curves for", length(curves_data), "specimens\n")
cat("Curves per specimen:", length(curves_data[[1]]), "\n\n")

# ============================================================================
# RUN GMM ANALYSIS
# ============================================================================

cat("Running GMM analysis...\n")

# Procrustes analysis
gpa <- gpagen(landmarks, print.progress = FALSE)

# PCA
pca <- gm.prcomp(gpa$coords)

# Calculate variance explained
var_pc1 <- round((pca$sdev[1]^2 / sum(pca$sdev^2)) * 100, 1)
var_pc2 <- round((pca$sdev[2]^2 / sum(pca$sdev^2)) * 100, 1)

cat("PCA complete: PC1 =", var_pc1, "%, PC2 =", var_pc2, "%\n\n")

# ============================================================================
# VISUALIZATION FUNCTION
# ============================================================================

plot_sign_with_curves <- function(specimen_idx, landmarks, curves, gpa, pca,
                                  specimen_name, show_labels = TRUE) {

  # Get landmark coordinates for this specimen
  lm <- landmarks[, , specimen_idx]

  # Get curve data
  spec_curves <- curves[[specimen_idx]]

  # Set up plot area
  par(mar = c(2, 2, 4, 2))

  # Calculate plot limits based on landmarks and curves
  all_x <- c(lm[, 1], unlist(lapply(spec_curves, function(c) c[, 1])))
  all_y <- c(lm[, 2], unlist(lapply(spec_curves, function(c) c[, 2])))

  xlim <- range(all_x) + c(-50, 50)
  ylim <- range(all_y) + c(-50, 50)

  # Create empty plot
  plot(NULL, xlim = xlim, ylim = ylim, asp = 1,
       xlab = "", ylab = "", axes = FALSE, frame.plot = TRUE)

  # Plot curves (semilandmarks) in light gray
  for (curve_name in names(spec_curves)) {
    curve_pts <- spec_curves[[curve_name]]
    lines(curve_pts[, 1], curve_pts[, 2], col = "gray70", lwd = 2)
  }

  # Plot landmarks
  points(lm[, 1], lm[, 2], pch = 19, col = "red", cex = 1.5)

  # Add landmark labels if requested
  if (show_labels && !is.null(rownames(lm))) {
    text(lm[, 1], lm[, 2], labels = rownames(lm),
         pos = 3, cex = 0.6, col = "darkred")
  }

  # Add title with GMM results
  pc1_score <- round(pca$x[specimen_idx, 1], 3)
  pc2_score <- round(pca$x[specimen_idx, 2], 3)
  csize <- round(gpa$Csize[specimen_idx], 1)

  title(main = paste0(specimen_name, "\n",
                     "PC1: ", pc1_score, " | PC2: ", pc2_score, " | CSize: ", csize),
        cex.main = 0.9)

  # Add legend
  legend("topright", legend = c("Landmarks", "Curves"),
         col = c("red", "gray70"), pch = c(19, NA), lty = c(NA, 1),
         lwd = c(NA, 2), cex = 0.7, bg = "white")
}

# ============================================================================
# CREATE INDIVIDUAL SPECIMEN PLOTS
# ============================================================================

cat("Creating individual sign visualizations...\n")

for (i in 1:dim(landmarks)[3]) {
  pdf(file.path(output_dir, paste0(specimen_names[i], "_shape.pdf")),
      width = 10, height = 10)

  plot_sign_with_curves(i, landmarks, curves_data, gpa, pca,
                       specimen_names[i], show_labels = TRUE)

  dev.off()
  cat("  -", specimen_names[i], "saved\n")
}

# ============================================================================
# CREATE COMPARISON PLOT (ALL SIGNS)
# ============================================================================

cat("\nCreating comparison plot...\n")

pdf(file.path(output_dir, "all_signs_comparison.pdf"),
    width = 15, height = 5 * ceiling(length(specimen_names) / 3))

n_spec <- dim(landmarks)[3]
n_cols <- 3
n_rows <- ceiling(n_spec / n_cols)

par(mfrow = c(n_rows, n_cols))

for (i in 1:n_spec) {
  plot_sign_with_curves(i, landmarks, curves_data, gpa, pca,
                       specimen_names[i], show_labels = FALSE)
}

dev.off()

# ============================================================================
# CREATE PROCRUSTES ALIGNMENT COMPARISON
# ============================================================================

cat("Creating Procrustes alignment comparison...\n")

pdf(file.path(output_dir, "procrustes_alignment.pdf"), width = 15, height = 5)
par(mfrow = c(1, 3))

# Raw landmarks only (no curves for Procrustes plots)
plotAllSpecimens(landmarks, mean = FALSE)
title("Raw Landmarks\n(Original Coordinates)")

plotAllSpecimens(gpa$coords, mean = FALSE)
title("Aligned Landmarks\n(After Procrustes)")

plotAllSpecimens(gpa$coords, mean = TRUE)
title("Aligned Landmarks\n(with Consensus Mean)")

dev.off()

# ============================================================================
# CREATE SHAPE VARIATION PLOTS
# ============================================================================

cat("Creating shape variation plots...\n")

consensus <- mshape(gpa$coords)

pdf(file.path(output_dir, "shape_variation_all.pdf"),
    width = 12, height = 4 * n_spec)

par(mfrow = c(n_spec, 3))

for (i in 1:n_spec) {
  # Plot 1: Consensus shape
  plot(consensus, pch = 19, col = "blue", asp = 1,
       main = paste(specimen_names[i], "- Consensus"),
       xlab = "", ylab = "")

  # Plot 2: Target shape
  plot(gpa$coords[, , i], pch = 19, col = "red", asp = 1,
       main = paste(specimen_names[i], "- Target"),
       xlab = "", ylab = "")

  # Plot 3: TPS deformation
  plotRefToTarget(consensus, gpa$coords[, , i],
                 method = "TPS", mag = 2)
  title(paste(specimen_names[i], "- Deformation"))
}

dev.off()

# ============================================================================
# SAVE GMM RESULTS TABLE
# ============================================================================

cat("Saving GMM results table...\n")

results <- data.frame(
  Specimen = specimen_names,
  CentroidSize = gpa$Csize,
  PC1 = pca$x[, 1],
  PC2 = pca$x[, 2],
  PC1_pct = var_pc1,
  PC2_pct = var_pc2
)

write.csv(results, file.path(output_dir, "gmm_results_with_signs.csv"),
         row.names = FALSE)

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n=== VISUALIZATION COMPLETE ===\n")
cat("Output directory:", output_dir, "\n\n")
cat("Generated files:\n")
cat("  - Individual sign plots:", n_spec, "PDF files\n")
cat("  - all_signs_comparison.pdf: All signs in one view\n")
cat("  - procrustes_alignment.pdf: Before/after alignment\n")
cat("  - shape_variation_all.pdf: TPS deformation grids\n")
cat("  - gmm_results_with_signs.csv: Results table\n\n")

cat("Results summary:\n")
print(results)
