# Cuneiform Geometric Morphometrics Analysis Guide

## Introduction

This guide explains how to perform geometric morphometrics (GMM) analysis on your cuneiform landmark data. The analysis quantifies shape variation across different cuneiform signs to understand scribal practices, sign evolution, and paleographic patterns.

## Your Data

### Current Dataset
- **Location**: `data/shapes/`
- **Format**: StereoMorph landmark files (*.txt)
- **Specimens**: 3 cuneiform signs from Ugarit
- **Landmarks**: 21 landmarks per specimen
  - 4 landmark types per wedge: tail_vertex, left_vertex, right_vertex, depth_point
  - Multiple wedges per sign (01-09)

### Landmark Configuration
Your landmarks capture the geometric structure of cuneiform wedges:
- **Tail vertex**: The starting point of each wedge stroke
- **Left vertex**: Left extremity of the wedge head
- **Right vertex**: Right extremity of the wedge head
- **Depth point**: Interior control point for wedge depth

## Analysis Scripts

### 1. Main Analysis Script (`cuneiform_gmm_analysis.R`)

**Purpose**: Comprehensive morphometric analysis with full documentation

**What it does**:
1. Imports landmark data from StereoMorph files
2. Performs Generalized Procrustes Analysis (GPA) to standardize orientation, position, and scale
3. Conducts Principal Components Analysis (PCA) to identify major axes of variation
4. Calculates morphological disparity (shape differences from average)
5. Creates multiple visualizations
6. Exports results in multiple formats

**Run it**:
```r
source("analysis/scripts/cuneiform_gmm_analysis.R")
```

**Outputs** (saved to `analysis/output/`):
- `pca_plot.pdf`: PCA morphospace plot
- `alignment_comparison.pdf`: Before/after Procrustes alignment
- `shape_variation_PC1.pdf`: Thin-plate spline deformation
- `gmm_results.csv`: Numerical results table
- `gmm_workspace.RData`: R workspace for further analysis

### 2. Quick Start Script (`quick_start_gmm.R`)

**Purpose**: Simplified version for beginners or quick analyses

**What it does**:
Same core analysis as above but with less detailed output and simpler code structure.

**Run it**:
```r
source("analysis/scripts/quick_start_gmm.R")
```

## Understanding the Analysis

### Generalized Procrustes Analysis (GPA)

**What it is**: A standardization procedure that removes differences in:
- **Translation**: Centers all shapes at the origin
- **Rotation**: Aligns all shapes to the same orientation
- **Scale**: Standardizes all shapes to unit size

**Why we need it**: To ensure we're comparing shape alone, not position, orientation, or size.

**Output**: Procrustes coordinates (aligned landmarks) and centroid size

### Principal Components Analysis (PCA)

**What it is**: A dimensional reduction technique that finds the main axes of shape variation.

**How to interpret**:
- **PC1** (first component): Captures the largest source of shape variation
- **PC2** (second component): Captures the second largest source (orthogonal to PC1)
- **Variance explained**: Percentage tells you how much variation each PC captures

**Reading the plot**:
- Each point = one cuneiform specimen
- Close points = similar shapes
- Distant points = different shapes
- Position along PC axes = specific shape changes

### Morphological Disparity

**What it is**: The amount of shape variation in your sample

**Procrustes distance**: Measures how different each specimen is from the average (consensus) shape
- Small values = typical shapes
- Large values = unusual or outlier shapes

## Extending the Analysis

### Adding More Specimens

1. Collect landmarks using StereoMorph or another tool
2. Save as .txt files in `data/shapes/`
3. **Important**: Use the same 21 landmarks in the same order
4. Re-run the analysis script

### Adding Metadata for Group Comparisons

Create a metadata file linking specimens to groups:

```csv
Specimen,Period,Archive,Scribe
EP2019-ScribOugaritPaleo_img_3358,Late_Bronze,Ugarit,Unknown
EP2019-ScribOugaritPaleo_img_3366,Late_Bronze,Ugarit,Unknown
EP2019-ScribOugaritPaleo_img_3376,Late_Bronze,Ugarit,Unknown
```

Test for group differences:

```r
# Load metadata
metadata <- read.csv("data/metadata.csv")

# Create geomorph data frame
gdf <- geomorph.data.frame(
  coords = gpa$coords,
  Csize = gpa$Csize,
  Period = metadata$Period,
  Archive = metadata$Archive
)

# Test for shape differences between archives
fit <- procD.lm(coords ~ Archive, data = gdf, print.progress = FALSE)
summary(fit)
```

### Testing for Allometry (Size-Shape Relationship)

Determine if larger signs have systematically different shapes:

```r
# Test relationship between size and shape
allometry <- procD.lm(coords ~ log(Csize),
                     data = gdf,
                     print.progress = FALSE)
summary(allometry)

# Visualize allometry
plot(allometry, type = "regression",
     predictor = log(gdf$Csize))
```

### Comparing Specific Specimens

Compare two specimens directly:

```r
# Compare specimens 1 and 2
plotRefToTarget(gpa$coords[, , 1],
               gpa$coords[, , 2],
               method = "TPS",
               mag = 2)
title("Specimen 1 vs Specimen 2")
```

## Common Research Questions

### 1. "Are there regional differences in sign forms?"

```r
# Requires metadata with Archive/Location column
fit_region <- procD.lm(coords ~ Archive, data = gdf)
summary(fit_region)
```

### 2. "Did sign forms change over time?"

```r
# Requires metadata with Period column
fit_time <- procD.lm(coords ~ Period, data = gdf)
summary(fit_time)
```

### 3. "Can we identify individual scribes by their writing style?"

```r
# Requires metadata with Scribe column
fit_scribe <- procD.lm(coords ~ Scribe, data = gdf)
summary(fit_scribe)

# Classification accuracy
CVA <- CVA(fit_scribe, data = gdf)
```

### 4. "Which landmarks vary the most?"

```r
# Calculate per-landmark variance
landmark_var <- apply(gpa$coords, 1, function(x) {
  var(as.vector(x))
})

# Plot
barplot(landmark_var,
        las = 2,
        main = "Variance by Landmark",
        ylab = "Variance")
```

## Interpretation Guidelines

### For Assyriological Research

1. **Scribal Training**: Less disparity suggests standardized training; high disparity suggests individual variation

2. **Canonical vs. Peripheral**: Compare shapes from Mesopotamian centers with peripheral archives

3. **Temporal Changes**: Track shape evolution across periods (OA → MA → NA)

4. **Scribal Identity**: If you have multiple signs from same scribe, test if they cluster together

### Statistical Significance

- **P-values < 0.05**: Significant difference detected
- **Effect size (Z score)**: Magnitude of difference
- **R-squared**: Proportion of variation explained by the factor

## Troubleshooting

### Problem: "Error: landmarks differ in number"
**Solution**: All specimens must have exactly 21 landmarks. Check your landmark files.

### Problem: "Cannot find package"
**Solution**: Install missing package: `install.packages("package_name")`

### Problem: "Singular matrix error"
**Solution**: You may have too few specimens for the number of landmarks. Need at least 3-4 specimens minimum.

### Problem: "PCA plot looks odd"
**Solution**:
- Check if landmarks are in correct order
- Look for outliers using `plotOutliers(gpa$coords)`
- Verify landmark homology across specimens

## Next Steps

1. **Collect more specimens**: Current sample of 3 is too small for robust conclusions
   - Aim for at least 20-30 specimens for preliminary analysis
   - More is better for statistical power

2. **Add metadata**: Create a metadata file with:
   - Period (OA, MA, NA, etc.)
   - Archive location
   - Scribe ID (if known)
   - Sign type/value
   - Text genre

3. **Expand landmark set**: Consider adding:
   - Additional wedges if complex signs
   - Curve semi-landmarks for wedge outlines
   - 3D landmarks if depth information available

4. **Comparative analysis**: Compare with:
   - Other sign types
   - Other archives
   - Other time periods

## References

### Key Papers on GMM
- Adams, D.C., et al. (2013). Geomorph: Software for geometric morphometric analyses. Methods Ecol. Evol.
- Mitteroecker & Gunz (2009). Advances in Geometric Morphometrics. Evol. Biol. 36:235-247.

### Tutorials Available
- `documentation/tutorials/GMM Tutorial_11.1.2018.pdf` (in your repo)
- `documentation/tutorials/General Geometric Morphometric Workflows.pdf` (in your repo)
- geomorph vignettes: https://cran.r-project.org/package=geomorph

### For Questions
- geomorph manual: Type `?geomorph` in R
- Tutorial PDFs in your `documentation/tutorials/` folder
- Online resources: https://www.sbmorphometrics.org/

## Citation

When publishing analyses using this workflow, cite:

```
Adams, D.C., M.L. Collyer, and A. Kaliontzopoulou. 2023.
geomorph: Software for geometric morphometric analyses.
R package version 4.0.6. https://cran.r-project.org/package=geomorph
```

---

**Last updated**: November 2025
**Author**: Geometric Morphometrics Analysis Pipeline
**Project**: Cuneimorph - Measuring Scribal Literacy Through Quantitative Analysis
