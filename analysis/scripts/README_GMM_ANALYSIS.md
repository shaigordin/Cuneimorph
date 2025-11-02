# Cuneiform Geometric Morphometrics Analysis

## Overview

This directory contains scripts for performing geometric morphometrics analysis on cuneiform landmark data.

## Main Analysis Script

**File**: `cuneiform_gmm_analysis.R`

This is a comprehensive script that performs a complete geometric morphometrics workflow on your cuneiform sign landmark data.

### What the Script Does

1. **Data Import**: Reads landmark data from StereoMorph format files in `data/shapes/`
2. **Generalized Procrustes Analysis (GPA)**: Removes effects of translation, rotation, and scale
3. **Principal Components Analysis (PCA)**: Identifies major axes of shape variation
4. **Visualization**: Creates multiple plots showing shape variation
5. **Statistics**: Calculates morphological disparity and distances
6. **Results Export**: Saves results in CSV and R formats

### Requirements

Install required R packages:

```r
install.packages(c("StereoMorph", "geomorph", "ggplot2", "dplyr"))
```

### How to Run

#### Option 1: From R Console
```r
source("analysis/scripts/cuneiform_gmm_analysis.R")
```

#### Option 2: From Command Line
```bash
Rscript analysis/scripts/cuneiform_gmm_analysis.R
```

### Output Files

All outputs are saved to `analysis/output/`:

1. **pca_plot.pdf** - Principal components plot showing specimen positions in morphospace
2. **alignment_comparison.pdf** - Before/after Procrustes alignment
3. **shape_variation_PC1.pdf** - Thin-plate spline showing shape changes along PC1
4. **gmm_results.csv** - Tabular results with PC scores and statistics
5. **gmm_workspace.RData** - R workspace for additional analyses

### Understanding the Results

#### PCA Plot
- Each point represents one cuneiform sign specimen
- Distance between points reflects morphological similarity
- PC1 (x-axis) captures the largest axis of shape variation
- PC2 (y-axis) captures the second largest axis

#### Procrustes Distance
- Measures shape difference from the consensus (average) shape
- Larger values indicate more unusual morphology
- Units are in Procrustes units (dimensionless)

#### Centroid Size
- Measure of overall size
- Geometric mean of distances from landmarks to centroid
- Can be used to test for allometry (size-shape relationships)

## Additional Analyses

### Testing for Group Differences

If you have grouping variables (e.g., period, scribe, location), add them to your analysis:

```r
# Example: test for differences between periods
# Assumes you have a metadata file with Period column
metadata <- read.csv("data/metadata.csv")

# Match metadata to specimens
gdf <- geomorph.data.frame(coords = gpa_result$coords,
                          Csize = gpa_result$Csize,
                          Period = metadata$Period)

# Test for shape differences between periods
fit <- procD.lm(coords ~ Period, data = gdf, print.progress = FALSE)
summary(fit)

# Pairwise comparisons
advanced.procD.lm(coords ~ Period,
                 groups = ~ Period,
                 data = gdf)
```

### Allometry Analysis

Test if shape changes with size:

```r
fit_allometry <- procD.lm(coords ~ log(Csize),
                         data = gdf,
                         print.progress = FALSE)
summary(fit_allometry)
```

### Adding More Specimens

To analyze additional specimens:

1. Place new landmark files (*.txt format from StereoMorph) in `data/shapes/`
2. Ensure landmarks are in the same order as existing specimens
3. Re-run the analysis script

## Troubleshooting

### Error: "landmarks differ in number"
- Ensure all specimen files have the same number of landmarks (21 expected)
- Check that no landmarks are missing

### Error: "cannot read file"
- Check that file paths are correct
- Ensure you're running from the project root directory

### Error: "package not found"
- Install missing packages: `install.packages("package_name")`

## Reference Materials

The analysis follows standard geometric morphometrics protocols as described in:

- **Tutorial**: `documentation/tutorials/GMM Tutorial_11.1.2018.pdf`
- **Workflows**: `documentation/tutorials/General Geometric Morphometric Workflows.pdf`
- **geomorph manual**: https://cran.r-project.org/package=geomorph

## Citation

If you use this analysis in publications, please cite:

```
Adams, D.C., M.L. Collyer, and A. Kaliontzopoulou. 2023.
geomorph: Software for geometric morphometric analyses.
R package version 4.0.6.
```

## Contact

For questions about the analysis, refer to the geomorph documentation or the tutorials in `documentation/tutorials/`.
