# Cuneiform Sign Visualization Scripts

These scripts create comprehensive visualizations of cuneiform sign shapes, showing how each sign manifests through geometric morphometric analysis.

## Scripts

### 1. `visualize_signs.R` - Basic Shape Visualization
Creates detailed visualizations of individual sign shapes with landmarks and curves.

**What it does:**
- Plots each specimen showing:
  - Fixed landmarks (21 points) in red
  - Curve outlines (semilandmarks) in gray
  - GMM results (PC scores, centroid size) in title
- Creates comparison plots showing all signs together
- Shows Procrustes alignment before/after
- Generates TPS deformation grids showing shape differences

**Usage:**
```bash
Rscript analysis/scripts/visualize_signs.R
```

**Output:** `analysis/output/sign_visualizations/`
- Individual PDF for each specimen
- `all_signs_comparison.pdf` - All signs in one view
- `procrustes_alignment.pdf` - Alignment visualization
- `shape_variation_all.pdf` - TPS deformation grids
- `gmm_results_with_signs.csv` - Results table

---

### 2. `gmm_with_semilandmarks.R` - Advanced GMM with Curves
Performs GMM analysis incorporating both fixed landmarks AND curve semilandmarks.

**What it does:**
- Samples evenly-spaced points from each curve (default: 10 per curve)
- Combines fixed landmarks (21) + semilandmarks (10 × 9 curves = 90) = 111 total landmarks
- Performs Procrustes analysis with sliding semilandmarks
- Runs PCA on the combined landmark configuration
- Compares results with fixed-landmarks-only analysis

**Why use this:**
- **More shape information**: Curves capture outline shape, not just corner points
- **Sliding semilandmarks**: Points slide along curves to minimize bending energy
- **Better shape description**: Captures subtle variations in wedge edges

**Configuration:**
Edit line 30 to change semilandmark density:
```r
N_SEMILANDMARKS_PER_CURVE <- 10  # Increase for more detail
```

**Usage:**
```bash
Rscript analysis/scripts/gmm_with_semilandmarks.R
```

**Output:** `analysis/output/gmm_with_curves/`
- `pca_with_semilandmarks.pdf` - PCA plot
- `alignment_comparison.pdf` - Before/after alignment
- `shape_variation.pdf` - TPS deformations
- `landmark_types.pdf` - Shows fixed (red) vs semi (blue) landmarks
- `gmm_results_full.csv` - Full results
- `comparison.csv` - Comparison with fixed-landmarks-only
- `gmm_workspace.RData` - R workspace for further analysis

---

## Data Structure

Your shape files contain:

### Fixed Landmarks (21 points)
- 3 wedges × 4 points each (tail_vertex, left_vertex, right_vertex, depth_point)
- Plus 9 additional landmark points
- Total: 21 fixed landmarks

### Curves (9 curves)
3 wedges × 3 edges each:
- `top_outer_edge` (top edge of wedge)
- `left_outer_edge` (left edge)
- `right_outer_edge` (right edge)

Each curve has two representations:
- **`curves.control`**: Bezier control points (5 per curve)
- **`curves.pixel`**: Interpolated pixel points (100-600+ per curve)

---

## Interpreting Results

### PC Scores
- **PC1**: Primary axis of shape variation (explains most variance)
- **PC2**: Secondary axis (explains second-most variance)
- Specimens far apart in PC space have different shapes

### Centroid Size
- Overall size measure (scale-invariant after Procrustes)
- Larger values = bigger signs

### TPS Deformation Grids
- Show how shapes differ from consensus mean
- Gridlines show bending/stretching needed to transform one shape to another
- Useful for visualizing which parts of the sign vary most

---

## Workflow Recommendations

### For Quick Visualization:
Use `visualize_signs.R` to see your sign shapes with GMM results overlaid.

### For Standardized Analysis Pipeline:
1. Annotate all signs consistently (same landmarks + curves)
2. Run `visualize_signs.R` to check annotation quality
3. Run `quick_start_gmm.R` for basic fixed-landmark analysis
4. Optionally run `gmm_with_semilandmarks.R` for enhanced analysis

### For Publication:
Use `gmm_with_semilandmarks.R` to incorporate full shape information from curves.

---

## Tips

### Annotation Quality
- Check `landmark_types.pdf` to verify landmarks and curves are correctly positioned
- Look at `all_signs_comparison.pdf` to spot annotation inconsistencies

### Semilandmark Density
- More semilandmarks = more detail but slower analysis
- 5-10 per curve is usually sufficient
- Very smooth curves need fewer points
- Complex curves may need more

### Troubleshooting
- If curves look disconnected: check that curve endpoints match landmark positions
- If Procrustes fails: verify all specimens have same number of curves
- If results look strange: visualize raw data first with `visualize_signs.R`

---

## Next Steps

Once you're satisfied with visualizations:

1. **Standardize annotation protocol** based on these visualizations
2. **Annotate remaining specimens** using the same landmark/curve scheme
3. **Re-run analyses** with complete dataset
4. **Compare across sign types** or time periods
5. **Statistical testing** with `geomorph::procD.lm()` for group comparisons
