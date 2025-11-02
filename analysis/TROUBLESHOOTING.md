# Troubleshooting Guide for GMM Analysis

## Error: "cannot change working directory"

**Problem:** The script can't find or access the project directory.

**Solutions:**

### Option 1: Run from the correct directory (RECOMMENDED)
```bash
cd /path/to/your/Cuneimorph
Rscript analysis/scripts/quick_start_gmm.R
```

### Option 2: In RStudio
1. Open RStudio
2. File → Open Project
3. Navigate to Cuneimorph folder
4. Open `Cuneimorph.Rproj` (if it exists) or just the folder
5. In the console: `source("analysis/scripts/quick_start_gmm.R")`

### Option 3: Manual path setting
Edit the script and manually set your path at the top:
```r
setwd("/Users/yourname/path/to/Cuneimorph")  # Mac/Linux
# OR
setwd("C:/Users/yourname/path/to/Cuneimorph")  # Windows
```

## Warning: "rgl.init failed, will use the null device"

**Problem:** The rgl package can't initialize 3D graphics.

**Solution:** This is just a warning, not an error! The updated scripts now handle this automatically with `options(rgl.useNULL = TRUE)`.

**Why it happens:**
- Running on a server without X11
- No graphics display available
- Running in terminal without graphics support

**Impact:** None! The analysis will still work perfectly. You just won't see interactive 3D plots (but PDFs will still be generated).

## Error: "cannot open file 'data/shapes/...' "

**Problem:** The data directory isn't where the script expects.

**Check:**
```bash
# Make sure you're in the right directory
pwd  # Should show: /path/to/Cuneimorph

# Check that data exists
ls data/shapes/
# Should show: cuneiform_ba.txt, cuneiform_ku.txt, cuneiform_ri.txt
```

**Solution:**
Ensure your directory structure looks like:
```
Cuneimorph/
├── data/
│   └── shapes/
│       ├── cuneiform_ba.txt
│       ├── cuneiform_ku.txt
│       └── cuneiform_ri.txt
├── analysis/
│   └── scripts/
│       ├── quick_start_gmm.R
│       └── cuneiform_gmm_analysis.R
```

## Error: "there is no package called 'XXX'"

**Problem:** Missing R packages.

**Solution:** Install them:
```r
install.packages(c("StereoMorph", "geomorph", "ggplot2", "dplyr"))
```

If StereoMorph installation fails, you may need:
```r
# Install dependencies first
install.packages(c("rgl", "jpeg", "png"))
# Then retry
install.packages("StereoMorph")
```

## Error: "package 'rstudioapi' is not available"

**Problem:** Running from command line and script tries to use RStudio API.

**Solution:** The updated scripts now handle this automatically with `tryCatch()`. If you're still seeing this, just run from the project root:
```bash
cd /path/to/Cuneimorph
Rscript analysis/scripts/quick_start_gmm.R
```

## Error reading landmark files

**Problem:** StereoMorph can't parse your .txt files.

**Check format:**
```bash
head -5 data/shapes/cuneiform_ba.txt
```

Should look like:
```
LM1 123.45 67.89
LM2 234.56 78.90
...
```

**Fix:** Make sure:
- No extra blank lines at start
- Tab or space-separated values
- Landmark names in first column (or no names)
- X coordinate in second column
- Y coordinate in third column

## The analysis runs but produces no output

**Check:**
```r
# See what directory the script is using
getwd()

# Check if output directory was created
list.files("analysis/output")
```

**Solution:**
```r
# Manually create output directory
dir.create("analysis/output", recursive = TRUE, showWarnings = FALSE)

# Re-run the analysis
source("analysis/scripts/quick_start_gmm.R")
```

## Need More Help?

1. Check that your working directory is correct:
   ```r
   getwd()  # Should end in "Cuneimorph"
   ```

2. Verify your data files exist:
   ```r
   list.files("data/shapes")
   ```

3. Check R package versions:
   ```r
   packageVersion("geomorph")
   packageVersion("StereoMorph")
   ```

4. If all else fails, try the absolute path version (see below).

## Absolute Path Version (Last Resort)

If nothing else works, use this standalone script:

Save as `analysis/scripts/absolute_path_gmm.R`:
```r
# SET YOUR PATH HERE - EDIT THIS LINE!
my_project_path <- "/full/path/to/Cuneimorph"  # CHANGE THIS!

# Go there
setwd(my_project_path)
cat("Working from:", getwd(), "\n\n")

# Now run the analysis
source("analysis/scripts/quick_start_gmm.R")
```

Then run:
```bash
Rscript analysis/scripts/absolute_path_gmm.R
```
