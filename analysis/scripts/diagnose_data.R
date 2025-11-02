# ============================================================================
# DIAGNOSTIC SCRIPT: Check what's in your landmark files
# Run this to see what StereoMorph is actually reading
# ============================================================================

options(rgl.useNULL = TRUE)
library(StereoMorph)

# Find the project root
script_dir <- getwd()
if (basename(script_dir) == "scripts") {
  setwd(file.path(script_dir, "..", ".."))
}

cat("Working from:", getwd(), "\n\n")

# Find shape files
shape_files <- list.files("data/shapes", pattern = "\\.txt$", full.names = TRUE)
cat("Found", length(shape_files), "shape files:\n")
print(shape_files)
cat("\n")

# Try reading with readShapes
cat("=== Attempting to read with readShapes() ===\n")
shapes <- tryCatch({
  readShapes(shape_files)
}, error = function(e) {
  cat("ERROR:", e$message, "\n")
  return(NULL)
})

if (!is.null(shapes)) {
  cat("\nreadShapes() successful!\n")
  cat("Structure of shapes object:\n")
  print(str(shapes, max.level = 2))

  # Check what's in landmarks
  if ("landmarks.scaled" %in% names(shapes)) {
    cat("\nlandmarks.scaled found!\n")
    cat("Number of specimens:", length(shapes$landmarks.scaled), "\n")
    if (length(shapes$landmarks.scaled) > 0) {
      cat("First specimen landmarks:\n")
      print(head(shapes$landmarks.scaled[[1]]))
    } else {
      cat("WARNING: landmarks.scaled is empty!\n")
    }
  } else {
    cat("\nWARNING: landmarks.scaled not found in shapes object\n")
    cat("Available names:", names(shapes), "\n")
  }

  if ("landmarks.pixel" %in% names(shapes)) {
    cat("\nlandmarks.pixel found!\n")
    if (length(shapes$landmarks.pixel) > 0) {
      cat("First specimen (pixel coords):\n")
      print(head(shapes$landmarks.pixel[[1]]))
    }
  }

  if ("curves.scaled" %in% names(shapes)) {
    cat("\ncurves.scaled found!\n")
  }
}

cat("\n=== Trying to read files directly ===\n")
for (i in 1:min(2, length(shape_files))) {
  cat("\nFile:", basename(shape_files[i]), "\n")

  # Try reading as simple text
  lines <- readLines(shape_files[i], n = 10)
  cat("First 10 lines:\n")
  cat(paste(lines, collapse = "\n"), "\n")

  # Try reading as table
  cat("\nAttempting read.table():\n")
  tryCatch({
    dat <- read.table(shape_files[i], header = TRUE, sep = "\t", fill = TRUE)
    cat("Success! Dimensions:", dim(dat), "\n")
    cat("Column names:", colnames(dat), "\n")
    print(head(dat))
  }, error = function(e) {
    cat("Error:", e$message, "\n")
  })

  cat("\n", paste(rep("-", 60), collapse = ""), "\n")
}

cat("\n=== DIAGNOSIS COMPLETE ===\n")
cat("Look at the output above to see:\n")
cat("1. Whether readShapes() works\n")
cat("2. What's actually in your .txt files\n")
cat("3. Which format we need to use\n")
