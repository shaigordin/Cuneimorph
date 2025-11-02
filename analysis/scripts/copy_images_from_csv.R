# Copy images referenced in a metadata CSV into a named folder under data/raw/images
# Usage:
#   Rscript analysis/scripts/copy_images_from_csv.R /path/to/Ug_TAR.csv
# Optional second arg: output images root (defaults to project/data/raw/images or CSV folder if it's already under data/raw/images)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: Rscript copy_images_from_csv.R /path/to/metadata.csv [images_root]")

csv_path <- normalizePath(args[1], mustWork = FALSE)
if (!file.exists(csv_path)) stop("CSV not found: ", csv_path)

# determine images_root: prefer user-specified, else if CSV is inside data/raw/images use that, else project/data/raw/images
if (length(args) >= 2 && nzchar(args[2])) {
  images_root <- args[2]
} else {
  csv_dir <- normalizePath(dirname(csv_path))
  if (grepl("data[/\\\\]raw[/\\\\]images", csv_dir, ignore.case = TRUE)) {
    images_root <- csv_dir
  } else {
    images_root <- file.path(getwd(), "data", "raw", "images")
  }
}
dir.create(images_root, recursive = TRUE, showWarnings = FALSE)

# output folder named after CSV (basename without extension)
out_folder_name <- tools::file_path_sans_ext(basename(csv_path))
out_dir <- file.path(images_root, out_folder_name)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# read CSV robustly with base utils (handles quoted fields)
meta <- utils::read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA", "NULL", "NaN"))

if (!"full_path" %in% colnames(meta)) stop("CSV must contain a 'full_path' column")
# prefer using 'filename' column for target names if present
use_filename_col <- "filename" %in% colnames(meta)

copied <- logical(nrow(meta))
new_full_paths <- character(nrow(meta))

for (i in seq_len(nrow(meta))) {
  src <- meta$full_path[i]
  if (is.na(src) || src == "") {
    copied[i] <- FALSE
    next
  }
  # try absolute first, then relative to CSV dir, then relative to repo root
  candidates <- c(src, file.path(dirname(csv_path), src), file.path(getwd(), src))
  src_found <- NULL
  for (c in candidates) {
    if (file.exists(c)) { src_found <- normalizePath(c); break }
  }
  if (is.null(src_found)) {
    warning(sprintf("Source missing for row %d: %s", i, src)); copied[i] <- FALSE; next
  }
  dest_name <- if (use_filename_col && nzchar(meta$filename[i])) meta$filename[i] else basename(src_found)
  dest_path <- file.path(out_dir, dest_name)

  # if destination already exists and is identical, skip copy; if exists and different, add suffix to avoid overwrite
  if (file.exists(dest_path)) {
    same <- FALSE
    try({ same <- identical(tools::md5sum(dest_path), tools::md5sum(src_found)) }, silent = TRUE)
    if (same) {
      copied[i] <- TRUE
      new_full_paths[i] <- dest_path
      next
    } else {
      # create a unique name
      base <- tools::file_path_sans_ext(dest_name)
      ext <- tools::file_ext(dest_name)
      k <- 1
      repeat {
        candidate <- file.path(out_dir, sprintf("%s_%03d.%s", base, k, ext))
        if (!file.exists(candidate)) { dest_path <- candidate; break }
        k <- k + 1
      }
    }
  }

  ok <- file.copy(src_found, dest_path, copy.mode = TRUE, copy.date = TRUE, overwrite = FALSE)
  if (!ok) {
    warning(sprintf("Failed to copy %s -> %s", src_found, dest_path))
    copied[i] <- FALSE
  } else {
    copied[i] <- TRUE
    new_full_paths[i] <- normalizePath(dest_path)
  }
}

# append new_full_path and copy_status columns and write metadata of copied files
meta$copied <- copied
meta$new_full_path <- new_full_paths

# keep header + only rows that were copied (or keep all rows if you prefer)
copied_meta <- meta[copied, , drop = FALSE]

out_meta_csv <- file.path(out_dir, paste0(out_folder_name, "_metadata.csv"))
utils::write.csv(copied_meta, out_meta_csv, row.names = FALSE, na = "")

cat(sprintf("Copied %d/%d files into: %s\nMetadata for copied files written to: %s\n",
            sum(copied), nrow(meta), out_dir, out_meta_csv))
invisible(list(out_dir = out_dir, metadata = copied_meta))