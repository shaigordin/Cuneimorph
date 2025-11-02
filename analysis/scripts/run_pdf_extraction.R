# Quick chooser for PDF image extraction examples (robust interactive / non-interactive behavior)

MAIN_SCRIPT <- "/Users/shaigordin/Dropbox/Git-projects/Cuneimorph/analysis/scripts/pdf_image_extractor.r"

# Load only function definitions (skip demo section via env var)
load_functions <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path)
  env <- new.env(parent = globalenv())
  old_val <- Sys.getenv("PDF_EXTRACTOR_LOAD_ONLY", unset = "")
  Sys.setenv(PDF_EXTRACTOR_LOAD_ONLY = "1")
  # Use sys.source so code is evaluated in the target env
  sys.source(path, envir = env)
  # restore previous environment variable state
  if (nzchar(old_val)) {
    Sys.setenv(PDF_EXTRACTOR_LOAD_ONLY = old_val)
  } else {
    Sys.unsetenv("PDF_EXTRACTOR_LOAD_ONLY")
  }
  env
}

# Helpers that behave safely in non-interactive runs
read_default <- function(prompt, default) {
  if (interactive()) {
    ans <- readline(sprintf("%s [%s]: ", prompt, default))
    if (nzchar(ans)) ans else default
  } else {
    # non-interactive: return default
    default
  }
}
read_num_default <- function(prompt, default) {
  if (interactive()) {
    ans <- readline(sprintf("%s [%s]: ", prompt, default))
    val <- suppressWarnings(as.numeric(ans))
    if (!is.na(val)) val else default
  } else {
    default
  }
}

# ## Preflight: ensure packages required by pdf_image_extractor.r are installed
required_packages <- c("pdftools", "magick", "dplyr", "tibble")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop(sprintf(
    "Missing required R packages: %s\nInstall them before running this helper script:\n  install.packages(c(%s))",
    paste(missing_packages, collapse = ", "),
    paste(sprintf('"%s"', missing_packages), collapse = ", ")
  ), call. = FALSE)
}

# Load functions from main script (skipping demos)
env <- load_functions(MAIN_SCRIPT)

# Sanity check: ensure the loaded environment did not run demos
if (exists("RUN_DEMOS", envir = env) && isTRUE(get("RUN_DEMOS", envir = env))) {
  stop("Loader failed: RUN_DEMOS is TRUE in the loaded environment. Aborting to avoid running demos.")
}

# Parse CLI args
args <- commandArgs(trailingOnly = TRUE)
choices <- c("render", "extract", "preserve", "batch")

# If non-interactive and no args -> print usage and exit (avoid auto-default behavior)
if (!interactive() && length(args) == 0) {
  cat("No arguments provided and session is non-interactive.\n")
  cat("Usage examples:\n")
  cat("  Rscript analysis/scripts/run_pdf_extraction_examples.R render /path/to/file.pdf /path/to/out\n")
  cat("  Rscript analysis/scripts/run_pdf_extraction_examples.R extract /path/to/file.pdf /path/to/out\n")
  cat("  Rscript analysis/scripts/run_pdf_extraction_examples.R preserve /path/to/file.pdf /path/to/out\n")
  cat("  Rscript analysis/scripts/run_pdf_extraction_examples.R batch /path/to/pdf_folder /path/to/base_out\n")
  quit(status = 1)
}

# Determine choice: CLI first, else interactive menu
if (length(args) >= 1) {
  choice <- tolower(args[1])
  if (choice %in% as.character(seq_along(choices))) {
    choice <- choices[as.integer(choice)]
  }
} else {
  cat("Select an example to run:\n")
  cat("  1) Render pages (300 DPI)\n")
  cat("  2) Extract embedded images\n")
  cat("  3) Extract embedded images (preserve original DPI)\n")
  cat("  4) Batch processing\n")
  choice <- switch(readline("Enter choice [1-4]: "),
                   "1" = "render",
                   "2" = "extract",
                   "3" = "preserve",
                   "4" = "batch",
                   "render")
}

# Main handlers
if (choice %in% c("render", "extract", "preserve")) {
  # Defaults (edit as needed)
  default_pdf <- "/Users/shaigordin/Dropbox/Git-projects/Cuneimorph/data/raw/images/EP2019-ScribOugaritPaleo.pdf"
  default_out <- "/Users/shaigordin/Dropbox/Git-projects/Cuneimorph/data/raw/images/EP_2019_Uga"
  pdf_path <- if (length(args) >= 2) args[2] else read_default("PDF path", default_pdf)
  out_dir  <- if (length(args) >= 3) args[3] else read_default("Output directory", default_out)
  dpi      <- if (choice == "render" || choice == "extract") read_num_default("DPI", 300) else NA
  text_margin <- read_num_default("Text margin (points)", 15)

  if (!file.exists(pdf_path)) stop("PDF not found: ", pdf_path)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  if (choice == "render") {
    meta <- env$extract_pdf_images(pdf_path, file.path(out_dir, "render"), dpi = dpi,
                                   format = "png", method = "render", text_margin = text_margin)
    if (!is.null(meta) && nrow(meta)) {
      write.csv(meta, file.path(out_dir, "render_metadata.csv"), row.names = FALSE)
      if (is.function(env$display_extraction_summary)) env$display_extraction_summary(meta)
      if (is.function(env$generate_extraction_report)) env$generate_extraction_report(meta, file.path(out_dir, "render_report.txt"))
    }
    cat("Done (render).\n")

  } else if (choice == "extract") {
    if (!env$has_pdfimages()) stop("pdfimages not found. Install Poppler: brew install poppler")
    meta <- env$extract_pdf_images(pdf_path, file.path(out_dir, "extract"), dpi = dpi,
                                   format = "png", method = "extract", text_margin = text_margin)
    if (!is.null(meta) && nrow(meta)) {
      write.csv(meta, file.path(out_dir, "extract_metadata.csv"), row.names = FALSE)
      if (is.function(env$display_extraction_summary)) env$display_extraction_summary(meta)
      if (is.function(env$generate_extraction_report)) env$generate_extraction_report(meta, file.path(out_dir, "extract_report.txt"))
    }
    cat("Done (extract).\n")

  } else if (choice == "preserve") {
    if (!env$has_pdfimages()) stop("pdfimages not found. Install Poppler: brew install poppler")
    meta <- env$extract_embedded_images_preserve_dpi(pdf_path, file.path(out_dir, "extract_preserve_dpi"),
                                                     text_margin = text_margin)
    if (!is.null(meta) && nrow(meta)) {
      write.csv(meta, file.path(out_dir, "extract_preserve_dpi_metadata.csv"), row.names = FALSE)
      if (is.function(env$display_extraction_summary)) env$display_extraction_summary(meta)
      if (is.function(env$generate_extraction_report)) env$generate_extraction_report(meta, file.path(out_dir, "extract_preserve_dpi_report.txt"))
    }
    cat("Done (preserve).\n")
  }

} else if (choice == "batch") {
  default_pdf_dir <- "pdf_documents"
  default_base_out <- "batch_output"
  pdf_dir <- if (length(args) >= 2) args[2] else read_default("PDF folder", default_pdf_dir)
  base_out <- if (length(args) >= 3) args[3] else read_default("Base output directory", default_base_out)
  dpi <- read_num_default("DPI", 300)
  method <- tolower(read_default("Method (render|extract)", "render"))
  if (method == "extract" && !env$has_pdfimages()) stop("pdfimages not found. Install Poppler: brew install poppler")

  results <- env$batch_extract_pdfs(pdf_dir, base_out, dpi = dpi, method = method)
  cat("Batch complete.\n")
} else {
  stop("Unknown choice: ", choice)
}

cat("All done.\n")

# USAGE:
# Interactive:
#   In R Terminal: source("analysis/scripts/run_pdf_extraction_examples.R")
# Non-interactive:
#   Rscript analysis/scripts/run_pdf_extraction_examples.R render /path/to/file.pdf /path/to/out
#   Rscript analysis/scripts/run_pdf_extraction_examples.R extract /path/to/file.pdf /path/to/out
#   Rscript analysis/scripts/run_pdf_extraction_examples.R preserve /path/to/file.pdf /path/to/out
#   Rscript analysis/scripts/run_pdf_extraction_examples.R batch /path/to/pdf_folder /path/to/base_out