# Batch wrapper: run process_archaeological_image() over a list of URLs.
# Usage:
#   Rscript analysis/scripts/batch_process_images.R <urls_file> [out_root] [--install] [--rows=N] [--cols=N] [--zoom=F]
#
# urls_file: plain text (one URL per line) or CSV with column named "url"
#
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  stop("Usage: Rscript analysis/scripts/batch_process_images.R <urls_file> [out_root] [--install] [--rows=N] [--cols=N] [--zoom=F]")
}

# parse positional and flag args
urls_file <- args[1]
other_args <- args[-1]
install_flag <- any(other_args == "--install")
get_opt <- function(prefix, default) {
  m <- grep(paste0("^", prefix, "="), other_args, value = TRUE)
  if (length(m)) {
    sub(paste0("^", prefix, "="), "", m[[1]])
  } else default
}
grid_rows <- as.integer(get_opt("--rows", 4))
grid_cols <- as.integer(get_opt("--cols", 4))
zoom_factor <- as.numeric(get_opt("--zoom", 2.5))
out_root <- if (length(other_args) && !grepl("^--", other_args[[1]])) other_args[[1]] else file.path("data","raw","images","batch_capture")
out_root <- normalizePath(out_root, mustWork = FALSE)
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

# read URLs (robust + sanitize)
if (!file.exists(urls_file)) stop("URLs file not found: ", urls_file)

# helper: sanitize and validate URL strings
sanitize_url <- function(u) {
  if (is.na(u) || length(u) == 0) return(NA_character_)
  u <- as.character(u)
  u <- trimws(u)
  # remove surrounding single/double quotes repeated
  u <- gsub('^"+|"+$', '', u)
  u <- gsub("^'+|'+$", '', u)
  if (!nzchar(u)) return(NA_character_)
  # replace literal spaces with %20
  u <- gsub(" ", "%20", u, fixed = TRUE)
  # ensure scheme present
  if (!grepl("^[a-zA-Z][a-zA-Z0-9+.-]*://", u)) {
    # try https first since your URLs use https
    u <- paste0("https://", u)
  }
  # attempt to parse; if parse fails try to URL-encode path/query parts
  parsed <- try(httr::parse_url(u), silent = TRUE)
  if (inherits(parsed, "try-error") || is.null(parsed$hostname) || !nzchar(parsed$hostname)) {
    # fallback: encode the whole URL more safely (preserve reserved characters)
    parts <- strsplit(u, "\\?", fixed = FALSE)[[1]]
    base <- utils::URLencode(parts[1], reserved = TRUE)
    if (length(parts) > 1) {
      q <- paste(sapply(parts[-1], function(x) utils::URLencode(x, reserved = TRUE)), collapse = "&")
      u <- paste0(base, "?", q)
    } else {
      u <- base
    }
    parsed2 <- try(httr::parse_url(u), silent = TRUE)
    if (inherits(parsed2, "try-error") || is.null(parsed2$hostname) || !nzchar(parsed2$hostname)) {
      return(NA_character_)
    }
  }
  u
}

urls <- character(0)
# try CSV first (column named url or URL)
df_try <- try(utils::read.csv(urls_file, stringsAsFactors = FALSE, check.names = FALSE), silent = TRUE)
if (!inherits(df_try, "try-error") && ncol(df_try) >= 1) {
  # prefer column named url (case-insensitive), else first column
  cn <- colnames(df_try)
  idx <- which(tolower(cn) == "url")
  if (length(idx) == 1) {
    urls_raw <- df_try[[idx]]
  } else {
    urls_raw <- df_try[[1]]
  }
  urls <- vapply(urls_raw, function(x) sanitize_url(x), FUN.VALUE = character(1), USE.NAMES = FALSE)
} else {
  # plain text list: one URL per line
  lines <- readLines(urls_file, warn = FALSE)
  lines <- lines[trimws(lines) != ""]
  urls <- vapply(lines, function(x) sanitize_url(x), FUN.VALUE = character(1), USE.NAMES = FALSE)
}

# drop any NA / empty results after sanitization
urls <- urls[!is.na(urls) & nzchar(urls)]
if (length(urls) == 0) stop("No valid URLs found in file: ", urls_file)

# optional install (mirror the extractor's required_packages)
required_packages <- c(
  "magick", "imager", "dplyr", "tibble", "jsonlite",
  "httr", "xml2", "stringr", "grid", "ggplot2"
)
if (install_flag) {
  missing_pkgs <- required_packages[!vapply(required_packages, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
  if (length(missing_pkgs)) {
    message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
    install.packages(missing_pkgs)
  }
}

# source extractor (it performs its own checks as well)
source(file.path("analysis","scripts","image_analysis_extractor.r"))

# ensure helper make_safe_filename available; otherwise define fallback
if (!exists("make_safe_filename")) {
  make_safe_filename <- function(x) {
    x <- gsub("[^A-Za-z0-9._-]", "_", x); x <- gsub("_+", "_", x); if (nchar(x)==0) x <- "image"; x
  }
}

results <- list()
summary_rows <- list()

for (u in urls) {
  u <- trimws(u)
  if (!nzchar(u)) next
  # derive folder name: use URL path basename or md5
  parsed <- try(httr::parse_url(u), silent = TRUE)
  if (!inherits(parsed, "try-error") && nzchar(parsed$path)) {
    base <- tools::file_path_sans_ext(basename(parsed$path))
    folder <- make_safe_filename(base)
  } else {
    folder <- substr(digest::digest(u, algo = "md5"), 1, 10)
  }
  out_dir <- file.path(out_root, folder)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  message("Processing: ", u)
  start_time <- Sys.time()
  res <- tryCatch({
    process_archaeological_image(
      image_url = u,
      output_base_dir = out_dir,
      grid_rows = grid_rows,
      grid_cols = grid_cols,
      zoom_factor = zoom_factor,
      extract_grid = TRUE,
      extract_roi = TRUE,
      verbose = FALSE
    )
  }, error = function(e) {
    list(error = TRUE, message = e$message)
  })
  end_time <- Sys.time()

  status <- if (is.list(res) && !isTRUE(res$error)) "ok" else "error"
  msg <- if (is.list(res) && !isTRUE(res$error)) "" else as.character(res$message)

  grid_n <- if (is.list(res) && !is.null(res$grid_metadata)) nrow(res$grid_metadata) else NA_integer_
  roi_n  <- if (is.list(res) && !is.null(res$roi_metadata)) nrow(res$roi_metadata) else NA_integer_

  summary_rows[[length(summary_rows)+1]] <- data.frame(
    url = u,
    out_dir = out_dir,
    status = status,
    message = msg,
    time_start = format(start_time, tz = "UTC"),
    time_end = format(end_time, tz = "UTC"),
    grid_count = grid_n,
    roi_count = roi_n,
    stringsAsFactors = FALSE
  )
}

summary_df <- do.call(rbind, summary_rows)
summary_csv <- file.path(out_root, "batch_summary.csv")
utils::write.csv(summary_df, file = summary_csv, row.names = FALSE, na = "")
message("Batch complete. Summary written to: ", summary_csv)
invisible(list(summary = summary_df, out_root = out_root))