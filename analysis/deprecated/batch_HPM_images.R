#!/usr/bin/env Rscript
#
# Wrapper: run capture_HPM_images.js for listing URLs (bildausw2.php or bildpraep.php),
# normalize if needed, then organize resulting raw images under processed/images/<site>/<Inventarnummer>/<image_id>
# and write metadata CSV.
#
# Usage:
#  Rscript analysis/scripts/wrap_capture_and_organize.R --input <URL_or_CSV> --mode url|csv --site SITE_NAME [--col COLNAME] [--raw_root data/raw/images] [--proc_root data/processed/images] [--min_kb 10] [--node_opts "zoomAttempts width height waitMs"]
#
# Examples:
#  Rscript analysis/scripts/wrap_capture_and_organize.R --input "https://.../bildausw2.php?n=549/c&x=..." --mode url --site Hat
#  Rscript analysis/scripts/wrap_capture_and_organize.R --input urls.csv --mode csv --col listing_url --site Hat
#
suppressPackageStartupMessages({
  require(jsonlite)
  require(httr)
  require(magick)
  require(optparse)
})

opt <- OptionParser(option_list = list(
  make_option(c("--input"), type="character", help="Single URL or CSV path"),
  make_option(c("--mode"), type="character", default="url", help="'url' or 'csv'"),
  make_option(c("--col"), type="character", default="url", help="CSV column name containing URLs"),
  make_option(c("--site"), type="character", help="site name (e.g. Hat)"),
  make_option(c("--raw_root"), type="character", default="data/raw/images", help="raw captures root"),
  make_option(c("--proc_root"), type="character", default="data/processed/images", help="processed images root"),
  make_option(c("--min_kb"), type="numeric", default=10, help="min file size to keep (KB)"),
  make_option(c("--node_opts"), type="character", default="", help="extra args for capture_HPM_images.js: zoomAttempts width height waitMs (space-separated)")
))
args <- parse_args(opt)

if (is.null(args$input) || is.null(args$site)) {
  stop("Required: --input and --site")
}

# convenience infix for NULL coalescing (MOVED EARLY so process_entry can use it)
`%||%` <- function(a, b) if (!is.null(a)) a else b

# --- ADD: URL query helpers (must exist before processing entries) ------------
parse_q <- function(url) {
  if (is.null(url) || !nzchar(as.character(url))) return(list())
  p <- tryCatch(httr::parse_url(as.character(url)), error = function(e) NULL)
  if (is.null(p) || is.null(p$query)) return(list())
  lapply(p$query, function(v) if (is.null(v)) NA_character_ else utils::URLdecode(as.character(v)))
}
get_param <- function(url, key) {
  q <- parse_q(url)
  if (!is.null(q[[key]])) return(q[[key]])
  m <- regexec(paste0("[?&]", key, "=([^&?#]+)"), as.character(url))
  rr <- regmatches(as.character(url), m)
  if (length(rr) && length(rr[[1]]) > 1) return(utils::URLdecode(rr[[1]][2]))
  NA_character_
}
# ------------------------------------------------------------------------------

# helpers
safe_mkdir <- function(p) if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)
# SANITIZE: do NOT allow slash (/) in names — replace with underscore
sanitize <- function(x) gsub("[^A-Za-z0-9_\\-]", "_", as.character(x))

# small helper: detect image extension from file header (jpeg/png/gif/svg)
detect_image_ext <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  con <- file(path, "rb"); on.exit(close(con))
  hdr <- readBin(con, "raw", n = 512)
  n <- length(hdr)
  if (n >= 4) {
    sig4 <- paste(sprintf("%02x", as.integer(hdr[1:4])), collapse = "")
    if (startsWith(sig4, "ffd8")) return("jpg")
    if (sig4 == "89504e47") return("png")
    if (sig4 == "47494638") return("gif")
  }
  # check for svg/text
  txt <- tryCatch(rawToChar(hdr), error = function(e) "")
  if (nzchar(txt) && grepl("<svg|<\\?xml", txt, ignore.case = TRUE)) return("svg")
  NA_character_
}

# read inputs
urls <- character(0)
if (tolower(args$mode) == "csv") {
  df <- tryCatch(read.csv(args$input, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) stop("Failed to read CSV: ", e$message))
  if (!(args$col %in% names(df))) stop("Column not found in CSV: ", args$col)
  urls <- na.omit(as.character(df[[args$col]]))
} else {
  urls <- as.character(args$input)
}
urls <- unique(urls)
if (length(urls) == 0) stop("No URLs provided")

site <- sanitize(args$site)
raw_root <- file.path(args$raw_root)    # e.g. data/raw/images
proc_root <- file.path(args$proc_root)  # e.g. data/processed/images
safe_mkdir(raw_root); safe_mkdir(proc_root)
raw_site_dir <- file.path(raw_root, site); safe_mkdir(raw_site_dir)
proc_site_dir <- file.path(proc_root, site); safe_mkdir(proc_site_dir)

# Guard: ensure raw_site_dir is a filesystem path (not a URL) — catch wrong CLI usage early
if (grepl("^https?://", args$raw_root, ignore.case = TRUE)) {
  stop("The --raw_root argument looks like a URL: ", args$raw_root, 
       ". Provide a local path (e.g. data/raw/images).")
}
# normalize raw_site_dir to absolute path
raw_site_dir <- normalizePath(raw_site_dir, mustWork = FALSE)
safe_mkdir(raw_site_dir)

# find node script and normalize path
node_script <- file.path("analysis","scripts","capture_HPM_images.js")
if (!file.exists(node_script)) stop("capture_HPM_images.js not found at analysis/scripts/")
node_script_path <- normalizePath(node_script)

# optional node options split
node_opts <- if (nzchar(args$node_opts)) strsplit(args$node_opts, "\\s+")[[1]] else character(0)

# run capture for each input URL — robust ordering and checks
message("Running capture_HPM_images.js for ", length(urls), " URL(s) into ", raw_site_dir)
for (u in urls) {
  url_arg <- as.character(u)

  # Ensure we always use a local path for out_arg. If raw_site_dir somehow resolved to a URL,
  # create a safe fallback local directory so capture_HPM_images.js does not create URL-like folders.
  out_arg <- raw_site_dir
  if (grepl("^https?://", out_arg, ignore.case = TRUE) || grepl("[/:\\s]", basename(out_arg))) {
    fallback <- file.path(normalizePath(args$raw_root, mustWork = FALSE), paste0(site, "_raw_", format(Sys.time(), "%Y%m%d_%H%M%S")))
    warning("Resolved out_dir looks like a URL or invalid path (", out_arg, "). Using fallback local out_dir: ", fallback)
    out_arg <- fallback
    safe_mkdir(out_arg)
  }

  # basic sanity: url_arg must be http(s)
  if (!grepl("^https?://", url_arg, ignore.case = TRUE)) {
    warning("Skipping input that does not look like an HTTP URL: ", url_arg); next
  }

  # build args: prefer positional args but also export env for JS compatibility
  cmd_args <- c(node_script_path, url_arg, out_arg)
  if (length(node_opts)) cmd_args <- c(cmd_args, node_opts)

  # set env variables too so capture_HPM_images.js works regardless of which interface it expects
  env_vars <- c(paste0("TARGET_URL=", url_arg), paste0("OUT_DIR=", out_arg))

  outf <- tempfile("node_out_"); errf <- tempfile("node_err_")
  message(" -> calling node ", paste(c("node", cmd_args), collapse = " "))
  rc <- system2("node", args = cmd_args, env = env_vars, stdout = outf, stderr = errf)

  if (rc != 0) {
    err_txt <- tryCatch(paste(readLines(errf), collapse = "\n"), error = function(e) "<no stderr>")
    out_txt <- tryCatch(paste(readLines(outf), collapse = "\n"), error = function(e) "<no stdout>")
    warning("capture_HPM_images.js failed (rc=", rc, "). stderr:\n", err_txt, "\nstdout:\n", out_txt)
  } else {
    out_preview <- tryCatch(paste(head(readLines(outf), 40), collapse = "\n"), error = function(e) "")
    if (nzchar(out_preview)) message("node stdout (preview):\n", substr(out_preview, 1, 1200))
  }
}

# After captures: find listing_harvest.json produced by the JS (preferred)
listing_jsons <- list.files(raw_site_dir, pattern = "listing_harvest\\.json$", recursive = TRUE, full.names = TRUE)
if (length(listing_jsons) == 0) {
  listing_jsons <- list.files(raw_site_dir, pattern = "listing_harvest\\.json$", recursive = FALSE, full.names = TRUE)
}
# If still none, then optionally fallback to captured_responses.* (but treat as last resort)
if (length(listing_jsons) == 0) {
  cr_files <- list.files(raw_site_dir, pattern = "captured_responses\\.json$|captured_responses\\.csv$", recursive = TRUE, full.names = TRUE)
  if (length(cr_files)) listing_jsons <- cr_files
}
meta_rows <- list()

process_entry <- function(link, responses, input_listing_url = NA_character_) {
  # link typically is mousepic.php with fundnr (Inventarnummer) and bildnr (image_id)
  if (is.null(link) || !nzchar(link)) return(NULL)
  fund <- get_param(link, "fundnr"); bild <- get_param(link, "bildnr")
  # decode and sanitize — IMPORTANT: remove slashes from folder names
  inv_raw <- if (!is.na(fund) && nzchar(fund)) utils::URLdecode(fund) else NA_character_
  imgid_raw <- if (!is.na(bild) && nzchar(bild)) utils::URLdecode(bild) else NA_character_
  inv <- if (!is.na(inv_raw)) sanitize(inv_raw) else NA_character_
  imgid <- if (!is.na(imgid_raw)) sanitize(imgid_raw) else NA_character_

  # find image response saved_path (jpeg) among responses
  saved <- NA_character_
  if (!is.null(responses) && length(responses)) {
    for (r in responses) {
      url_r <- r$url %||% r$URL %||% (if (!is.null(r[["url"]])) r[["url"]] else NULL)
      sp <- r$saved_path %||% r$savedPath %||% r$saved_path
      ctype <- r$content_type %||% r$contentType %||% ""
      # If saved path is present and exists locally, use it
      if (!is.null(sp) && nzchar(sp)) {
        # if saved path is relative or points into the capture output, try to resolve it
        if (file.exists(sp)) { saved <- sp; break }
        # try relative to the raw_site_dir
        candidate <- file.path(raw_site_dir, sp)
        if (file.exists(candidate)) { saved <- candidate; break }
        # try to find by basename (common case)
        b <- basename(sp)
        found <- list.files(raw_site_dir, pattern = paste0("^", gsub("\\.","\\\\.", b), "$"), recursive = TRUE, full.names = TRUE)
        if (length(found)) { saved <- found[1]; break }
      }
      # fallback: use content_type to prefer image entries
      if (!is.null(ctype) && grepl("image", ctype, ignore.case = TRUE) && !is.null(r$saved_path) && nzchar(r$saved_path)) {
        sp2 <- r$saved_path
        if (file.exists(sp2)) { saved <- sp2; break }
      }
    }
  }

  # If no saved path found in responses, try to find recent jpg/png/svg under raw_site_dir
  if (is.na(saved) || !nzchar(saved) || !file.exists(saved)) {
    jpgs <- list.files(raw_site_dir, pattern="\\.(jpe?g|png|svg|gif|bin)$", recursive=TRUE, full.names=TRUE)
    if (length(jpgs)) {
      # pick the most recent file (likely from this capture run)
      saved <- jpgs[ which.max(file.info(jpgs)$mtime) ]
    }
  }

  if (is.na(saved) || !nzchar(saved) || !file.exists(saved)) {
    warning("No saved image found for link: ", link); return(NULL)
  }

  # If inv/imgid still missing, try to extract from saved filename or link
  if (is.na(inv) || !nzchar(inv)) {
    inv_guess <- get_param(link, "fundnr") %||% get_param(link, "n") %||% basename(dirname(saved))
    inv <- sanitize(utils::URLdecode(ifelse(is.na(inv_guess), "unknown", inv_guess)))
  }
  if (is.na(imgid) || !nzchar(imgid)) {
    img_guess <- get_param(link, "bildnr") %||% sub("\\.[^.]*$", "", basename(saved))
    imgid <- sanitize(utils::URLdecode(ifelse(is.na(img_guess), sub("\\.[^.]*$","",basename(saved)), img_guess)))
  }

  # If saved file has no extension or is .bin, detect and fix extension
  orig_ext <- tolower(tools::file_ext(saved))
  detected <- detect_image_ext(saved)
  if (is.na(detected)) detected <- orig_ext
  if (is.na(detected) || !nzchar(detected)) detected <- "jpg"  # conservative fallback

  # ensure destination dir and extension-correct filename
  dest_dir <- file.path(proc_site_dir, inv, imgid)
  safe_mkdir(dest_dir)
  dest_file <- file.path(dest_dir, paste0(imgid, ".", detected))

  # If the source file extension doesn't match detected extension, copy and set new dest name
  ok <- file.copy(saved, dest_file, overwrite = FALSE)
  if (!ok) { warning("Failed to copy ", saved, " -> ", dest_file); return(NULL) }

  # image info: use image_read_svg for SVGs (better rendering via rsvg) otherwise image_read
  info <- tryCatch({
    ext <- tolower(tools::file_ext(dest_file))
    if (ext == "svg") {
      if (!requireNamespace("rsvg", quietly = TRUE)) stop("Install the 'rsvg' R package (and librsvg system lib) for SVG rendering")
      magick::image_info(magick::image_read_svg(dest_file))
    } else {
      magick::image_info(magick::image_read(dest_file))
    }
  }, error = function(e) { warning("Failed to read image for info: ", e$message); NULL })

  list(
    Inventarnummer = inv,
    image_id = imgid,
    citation = paste0("hethiter.net/: fotarch ", imgid),
    Inventarnummer_url = if (!is.null(input_listing_url) && nzchar(input_listing_url)) input_listing_url else "",
    image_url = link,
    image_path = dest_file,
    image_size_bytes = as.integer(file.info(dest_file)$size),
    width_px = if (!is.null(info)) as.integer(info$width) else NA_integer_,
    height_px = if (!is.null(info)) as.integer(info$height) else NA_integer_,
    dpi_x = if (!is.null(info)) as.numeric(ifelse(is.null(info$density), NA, as.numeric(strsplit(as.character(info$density),"x")[[1]][1]))) else NA_real_,
    dpi_y = if (!is.null(info)) as.numeric(ifelse(is.null(info$density), NA, as.numeric(ifelse(length(strsplit(as.character(info$density),"x")[[1]])>1, strsplit(as.character(info$density),"x")[[1]][2], strsplit(as.character(info$density),"x")[[1]][1])))) else NA_real_,
    width_cm = NA_real_, height_cm = NA_real_,
    capture_time = format(Sys.time(), tz = "UTC")
  )
}

# iterate listing jsons (if none found, try to fall back to captured_responses.json per detail_*)
all_listing_entries <- list()
if (length(listing_jsons) > 0) {
  for (lj in listing_jsons) {
    j <- tryCatch(read_json(lj, simplifyVector = FALSE), error=function(e) {
      warning("Failed to parse listing JSON: ", lj); NULL
    })
    if (is.null(j)) next
    # j is a list of objects {link, responses}
    for (entry in j) {
      link <- entry$link %||% entry$linkurl %||% NA_character_
      responses <- entry$responses %||% entry$responses
      all_listing_entries[[length(all_listing_entries)+1]] <- list(link=link, responses=responses, source=lj)
    }
  }
} else {
  # fallback: search for captured_responses.json under raw_site_dir and use those
  cr_files <- list.files(raw_site_dir, pattern="captured_responses\\.json$|captured_responses\\.csv$", recursive = TRUE, full.names = TRUE)
  for (cf in cr_files) {
    if (grepl("\\.json$", cf, ignore.case = TRUE)) {
      rows <- tryCatch(read_json(cf, simplifyVector = FALSE), error=function(e) NULL)
      if (is.null(rows)) next
      # rows may be list of responses, each containing url and maybe detailUrl
      for (r in rows) {
        # try to find detail/listing link inside r
        link <- r$detailUrl %||% r$detail_url %||% r$page_url %||% r$url %||% NA_character_
        responses <- list(list(url = r$url %||% NA_character_, saved_path = r$saved_path %||% NA_character_, content_type = r$content_type %||% NA_character_))
        all_listing_entries[[length(all_listing_entries)+1]] <- list(link=link, responses=responses, source=cf)
      }
    } else {
      # CSV fallback
      d <- tryCatch(read.csv(cf, stringsAsFactors = FALSE, check.names = FALSE), error=function(e) NULL)
      if (is.null(d)) next
      if ("url" %in% names(d)) {
        for (i in seq_len(nrow(d))) all_listing_entries[[length(all_listing_entries)+1]] <- list(link=d$url[i], responses=NULL, source=cf)
      }
    }
  }
}

# process each entry
for (e in all_listing_entries) {
  entry_meta <- tryCatch(process_entry(e$link, e$responses, input_listing_url = e$source), error = function(ex) { warning("Entry processing failed: ", ex$message); NULL })
  if (!is.null(entry_meta)) meta_rows[[length(meta_rows)+1]] <- entry_meta
}

# write metadata CSV
if (length(meta_rows) == 0) {
  message("No images processed.")
  quit(status = 0)
}

meta_df <- do.call(rbind, lapply(meta_rows, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
# ensure column order requested
cols <- c("Inventarnummer","image_id","citation","Inventarnummer_url","image_url","image_path","image_size_bytes","width_px","height_px","dpi_x","dpi_y","width_cm","height_cm","capture_time")
present <- intersect(cols, names(meta_df))
out_csv <- file.path(proc_site_dir, "captured_images_metadata.csv")
write.csv(meta_df[,present, drop=FALSE], out_csv, row.names = FALSE, na = "")
message("Wrote metadata: ", out_csv)
invisible(out_csv)