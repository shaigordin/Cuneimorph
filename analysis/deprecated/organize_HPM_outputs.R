# Restore/simple organizer: derive Inventarnummer from capture_HPM_images outputs (captured_responses.json/csv).
# Usage:
#   Rscript analysis/scripts/organize_HPM_outputs.R <captures_root> <out_root> [min_kb]
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("Usage: Rscript analysis/scripts/organize_HPM_outputs.R <captures_root> <out_root> [min_kb]")
captures_root <- args[1]; out_root <- args[2]; min_kb <- if (length(args)>=3) as.numeric(args[3]) else 10
stopifnot(dir.exists(captures_root)); dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

require_pkg <- function(pk) if (!requireNamespace(pk, quietly=TRUE)) stop("Install package: ", pk)
require_pkg("jsonlite"); require_pkg("magick"); require_pkg("httr")
trim <- function(x) ifelse(is.na(x), x, gsub("^\\s+|\\s+$","", x))

# helpers
extract_param <- function(url, key) {
  if (is.na(url) || !nzchar(url)) return(NA_character_)
  p <- tryCatch(httr::parse_url(url), error = function(e) NULL)
  if (!is.null(p) && !is.null(p$query) && !is.null(p$query[[key]]) && nzchar(p$query[[key]])) {
    return(utils::URLdecode(as.character(p$query[[key]])))
  }
  m <- regexec(paste0("[?&]", key, "=([^&?#]+)"), url); rr <- regmatches(url, m)
  if (length(rr) && length(rr[[1]])>1) return(utils::URLdecode(rr[[1]][2]))
  NA_character_
}

# single-URL image id extractor: prefer 'bildnr' (bild number), then 'p', then other keys
extract_image_id <- function(url) {
  if (is.na(url) || !nzchar(url)) return(NA_character_)
  # parse and decode query params
  p <- tryCatch(httr::parse_url(url), error = function(e) NULL)
  qval <- function(k) {
    if (!is.null(p) && !is.null(p$query) && !is.null(p$query[[k]]) && nzchar(p$query[[k]])) return(utils::URLdecode(as.character(p$query[[k]])))
    # fallback by regex
    m <- regexec(paste0("[?&]", k, "=([^&?#]+)"), url); rr <- regmatches(url, m)
    if (length(rr) && length(rr[[1]])>1) return(utils::URLdecode(rr[[1]][2]))
    NA_character_
  }
  for (k in c("bildnr","bild","p","id","img","file","name","xy")) {
    v <- qval(k)
    if (!is.na(v) && nzchar(v)) return(gsub("[^A-Za-z0-9_\\-]","_", v))
  }
  # fallback to filename
  bn <- basename(url); bn <- sub("\\?.*$","", bn); bn <- sub("\\.[^.]*$","", bn)
  bn <- gsub("[^A-Za-z0-9_\\-]","_", bn)
  if (nzchar(bn)) return(bn)
  NA_character_
}

# prefer fundnr (mousepic) or n (bildausw2) or decoded fundnr-like values in any captured url
extract_inventarnummer_from_urls <- function(urls) {
  for (u in urls) {
    if (is.na(u) || !nzchar(u)) next
    # prefer fundnr (may be percent-encoded like 806%2Fb)
    p <- tryCatch(httr::parse_url(u), error = function(e) NULL)
    if (!is.null(p) && !is.null(p$query) && !is.null(p$query$fundnr) && nzchar(p$query$fundnr)) {
      return(gsub("[^A-Za-z0-9_\\-\\/]","_", utils::URLdecode(as.character(p$query$fundnr))))
    }
    # direct 'n' parameter on bildausw2.php
    if (!is.null(p) && !is.null(p$query) && !is.null(p$query$n) && nzchar(p$query$n)) {
      return(gsub("[^A-Za-z0-9_\\-\\/]","_", utils::URLdecode(as.character(p$query$n))))
    }
    # try regex fallbacks
    fn <- NA_character_
    m1 <- regexec("[?&]fundnr=([^&?#]+)", u); mm1 <- regmatches(u, m1)
    if (length(mm1) && length(mm1[[1]])>1) fn <- utils::URLdecode(mm1[[1]][2])
    if (!is.na(fn) && nzchar(fn)) return(gsub("[^A-Za-z0-9_\\-\\/]","_", fn))
    m2 <- regexec("[?&]n=([^&?#]+)", u); mm2 <- regmatches(u, m2)
    if (length(mm2) && length(mm2[[1]])>1) {
      return(gsub("[^A-Za-z0-9_\\-\\/]","_", utils::URLdecode(mm2[[1]][2])))
    }
    # sometimes fundnr appears percent-encoded in mousepic links as fundnr=
    # if none found, look for tokens that contain a slash which likely indicate Inventarnummer (e.g. 806/b)
    for (k in c("p","bildnr","bild")) {
      v <- extract_param(u, k)
      if (!is.na(v) && nzchar(v) && grepl("/", v)) return(gsub("[^A-Za-z0-9_\\-\\/]","_", utils::URLdecode(v)))
    }
  }
  NA_character_
}

# helpers
extract_param <- function(url, key) {
  if (is.na(url) || !nzchar(url)) return(NA_character_)
  p <- tryCatch(httr::parse_url(url), error = function(e) NULL)
  if (!is.null(p) && !is.null(p$query) && !is.null(p$query[[key]]) && nzchar(p$query[[key]])) {
    return(utils::URLdecode(as.character(p$query[[key]])))
  }
  m <- regexec(paste0("[?&]", key, "=([^&?#]+)"), url); rr <- regmatches(url, m)
  if (length(rr) && length(rr[[1]])>1) return(utils::URLdecode(rr[[1]][2]))
  NA_character_
}

# ---- new helpers: locate listing URL (bildausw2.php) produced by capture_HPM_images.js ----
find_listing_url_in_json <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  j <- tryCatch(jsonlite::fromJSON(path), error = function(e) NULL)
  if (is.null(j)) return(NA_character_)
  # common keys that may contain the listing page
  candidates <- character(0)
  for (k in c("listingUrl","listing_url","listing","detailUrl","detail_url","detail_page","listingPage","listing_page")) {
    if (!is.null(j[[k]])) {
      if (is.character(j[[k]])) candidates <- c(candidates, as.character(j[[k]]))
      else if (is.list(j[[k]])) candidates <- c(candidates, unlist(j[[k]]))
    }
  }
  candidates <- unique(na.omit(candidates))
  # prefer any that look like bildausw*.php
  b <- candidates[ grepl("bildausw2?\\.php", candidates, ignore.case = TRUE) ]
  if (length(b)) return(b[1])
  # otherwise return any http(s) candidate
  h <- candidates[ grepl("^https?://", candidates, ignore.case = TRUE) ]
  if (length(h)) return(h[1])
  NA_character_
}

# search unit and global captures_root for listing URLs
find_listing_url_for_unit <- function(unit_dir, captures_root) {
  # 1. listing_harvest.json in unit
  lj <- file.path(unit_dir, "listing_harvest.json")
  if (file.exists(lj)) {
    v <- find_listing_url_in_json(lj)
    if (!is.na(v)) return(v)
  }
  # 2. captured_responses.json in unit: may include detail_page or similar
  cr <- file.path(unit_dir, "captured_responses.json")
  if (file.exists(cr)) {
    # parse and look for 'detail_page' or listing-like urls inside entries
    rows <- tryCatch(jsonlite::fromJSON(cr), error = function(e) NULL)
    if (!is.null(rows)) {
      # rows may be list/data.frame with detail_page or page fields
      vals <- character(0)
      if (is.data.frame(rows)) {
        for (nm in c("detail_page","detailUrl","page","page_url","referrer")) if (nm %in% names(rows)) vals <- c(vals, as.character(rows[[nm]]))
        if ("url" %in% names(rows)) vals <- c(vals, as.character(rows$url))
      } else if (is.list(rows)) {
        vals <- unlist(lapply(rows, function(r) {
          if (is.list(r)) {
            for (nm in c("detail_page","detailUrl","page","page_url","referrer")) if (!is.null(r[[nm]])) return(as.character(r[[nm]]))
            if (!is.null(r$url)) return(as.character(r$url))
          }
          NULL
        }))
      }
      vals <- unique(na.omit(vals))
      b <- vals[ grepl("bildausw2?\\.php", vals, ignore.case = TRUE) ]
      if (length(b)) return(b[1])
      h <- vals[ grepl("^https?://", vals, ignore.case = TRUE) ]
      if (length(h)) return(h[1])
    }
  }
  # 3. global search (first matching listing JSON anywhere under captures_root)
  jsons <- list.files(captures_root, pattern = "\\.json$", recursive = TRUE, full.names = TRUE)
  for (jf in jsons) {
    v <- find_listing_url_in_json(jf)
    if (!is.na(v) && grepl("bildausw2?\\.php", v, ignore.case = TRUE)) return(v)
  }
  NA_character_
}

# ---- ADD: extractor for listing 'n' param (Inventarnummer) used by find_listing_url_for_unit ----
extract_inventarnummer_from_listing <- function(url) {
  if (is.na(url) || !nzchar(url)) return(NA_character_)
  p <- tryCatch(httr::parse_url(url), error = function(e) NULL)
  # prefer explicit query param n
  if (!is.null(p) && !is.null(p$query) && !is.null(p$query$n) && nzchar(p$query$n)) {
    return(gsub("[^A-Za-z0-9_\\-\\/]","_", p$query$n))
  }
  # fallback: regex search for n= in the URL string
  m <- regexec("n=([^&?#]+)", url)
  mm <- regmatches(url, m)
  if (length(mm) && length(mm[[1]]) > 1) return(gsub("[^A-Za-z0-9_\\-\\/]","_", mm[[1]][2]))
  NA_character_
}

# find capture units
json_files <- list.files(captures_root, pattern = "captured_responses\\.json$", recursive = TRUE, full.names = TRUE)
csv_files  <- list.files(captures_root, pattern = "captured_responses\\.csv$",  recursive = TRUE, full.names = TRUE)
units <- unique(dirname(c(json_files, csv_files)))
if (length(units) == 0) { message("No capture units found"); quit(status = 0) }

meta_rows <- list(); idx <- 0L
for (unit in units) {
  idx <- idx + 1L
  message(sprintf("[%d/%d] %s", idx, length(units), unit))
  responses_dir <- file.path(unit, "responses"); if (!dir.exists(responses_dir)) responses_dir <- unit
  imgs <- list.files(responses_dir, pattern="\\.(jpg|jpeg|png|webp|tif|tiff)$", full.names=TRUE, ignore.case=TRUE)
  if (length(imgs)==0) { message(" - no images"); next }
  sizes <- file.info(imgs)$size; keeps <- imgs[ which(!is.na(sizes) & sizes > (min_kb*1024)) ]
  if (length(keeps)==0) { message(" - no images > min_kb"); next }
  best <- keeps[ which.max(file.info(keeps)$size) ]; best_size <- file.info(best)$size

  # gather captured URLs from metadata files
  urls <- character(0)
  jpath <- file.path(unit, "captured_responses.json"); cpath <- file.path(unit, "captured_responses.csv")
  if (file.exists(jpath)) {
    rows <- tryCatch(jsonlite::fromJSON(jpath), error = function(e) NULL)
    if (!is.null(rows)) {
      if (is.data.frame(rows) && "url" %in% names(rows)) urls <- c(urls, as.character(rows$url))
      else if (is.list(rows)) urls <- c(urls, as.character(unlist(lapply(rows, function(r) if (is.list(r) && !is.null(r$url)) r$url else if (!is.null(r$URL)) r$URL else NULL))))
    }
  }
  if (file.exists(cpath)) {
    df <- tryCatch(read.csv(cpath, stringsAsFactors=FALSE, check.names=FALSE), error=function(e) NULL)
    if (!is.null(df)) {
      if ("url" %in% names(df)) urls <- c(urls, as.character(df$url)) else urls <- c(urls, as.character(df[[1]]))
    }
  }
  urls <- unique(urls); urls <- urls[!is.na(urls) & nzchar(urls)]
  # also check listing_harvest.json if present
  listing_json <- file.path(unit, "listing_harvest.json")
  if (file.exists(listing_json)) {
    lj <- tryCatch(jsonlite::fromJSON(listing_json), error=function(e) NULL)
    if (!is.null(lj)) {
      if (!is.null(lj$listingUrl)) urls <- c(urls, as.character(lj$listingUrl))
      if (!is.null(lj$detailUrl)) urls <- c(urls, as.character(lj$detailUrl))
    }
  }
  urls <- unique(urls)
  urls <- urls[!is.na(urls) & nzchar(urls)]
  urls <- urls[ !grepl("\\.svg($|\\?)", urls, ignore.case = TRUE) ]
  urls <- urls[ !grepl("infomouse", urls, ignore.case = TRUE) ]

  chosen_url <- if (length(urls)) urls[1] else NA_character_

  # locate listing/detail URL and extract Inventarnummer (prefer capture-produced info)
  listing_url <- find_listing_url_for_unit(unit, captures_root)
  inventarnummer <- NA_character_
  if (!is.na(listing_url) && nzchar(listing_url)) {
    inventarnummer <- extract_inventarnummer_from_listing(listing_url)
  }
  # fallback: try to derive from any captured urls
  if (is.na(inventarnummer) || !nzchar(inventarnummer)) inventarnummer <- extract_inventarnummer_from_urls(urls)
  # final fallback: parent folder name (but this should be rare now)
  if (is.na(inventarnummer) || !nzchar(inventarnummer)) {
    candidate <- basename(dirname(unit))
    inventarnummer <- if (nzchar(candidate)) gsub("[^A-Za-z0-9_\\-\\/]","_", candidate) else "unknown"
  }

  # determine image_id (previously 'fundnr' role): prefer p/bildnr param or captured urls
  image_id <- NA_character_
  for (u in c(chosen_url, urls)) {
    if (is.na(u) || !nzchar(u)) next
    idc <- extract_image_id(u)
    if (!is.na(idc) && nzchar(idc) && nchar(idc) >= 2) { image_id <- idc; break }
  }
  if (is.na(image_id)) image_id <- gsub("[^A-Za-z0-9_\\-]","_", sub("\\.[^.]*$","", basename(best)))
  image_id <- gsub("[^A-Za-z0-9_\\-]", "_", image_id)

  # build mousepic link from chosen_url or urls (prefer mousepic if present, else reconstruct from pixl3/p)
  mousepic <- NA_character_
  mp <- urls[ grepl("mousepic\\.php", urls, ignore.case = TRUE) ]
  if (length(mp)) mousepic <- mp[1]
  else {
    mousepic <- build_mousepic_url(chosen_url, inventarnummer)
  }

  # set Inventarnummer_url in metadata to the resolved listing_url if available
  Inventarnummer_url <- if (!is.na(listing_url) && nzchar(listing_url)) listing_url else if (!is.na(chosen_url) && nzchar(chosen_url)) chosen_url else ""

  # destination
  dest_dir <- file.path(out_root, inventarnummer, image_id)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  ext <- tolower(tools::file_ext(best)); final_path <- file.path(dest_dir, paste0(image_id, ".", ext))
  if (file.exists(final_path)) {
    k <- 1L
    repeat {
      cand <- file.path(dest_dir, paste0(image_id, "_", k, ".", ext))
      if (!file.exists(cand)) { final_path <- cand; break }
      k <- k + 1L
    }
  }
  file.copy(best, final_path, overwrite = FALSE)

  # image info
  info <- tryCatch(magick::image_info(magick::image_read(final_path)), error=function(e) NULL)
  width_px <- if (!is.null(info)) as.integer(info$width) else NA_integer_
  height_px<- if (!is.null(info)) as.integer(info$height) else NA_integer_
  dpi_x <- dpi_y <- NA_real_
  if (!is.null(info) && !is.null(info$density) && nzchar(as.character(info$density))) {
    parts <- strsplit(as.character(info$density),"x")[[1]]; dx <- as.numeric(parts[1]); dy <- ifelse(length(parts)>1, as.numeric(parts[2]), dx)
    if (!is.na(dx)) dpi_x <- dx; if (!is.na(dy)) dpi_y <- dy
  }
  width_cm <- if (!is.na(dpi_x) && dpi_x>0) round(width_px/dpi_x*2.54,2) else NA_real_
  height_cm<- if (!is.na(dpi_y) && dpi_y>0) round(height_px/dpi_y*2.54,2) else NA_real_

  citation <- paste0("hethiter.net/: fotarch ", image_id)
  Inventarnummer_url <- NA_character_
  # prefer any captured listing URL
  lu <- urls[ grepl("bildausw2\\.php|bildausw\\.php", urls, ignore.case = TRUE) ]
  if (length(lu)) Inventarnummer_url <- lu[1]

  meta_rows[[length(meta_rows)+1]] <- data.frame(
    Inventarnummer = inventarnummer,
    image_id = image_id,
    citation = citation,
    Inventarnummer_url = ifelse(!is.na(Inventarnummer_url), Inventarnummer_url, ""),
    image_url = ifelse(!is.na(mousepic) && nzchar(mousepic), mousepic, ""),
    saved_path = final_path,
    file_size_bytes = as.integer(best_size),
    width_px = as.integer(width_px),
    height_px = as.integer(height_px),
    dpi_x = as.numeric(dpi_x),
    dpi_y = as.numeric(dpi_y),
    width_cm = as.numeric(width_cm),
    height_cm = as.numeric(height_cm),
    capture_time = format(Sys.time(), tz="UTC"),
    stringsAsFactors = FALSE
  )
  message(" - kept ", final_path)
}

if (length(meta_rows)==0) { message("No items"); quit(status=0) }
# safe bind
all_names <- unique(unlist(lapply(meta_rows, names)))
aligned <- lapply(meta_rows, function(df) { missing <- setdiff(all_names, names(df)); if (length(missing)) df[missing] <- NA; df[all_names] })
meta_df <- do.call(rbind, aligned)
cols_order <- c("Inventarnummer","image_id","citation","Inventarnummer_url","image_url","saved_path","file_size_bytes","width_px","height_px","dpi_x","dpi_y","width_cm","height_cm","capture_time")
cols_present <- intersect(cols_order, names(meta_df))
write.csv(meta_df[, cols_present, drop=FALSE], file.path(out_root, "captured_images_metadata.csv"), row.names = FALSE, na = "")
message("Wrote metadata: ", file.path(out_root, "captured_images_metadata.csv"))