args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3) stop("Usage: Rscript generate_image_metadata_from_puppeteer.R <input_urls.csv_or_txt> <captured_responses.csv> <out_metadata.csv> [responses_dir]")
in_file <- args[1]
captured_csv <- args[2]
out_meta <- args[3]
responses_dir <- if (length(args) >= 4) args[4] else dirname(captured_csv)

if (!requireNamespace("magick", quietly=TRUE)) stop("Install 'magick': install.packages('magick')")
if (!requireNamespace("utils", quietly=TRUE)) stop("utils required")

read_urls <- function(path) {
  if (!file.exists(path)) stop("URLs file not found: ", path)
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("csv","tsv")) {
    df <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
    if (!is.null(df)) {
      # try to find url column (case-insensitive)
      col <- names(df)[tolower(names(df)) %in% c("url","link","uri")]
      if (length(col) >= 1) return(as.character(df[[col[1]]]))
    }
  }
  # fallback: plain text list
  lines <- readLines(path, warn = FALSE)
  lines <- trimws(lines)
  lines[nzchar(lines)]
}

parse_density <- function(dens) {
  if (is.null(dens) || !nzchar(as.character(dens))) return(c(NA_real_, NA_real_))
  dens <- as.character(dens)
  parts <- strsplit(dens, "x", fixed = TRUE)[[1]]
  dx <- suppressWarnings(as.numeric(parts[1])); dy <- if (length(parts)>1) suppressWarnings(as.numeric(parts[2])) else dx
  if (is.na(dx)) dx <- NA_real_; if (is.na(dy)) dy <- dx
  c(dx, dy)
}

extract_id_from_url <- function(u) {
  if (is.na(u) || !nzchar(u)) return(NA_character_)
  # try bildnr param
  m <- regmatches(u, regexec("[\\?&]bildnr=([^&]+)", u, ignore.case = TRUE))[[1]]
  if (length(m) >= 2 && nzchar(m[2])) return(URLdecode(m[2]))
  # try fundnr, id, img, file, name
  for (k in c("bild","id","img","file","name","fundnr")) {
    m <- regmatches(u, regexec(paste0("[\\?&]", k, "=([^&]+)"), u, ignore.case = TRUE))[[1]]
    if (length(m) >= 2 && nzchar(m[2])) return(URLdecode(m[2]))
  }
  # fallback to last path segment (basename without ext)
  p <- tryCatch(httr::parse_url(u)$path, error = function(e) NA_character_)
  if (!is.na(p) && nzchar(p)) {
    bn <- basename(p)
    bn <- sub("\\.[^\\.]+$", "", bn)
    if (nzchar(bn)) return(bn)
  }
  # last fallback: md5 of url
  substr(digest::digest(u, algo="md5"), 1, 10)
}

# read inputs
urls <- read_urls(in_file)
cap <- if (file.exists(captured_csv)) utils::read.csv(captured_csv, stringsAsFactors = FALSE, check.names = FALSE) else stop("captured_responses.csv not found")
# normalize column names if needed
if (!"url" %in% names(cap) && "URL" %in% names(cap)) names(cap)[names(cap)=="URL"] <- "url"
if (!"saved_path" %in% names(cap) && "saved_path" %in% tolower(names(cap))) names(cap)[tolower(names(cap))=="saved_path"] <- "saved_path"
# ensure saved_path absolute
cap$saved_path <- file.path(responses_dir, basename(cap$saved_path))
cap$saved_path[!file.exists(cap$saved_path)] <- cap$saved_path[!file.exists(cap$saved_path)] # leave as-is if absolute existed

library(magick)
if (!requireNamespace("digest", quietly = TRUE)) install.packages("digest")

# Improved matching heuristics for hethport-like URLs
normalize_url <- function(u) {
  if (is.na(u) || !nzchar(u)) return(NA_character_)
  parsed <- tryCatch(httr::parse_url(u), error = function(e) NULL)
  if (is.null(parsed)) return(u)
  # remove common volatile query params
  if (!is.null(parsed$query) && length(parsed$query)) {
    parsed$query$ts <- NULL
    parsed$query$timestamp <- NULL
    parsed$query$session <- NULL
    parsed$query$sid <- NULL
    # keep identification params like bildnr, fundnr, xy
  }
  # rebuild canonical URL with sorted query keys (for stable comparison)
  base <- paste0(parsed$scheme, "://", parsed$hostname, parsed$path %||% "")
  if (!is.null(parsed$query) && length(parsed$query)) {
    qkeys <- sort(names(parsed$query))
    qpairs <- vapply(qkeys, function(k) paste0(k, "=", parsed$query[[k]]), FUN.VALUE = character(1))
    base <- paste0(base, "?", paste(qpairs, collapse = "&"))
  }
  base
}

extract_identifiers <- function(u) {
  # return a list with bildnr, fundnr, xy, basename
  res <- list(bildnr = NA_character_, fundnr = NA_character_, xy = NA_character_, basename = NA_character_, host = NA_character_)
  if (is.na(u) || !nzchar(u)) return(res)
  parsed <- tryCatch(httr::parse_url(u), error = function(e) NULL)
  if (!is.null(parsed)) {
    res$host <- parsed$hostname
    if (!is.null(parsed$query)) {
      q <- parsed$query
      res$bildnr <- if (!is.null(q$bildnr)) q$bildnr else res$bildnr
      # some keys might be uppercase or slightly different
      if (is.null(res$bildnr)) {
        for (k in names(q)) if (tolower(k) == "bildnr") res$bildnr <- q[[k]]
      }
      res$fundnr <- if (!is.null(q$fundnr)) q$fundnr else res$fundnr
      for (k in names(q)) if (tolower(k) == "fundnr") res$fundnr <- q[[k]]
      res$xy <- if (!is.null(q$xy)) q$xy else res$xy
    }
    if (!is.null(parsed$path)) {
      bn <- basename(parsed$path)
      bn <- sub("\\.[^\\.]+$", "", bn)
      res$basename <- bn
    }
  }
  res
}

score_match <- function(target_url, candidate_url, candidate_saved) {
  # returns numeric score higher = better
  if (is.na(target_url) || !nzchar(target_url)) return(0)
  if (is.na(candidate_url) && is.na(candidate_saved)) return(0)
  tnorm <- normalize_url(target_url)
  cnorm <- normalize_url(if (!is.na(candidate_url)) candidate_url else "")
  score <- 0
  # exact or canonical exact
  if (!is.na(candidate_url) && identical(target_url, candidate_url)) score <- score + 200
  if (!is.na(tnorm) && nzchar(tnorm) && !is.na(cnorm) && identical(tnorm, cnorm)) score <- score + 150
  # identifiers
  tid <- extract_identifiers(target_url)
  cid <- extract_identifiers(candidate_url)
  csaved_id <- extract_identifiers(candidate_saved)
  # match bildnr
  if (!is.na(tid$bildnr) && nzchar(tid$bildnr)) {
    if ((!is.null(cid$bildnr) && identical(tid$bildnr, cid$bildnr)) ||
        (!is.null(csaved_id$basename) && grepl(tid$bildnr, csaved_id$basename, fixed = TRUE))) score <- score + 120
  }
  # match fundnr
  if (!is.na(tid$fundnr) && nzchar(tid$fundnr)) {
    if ((!is.null(cid$fundnr) && identical(tid$fundnr, cid$fundnr)) ||
        (!is.null(csaved_id$basename) && grepl(tid$fundnr, csaved_id$basename, fixed = TRUE))) score <- score + 80
  }
  # match xy token (usually long hex) — very discriminative
  if (!is.na(tid$xy) && nzchar(tid$xy)) {
    if ((!is.null(cid$xy) && identical(tid$xy, cid$xy)) || (!is.null(csaved_id$basename) && grepl(tid$xy, csaved_id$basename, fixed = TRUE))) score <- score + 180
  }
  # basename match
  if (!is.na(tid$basename) && nzchar(tid$basename)) {
    if (!is.null(cid$basename) && nzchar(cid$basename) && identical(tid$basename, cid$basename)) score <- score + 60
    if (!is.null(csaved_id$basename) && nzchar(csaved_id$basename) && identical(tid$basename, csaved_id$basename)) score <- score + 60
  }
  # host match small bonus
  if (!is.na(tid$host) && !is.null(cid$host) && identical(tid$host, cid$host)) score <- score + 10
  # saved_path contains meaningful substring
  if (!is.na(candidate_saved) && nzchar(candidate_saved) && grepl(tid$basename, candidate_saved, fixed = TRUE)) score <- score + 40
  # fuzzy URL similarity (adist normalized)
  if (!is.na(candidate_url) && nzchar(candidate_url)) {
    d <- as.numeric(adist(target_url, candidate_url))
    # normalize by length and convert to score
    len <- max(nchar(target_url), nchar(candidate_url), 1)
    sim_score <- max(0, 50 - round((d / len) * 100))
    score <- score + sim_score
  }
  score
}

find_best_match <- function(target_url, cap_df) {
  scores <- vapply(seq_len(nrow(cap_df)), function(i) {
    candidate_url <- if ("url" %in% names(cap_df)) as.character(cap_df$url[i]) else NA_character_
    candidate_saved <- if ("saved_path" %in% names(cap_df)) as.character(cap_df$saved_path[i]) else NA_character_
    score_match(target_url, candidate_url, candidate_saved)
  }, FUN.VALUE = numeric(1))
  best_i <- which.max(scores)
  if (length(best_i) == 0 || scores[best_i] < 80) return(NA_integer_) # threshold; tune if needed
  best_i
}

# read inputs (unchanged)
# urls <- read_urls(in_file)
# cap <- ...

rows <- list()
for (u in urls) {
  u_trim <- trimws(u)
  match_row <- NA_integer_

  # new robust matcher: try direct exact, canonical, identifier-based, then best-scoring candidate
  # 1) direct exact
  idx_exact <- which(cap$url == u_trim)
  if (length(idx_exact) >= 1) {
    match_row <- idx_exact[1]
  } else {
    # 2) canonicalized comparison
    u_norm <- normalize_url(u_trim)
    if (!is.na(u_norm)) {
      idx_norm <- which(!is.na(cap$url) & vapply(as.character(cap$url), function(x) identical(normalize_url(x), u_norm), FUN.VALUE = logical(1)))
      if (length(idx_norm) >= 1) match_row <- idx_norm[1]
    }
    # 3) identifier-matching (bildnr/fundnr/xy)
    if (is.na(match_row)) {
      tid <- extract_identifiers(u_trim)
      # search for same bildnr or fundnr or xy in url or saved_path
      cand_idx <- integer(0)
      if (!is.na(tid$bildnr) && nzchar(tid$bildnr)) {
        cand_idx <- unique(cand_idx, which(grepl(tid$bildnr, cap$url, fixed = TRUE) | grepl(tid$bildnr, cap$saved_path, fixed = TRUE)))
      }
      if (length(cand_idx) == 0 && !is.na(tid$fundnr) && nzchar(tid$fundnr)) {
        cand_idx <- unique(cand_idx, which(grepl(tid$fundnr, cap$url, fixed = TRUE) | grepl(tid$fundnr, cap$saved_path, fixed = TRUE)))
      }
      if (length(cand_idx) == 0 && !is.na(tid$xy) && nzchar(tid$xy)) {
        cand_idx <- unique(cand_idx, which(grepl(tid$xy, cap$url, fixed = TRUE) | grepl(tid$xy, cap$saved_path, fixed = TRUE)))
      }
      if (length(cand_idx) == 1) match_row <- cand_idx[1]
      if (length(cand_idx) > 1) {
        # choose best among candidates using scoring
        subcap <- cap[cand_idx, , drop = FALSE]
        rel_best <- find_best_match(u_trim, subcap)
        if (!is.na(rel_best)) match_row <- cand_idx[rel_best]
      }
    }
    # 4) fallback: global best scoring across all captured responses
    if (is.na(match_row)) {
      best <- find_best_match(u_trim, cap)
      if (!is.na(best)) match_row <- best
    }
  }

  saved <- if (!is.na(match_row)) cap$saved_path[match_row] else NA_character_
  meta_row <- list(
    source_url = u_trim,
    id = extract_id_from_url(u_trim),
    matched_saved = saved
  )
  if (!is.na(saved) && file.exists(saved)) {
    info <- magick::image_info(magick::image_read(saved))
    dens <- if (!is.null(info$density)) info$density else ""
    dpi <- parse_density(dens)
    dpi_x <- dpi[1]; dpi_y <- dpi[2]
    width_px <- info$width; height_px <- info$height
    width_cm <- if (!is.na(dpi_x) && dpi_x>0) round(width_px / dpi_x * 2.54, 2) else NA_real_
    height_cm <- if (!is.na(dpi_y) && dpi_y>0) round(height_px / dpi_y * 2.54, 2) else NA_real_
    fsize <- file.info(saved)$size
    meta_row <- c(meta_row, list(
      width_px = width_px,
      height_px = height_px,
      dpi_x = dpi_x,
      dpi_y = dpi_y,
      width_cm = width_cm,
      height_cm = height_cm,
      file_size_bytes = if (!is.na(fsize)) fsize else NA_integer_
    ))
  } else {
    meta_row <- c(meta_row, list(
      width_px = NA_integer_, height_px = NA_integer_,
      dpi_x = NA_real_, dpi_y = NA_real_,
      width_cm = NA_real_, height_cm = NA_real_,
      file_size_bytes = NA_integer_
    ))
  }
  rows[[length(rows)+1]] <- as.data.frame(meta_row, stringsAsFactors = FALSE)
}

out_df <- do.call(rbind, rows)
# write CSV
write.csv(out_df, out_meta, row.names = FALSE, na = "")
cat("Wrote metadata to:", out_meta, "\n")
invisible(out_df)