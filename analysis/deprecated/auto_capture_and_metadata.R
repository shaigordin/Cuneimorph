# Automatic pipeline: discover image links from pages, capture with Puppeteer, keep >10KB, write metadata
# Usage:
#   Rscript analysis/scripts/auto_capture_and_metadata.R <urls_input> <out_root> [csv_url_field] [zoomAttempts] [viewportW] [viewportH] [waitMs]
#
# This patched version integrates with capture_HPM_images.js, organizes images under fundnr directories,
# and writes a metadata CSV with dpi/size fields already present in the script.

# Cleaned header and robust argv-repair for shell-split URLs, then discovery+capture pipeline.

# ---- args parsing and robust repair -------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

# Helper: try to repair argv when a single URL was split into multiple tokens (common when & not quoted)
repair_split_url_args <- function(a) {
  if (length(a) < 2) return(a)
  if (!grepl("^https?://", a[1])) return(a)

  # If first token already looks like a full host (contains a dot in host part), assume ok
  if (grepl("^https?://[^/]+\\.[A-Za-z]{2,}", a[1])) return(a)

  # Try concatenating a[1:k] for increasing k to find a valid-looking URL (contains domain dot)
  for (k in seq(2, length(a))) {
    cand <- paste0(a[1:k], collapse = "")
    if (grepl("^https?://[^/]+\\.[A-Za-z]{2,}", cand)) {
      # Rebuild remaining args
      rem <- if (k + 1 <= length(a)) a[(k+1):length(a)] else character(0)
      return(c(cand, rem))
    }
  }

  # Fallback heuristic: join a[1:(n-1)] with '&' (common if shell removed & separators)
  if (length(a) >= 3 && !grepl("^https?://", a[length(a)])) {
    cand2 <- paste(a[1:(length(a)-1)], collapse = "&")
    return(c(cand2, a[length(a)]))
  }

  a
}

args <- repair_split_url_args(args)

if (length(args) < 2) stop("Usage: Rscript analysis/scripts/auto_capture_and_metadata.R <urls_input> <out_root> [csv_url_field] [zoomAttempts] [viewportW] [viewportH] [waitMs]")

# ---- positional parameters ---------------------------------------------------------
urls_input <- args[1]
out_root   <- args[2]
csv_url_field <- if (length(args) >= 3) args[3] else NULL
zoomAttempts  <- if (length(args) >= 4) as.integer(args[4]) else 1L
viewportW     <- if (length(args) >= 5) as.integer(args[5]) else 1600L
viewportH     <- if (length(args) >= 6) as.integer(args[6]) else 1200L
waitMs        <- if (length(args) >= 7) as.integer(args[7]) else 800L

dir.create(out_root, recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(a,b) if (!is.null(a)) a else b
trim <- function(x) ifelse(is.na(x), x, gsub("^\\s+|\\s+$", "", x))
require_pkg <- function(pk) if (!requireNamespace(pk, quietly = TRUE)) stop("Install package: ", pk)

# ---- helper: control detail discovery wait ----------------------------------------
# choose a longer wait (in seconds) for complex listing pages that load links lazily
detail_wait_seconds <- function(waitMs) {
  # use waitMs as ms; multiply and ensure at least 4s
  max(4L, ceiling(waitMs / 1000 * 3))
}

# ---- helpers ----------------------------------------------------------------------
# read input URLs (file or pasted multi-line string)
read_urls_input <- function(input, csv_field = NULL) {
  if (file.exists(input)) {
    ext <- tolower(tools::file_ext(input))
    if (ext %in% c("csv","tsv")) {
      df <- tryCatch(utils::read.csv(input, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
      if (!is.null(df)) {
        if (!is.null(csv_field) && csv_field %in% names(df)) return(as.character(df[[csv_field]]))
        col <- names(df)[tolower(names(df)) %in% c("url","link","uri","href")]
        if (length(col)) return(as.character(df[[col[1]]]))
        return(as.character(df[[1]]))
      }
    }
    lines <- readLines(input, warn = FALSE)
    return(trim(lines[trim(lines)!=""]))
  }
  # treat as pasted list/string
  parts <- unlist(strsplit(input, "[\\r\\n,;]+"))
  parts <- trim(parts)
  parts[parts != ""]
}

# discover images on page: try Chromote (dynamic) then HTML parse (xml2)
explore_page_for_images <- function(page_url, wait_seconds = 4) {
  imgs <- character(0)

  # Try Chromote for JS-rendered pages (safe close in finally)
  if (requireNamespace("chromote", quietly = TRUE)) {
    b <- NULL; s <- NULL
    tryCatch({
      b <- chromote::Chromote$new()
      s <- b$new_session()
      s$Page$enable(); s$Runtime$enable()
      s$Emulation$setDeviceMetricsOverride(width = 1200L, height = 900L, deviceScaleFactor = 1, mobile = FALSE)
      s$Page$navigate(page_url)
      Sys.sleep(wait_seconds)
      js <- "
        (function(){
          const out = [];
          document.querySelectorAll('img').forEach(i=>{ if (i.src) out.push(i.src); });
          Array.from(document.querySelectorAll('*')).forEach(el=>{
            try {
              const bg = window.getComputedStyle(el).backgroundImage || '';
              const m = bg.match(/url\\(['\\\"]?(.*?)['\\\"]?\\)/);
              if (m && m[1]) out.push(m[1]);
            } catch(e){}
          });
          document.querySelectorAll('a').forEach(a=>{ if (a.href && a.href.match(/\\.(jpg|jpeg|png|tif|tiff|webp|gif)([?#]|$)/i)) out.push(a.href); });
          return Array.from(new Set(out));
        })();
      "
      ev <- s$Runtime$evaluate(js, awaitPromise = TRUE)
      cand <- ev$result$value
      if (length(cand)) imgs <- unique(as.character(cand))
    }, error = function(e) {
      # silent fallback below
      NULL
    }, finally = {
      try(if (!is.null(s)) s$close(), silent = TRUE)
      try(if (!is.null(b)) b$close(), silent = TRUE)
    })
  }

  # Fallback: server-side HTML parse and proper absolutization
  if (length(imgs) == 0) {
    require_pkg("httr"); require_pkg("xml2")
    resp <- tryCatch(httr::GET(page_url, httr::user_agent("R (image-discover)"), httr::timeout(20)), error = function(e) NULL)
    if (!is.null(resp) && httr::status_code(resp) >= 200 && httr::status_code(resp) < 300) {
      html <- httr::content(resp, as = "text", encoding = "UTF-8")
      doc <- tryCatch(xml2::read_html(html), error = function(e) NULL)
      if (!is.null(doc)) {
        srcs  <- xml2::xml_attr(xml2::xml_find_all(doc, ".//img"), "src")
        ahref <- xml2::xml_attr(xml2::xml_find_all(doc, ".//a"), "href")
        candidates <- unique(na.omit(c(srcs, ahref)))
        abs_cands <- vapply(candidates, function(u) {
          if (is.na(u) || u == "") return(NA_character_)
          tryCatch(xml2::url_absolute(u, page_url), error = function(e) NA_character_)
        }, FUN.VALUE = character(1))
        imgs <- unique(c(imgs, abs_cands[!is.na(abs_cands)]))
      }
    }
  }

  imgs <- imgs[!is.na(imgs)]
  imgs <- imgs[ grepl("\\.(jpg|jpeg|png|gif|webp|tif|tiff)([?#]|$)", imgs, ignore.case = TRUE) | grepl("^data:image/", imgs) ]
  unique(imgs)
}

extract_image_id <- function(img_url) {
  if (is.na(img_url) || !nzchar(img_url)) return(NA_character_)
  p <- tryCatch(httr::parse_url(img_url), error = function(e) NULL)
  if (!is.null(p) && !is.null(p$query)) {
    q <- p$query
    for (k in c("bildnr","bild","fundnr","id","img","file","name")) {
      if (!is.null(q[[k]])) return(gsub("[^A-Za-z0-9_-]", "_", q[[k]]))
    }
    if (!is.null(q$xy)) return(gsub("[^A-Za-z0-9_-]", "_", q$xy))
  }
  bn <- if (!is.null(p$path)) basename(p$path) else img_url
  bn <- sub("\\.[^.]*$", "", bn)
  bn <- gsub("[^A-Za-z0-9_-]", "_", bn)
  if (nzchar(bn)) return(bn)
  require_pkg("digest"); substr(digest::digest(img_url, algo="md5"), 1, 10)
}

# new helper: extract fundnr (image no) from a URL (query param 'fundnr' or 'bildnr')
extract_fundnr <- function(url) {
  if (is.na(url) || !nzchar(url)) return(NA_character_)
  p <- tryCatch(httr::parse_url(url), error = function(e) NULL)
  if (!is.null(p) && !is.null(p$query)) {
    q <- p$query
    # prefer fundnr
    if (!is.null(q$fundnr) && nzchar(q$fundnr)) return(gsub("[^A-Za-z0-9_\\-]", "_", q$fundnr))
    if (!is.null(q$bildnr) && nzchar(q$bildnr)) return(gsub("[^A-Za-z0-9_\\-]", "_", q$bildnr))
    # sometimes fundnr may be embedded in other params; try common keys
    for (k in c("bild","id","name","img")) {
      if (!is.null(q[[k]]) && nzchar(q[[k]])) return(gsub("[^A-Za-z0-9_\\-]", "_", q[[k]]))
    }
  }
  # fallback: try to pick pattern from path/filename
  bn <- basename(url)
  bn <- sub("\\?.*$", "", bn)
  bn <- sub("\\.[^.]*$", "", bn)
  bn <- gsub("[^A-Za-z0-9_\\-]", "_", bn)
  if (nzchar(bn)) return(bn)
  NA_character_
}

# new helper: inspect captured_responses metadata for fundnr or bildnr
extract_fundnr_from_responses <- function(img_out_dir) {
  jpath <- file.path(img_out_dir, "captured_responses.json")
  cpath <- file.path(img_out_dir, "captured_responses.csv")
  urls <- character(0)
  if (file.exists(jpath)) {
    rows <- tryCatch(jsonlite::fromJSON(jpath), error = function(e) NULL)
    if (!is.null(rows) && length(rows) > 0) {
      urls <- as.character(unlist(rows$url))
    }
  } else if (file.exists(cpath)) {
    df <- tryCatch(read.csv(cpath, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
    if (!is.null(df) && nrow(df) > 0) {
      if ("url" %in% names(df)) urls <- as.character(df$url)
      else urls <- as.character(df[[1]])
    }
  }
  urls <- urls[!is.na(urls) & nzchar(urls)]
  if (length(urls) == 0) return(NA_character_)
  for (u in urls) {
    fn <- extract_fundnr(u)
    if (!is.na(fn) && nzchar(fn)) return(fn)
  }
  NA_character_
}

# ---- prepare environment ----------------------------------------------------------
# use the HPM-specific node script (capture_HPM_images.js)
node_script <- file.path("analysis","scripts","capture_HPM_images.js")
if (!file.exists(node_script)) stop("Missing node script: ", node_script)
if (!nzchar(Sys.which("node"))) stop("Node.js not found on PATH")

require_pkg("magick"); require_pkg("jsonlite"); require_pkg("httr")

# ---- helpers ----------------------------------------------------------------------
# discover detail pages (e.g. links to mousepic.php) on a listing page
discover_detail_pages <- function(page_url, wait_seconds = 4) {
  detail_links <- character(0)

  # try chromote DOM extraction first; improved: scroll page to trigger lazy-load before collecting links
  if (requireNamespace("chromote", quietly = TRUE)) {
    b <- NULL; s <- NULL
    tryCatch({
      b <- chromote::Chromote$new()
      s <- b$new_session()
      s$Page$enable(); s$Runtime$enable()
      s$Emulation$setDeviceMetricsOverride(width = 1200L, height = 900L, deviceScaleFactor = 1, mobile = FALSE)
      s$Page$navigate(page_url)
      # progressive scroll
      scroll_js <- sprintf("
        (async function(){
          const total = Math.max(document.body.scrollHeight, window.innerHeight);
          const steps = 8;
          const delay = %d / steps;
          for (let i=0;i<steps;i++){
            window.scrollTo(0, Math.floor(total * (i/steps)));
            await new Promise(r=>setTimeout(r, Math.max(50, Math.round(delay))));
          }
          window.scrollTo(0,0);
          return true;
        })();
      ", as.integer(wait_seconds * 1000))
      tryCatch({ s$Runtime$evaluate(scroll_js, awaitPromise = TRUE) }, error = function(e) NULL)
      Sys.sleep(min(2, wait_seconds))
      js <- "
        (function(){
          const out = [];
          document.querySelectorAll('a').forEach(a=>{
            try {
              const href = a.getAttribute('href') || a.href || '';
              if (href && href.toLowerCase().includes('mousepic.php')) out.push(href);
            } catch(e){}
          });
          // also scan onclicks that may produce mousepic links
          Array.from(document.querySelectorAll('[onclick]')).forEach(el=>{
            try {
              const v = el.getAttribute('onclick') || '';
              if (v && v.toLowerCase().includes('mousepic.php')) {
                const m = v.match(/(mousepic\\.php[^'\"\\)\\s]+)/i);
                if (m && m[0]) out.push(m[0]);
              }
            } catch(e){}
          });
          return Array.from(new Set(out));
        })();
      "
      ev <- s$Runtime$evaluate(js, awaitPromise = TRUE)
      cand <- ev$result$value
      if (length(cand)) {
        # absolutize relative links
        detail_links <- vapply(unique(as.character(cand)), function(u) {
          tryCatch(xml2::url_absolute(u, page_url), error = function(e) NA_character_)
        }, FUN.VALUE = character(1))
        detail_links <- unique(detail_links[!is.na(detail_links)])
      }
    }, error = function(e) NULL, finally = {
      try(if (!is.null(s)) s$close(), silent = TRUE)
      try(if (!is.null(b)) b$close(), silent = TRUE)
    })
  }

  # fallback server-side parse: look for anchor hrefs containing mousepic.php
  if (length(detail_links) == 0) {
    require_pkg("httr"); require_pkg("xml2")
    resp <- tryCatch(httr::GET(page_url, httr::user_agent("R (detail-discover)"), httr::timeout(30)), error = function(e) NULL)
    if (!is.null(resp) && httr::status_code(resp) >= 200 && httr::status_code(resp) < 300) {
      html <- httr::content(resp, as = "text", encoding = "UTF-8")
      doc <- tryCatch(xml2::read_html(html), error = function(e) NULL)
      if (!is.null(doc)) {
        ahref <- xml2::xml_attr(xml2::xml_find_all(doc, ".//a"), "href")
        candidates <- unique(na.omit(ahref))
        matches <- candidates[grepl("mousepic\\.php", candidates, ignore.case = TRUE)]
        if (length(matches)) {
          abs_cands <- vapply(matches, function(u) {
            if (is.na(u) || u == "") return(NA_character_)
            tryCatch(xml2::url_absolute(u, page_url), error = function(e) NA_character_)
          }, FUN.VALUE = character(1))
          detail_links <- unique(abs_cands[!is.na(abs_cands)])
        }
      }
    }
  }

  unique(detail_links)
}

# new helper: try to extract an authoritative image id from captured responses metadata (prefers bildnr/fundnr/xy)
resolve_img_id_from_responses <- function(img_out_dir, fallback_id = NA_character_) {
  # possible metadata files created by node script
  jpath <- file.path(img_out_dir, "captured_responses.json")
  cpath <- file.path(img_out_dir, "captured_responses.csv")
  rows <- NULL
  if (file.exists(jpath)) {
    rows <- tryCatch(jsonlite::fromJSON(jpath), error = function(e) NULL)
    if (!is.null(rows) && is.data.frame(rows)) {
      urls <- as.character(rows$url %||% rows$URL %||% rows$req_url %||% rows$request_url)
    } else if (!is.null(rows) && is.list(rows)) {
      urls <- as.character(unlist(rows$url)[!is.na(unlist(rows$url))])
    } else urls <- NULL
  } else if (file.exists(cpath)) {
    df <- tryCatch(read.csv(cpath, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
    if (!is.null(df)) {
      urls <- as.character(df$url %||% df$URL %||% df$request_url %||% df$req_url)
    } else urls <- NULL
  } else urls <- NULL

  if (!is.null(urls) && length(urls) > 0) {
    # prefer any url that contains bildnr, fundnr or xy
    for (u in urls) {
      if (is.na(u) || !nzchar(u)) next
      uid <- extract_image_id(u)
      if (!is.na(uid) && grepl("^[A-Za-z0-9_-]+$", uid) && nchar(uid) >= 3) return(uid)
    }
  }
  fallback_id
}

# ---- main loop --------------------------------------------------------------------
urls <- read_urls_input(urls_input, csv_url_field)
if (length(urls) == 0) stop("No URLs found in input")

meta_rows <- list()
i <- 0L
for (page_url in urls) {
  i <- i + 1L
  page_url <- trim(page_url)
  if (!nzchar(page_url)) next
  message(sprintf("[%d/%d] Explore page: %s", i, length(urls), page_url))

  # 1) Find detail pages (mousepic.php). If found, follow each and run capture there.
  detail_pages <- tryCatch(discover_detail_pages(page_url, wait_seconds = detail_wait_seconds(waitMs)), error = function(e) character(0))

  targets <- character(0)
  mode <- "detail"
  if (length(detail_pages) > 0) {
    message(" - found ", length(detail_pages), " detail pages; will capture from each")
    targets <- detail_pages
  } else {
    # fallback: discover direct image candidates on the page (legacy behavior)
    imgs <- tryCatch(explore_page_for_images(page_url, wait_seconds = ceiling(waitMs/1000)), error = function(e) character(0))
    if (length(imgs) == 0) {
      message(" - no image candidates or detail pages found")
      next
    }
    message(" - found ", length(imgs), " image candidates (no detail pages found)")
    targets <- imgs
    mode <- "image"
  }

  # For each target (either a detail page or an image URL), run the Puppeteer capture.
  for (target_url in targets) {
    # fallback image id based on target url (may be detail page or image direct)
    fallback_img_id <- extract_image_id(target_url)
    img_out_dir <- file.path(out_root, paste0(fallback_img_id, "_tmp"))
    dir.create(img_out_dir, recursive = TRUE, showWarnings = FALSE)

    # per-target logs
    log_out <- file.path(img_out_dir, "node_stdout.log")
    log_err <- file.path(img_out_dir, "node_stderr.log")

    # Node args: pass the out dir and other numeric args; pass URL via env to avoid shell truncation
    node_args <- c(node_script, img_out_dir, as.character(zoomAttempts), as.character(viewportW), as.character(viewportH), as.character(waitMs))
    message(sprintf("   Capturing (%s): %s", mode, target_url))

    # PASS ONLY TARGET_URL (as "NAME=value") to system2 - do NOT include Sys.getenv()
    env_vars <- c(paste0("TARGET_URL=", target_url))

    res_code <- tryCatch({
      status <- system2("node", args = node_args, stdout = log_out, stderr = log_err, env = env_vars)
      if (is.null(status)) 0 else as.integer(status)
    }, error = function(e) {
      message("   node invocation error: ", e$message)
      NA_integer_
    })

    if (is.na(res_code) || res_code != 0) {
      # print node logs to console for debugging
      if (file.exists(log_err)) {
        cat("\n--- node stderr (last 200 lines) ---\n")
        cat(paste(tail(readLines(log_err, warn = FALSE), 200), collapse = "\n"), "\n")
      }
      if (file.exists(log_out)) {
        cat("\n--- node stdout (last 200 lines) ---\n")
        cat(paste(tail(readLines(log_out, warn = FALSE), 200), collapse = "\n"), "\n")
      }
      message("   node capture failed (exit ", res_code, "), see logs: ", log_out, " ", log_err)
      next
    }

    # After capture, attempt to resolve authoritative image id from the captured responses metadata
    resolved_id <- resolve_img_id_from_responses(img_out_dir, fallback_img_id)
    img_id <- if (!is.na(resolved_id) && nzchar(resolved_id)) resolved_id else fallback_img_id

    # extract fundnr (image no) from responses, fallback to parsing the target URL
    fundnr <- extract_fundnr_from_responses(img_out_dir)
    if (is.na(fundnr) || !nzchar(fundnr)) fundnr <- extract_fundnr(target_url)
    if (is.na(fundnr) || !nzchar(fundnr)) fundnr <- "unknown"

    # ensure responses directory exists
    responses_dir <- file.path(img_out_dir, "responses")
    if (!dir.exists(responses_dir)) responses_dir <- img_out_dir
    saved_files <- list.files(responses_dir, pattern = "resp_.*\\.(jpg|jpeg|png|webp|tif|tiff)$", full.names = TRUE, ignore.case = TRUE)
    if (length(saved_files) == 0) {
      saved_files <- list.files(responses_dir, pattern = "\\.(jpg|jpeg|png|webp|tif|tiff)$", full.names = TRUE, ignore.case = TRUE)
    }
    if (length(saved_files) == 0) { message("   no image responses saved"); next }

    # keep only >10KB
    file_sizes <- file.info(saved_files)$size
    keep_idx <- which(!is.na(file_sizes) & file_sizes > 10*1024)
    keep <- saved_files[keep_idx]

    # remove small files (files only)
    small <- setdiff(saved_files, keep)
    if (length(small)) {
      small_files <- small[!file.info(small)$isdir]
      if (length(small_files)) file.remove(small_files)
    }
    if (length(keep) == 0) { message("   all images <=10KB (discarded)"); next }

    # choose largest kept file
    ksizes <- file.info(keep)$size
    best <- keep[which.max(ksizes)]

    # organize under fundnr directory
    fund_dir <- file.path(out_root, fundnr)
    dir.create(fund_dir, recursive = TRUE, showWarnings = FALSE)

    # unique final name using resolved img_id under fundnr dir
    ext <- tolower(tools::file_ext(best))
    base_final <- file.path(fund_dir, img_id)
    final_path <- paste0(base_final, ".", ext)
    if (file.exists(final_path)) {
      k <- 1L
      repeat {
        candidate <- paste0(base_final, "_", k, ".", ext)
        if (!file.exists(candidate)) { final_path <- candidate; break }
        k <- k + 1L
      }
    }

    file.copy(best, final_path, overwrite = FALSE)

    # metadata
    info <- tryCatch(magick::image_info(magick::image_read(final_path)), error = function(e) NULL)
    width_px <- height_px <- NA_integer_
    dpi_x <- dpi_y <- NA_real_
    width_cm <- height_cm <- NA_real_
    if (!is.null(info)) {
      width_px <- info$width; height_px <- info$height
      dens <- if (!is.null(info$density)) info$density else ""
      if (nzchar(dens)) {
        parts <- strsplit(as.character(dens), "x")[[1]]
        dx <- suppressWarnings(as.numeric(parts[1])); dy <- if (length(parts)>1) suppressWarnings(as.numeric(parts[2])) else dx
        dpi_x <- ifelse(is.na(dx), NA_real_, dx); dpi_y <- ifelse(is.na(dy), dpi_x, dy)
      }
      if (!is.na(dpi_x) && dpi_x>0) width_cm <- round(width_px / dpi_x * 2.54, 2)
      if (!is.na(dpi_y) && dpi_y>0) height_cm <- round(height_px / dpi_y * 2.54, 2)
    }
    fsize <- file.info(final_path)$size
    meta_rows[[length(meta_rows)+1]] <- data.frame(
      image_id = img_id,
      fundnr = fundnr,
      page_url = page_url,
      image_url = target_url,
      saved_path = final_path,
      file_size_bytes = as.integer(fsize),
      width_px = as.integer(width_px),
      height_px = as.integer(height_px),
      dpi_x = as.numeric(dpi_x),
      dpi_y = as.numeric(dpi_y),
      width_cm = as.numeric(width_cm),
      height_cm = as.numeric(height_cm),
      capture_time = format(Sys.time(), tz = "UTC"),
      stringsAsFactors = FALSE
    )
    message("   kept ", final_path, " (", round(fsize/1024,1), " KB)")

    # cleanup temporary files but keep logs & final file
    try({
      tmp <- list.files(img_out_dir, full.names = TRUE, recursive = TRUE)
      keep_paths <- c(final_path, log_out, log_err, file.path(img_out_dir, "captured_responses.json"), file.path(img_out_dir, "captured_responses.csv"))
      rem <- setdiff(tmp, keep_paths)
      rem_files <- rem[!file.info(rem)$isdir]
      if (length(rem_files)) file.remove(rem_files)
    }, silent = TRUE)
  }
}

if (length(meta_rows) == 0) {
  message("No images captured.")
  quit(status = 0)
}
meta_df <- do.call(rbind, meta_rows)
# ensure column order
cols_order <- c("image_id","fundnr","page_url","image_url","saved_path","file_size_bytes","width_px","height_px","dpi_x","dpi_y","width_cm","height_cm","capture_time")
cols_present <- intersect(cols_order, names(meta_df))
meta_csv <- file.path(out_root, "captured_images_metadata.csv")
write.csv(meta_df[,cols_present], meta_csv, row.names = FALSE, na = "")
message("Wrote metadata: ", meta_csv)
invisible(meta_df)