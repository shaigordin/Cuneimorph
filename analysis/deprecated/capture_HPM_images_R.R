#!/usr/bin/env Rscript
# capture_HPM_images_R.R
# Reimplementation of capture_HPM_images.js using chromote (Chrome DevTools Protocol).
# Dependencies: install.packages(c("chromote","optparse","jsonlite","fs","base64enc","digest"))

suppressPackageStartupMessages({
  library(optparse)
  library(chromote)
  library(jsonlite)
  library(fs)
  library(base64enc)
  library(digest)
})

# Null coalesce
`%||%` <- function(a,b) if (!is.null(a) && !is.na(a)) a else b

# ---- CLI ---------------------------------------------------------------------
opt <- OptionParser(option_list = list(
  make_option(c("--url"), type = "character",
              default = Sys.getenv("TARGET_URL", ""),
              help = "Target URL (or set TARGET_URL env)"),
  make_option(c("--outdir"), type = "character",
              default = Sys.getenv("OUT_DIR", file.path("data","raw","images","chromote_capture")),
              help = "Output directory (local path)"),
  make_option(c("--zoomAttempts"), type = "integer", default = 3),
  make_option(c("--viewportW"), type = "integer", default = 1600),
  make_option(c("--viewportH"), type = "integer", default = 1200),
  make_option(c("--waitMs"), type = "integer", default = 1200),
  make_option(c("--min_kb"), type = "integer", default = 10),
  make_option(c("--verbose"), type = "logical", default = TRUE)
))
args <- parse_args(opt)

target_url <- args$url
if (!nzchar(target_url)) stop("Provide --url or TARGET_URL env.")
if (!grepl("^https?://", target_url, ignore.case = TRUE)) stop("URL must start with http/https.")

out_dir <- args$outdir
if (grepl("^https?://", out_dir, ignore.case = TRUE)) stop("--outdir must be a local path, not a URL.")
dir_create(out_dir, recurse = TRUE)
resp_dir <- path(out_dir, "responses")
dir_create(resp_dir, recurse = TRUE)

# ---- Helpers -----------------------------------------------------------------
ts_iso <- function() format(Sys.time(), tz = "UTC", usetz = TRUE)
safe_fname <- function(i, ext) sprintf("resp_%04d.%s", i, ext)

sniff_ext <- function(raw, mime = "", url = "") {
  mime <- tolower(mime %||% "")
  url_ext <- tolower(sub(".*\\.([^./?#]+)([?#].*)?$","\\1", url))
  if (url_ext %in% c("jpg","jpeg","png","gif","webp","tif","tiff","svg")) {
    return(ifelse(url_ext == "jpeg","jpg", ifelse(url_ext == "tiff","tif", url_ext)))
  }
  if (nzchar(mime)) {
    if (grepl("jpeg|jpg", mime)) return("jpg")
    if (grepl("png", mime)) return("png")
    if (grepl("gif", mime)) return("gif")
    if (grepl("webp", mime)) return("webp")
    if (grepl("tiff?", mime)) return("tif")
    if (grepl("svg", mime)) return("svg")
  }
  if (length(raw) > 0) {
    sig <- raw
    sig_jpg   <- as.raw(c(0xFF,0xD8,0xFF))
    sig_png   <- as.raw(c(0x89,0x50,0x4E,0x47,0x0D,0x0A,0x1A,0x0A))
    sig_gif   <- as.raw(c(0x47,0x49,0x46))                      # "GIF"
    sig_tif_le<- as.raw(c(0x49,0x49,0x2A,0x00))                 # II*\0
    sig_tif_be<- as.raw(c(0x4D,0x4D,0x00,0x2A))                 # MM\0*
    if (length(sig) >= 3  && all(sig[1:3]  == sig_jpg))    return("jpg")
    if (length(sig) >= 8  && all(sig[1:8]  == sig_png))    return("png")
    if (length(sig) >= 3  && all(sig[1:3]  == sig_gif))    return("gif")
    if (length(sig) >= 4  && (all(sig[1:4] == sig_tif_le) ||
                              all(sig[1:4] == sig_tif_be))) return("tif")
  }
  "bin"
}

write_meta_files <- function(meta_list, base_dir, prefix) {
  json_path <- path(base_dir, paste0(prefix, ".json"))
  csv_path  <- path(base_dir, paste0(prefix, ".csv"))
  write_json(meta_list, json_path, pretty = TRUE, auto_unbox = TRUE)
  if (length(meta_list)) {
    df <- do.call(rbind, lapply(meta_list, function(m)
      data.frame(id = m$id, url = m$url, saved_path = m$saved_path,
                 content_type = m$content_type, size_bytes = m$size_bytes,
                 status = m$status, timestamp = m$timestamp,
                 detail_page = m$detail_page %||% NA_character_,
                 stringsAsFactors = FALSE)))
    write.csv(df, csv_path, row.names = FALSE, na = "")
  }
}

scroll_and_zoom <- function(s, zoomAttempts, waitMs) {
  for (i in seq_len(max(1, zoomAttempts))) {
    Sys.sleep(waitMs/1000)
    s$Runtime$evaluate(expression =
      "var cx=window.innerWidth/2, cy=window.innerHeight/2;
       document.dispatchEvent(new WheelEvent('wheel',{deltaY:-200,clientX:cx,clientY:cy,bubbles:true}))")
    Sys.sleep(waitMs/1000)
  }
  # Scroll cycle
  s$Runtime$evaluate(
    "(async ()=>{
       const total = Math.max(document.body.scrollHeight, window.innerHeight);
       const steps = 4;
       for (let i=0;i<steps;i++){
         window.scrollTo(0, Math.floor(total*(i/(steps-1))));
         await new Promise(r=>setTimeout(r,300));
       }
       window.scrollTo(0,0);
     })();")
  Sys.sleep(1)
}

extract_mousepic_links <- function(s, debug = FALSE) {
  js <- "
  (() => {
    const out = new Set();
    // Direct anchors with mousepic.php
    document.querySelectorAll('a[href*=\"mousepic.php\"]').forEach(a => {
      if (a.href) out.add(a.href);
    });
    // Elements with onclick referencing mousepic.php
    document.querySelectorAll('[onclick*=\"mousepic.php\"]').forEach(el => {
      const oc = el.getAttribute('onclick') || '';
      const m = oc.match(/mousepic\\.php[^'\"\\s)]+/i);
      if (m) {
        // Resolve relative
        let href = m[0];
        if (!/^https?:\\/\\//i.test(href)) {
          try { href = new URL(href, document.location.href).href; } catch(e){}
        }
        out.add(href);
      }
    });
    return Array.from(out);
  })();"
  res <- tryCatch(s$Runtime$evaluate(js), error = function(e) {
    if (debug) message("extract_mousepic_links eval error: ", e$message)
    return(NULL)
  })
  if (is.null(res) || is.null(res$result$value)) {
    if (debug) message("extract_mousepic_links: no result value")
    return(character(0))
  }
  links <- unlist(res$result$value, use.names = FALSE)
  if (debug) message("extract_mousepic_links found: ", length(links))
  unique(links)
}

# Simple thumbnail fallback (anchors whose href includes bildpraep.php or mousepic.php)
extract_fallback_detail_links <- function(s, debug = FALSE) {
  js <- "
  (() => Array.from(document.querySelectorAll(\"a[href*='bildpraep.php'],a[href*='mousepic.php']\"))
          .map(a=>a.href)
          .filter(u=>u && u.toLowerCase().includes('mousepic.php') || u.toLowerCase().includes('bildpraep.php'))
          )();"
  res <- tryCatch(s$Runtime$evaluate(js), error = function(e) {
    if (debug) message("fallback eval error: ", e$message)
    NULL
  })
  vals <- res$result$value %||% list()
  out <- unlist(vals, use.names = FALSE)
  if (debug) message("fallback detail links: ", length(out))
  unique(out)
}

# ---- Browser / session -------------------------------------------------------
chromote_available <- TRUE
b <- tryCatch(Chromote$new(), error = function(e){ chromote_available <<- FALSE; stop("Chromote start failed: ", e$message) })
s_main <- tryCatch(b$new_session(), error = function(e) stop("Session creation failed: ", e$message))
on.exit({ try(s_main$close(), silent=TRUE); try(b$close(), silent=TRUE) }, add = TRUE)

# Configure viewport
s_main$Emulation$setDeviceMetricsOverride(width = args$viewportW,
                                          height = args$viewportH,
                                          deviceScaleFactor = 1,
                                          mobile = FALSE)
s_main$Page$enable(); s_main$Runtime$enable(); s_main$Network$enable()

# ---- Response capture core ---------------------------------------------------
make_captor <- function(resp_dir, min_bytes) {
  saved <- new.env(parent=emptyenv())
  meta <- list()
  counter <- 0L
  handler <- function(ev) {
    params <- ev$params %||% ev
    r <- params$response
    if (is.null(r)) return()
    url <- r$url %||% ""
    mime <- r$mimeType %||% ""
    status <- r$status %||% 0
    if (!nzchar(url) || status < 200 || status >= 300) return()
    if (!(grepl("^image/", mime, TRUE) || grepl("\\.(png|jpe?g|gif|webp|tiff?)($|[?#])", url, TRUE))) return()
    if (exists(url, envir = saved, inherits = FALSE)) return()
    body <- tryCatch(s_main$Network$getResponseBody(requestId = params$requestId),
                     error = function(e) NULL)
    if (is.null(body) || is.null(body$body)) return()
    rawdat <- if (isTRUE(body$base64Encoded))
      tryCatch(base64decode(body$body), error = function(e) raw(0))
    else charToRaw(body$body)
    if (length(rawdat) < min_bytes) return()
    counter <<- counter + 1L
    ext <- sniff_ext(rawdat, mime, url)
    fname <- safe_fname(counter, ext)
    fpath <- path(resp_dir, fname)
    try(writeBin(rawdat, fpath), silent=TRUE)
    assign(url, TRUE, envir = saved)
    meta[[length(meta)+1]] <<- list(
      id = counter, url = url, saved_path = fpath,
      content_type = mime, size_bytes = length(rawdat),
      status = status, timestamp = ts_iso()
    )
    if (args$verbose) message("[saved] ", fname, " <- ", url, " (", mime, ", ", length(rawdat), " bytes)")
  }
  list(handler = handler,
       get_meta = function() meta,
       reset = function(){ meta <<- list(); counter <<- 0L; rm(list = ls(saved), envir = saved) })
}

register_network_listener <- function(s, handler) {
  ok <- FALSE
  if (is.function(s$add_message_callback)) {
    s$add_message_callback(function(msg){
      if (identical(msg$method, "Network.responseReceived")) handler(msg)
    })
    ok <- TRUE
  } else if (!is.null(s$Network$responseReceived) && is.function(s$Network$responseReceived)) {
    s$Network$responseReceived(function(ev) handler(list(params = ev)))
    ok <- TRUE
  }
  if (!ok) warning("No usable listener API; responses will not be captured.")
  invisible(ok)
}

# ---- Navigation capture ------------------------------------------------------
detect_listing <- function(s) {
  tn <- tryCatch({
    v <- s$Runtime$evaluate(
      "Array.from(document.querySelectorAll('img'))
        .filter(i => (i.src||'').match(/\\/photos\\/tn\\/|\\/tn_/i)).length"
    )$result$value
    as.integer(v %||% 0)
  }, error = function(e) 0)
  linkCount <- tryCatch({
    v <- s$Runtime$evaluate(
      "document.querySelectorAll('a[href*=\"mousepic.php\"]').length"
    )$result$value
    as.integer(v %||% 0)
  }, error = function(e) 0)
  list(is_listing = (tn >= 2L || linkCount >= 2L), tn = tn, linkCount = linkCount)
}

capture_page <- function(url, detail_out = out_dir, reset = TRUE, detail_label = NULL) {
  if (reset) caps$reset()
  before_n <- length(caps$get_meta())
  s_main$Page$navigate(url = url)
  Sys.sleep((1500 + args$waitMs)/1000)
  scroll_and_zoom(s_main, args$zoomAttempts, args$waitMs)
  if (!is.null(detail_label)) {
    meta <- caps$get_meta()
    if (length(meta) > before_n) {
      for (i in seq.int(before_n + 1, length(meta))) meta[[i]]$detail_page <- detail_label
    }
  }
  caps$get_meta()
}

# ---- Main --------------------------------------------------------------------
# Suppress printing of TRUE / list() by using invisible()
message("TARGET URL: ", target_url)
message("Output dir: ", out_dir)

caps <- make_captor(resp_dir, min_bytes = args$min_kb * 1024)
invisible(register_network_listener(s_main, caps$handler))

invisible(capture_page(target_url, out_dir, reset = TRUE, detail_label = target_url))
det <- detect_listing(s_main)
message("Detection: tn=", det$tn, " linkCount=", det$linkCount, " -> listing=", det$is_listing)

if (det$is_listing) {
  listing <- harvest_listing(target_url)
  write_json(listing, path(out_dir, "listing_harvest.json"), pretty = TRUE, auto_unbox = TRUE)
  write_meta_files(caps$get_meta(), out_dir, "captured_responses_listing_root")
} else {
  write_meta_files(caps$get_meta(), out_dir, "captured_responses")
}

message("Done.")