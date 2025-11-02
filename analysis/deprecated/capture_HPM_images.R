#!/usr/bin/env Rscript
# Rough port of capture_HPM_images.js -> R using chromote + CDP
# Requires: install.packages(c("chromote","jsonlite","fs","base64enc","optparse"))

suppressPackageStartupMessages({
  library(optparse)
  library(chromote)
  library(jsonlite)
  library(fs)
  library(base64enc)
})

# NULL-coalesce
`%||%` <- function(a, b) if (!is.null(a)) a else b

opt <- OptionParser(option_list = list(
  make_option(c("--url"), type="character", default=Sys.getenv("TARGET_URL", ""), help="Target URL (or use TARGET_URL env)"),
  make_option(c("--outdir"), type="character", default=Sys.getenv("OUT_DIR", file.path("data","raw","images","chromote_capture")), help="Output directory"),
  make_option(c("--zoomAttempts"), type="integer", default=3),
  make_option(c("--viewportW"), type="integer", default=1600),
  make_option(c("--viewportH"), type="integer", default=1200),
  make_option(c("--waitMs"), type="integer", default=1200),
  make_option(c("--min_kb"), type="integer", default=10)
))
args <- parse_args(opt)

url <- ifelse(nzchar(args$url), args$url, Sys.getenv("TARGET_URL", ""))
if (!nzchar(url)) stop("Provide --url or TARGET_URL")
if (!grepl("^https?://", url, ignore.case = TRUE)) stop("Invalid URL scheme")

outDir <- args$outdir
if (grepl("^https?://", outDir, ignore.case = TRUE)) stop("--outdir must be local path, not URL")
if (!dir.exists(outDir)) dir_create(outDir, recurse = TRUE)
if (file.access(outDir, 2) != 0) stop("outDir not writable: ", outDir)

responses_dir <- path(outDir, "responses")
dir_create(responses_dir, recurse = TRUE)

# Helpers (previously missing)
safe_name <- function(prefix, i, ext) sprintf("%s_%04d.%s", prefix, i, ext)
guess_ext <- function(rurl, mime) {
  uext <- tolower(gsub("^.+\\.([^./?#]+)([?#].*)?$", "\\1", rurl))
  if (nzchar(uext) && uext %in% c("jpg","jpeg","png","gif","svg","webp","tif","tiff")) {
    if (uext == "jpeg") return("jpg")
    return(uext)
  }
  if (nzchar(mime)) {
    if (grepl("jpeg", mime, TRUE)) return("jpg")
    if (grepl("png",  mime, TRUE)) return("png")
    if (grepl("gif",  mime, TRUE)) return("gif")
    if (grepl("svg",  mime, TRUE)) return("svg")
    if (grepl("webp", mime, TRUE)) return("webp")
    if (grepl("tif",  mime, TRUE)) return("tif")
  }
  # magic number sniff fallback
  ext <- "bin"
  # (Optional: could add raw sniff here after body fetch)
  ext
}

# Start chromote
b <- tryCatch(Chromote$new(), error = function(e) stop("Chromote start failed: ", e$message))
s <- tryCatch(b$new_session(), error = function(e) stop("Chromote session failed: ", e$message))
on.exit({
  try(s$close(), silent = TRUE)
  try(b$close(), silent = TRUE)
}, add = TRUE)

# Viewport & Network
s$callMethod("Emulation.setDeviceMetricsOverride",
             params = list(width = args$viewportW, height = args$viewportH,
                           deviceScaleFactor = 1L, mobile = FALSE))
s$Network$enable()

meta <- list()
saved_urls <- new.env(parent = emptyenv())
counter <- 0L
min_bytes <- args$min_kb * 1024L
wait_ms <- function(ms) Sys.sleep(ms/1000)

# Capture handler (using official event API)
on_response_received <- function(params) {
  r <- params$response
  if (is.null(r)) return()
  rurl <- r$url %||% ""
  status <- r$status %||% 0
  mime <- r$mimeType %||% ""
  if (!nzchar(rurl) || status < 200 || status >= 300) return()
  if (!(grepl("^image/", mime, TRUE) || grepl("\\.(png|jpe?g|gif|webp|tiff?)($|[?#])", rurl, TRUE))) return()
  if (exists(rurl, envir = saved_urls, inherits = FALSE)) return()
  # Get body
  rb <- tryCatch(s$Network$getResponseBody(requestId = params$requestId),
                 error = function(e) { warning("getResponseBody failed: ", e$message); return(NULL) })
  if (is.null(rb) || is.null(rb$body) || !nzchar(rb$body)) return()
  rawdat <- if (isTRUE(rb$base64Encoded)) {
    tryCatch(base64decode(rb$body), error = function(e) { warning("base64 decode failed: ", e$message); return(raw(0)) })
  } else charToRaw(rb$body)
  if (length(rawdat) < min_bytes) return()
  counter <<- counter + 1L
  ext <- guess_ext(rurl, mime)
  fname <- safe_name("resp", counter, ext)
  fpath <- path(responses_dir, fname)
  ok <- tryCatch({ writeBin(rawdat, fpath); TRUE }, error = function(e) { warning("Write failed: ", e$message); FALSE })
  if (!ok) return()
  assign(rurl, fpath, envir = saved_urls)
  meta[[length(meta)+1L]] <<- list(
    id = counter, url = rurl, saved_path = fpath, content_type = mime,
    size_bytes = length(rawdat), status = status,
    timestamp = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  message("[saved] ", fname, " <- ", rurl, " (", mime, ", ", length(rawdat), " bytes)")
}

# Ensure %||% exists early (idempotent)
if (!exists("%||%")) `%||%` <- function(a,b) if (!is.null(a)) a else b

# Simple step logger
.step <- local({ i <- 0L; function(msg){ i <<- i + 1L; message(sprintf("[STEP %02d] %s", i, msg)) } })

.step("Chromote session ready")

# Defensive network listener registration
register_network_listener <- function(s, handler) {
  cand <- c("responseReceived","onResponseReceived")
  for (nm in cand) {
    f <- tryCatch(s$Network[[nm]], error = function(e) NULL)
    if (is.function(f)) {
      f(function(ev){
        # ev may already be params or may wrap in $params
        params <- ev$params %||% ev
        handler(params)
      })
      message("Registered listener via Network$", nm)
      return(TRUE)
    }
  }
  message("No direct Network$responseReceived API; skipping listener (will capture nothing).")
  FALSE
}

# Wrapper handler (expects params list with $response and $requestId)
on_response_params <- function(params) {
  # (Keep existing body you already wrote, but make sure it takes 'params')
  # TEMP minimal guard to prove listener works:
  if (!is.null(params$response$url)) message("[EVT] ", params$response$url)
}

listener_ok <- register_network_listener(s, on_response_params)
.step(paste("Listener ok =", listener_ok))

# Register event listener (preferred API)
if (is.function(s$Network$responseReceived)) {
  s$Network$responseReceived(function(event) {
    # event is a list with params
    try(on_response_received(event), silent = TRUE)
  })
} else {
  warning("Network$responseReceived callback API unavailable; capture may fail.")
}

do_zoom_attempts <- function() {
  for (i in seq_len(max(1, args$zoomAttempts))) {
    wait_ms(max(800, args$waitMs))
    s$Runtime$evaluate(expression =
      "var cx=window.innerWidth/2, cy=window.innerHeight/2;
       document.dispatchEvent(new WheelEvent('wheel',
         {deltaY:-200, clientX:cx, clientY:cy, bubbles:true}))")
    wait_ms(max(800, args$waitMs))
  }
}

write_meta_snapshot <- function(prefix = "captured_responses") {
  json_path <- path(outDir, paste0(prefix, ".json"))
  csv_path  <- path(outDir, paste0(prefix, ".csv"))
  try(write_json(meta, json_path, pretty = TRUE, auto_unbox = TRUE), silent = TRUE)
  if (length(meta)) {
    df <- do.call(rbind, lapply(meta, \(m)
      data.frame(id = m$id, url = m$url, saved_path = m$saved_path,
                 content_type = m$content_type, size_bytes = m$size_bytes,
                 status = m$status, timestamp = m$timestamp,
                 stringsAsFactors = FALSE)))
    try(write.csv(df, csv_path, row.names = FALSE, na = ""), silent = TRUE)
  }
}

capture_responses_during_navigation <- function(navigate_to, reset_meta = TRUE) {
  if (reset_meta) meta <<- list()
  message("Navigate: ", navigate_to)
  s$Page$navigate(url = navigate_to)
  # crude wait (improve with event-based idle if needed)
  wait_ms(1500 + args$waitMs)
  do_zoom_attempts()
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
  wait_ms(1000)
  meta
}

find_mousepic_links <- function() {
  js <- "(() => {
    let out=[];
    document.querySelectorAll('a, [onclick]').forEach(el=>{
      try{
        const href = el.href || el.getAttribute('href') || '';
        if (href && href.toLowerCase().includes('mousepic.php')) out.push(href);
        const oc = el.getAttribute('onclick') || '';
        if (oc && oc.toLowerCase().includes('mousepic.php')) {
          const m = oc.match(/mousepic\\.php[^'\")\\s]+/i);
            if (m) out.push(m[0]);
        }
      }catch(e){}
    });
    return Array.from(new Set(out));
  })();"
  res <- tryCatch(s$Runtime$evaluate(js), error = function(e) { warning("Eval mousepic failed: ", e$message); return(NULL) })
  if (is.null(res) || is.null(res$result$value)) return(character(0))
  links <- unlist(res$result$value, use.names = FALSE)
  if (!length(links)) return(character(0))
  page_url <- tryCatch(s$Runtime$evaluate("location.href")$result$value,
                       error = function(e) "")
  links_abs <- vapply(links, function(h) {
    if (!grepl("^https?://", h)) paste0(sub("/+$","", page_url), "/", sub("^/+", "", h)) else h
  }, character(1))
  unique(links_abs)
}

harvest_listing <- function() {
  links <- find_mousepic_links()
  if (length(links)) {
    message("Found mousepic links: ", length(links))
    results <- list()
    for (i in seq_along(links)) {
      capture_responses_during_navigation(links[i], reset_meta = TRUE)
      # snapshot per detail (optional)
      results[[length(results)+1]] <- list(link = links[i], responses = meta)
    }
    try(write_json(results, path(outDir, "listing_harvest.json"),
                   pretty = TRUE, auto_unbox = TRUE), silent = TRUE)
    return(invisible(results))
  } else {
    message("No mousepic links found; (thumbnail fallback not implemented here).")
    return(invisible(list()))
  }
}

message("TARGET URL: ", url)
message("outDir: ", outDir)

# Enable core domains
s$Page$enable()
s$Runtime$enable()
s$Network$enable()

# Initial navigation
capture_responses_during_navigation(url, reset_meta = TRUE)

# Heuristic listing detection
tnCount <- tryCatch({
  val <- s$Runtime$evaluate(
    "Array.from(document.querySelectorAll('img'))
      .filter(i => (i.src||'').match(/\\/photos\\/tn\\/|\\/tn_/i)).length"
  )$result$value
  as.integer(val %||% 0)
}, error = function(e) 0)

linkCount <- tryCatch({
  val <- s$Runtime$evaluate("document.querySelectorAll('a[href*=\"mousepic.php\"]').length")$result$value
  as.integer(val %||% 0)
}, error = function(e) 0)

isListing <- (tnCount >= 2L) || (linkCount >= 2L)

if (isListing) {
  message("Detected listing (tnCount=", tnCount, ", linkCount=", linkCount, ")")
  harvest_listing()
} else {
  message("Detected detail page (tnCount=", tnCount, ", linkCount=", linkCount, ")")
  # current meta already corresponds to initial page navigation
}

# Final snapshot (detail or last listing pass)
write_meta_snapshot(if (isListing) "captured_responses_listing_root" else "captured_responses")

message("Done.")