# Extract images from a JS/Flash viewer via chromote (DOM img, CSS background-image, canvas -> PNG)
# Usage:
#   Rscript analysis/scripts/extract_from_viewer_chromote.R "<URL>" [out_dir] [wait_seconds]
#
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: Rscript extract_from_viewer_chromote.R <URL> [out_dir] [wait_seconds]")
url <- args[[1]]
out_dir <- if (length(args) >= 2 && nzchar(args[2])) args[2] else file.path("data","raw","images","viewer_extract")
wait_seconds <- if (length(args) >= 3) as.numeric(args[3]) else 4
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("chromote", quietly = TRUE)) stop("Install 'chromote' first: install.packages('chromote')")
if (!requireNamespace("base64enc", quietly = TRUE)) install.packages("base64enc")
if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")

library(chromote)
library(base64enc)
library(httr)

message("Starting chromote session; navigating to: ", url)
b <- chromote::Chromote$new()
s <- b$new_session()
s$Page$enable()
s$Runtime$enable()

# set a viewport reasonably large to force viewer to render at higher res
s$Emulation$setDeviceMetricsOverride(width = 1600L, height = 1200L, deviceScaleFactor = 2, mobile = FALSE)
s$Page$navigate(url)
message("Waiting ", wait_seconds, "s for viewer to initialise (increase if necessary)...")
Sys.sleep(wait_seconds)

# JS: collect <img> src, CSS background-image URLs, canvas dataURLs, and attempt to read common viewer vars
js_collect <- "
(function(){
  function uniq(a){return Array.from(new Set(a));}
  const imgs = Array.from(document.querySelectorAll('img')).map(i=>({type:'img', src:i.src, alt:i.alt||''}));
  const bgs = Array.from(document.querySelectorAll('*')).map(el=>{
    const bg = window.getComputedStyle(el).backgroundImage;
    if (bg && bg!=='none'){
      const m = bg.match(/url\\(['\"]?(.*?)['\"]?\\)/);
      if (m && m[1]) return ({type:'bg', src:m[1], selector: el.nodeName.toLowerCase()});
    }
    return null;
  }).filter(Boolean);
  const canvases = Array.from(document.querySelectorAll('canvas')).map((c,idx)=>{
    try { return ({type:'canvas', data: c.toDataURL('image/png'), idx: idx}); } catch(e) { return null; }
  }).filter(Boolean);
  // try to find image/tile URLs in window objects (heuristic)
  let viewer_urls = [];
  try {
    Object.keys(window).slice(-200).forEach(k => {
      try {
        const v = window[k];
        if (v && typeof v === 'object') {
          if (v.url && typeof v.url === 'string') viewer_urls.push({type:'var', src:v.url, name:k});
          if (v.tileUrl && typeof v.tileUrl === 'string') viewer_urls.push({type:'var', src:v.tileUrl, name:k});
        }
      } catch(e){}
    });
  } catch(e){}
  return {candidates: imgs.concat(bgs).concat(canvases).concat(viewer_urls)};
})();
"

ev <- tryCatch(s$Runtime$evaluate(js_collect, awaitPromise = TRUE), error = function(e) { b$close(); stop("DOM evaluation failed: ", e$message) })
res <- ev$result$value
cands <- res$candidates
if (is.null(cands) || length(cands) == 0) {
  b$close()
  stop("No image candidates found in DOM. Increase wait_seconds or use network interception (Puppeteer).")
}

message("Found ", length(cands), " candidate(s). Saving...")
saved <- list()
i <- 0L
for (c in cands) {
  i <- i + 1L
  typ <- if (!is.null(c$type)) c$type else NA_character_
  if (!is.na(typ) && typ == "canvas" && !is.null(c$data) && grepl("^data:image", c$data)) {
    b64 <- sub("^data:image/[^;]+;base64,", "", c$data)
    outf <- file.path(out_dir, sprintf("canvas_%02d.png", i))
    writeBin(base64enc::base64decode(b64), outf)
    saved[[length(saved)+1]] <- list(source = "canvas", original = NA_character_, path = normalizePath(outf))
    message("Wrote canvas -> ", outf)
  } else if (!is.null(c$src) && nzchar(c$src)) {
    src <- trimws(gsub("^[\"']|[\"']$", "", c$src))
    # make absolute if path-absolute
    if (!grepl("^https?://", src) && grepl("^/", src)) {
      p <- httr::parse_url(url)
      src <- paste0(p$scheme, "://", p$hostname, src)
    }
    # attempt to download
    ext <- tools::file_ext(src); if (!nzchar(ext)) ext <- "jpg"
    safe <- gsub("[^A-Za-z0-9._-]+", "_", basename(src))
    dest <- file.path(out_dir, sprintf("candidate_%02d_%s.%s", i, safe, ext))
    dl <- tryCatch({
      r <- httr::GET(src, httr::user_agent("R (chromote)"), timeout(30))
      if (httr::status_code(r) >= 200 && httr::status_code(r) < 300) {
        writeBin(httr::content(r, "raw"), dest)
        TRUE
      } else {
        writeLines(rawToChar(httr::content(r, "raw")), file.path(out_dir, sprintf("candidate_%02d_http%03d.txt", i, httr::status_code(r))))
        FALSE
      }
    }, error = function(e) FALSE)
    if (isTRUE(dl) && file.exists(dest)) {
      saved[[length(saved)+1]] <- list(source = src, original = src, path = normalizePath(dest))
      message("Downloaded: ", src, " -> ", dest)
    } else {
      saved[[length(saved)+1]] <- list(source = src, original = src, path = NA_character_, error = "download_failed")
      message("Failed download: ", src)
    }
  } else {
    saved[[length(saved)+1]] <- list(source = NA, original = NA, path = NA)
  }
}

b$close()

meta <- do.call(rbind, lapply(saved, function(x) {
  data.frame(source = ifelse(is.null(x$source), NA, x$source),
             original = ifelse(is.null(x$original), NA, x$original),
             path = ifelse(is.null(x$path), NA, x$path),
             error = ifelse(is.null(x$error), NA, x$error),
             stringsAsFactors = FALSE)
}))
meta_file <- file.path(out_dir, "viewer_extract_candidates.csv")
write.csv(meta, meta_file, row.names = FALSE, na = "")
message("Saved ", nrow(meta), " candidate records to ", meta_file)
invisible(list(out_dir = out_dir, metadata = meta))