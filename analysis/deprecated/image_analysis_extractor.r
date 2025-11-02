# Archaeological Image Analysis and Region Extraction Tool
# Systematic image processing for detailed analysis and documentation
# --- Updated: moved install logic out, added robust downloader, cleaner outputs, and helper to write cleaned CSVs

# Required packages (install if --install passed on command line)
required_packages <- c(
  "magick", "imager", "dplyr", "tibble", "jsonlite",
  "httr", "xml2", "stringr", "grid", "ggplot2"
)

# Allow optional auto-install when user runs script with --install
.cmd_args <- commandArgs(trailingOnly = TRUE)
.install_flag <- "--install" %in% .cmd_args

.missing_pkgs <- required_packages[!vapply(required_packages, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
if (length(.missing_pkgs)) {
  if (isTRUE(.install_flag)) {
    message("Installing missing packages: ", paste(.missing_pkgs, collapse = ", "))
    install.packages(.missing_pkgs)
    .missing_pkgs <- required_packages[!vapply(required_packages, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
    if (length(.missing_pkgs)) {
      stop("Some packages failed to install: ", paste(.missing_pkgs, collapse = ", "))
    }
  } else {
    stop(sprintf(
      "Missing required packages: %s\nEither install them manually or re-run with --install to auto-install:\n  install.packages(c(%s))\n  Rscript ... --install",
      paste(.missing_pkgs, collapse = ", "),
      paste(sprintf('"%s"', .missing_pkgs), collapse = ", ")
    ), call. = FALSE)
  }
}

# Attach libraries
invisible(lapply(required_packages, function(p) library(p, character.only = TRUE)))

#' Clean and write metadata CSV (normalize whitespace, remove NULL/NaN, drop empty rows)
write_clean_metadata_csv <- function(df, path) {
  if (is.null(df) || (is.data.frame(df) && nrow(df) == 0)) {
    # write an empty CSV with headers if possible or create file
    if (!is.null(df) && ncol(df) > 0) {
      utils::write.csv(df[FALSE, , drop = FALSE], file = path, row.names = FALSE, na = "")
    } else {
      file.create(path)
    }
    return(invisible(path))
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  clean_cell <- function(x) {
    if (is.null(x) || is.na(x)) return("")
    s <- as.character(x)
    s <- gsub("[\r\n]+", " ", s)        # convert embedded line breaks to single space
    s <- gsub("[[:space:]]+", " ", s)   # collapse whitespace
    s <- trimws(s)                      # trim ends
    if (nzchar(s) && tolower(s) %in% c("null", "nan")) s <- ""
    s
  }
  cleaned <- as.data.frame(lapply(df, function(col) vapply(col, clean_cell, FUN.VALUE = character(1))),
                           stringsAsFactors = FALSE, check.names = FALSE)
  keep <- apply(cleaned, 1, function(r) any(nzchar(r)))
  cleaned <- cleaned[keep, , drop = FALSE]
  utils::write.csv(cleaned, file = path, row.names = FALSE, na = "")
  invisible(path)
}

#' Robust downloader and analyzer for images
#' Uses httr::RETRY, preserves extension, writes timestamped original file
# Helper: sanitize filename and parse density like "300x300"
make_safe_filename <- function(x) {
  x <- gsub("[^A-Za-z0-9._-]", "_", x)
  x <- gsub("_+", "_", x)
  if (nchar(x) == 0) x <- "original"
  x
}
parse_density <- function(dens) {
  if (is.null(dens) || !nzchar(dens)) return(c(NA_real_, NA_real_))
  parts <- strsplit(dens, "x", fixed = TRUE)[[1]]
  dx <- suppressWarnings(as.numeric(parts[1])); dy <- if (length(parts) > 1) suppressWarnings(as.numeric(parts[2])) else dx
  if (is.na(dx)) dx <- NA_real_
  if (is.na(dy)) dy <- dx
  c(dx, dy)
}

download_and_analyze_image <- function(image_url,
                                      output_dir = "image_analysis",
                                      user_agent = "Mozilla/5.0 (compatible; ImageAnalysisBot/1.0)",
                                      timeout_sec = 60,
                                      verbose = TRUE,
                                      screenshot_fallback = TRUE,
                                      screenshot_width = 2000,
                                      screenshot_height = 2000,
                                      screenshot_scale = 2) {
  stopifnot(is.character(image_url), nzchar(image_url))
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Helper: find Chrome/Chromium binary (macOS / Linux common paths)
  find_chrome <- function() {
    candidates <- c(
      Sys.which("google-chrome"),
      Sys.which("google-chrome-stable"),
      Sys.which("chromium"),
      Sys.which("chromium-browser"),
      "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
      "/Applications/Chromium.app/Contents/MacOS/Chromium"
    )
    candidates <- candidates[nzchar(candidates)]
    if (length(candidates)) return(unname(candidates)[1])
    NA_character_
  }

  # Helper: use chromote when available, otherwise Chrome CLI to capture a screenshot
  take_screenshot <- function(url, out_png, w, h, scale) {
    # Prefer chromote + base64enc if both available
    if (requireNamespace("chromote", quietly = TRUE) && requireNamespace("base64enc", quietly = TRUE)) {
      message("Using chromote for screenshot capture")
      b <- chromote::Chromote$new()
      s <- b$new_session()
      # enable page and emulation
      s$Page$enable()
      s$Emulation$setDeviceMetricsOverride(width = as.integer(w),
                                           height = as.integer(h),
                                           deviceScaleFactor = scale,
                                           mobile = FALSE)
      s$Page$navigate(url)
      Sys.sleep(2) # allow viewer to render; increase if necessary
      b64 <- s$Page$captureScreenshot(format = "png", fromSurface = TRUE)
      bin <- base64enc::base64decode(b64)
      writeBin(bin, out_png)
      b$close()
      if (!file.exists(out_png)) stop("chromote capture completed but file missing: ", out_png)
      return(invisible(TRUE))
    }
    # Fallback to CLI
    chrome <- find_chrome()
    if (is.na(chrome)) stop("No Chrome/Chromium binary found for screenshot fallback and chromote/base64enc not available")
    args <- c(
      "--headless",
      "--disable-gpu",
      "--no-sandbox",
      "--disable-dev-shm-usage",
      "--disable-software-rasterizer",
      "--hide-scrollbars",
      "--disable-extensions",
      paste0("--screenshot=", normalizePath(out_png, mustWork = FALSE)),
      paste0("--window-size=", as.integer(w), ",", as.integer(h)),
      paste0("--force-device-scale-factor=", scale),
      url
    )
    res <- tryCatch({
      out <- system2(command = chrome, args = args, stdout = TRUE, stderr = TRUE)
      list(success = TRUE, output = out)
    }, error = function(e) list(success = FALSE, msg = e$message))
    if (!isTRUE(res$success)) {
      stop("Chrome CLI screenshot failed: ", res$msg, call. = FALSE)
    }
    if (!file.exists(out_png)) {
      stop("Chrome CLI completed but did not create screenshot. Command output:\n", paste(res$output, collapse = "\n"), call. = FALSE)
    }
    invisible(res$output)
  }

  # Primary download
  resp <- tryCatch({
    httr::RETRY("GET", image_url,
                httr::user_agent(user_agent),
                httr::timeout(timeout_sec),
                times = 3, pause_base = 1)
  }, error = function(e) e)

  if (inherits(resp, "error")) stop("Download failed: ", resp$message)
  status <- httr::status_code(resp)
  if (status < 200 || status >= 300) {
    dbg <- tempfile(fileext = ".txt")
    try(writeBin(httr::content(resp, "raw"), dbg), silent = TRUE)
    stop(sprintf("HTTP error %d when fetching %s — response saved to %s", status, image_url, dbg), call. = FALSE)
  }

  # Inspect content-type and write raw body
  ct <- tolower(httr::headers(resp)[["content-type"]] %||% "")
  is_image_ct <- nzchar(ct) && grepl("^image/", ct)
  tmpfile <- tempfile(fileext = if (grepl("png", ct)) ".png" else if (grepl("jpeg|jpg", ct)) ".jpg" else ".bin")
  writeBin(httr::content(resp, "raw"), tmpfile)

  # If content-type is text/html or starts with PHP/HTML, attempt fallback immediately
  first_bytes <- tryCatch({ rawToChar(readBin(tmpfile, what = "raw", n = 1024)) }, error = function(e) "")
  if (!is_image_ct && grepl("<\\?php|<html|<!doctype|<body|<script", first_bytes, ignore.case = TRUE)) {
    if (isTRUE(screenshot_fallback)) {
      message("Downloaded non-image response; attempting headless Chromium screenshot fallback...")
      screenshot_out <- file.path(output_dir, paste0("screenshot_", format(Sys.time(), "%Y%m%dT%H%M%S"), ".png"))
      tryCatch({
        take_screenshot(image_url, screenshot_out, screenshot_width, screenshot_height, screenshot_scale)
        img <- magick::image_read(screenshot_out)
        info <- magick::image_info(img)
        return(list(image = img, info = info, url = image_url, local_path = screenshot_out, download_time = Sys.time(), from_screenshot = TRUE))
      }, error = function(e) {
        stop("Screenshot fallback failed: ", e$message, call. = FALSE)
      })
    } else {
      stop("Downloaded content is not an image and screenshot_fallback is FALSE", call. = FALSE)
    }
  }

  # Try to read the saved file with magick, handle read errors gracefully
  out_path <- tmpfile
  # if content-type looked image-like, ensure reasonable extension/name for reading/writing
  ext <- if (nzchar(ct) && grepl("jpeg", ct)) "jpg" else if (nzchar(ct) && grepl("png", ct)) "png" else tools::file_ext(httr::parse_url(image_url)$path)
  if (!nzchar(ext)) ext <- "jpg"
  # move to output_dir with a sensible name
  url_basename <- basename(httr::parse_url(image_url)$path)
  base_no_ext <- tools::file_path_sans_ext(url_basename)
  base_no_ext <- if (nzchar(base_no_ext)) gsub("[^A-Za-z0-9._-]+", "_", base_no_ext) else "original"
  final_name <- sprintf("%s_%s.%s", base_no_ext, format(Sys.time(), "%Y%m%dT%H%M%S"), ext)
  final_path <- file.path(output_dir, final_name)
  file.copy(tmpfile, final_path, overwrite = TRUE)

  img <- tryCatch({
    magick::image_read(final_path)
  }, error = function(e) {
    # If magick cannot read (e.g. PHP text), attempt screenshot fallback if allowed
    if (grepl("PHP|HTML|no decode delegate", e$message, ignore.case = TRUE) && isTRUE(screenshot_fallback)) {
      message("magick::image_read failed on downloaded file; trying screenshot fallback...")
      screenshot_out <- file.path(output_dir, paste0("screenshot_", format(Sys.time(), "%Y%m%dT%H%M%S"), ".png"))
      tryCatch({
        take_screenshot(image_url, screenshot_out, screenshot_width, screenshot_height, screenshot_scale)
        magick::image_read(screenshot_out)
      }, error = function(e2) {
        stop("Both direct download and screenshot fallback failed: ", e$message, " | ", e2$message, call. = FALSE)
      })
    } else {
      stop("magick::image_read failed: ", e$message, call. = FALSE)
    }
  })

  info <- magick::image_info(img)
  dens <- if (!is.null(info$density)) info$density else ""
  dpi_vals <- parse_density(dens)
  dpi_x <- dpi_vals[1]; dpi_y <- dpi_vals[2]
  width_cm <- if (!is.na(dpi_x) && dpi_x > 0) round(info$width / dpi_x * 2.54, 2) else NA_real_
  height_cm <- if (!is.na(dpi_y) && dpi_y > 0) round(info$height / dpi_y * 2.54, 2) else NA_real_

  if (isTRUE(verbose)) {
    message("Saved to: ", final_path, " (", info$width, "x", info$height, " px; dpi=", dpi_x, "x", dpi_y, ")")
  }

  list(image = img, info = info, url = image_url, local_path = final_path, download_time = Sys.time(),
       dpi_x = dpi_x, dpi_y = dpi_y, width_cm = width_cm, height_cm = height_cm, from_screenshot = FALSE)
}

#' Extract systematic grid regions from image
extract_grid_regions <- function(img,
                                 grid_rows = 4,
                                 grid_cols = 4,
                                 overlap_percent = 10,
                                 zoom_factor = 2,
                                 output_dir = "grid_extractions",
                                 write_metadata = TRUE) {

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  img_info <- magick::image_info(img)
  width <- img_info$width
  height <- img_info$height

  overlap_pixels_x <- round(width * overlap_percent / 100)
  overlap_pixels_y <- round(height * overlap_percent / 100)

  region_width <- round(width / grid_cols + overlap_pixels_x)
  region_height <- round(height / grid_rows + overlap_pixels_y)

  region_width <- min(region_width, width)
  region_height <- min(region_height, height)

  extraction_metadata <- tibble()
  region_count <- 0

  for (row in 1:grid_rows) {
    for (col in 1:grid_cols) {
      region_count <- region_count + 1

      x_start <- round((col - 1) * width / grid_cols - overlap_pixels_x / 2)
      y_start <- round((row - 1) * height / grid_rows - overlap_pixels_y / 2)

      x_start <- max(0, x_start)
      y_start <- max(0, y_start)

      x_end <- min(width, x_start + region_width)
      y_end <- min(height, y_start + region_height)

      actual_width <- x_end - x_start
      actual_height <- y_end - y_start

      geometry <- sprintf("%dx%d+%d+%d", actual_width, actual_height, x_start, y_start)
      region <- magick::image_crop(img, geometry)

      if (zoom_factor > 1) {
        new_width <- round(actual_width * zoom_factor)
        new_height <- round(actual_height * zoom_factor)
        region <- magick::image_resize(region, sprintf("%dx%d", new_width, new_height))
        region <- magick::image_enhance(region)
        region <- apply_sharpen(region, radius = 1, sigma = 0.5)
      }

      filename <- sprintf("region_r%02d_c%02d.png", row, col)
      filepath <- file.path(output_dir, filename)
      magick::image_write(region, filepath, format = "PNG")

      region_info <- magick::image_info(region)
      dens <- if (!is.null(region_info$density)) region_info$density else ""
      dpi_vals <- parse_density(dens); region_dpi_x <- dpi_vals[1]; region_dpi_y <- dpi_vals[2]
      region_width_cm <- if (!is.na(region_dpi_x) && region_dpi_x > 0) round(region_info$width / region_dpi_x * 2.54, 2) else NA_real_
      region_height_cm <- if (!is.na(region_dpi_y) && region_dpi_y > 0) round(region_info$height / region_dpi_y * 2.54, 2) else NA_real_

      file_size <- if (file.exists(filepath)) file.info(filepath)$size else NA_integer_

      extraction_metadata <- bind_rows(extraction_metadata, tibble(
        region_id = region_count,
        row = row,
        col = col,
        filename = filename,
        filepath = filepath,
        original_x_start = x_start,
        original_y_start = y_start,
        original_width = actual_width,
        original_height = actual_height,
        final_width = region_info$width,
        final_height = region_info$height,
        zoom_factor = zoom_factor,
        overlap_percent = overlap_percent,
        file_size_bytes = file_size,
        file_size_kb = if (!is.na(file_size)) round(file_size / 1024, 2) else NA_real_,
        extraction_time = Sys.time(),
        dpi_x = region_dpi_x,
        dpi_y = region_dpi_y,
        width_cm = region_width_cm,
        height_cm = region_height_cm
      ))
    }
  }

  if (isTRUE(write_metadata)) {
    meta_path <- file.path(output_dir, "grid_extractions_metadata.csv")
    write_clean_metadata_csv(extraction_metadata, meta_path)
  }

  return(extraction_metadata)
}

#' Extract regions of interest based on image analysis
extract_regions_of_interest <- function(img,
                                       detection_method = "edges",
                                       output_dir = "roi_extractions",
                                       write_metadata = TRUE) {

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  roi_metadata <- tibble()

  if (detection_method == "edges") {
    gray_img <- magick::image_convert(img, "grayscale")
    edge_img <- magick::image_edge(gray_img, radius = 1)

    edge_path <- file.path(output_dir, "edge_detection.png")
    magick::image_write(edge_img, edge_path)

    img_info <- magick::image_info(img)

    regions <- list(
      list(name = "top_left", x = 0, y = 0, width = img_info$width/3, height = img_info$height/3),
      list(name = "top_center", x = img_info$width/3, y = 0, width = img_info$width/3, height = img_info$height/3),
      list(name = "top_right", x = 2*img_info$width/3, y = 0, width = img_info$width/3, height = img_info$height/3),
      list(name = "center", x = img_info$width/4, y = img_info$height/4, width = img_info$width/2, height = img_info$height/2),
      list(name = "bottom_inscriptions", x = 0, y = 2*img_info$height/3, width = img_info$width, height = img_info$height/3)
    )

    for (i in seq_along(regions)) {
      region <- regions[[i]]

      geometry <- sprintf("%.0fx%.0f+%.0f+%.0f",
                         region$width, region$height, region$x, region$y)
      roi <- magick::image_crop(img, geometry)

      roi <- magick::image_enhance(roi)
      roi <- magick::image_contrast(roi, sharpen = 1)
      roi <- magick::image_noise(roi, noisetype = "gaussian")

      filename <- sprintf("roi_%02d_%s.png", i, region$name)
      filepath <- file.path(output_dir, filename)
      magick::image_write(roi, filepath)

      roi_info <- magick::image_info(roi)
      dens <- if (!is.null(roi_info$density)) roi_info$density else ""
      dpi_vals <- parse_density(dens); roi_dpi_x <- dpi_vals[1]; roi_dpi_y <- dpi_vals[2]
      roi_width_cm <- if (!is.na(roi_dpi_x) && roi_dpi_x > 0) round(roi_info$width / roi_dpi_x * 2.54, 2) else NA_real_
      roi_height_cm <- if (!is.na(roi_dpi_y) && roi_dpi_y > 0) round(roi_info$height / roi_dpi_y * 2.54, 2) else NA_real_

      file_size <- if (file.exists(filepath)) file.info(filepath)$size else NA_integer_

      roi_metadata <- bind_rows(roi_metadata, tibble(
        roi_id = i,
        roi_name = region$name,
        filename = filename,
        filepath = filepath,
        x_start = region$x,
        y_start = region$y,
        width = roi_info$width,
        height = roi_info$height,
        detection_method = detection_method,
        file_size_kb = if (!is.na(file_size)) round(file_size / 1024, 2) else NA_real_,
        extraction_time = Sys.time(),
        dpi_x = roi_dpi_x,
        dpi_y = roi_dpi_y,
        width_cm = roi_width_cm,
        height_cm = roi_height_cm
      ))
    }
  }

  if (isTRUE(write_metadata)) {
    meta_path <- file.path(output_dir, "roi_extractions_metadata.csv")
    write_clean_metadata_csv(roi_metadata, meta_path)
  }

  return(roi_metadata)
}

#' Create comprehensive analysis report
create_analysis_report <- function(original_analysis,
                                  grid_metadata = NULL,
                                  roi_metadata = NULL,
                                  output_dir = "analysis_report") {

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  analysis_summary <- list(
    source_info = list(
      url = original_analysis$url,
      download_time = original_analysis$download_time,
      original_dimensions = paste(original_analysis$info$width, "x", original_analysis$info$height),
      original_format = original_analysis$info$format,
      original_file = original_analysis$local_path
    ),
    processing_summary = list(
      total_grid_regions = ifelse(is.null(grid_metadata), 0, nrow(grid_metadata)),
      total_roi_regions = ifelse(is.null(roi_metadata), 0, nrow(roi_metadata)),
      total_extracted_images = ifelse(is.null(grid_metadata), 0, nrow(grid_metadata)) +
        ifelse(is.null(roi_metadata), 0, nrow(roi_metadata))
    )
  )

  json_path <- file.path(output_dir, "analysis_metadata.json")
  write(jsonlite::toJSON(analysis_summary, pretty = TRUE, auto_unbox = TRUE), json_path)

  if (!is.null(grid_metadata) && nrow(grid_metadata) > 0) {
    grid_csv_path <- file.path(output_dir, "grid_extractions_metadata.csv")
    write_clean_metadata_csv(grid_metadata, grid_csv_path)
  }

  if (!is.null(roi_metadata) && nrow(roi_metadata) > 0) {
    roi_csv_path <- file.path(output_dir, "roi_extractions_metadata.csv")
    write_clean_metadata_csv(roi_metadata, roi_csv_path)
  }

  report_text <- sprintf("
# Image Analysis Report

**Generated:** %s

## Source Information
- **URL:** %s
- **Dimensions:** %s
- **Format:** %s
- **Download Time:** %s

## Processing Summary
- **Grid Regions Extracted:** %d
- **ROI Regions Extracted:** %d
- **Total Images Generated:** %d

## Files Generated
- Original image: %s
- Analysis metadata: %s
%s%s

## Next Steps
1. Review extracted regions for quality
2. Apply additional enhancement if needed
3. Conduct detailed analysis on regions of interest
4. Document findings and observations

",
    Sys.time(),
    analysis_summary$source_info$url,
    analysis_summary$source_info$original_dimensions,
    analysis_summary$source_info$original_format,
    analysis_summary$source_info$download_time,
    analysis_summary$processing_summary$total_grid_regions,
    analysis_summary$processing_summary$total_roi_regions,
    analysis_summary$processing_summary$total_extracted_images,
    analysis_summary$source_info$original_file,
    json_path,
    ifelse(!is.null(grid_metadata) && nrow(grid_metadata) > 0, paste("- Grid metadata:", file.path(output_dir, "grid_extractions_metadata.csv"), "\n"), ""),
    ifelse(!is.null(roi_metadata) && nrow(roi_metadata) > 0, paste("- ROI metadata:", file.path(output_dir, "roi_extractions_metadata.csv"), "\n"), "")
  )

  report_path <- file.path(output_dir, "analysis_report.md")
  writeLines(report_text, report_path)

  message("=== ANALYSIS COMPLETE ===")
  message("Report saved to: ", report_path)
  message("Total files generated: ", analysis_summary$processing_summary$total_extracted_images + 3)

  return(analysis_summary)
}

#' Main processing workflow (returns structured result)
process_archaeological_image <- function(image_url,
                                        output_base_dir = "archaeological_analysis",
                                        grid_rows = 4,
                                        grid_cols = 4,
                                        zoom_factor = 2.5,
                                        extract_grid = TRUE,
                                        extract_roi = TRUE,
                                        verbose = TRUE) {

  if (!dir.exists(output_base_dir)) dir.create(output_base_dir, recursive = TRUE)

  original_dir <- file.path(output_base_dir, "original")
  original_analysis <- download_and_analyze_image(image_url, output_dir = original_dir, verbose = verbose)

  grid_metadata <- NULL
  if (isTRUE(extract_grid)) {
    grid_metadata <- extract_grid_regions(
      img = original_analysis$image,
      grid_rows = grid_rows,
      grid_cols = grid_cols,
      overlap_percent = 10,
      zoom_factor = zoom_factor,
      output_dir = file.path(output_base_dir, "grid_regions")
    )
  }

  roi_metadata <- NULL
  if (isTRUE(extract_roi)) {
    roi_metadata <- extract_regions_of_interest(
      img = original_analysis$image,
      detection_method = "edges",
      output_dir = file.path(output_base_dir, "roi_regions")
    )
  }

  reports_dir <- file.path(output_base_dir, "reports")
  final_report <- create_analysis_report(
    original_analysis = original_analysis,
    grid_metadata = grid_metadata,
    roi_metadata = roi_metadata,
    output_dir = reports_dir
  )

  invisible(list(
    original = original_analysis,
    grid_metadata = grid_metadata,
    roi_metadata = roi_metadata,
    report = final_report,
    output_base_dir = output_base_dir
  ))
}

# Example usage instructions (print only)
archaeological_image_url <- "https://www.hethport.adwmainz.de/AlTfot/mousepic.php?ori=&po=0&si=100&bildnr=AlTqi35&fundnr=AlT%20%20%206&xy=eafdff79816e85ca8865c6ef65dfd641"

message("=== READY TO PROCESS ARCHAEOLOGICAL IMAGE ===")
message("URL: ", archaeological_image_url)
message("To run: result <- process_archaeological_image(image_url = '...url...', output_base_dir = 'alalach_tablet_analysis', grid_rows = 5, grid_cols = 5, zoom_factor = 3)")