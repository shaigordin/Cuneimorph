# PDF Image Extraction with Metadata in R
# Reproducible solution for extracting images at 300 DPI with detailed metadata

# Ensure demo blocks only run when intended
RUN_DEMOS <- !identical(Sys.getenv("PDF_EXTRACTOR_LOAD_ONLY"), "1")

# Required packages (do NOT auto-install; fail with instructions if missing)
required_packages <- c("pdftools", "magick", "dplyr", "tibble")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop(sprintf(
    "Missing required R packages: %s\nInstall them and re-run:\n  install.packages(c(%s))",
    paste(missing_packages, collapse = ", "),
    paste(sprintf('"%s"', missing_packages), collapse = ", ")
  ), call. = FALSE)
}
# Attach packages (keep using library() elsewhere in the script)
invisible(lapply(required_packages, function(p) library(p, character.only = TRUE)))

# -- Text extraction helpers (must be defined before they are used) ------------
parse_pdfimages_list <- function(pdf_path) {
  bin <- Sys.which("pdfimages")
  if (!nzchar(bin)) return(tibble(num = integer(), page = integer(), x = NA_real_, y = NA_real_, dx = NA_real_, dy = NA_real_))
  out <- tryCatch(system2(bin, c("-list", shQuote(pdf_path)), stdout = TRUE, stderr = TRUE), error = function(e) character(0))
  if (!length(out)) return(tibble(num = integer(), page = integer(), x = NA_real_, y = NA_real_, dx = NA_real_, dy = NA_real_))

  h <- which(grepl("^\\s*page\\s+", tolower(out)))
  if (!length(h)) return(tibble(num = integer(), page = integer(), x = NA_real_, y = NA_real_, dx = NA_real_, dy = NA_real_))
  header <- strsplit(gsub("[^[:alnum:]-]+", " ", tolower(out[h[1]])), "\\s+")[[1]]
  header <- header[nzchar(header)]
  idx_page <- match("page", header)
  idx_num  <- match("num",  header)
  find_idx <- function(names, targets) {
    idx <- match(targets, names)
    if (all(!is.na(idx))) return(idx)
    sapply(targets, function(t) { k <- which(startsWith(names, t)); if (length(k)) k[1] else NA_integer_ })
  }
  idx_xywh <- find_idx(header, c("x", "y", "dx", "dy"))

  lines <- out[(h[1] + 1):length(out)]
  lines <- lines[!grepl("^[-=]+\\s*$", lines)]
  lines <- lines[grepl("^\\s*\\d+\\s", lines)]
  if (!length(lines)) return(tibble(num = integer(), page = integer(), x = NA_real_, y = NA_real_, dx = NA_real_, dy = NA_real_))

  parse_line <- function(line) {
    toks <- strsplit(gsub("[^[:alnum:].+-]+", " ", line), "\\s+")[[1]]
    toks <- toks[nzchar(toks)]
    val <- function(i) if (!is.na(i) && i <= length(toks)) toks[[i]] else NA_character_
    tibble(
      page = suppressWarnings(as.integer(val(idx_page))),
      num  = suppressWarnings(as.integer(val(idx_num))),
      x    = suppressWarnings(as.numeric(val(idx_xywh[1]))),
      y    = suppressWarnings(as.numeric(val(idx_xywh[2]))),
      dx   = suppressWarnings(as.numeric(val(idx_xywh[3]))),
      dy   = suppressWarnings(as.numeric(val(idx_xywh[4])))
    )
  }
  dplyr::bind_rows(lapply(lines, parse_line)) |> dplyr::filter(!is.na(num), !is.na(page))
}

get_words_by_page <- function(pdf_path) {
  out <- pdftools::pdf_data(pdf_path)
  lapply(out, tibble::as_tibble)
}

words_near_bbox <- function(words_df, x0, y0, x1, y1, margin = 20) {
  if (any(is.na(c(x0, y0, x1, y1))) || nrow(words_df) == 0) return(NA_character_)
  xx0 <- x0 - margin; yy0 <- y0 - margin; xx1 <- x1 + margin; yy1 <- y1 + margin
  inside <- with(words_df, (x + width) >= xx0 & x <= xx1 & (y + height) >= yy0 & y <= yy1)
  texts <- words_df$text[inside]
  if (!length(texts)) "" else paste(texts, collapse = " ")
}

get_page_text <- function(pdf_path) {
  txt <- pdftools::pdf_text(pdf_path)
  if (length(txt)) txt else character(0)
}

# Helpers
has_pdfimages <- function() nzchar(Sys.which("pdfimages"))

safe_magick_read <- function(path) {
  tryCatch(magick::image_read(path), error = function(e) NULL)
}

convert_jb2_to_png <- function(jb2_file, out_dir) {
  jbig2dec_bin <- Sys.which("jbig2dec")
  if (!nzchar(jbig2dec_bin)) return(character(0))
  base <- tools::file_path_sans_ext(basename(jb2_file))
  out_base <- file.path(out_dir, paste0(base, "_jb2"))
  tryCatch({
    system2(jbig2dec_bin, c("-t", "png", "-o", out_base, shQuote(jb2_file)),
            stdout = TRUE, stderr = TRUE)
    list.files(out_dir, pattern = paste0("^", basename(out_base), ".*\\.(png)$"),
               full.names = TRUE, ignore.case = TRUE)
  }, error = function(e) character(0))
}

# --- NEW: central preparation of extracted images (pdfimages + JBIG2 handling + dedupe) ---
prepare_extracted_images <- function(pdf_path, temp_dir, convert_jb2 = TRUE) {
  pdfimages_bin <- Sys.which("pdfimages")
  if (!nzchar(pdfimages_bin)) stop("pdfimages not found")
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  # run pdfimages -all -> files like img-000.ppm / img-000.jpg / img-000.jb2
  tryCatch(
    system2(pdfimages_bin, c("-all", shQuote(pdf_path), file.path(temp_dir, "img")), stdout = TRUE, stderr = TRUE),
    error = function(e) NULL
  )
  files <- list.files(temp_dir, full.names = TRUE)
  if (!length(files)) return(list(targets_tbl = tibble(), skipped = character(0)))

  targets <- character(0)
  origs   <- character(0)
  skipped <- character(0)

  for (f in files) {
    ext <- tolower(tools::file_ext(f))
    if (ext %in% c("jb2","jbig2")) {
      if (isTRUE(convert_jb2)) {
        conv <- convert_jb2_to_png(f, temp_dir)
        if (length(conv)) {
          targets <- c(targets, conv)
          origs   <- c(origs, rep(f, length(conv)))
        } else {
          skipped <- c(skipped, f)
        }
      } else {
        skipped <- c(skipped, f)
      }
    } else {
      targets <- c(targets, f)
      origs   <- c(origs, f)
    }
  }

  if (!length(targets)) {
    return(list(targets_tbl = tibble(), skipped = skipped))
  }

  # deduplicate by content hash (prevent duplicates from conversions)
  hashes <- tools::md5sum(targets)
  keep <- !duplicated(hashes)
  targets <- targets[keep]
  origs   <- origs[keep]

  # Map image numbers -> page using pdfimages -list
  img_map <- parse_pdfimages_list(pdf_path)

  img_nums <- vapply(origs, function(o) {
    m <- regexpr("\\d+", basename(o))
    if (m[1] > 0) as.integer(regmatches(basename(o), m)) else NA_integer_
  }, integer(1))

  page_nos <- vapply(img_nums, function(n) {
    if (is.na(n)) return(NA_integer_)
    row <- img_map %>% dplyr::filter(num == n) %>% dplyr::slice_head(n = 1)
    if (nrow(row)) row$page[1] else NA_integer_
  }, integer(1))

  tib <- tibble(
    target = targets,
    orig   = origs,
    ext    = tolower(tools::file_ext(targets)),
    img_num = img_nums,
    page_no = page_nos
  )

  # stable deterministic order
  tib <- tib %>% arrange(basename(target))
  list(targets_tbl = tib, skipped = skipped)
}

# --- CLEANING helper: normalize metadata before writing ---------------------
write_clean_metadata_csv <- function(df, path) {
  if (is.null(df) || (is.data.frame(df) && nrow(df) == 0)) {
    # write header if df has columns, else create empty file
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
    s <- gsub("[[:space:]]+", " ", s)   # collapse whitespace (spaces, tabs, etc.)
    s <- trimws(s)                      # trim leading/trailing space
    if (nzchar(s) && tolower(s) %in% c("null", "nan")) s <- ""  # normalize NULL/NaN
    s
  }
  cleaned <- as.data.frame(lapply(df, function(col) vapply(col, clean_cell, FUN.VALUE = character(1))),
                           stringsAsFactors = FALSE, check.names = FALSE)
  keep <- apply(cleaned, 1, function(r) any(nzchar(r)))
  cleaned <- cleaned[keep, , drop = FALSE]
  utils::write.csv(cleaned, file = path, row.names = FALSE, na = "")
  invisible(path)
}

#' Extract images from PDF with detailed metadata
#'
#' @param pdf_path Character. Path to the PDF file
#' @param output_dir Character. Directory to save extracted images (default: current directory)
#' @param dpi Numeric. DPI for image extraction (default: 300)
#' @param format Character. Output image format (default: "png")
#' @param method Character. Extraction method: "render" (render pages) or "extract" (extract embedded images)
#' @param text_margin Numeric. Margin in pixels for text extraction around images (default: 40)
#' @return Data frame with image metadata
extract_pdf_images <- function(pdf_path,
                               output_dir = ".",
                               dpi = 300,
                               format = "png",
                               method = "render",
                               text_margin = 40,
                               write_clean = TRUE) {  # <-- added write_clean

  # Validate inputs
  if (!file.exists(pdf_path)) {
    stop("PDF file not found: ", pdf_path)
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Get PDF info
  pdf_info <- pdf_info(pdf_path)
  cat("Processing PDF:", basename(pdf_path), "\n")
  cat("Pages:", pdf_info$pages, "\n")
  cat("Method:", method, "\n\n")

  # Initialize metadata data frame
  image_metadata <- tibble()

  # Preload text data once
  words_by_page <- get_words_by_page(pdf_path)
  page_texts <- get_page_text(pdf_path)

  if (method == "render") {
    # Method 1: Render each page as image at specified DPI
    images <- list()
    for (i in seq_len(pdf_info$pages)) {
      images[[i]] <- magick::image_read(
        pdftools::pdf_render_page(pdf_path, page = i, dpi = dpi)
      )
    }

    # padding width so filenames sort numerically (e.g. page_001, page_010, page_100)
    pad_width <- max(3, nchar(as.character(pdf_info$pages)))
    for (i in seq_along(images)) {
      # Create filename with zero-padding
      filename <- file.path(output_dir, sprintf(paste0("page_%0", pad_width, "d.", format), i))

      # Save image
      magick::image_write(images[[i]], filename, format = format)

      # Get image info using magick
      img_info <- magick::image_info(images[[i]])

      # Calculate file size
      file_size <- file.info(filename)$size

      # Add to metadata
      associated_text <- if (length(page_texts) >= i) page_texts[[i]] else NA_character_

      image_metadata <- bind_rows(image_metadata, tibble(
        source_pdf = basename(pdf_path),
        page_number = i,
        filename = basename(filename),
        full_path = filename,
        method = "page_render",
        width_pixels = img_info$width,
        height_pixels = img_info$height,
        dpi_x = dpi,
        dpi_y = dpi,
        width_cm = round(img_info$width / dpi * 2.54, 2),
        height_cm = round(img_info$height / dpi * 2.54, 2),
        format = toupper(format),
        colorspace = img_info$colorspace,
        file_size_bytes = file_size,
        file_size_mb = round(file_size / 1024^2, 3),
        extraction_timestamp = Sys.time(),
        text_near_image = associated_text
      ))
    }

  } else if (method == "extract") {
    # Method 2: Extract embedded images (centralized handling)
    temp_dir <- file.path(output_dir, "temp_extracted")
    prep <- prepare_extracted_images(pdf_path, temp_dir, convert_jb2 = TRUE)
    targets_tbl <- prep$targets_tbl
    skipped_jb2 <- prep$skipped

    if (nrow(targets_tbl) == 0) {
      # Only JBIG2/skipped entries (or nothing) -> record skipped metadata and exit
      if (length(skipped_jb2)) {
        img_map <- parse_pdfimages_list(pdf_path)
        for (s in skipped_jb2) {
          fn_orig <- basename(s)
          m <- regexpr("\\d+", fn_orig)
          img_num <- if (m[1] > 0) suppressWarnings(as.integer(regmatches(fn_orig, m))) else NA_integer_
          map_row <- if (!is.na(img_num)) dplyr::filter(img_map, num == img_num) %>% dplyr::slice_head(n = 1) else NULL
          page_no <- if (!is.null(map_row) && nrow(map_row)) map_row$page[1] else NA_integer_
          image_metadata <- bind_rows(image_metadata, tibble(
            source_pdf = basename(pdf_path),
            page_number = page_no,
            filename = basename(s),
            full_path = s,
            method = "embedded_extract",
            width_pixels = NA_integer_, height_pixels = NA_integer_,
            dpi_x = NA_real_, dpi_y = NA_real_,
            width_cm = NA_real_, height_cm = NA_real_,
            format = toupper(tools::file_ext(s)),
            colorspace = NA_character_,
            file_size_bytes = file.info(s)$size,
            file_size_mb = round(file.info(s)$size / 1024^2, 3),
            extraction_timestamp = Sys.time(),
            text_near_image = if (!is.na(page_no) && page_no <= length(page_texts)) page_texts[[page_no]] else NA_character_,
            status = "skipped_unsupported_jbig2"
          ))
        }
      }
      unlink(temp_dir, recursive = TRUE)
      if (isTRUE(write_clean) && nrow(image_metadata) > 0) {
        write_clean_metadata_csv(image_metadata, file.path(output_dir, "extract_metadata.csv"))
      }
      return(image_metadata)
    }

    pad_width <- max(3, nchar(as.character(nrow(targets_tbl))))
    pdf_base <- tools::file_path_sans_ext(basename(pdf_path))
    idx <- 0L

    # Loop targets in deterministic order
    for (r in seq_len(nrow(targets_tbl))) {
      idx <- idx + 1L
      row <- targets_tbl[r, ]
      src_target <- row$target
      src_orig   <- row$orig
      page_no    <- row$page_no
      out_ext    <- row$ext

      img <- safe_magick_read(src_target)
      if (is.null(img)) {
        image_metadata <- bind_rows(image_metadata, tibble(
          source_pdf = basename(pdf_path),
          page_number = page_no,
          filename = basename(src_target),
          full_path = src_target,
          method = "embedded_extract",
          width_pixels = NA_integer_, height_pixels = NA_integer_,
          dpi_x = NA_real_, dpi_y = NA_real_,
          width_cm = NA_real_, height_cm = NA_real_,
          format = toupper(out_ext),
          colorspace = NA_character_,
          file_size_bytes = ifelse(file.exists(src_target), file.info(src_target)$size, NA_integer_),
          file_size_mb = ifelse(file.exists(src_target), round(file.info(src_target)$size / 1024^2, 3), NA_real_),
          extraction_timestamp = Sys.time(),
          text_near_image = if (!is.na(page_no) && page_no <= length(page_texts)) page_texts[[page_no]] else NA_character_,
          status = "read_error"
        ))
        next
      }

      img_info <- magick::image_info(img)

      out_filename <- sprintf(paste0("%s_img_%0", pad_width, "d.", out_ext), pdf_base, idx)
      out_path <- file.path(output_dir, out_filename)

      # Optionally resize if requested DPI != 72 (only for raster formats)
      if (dpi != 72 && out_ext %in% c("png","jpg","jpeg","tiff","tif")) {
        scale_factor <- dpi / 72
        new_width  <- round(img_info$width * scale_factor)
        new_height <- round(img_info$height * scale_factor)
        img <- magick::image_resize(img, sprintf("%dx%d", new_width, new_height))
        img_info <- magick::image_info(img)
      }

      # Write final image (preserve extension)
      magick::image_write(img, out_path, format = out_ext, density = sprintf("%dx%d", dpi, dpi))
      file_size <- file.info(out_path)$size

      orig_ext <- tolower(tools::file_ext(src_orig))
      status_val <- if (grepl("^jb2(e)?$|^jbig2$", orig_ext)) "converted_from_jbig2" else "ok"

      # Find nearby text using words_by_page / img_map mapping (if available)
      near_txt <- NA_character_
      img_map <- parse_pdfimages_list(pdf_path)
      if (!is.na(page_no) && page_no <= length(words_by_page)) {
        # attempt bbox-based if mapping exists
        map_row <- NULL
        if (!is.null(row$img_num) && !is.na(row$img_num)) {
          map_row <- img_map %>% dplyr::filter(num == row$img_num) %>% dplyr::slice_head(n = 1)
        }
        if (!is.null(map_row) && nrow(map_row) && !any(is.na(map_row[1, c("x","y","dx","dy")]))) {
          x0 <- map_row$x[1]; y0 <- map_row$y[1]; dx <- map_row$dx[1]; dy <- map_row$dy[1]
          near_txt <- words_near_bbox(words_by_page[[page_no]], x0, y0, x0 + dx, y0 + dy, margin = text_margin)
        } else {
          near_txt <- page_texts[[page_no]]
        }
      }

      image_metadata <- bind_rows(image_metadata, tibble(
        source_pdf = basename(pdf_path),
        page_number = page_no,
        filename = basename(out_path),
        full_path = out_path,
        method = "embedded_extract",
        width_pixels = img_info$width,
        height_pixels = img_info$height,
        dpi_x = dpi,
        dpi_y = dpi,
        width_cm = round(img_info$width / dpi * 2.54, 2),
        height_cm = round(img_info$height / dpi * 2.54, 2),
        format = toupper(out_ext),
        colorspace = img_info$colorspace,
        file_size_bytes = file_size,
        file_size_mb = round(file_size / 1024^2, 3),
        extraction_timestamp = Sys.time(),
        text_near_image = near_txt,
        status = status_val
      ))
    }

    # Record skipped JBIG2 entries in metadata (they were not saved)
    if (length(skipped_jb2)) {
      img_map <- parse_pdfimages_list(pdf_path)
      for (s in skipped_jb2) {
        fn_orig <- basename(s)
        m <- regexpr("\\d+", fn_orig)
        img_num <- if (m[1] > 0) suppressWarnings(as.integer(regmatches(fn_orig, m))) else NA_integer_
        map_row <- if (!is.na(img_num)) dplyr::filter(img_map, num == img_num) %>% dplyr::slice_head(n = 1) else NULL
        page_no <- if (!is.null(map_row) && nrow(map_row)) map_row$page[1] else NA_integer_
        image_metadata <- bind_rows(image_metadata, tibble(
          source_pdf = basename(pdf_path),
          page_number = page_no,
          filename = basename(s),
          full_path = s,
          method = "embedded_extract",
          width_pixels = NA_integer_, height_pixels = NA_integer_,
          dpi_x = NA_real_, dpi_y = NA_real_,
          width_cm = NA_real_, height_cm = NA_real_,
          format = toupper(tools::file_ext(s)),
          colorspace = NA_character_,
          file_size_bytes = file.info(s)$size,
          file_size_mb = round(file.info(s)$size / 1024^2, 3),
          extraction_timestamp = Sys.time(),
          text_near_image = if (!is.na(page_no) && page_no <= length(page_texts)) page_texts[[page_no]] else NA_character_,
          status = "skipped_unsupported_jbig2"
        ))
      }
    }

    # Clean up and write cleaned metadata
    unlink(temp_dir, recursive = TRUE)
    if (isTRUE(write_clean) && nrow(image_metadata) > 0) {
      write_clean_metadata_csv(image_metadata, file.path(output_dir, "extract_metadata.csv"))
    }
  }

  # write cleaned metadata automatically if requested
  if (isTRUE(write_clean) && nrow(image_metadata) > 0) {
    meta_fname <- switch(
      method,
      render = file.path(output_dir, "render_metadata.csv"),
      extract = file.path(output_dir, "extract_metadata.csv"),
      file.path(output_dir, paste0(method, "_metadata.csv"))
    )
    write_clean_metadata_csv(image_metadata, meta_fname)
  }

  return(image_metadata)
}

#' Extract embedded images from PDF and preserve original DPI
#'
#' @param pdf_path Character. Path to the PDF file
#' @param output_dir Character. Directory to save extracted images
#' @return Data frame with image metadata, including original DPI
extract_embedded_images_preserve_dpi <- function(pdf_path, output_dir, text_margin = 40, write_clean = TRUE) {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  temp_dir <- file.path(output_dir, "temp_embedded")
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

  prep <- prepare_extracted_images(pdf_path, temp_dir, convert_jb2 = TRUE)
  targets_tbl <- prep$targets_tbl
  skipped_jb2 <- prep$skipped

  metadata <- tibble()
  words_by_page <- get_words_by_page(pdf_path)
  page_texts <- get_page_text(pdf_path)
  img_map <- parse_pdfimages_list(pdf_path)

  if (nrow(targets_tbl) == 0) {
    # record skipped JBIG2 entries and exit
    if (length(skipped_jb2)) {
      for (s in skipped_jb2) {
        fn_orig <- basename(s)
        m <- regexpr("\\d+", fn_orig)
        img_num <- if (m[1] > 0) suppressWarnings(as.integer(regmatches(fn_orig, m))) else NA_integer_
        map_row <- if (!is.na(img_num)) dplyr::filter(img_map, num == img_num) %>% dplyr::slice_head(n = 1) else NULL
        page_no <- if (!is.null(map_row) && nrow(map_row)) map_row$page[1] else NA_integer_
        metadata <- bind_rows(metadata, tibble(
          source_pdf = basename(pdf_path),
          page_number = page_no,
          filename = basename(s),
          full_path = s,
          method = "embedded_extract_preserve_dpi",
          width_pixels = NA_integer_, height_pixels = NA_integer_,
          dpi_x = NA_real_, dpi_y = NA_real_,
          width_cm = NA_real_, height_cm = NA_real_,
          format = toupper(tools::file_ext(s)),
          colorspace = NA_character_,
          file_size_bytes = file.info(s)$size,
          file_size_mb = round(file.info(s)$size / 1024^2, 3),
          extraction_timestamp = Sys.time(),
          status = "skipped_unsupported_jbig2",
          text_near_image = if (!is.na(page_no) && page_no <= length(page_texts)) page_texts[[page_no]] else NA_character_
        ))
      }
    }
    unlink(temp_dir, recursive = TRUE)
    if (isTRUE(write_clean) && nrow(metadata) > 0) {
      write_clean_metadata_csv(metadata, file.path(output_dir, "extract_preserve_dpi_metadata.csv"))
    }
    return(metadata)
  }

  pad_width <- max(3, nchar(as.character(nrow(targets_tbl))))
  pdf_base <- tools::file_path_sans_ext(basename(pdf_path))
  idx <- 0L

  for (r in seq_len(nrow(targets_tbl))) {
    idx <- idx + 1L
    row <- targets_tbl[r, ]
    src_target <- row$target
    src_orig   <- row$orig
    page_no    <- row$page_no
    out_ext    <- row$ext

    img <- safe_magick_read(src_target)
    if (is.null(img)) {
      metadata <- bind_rows(metadata, tibble(
        source_pdf = basename(pdf_path),
        page_number = page_no,
        filename = basename(src_target),
        full_path = src_target,
        method = "embedded_extract_preserve_dpi",
        width_pixels = NA_integer_, height_pixels = NA_integer_,
        dpi_x = NA_real_, dpi_y = NA_real_,
        width_cm = NA_real_, height_cm = NA_real_,
        format = toupper(out_ext),
        colorspace = NA_character_,
        file_size_bytes = ifelse(file.exists(src_target), file.info(src_target)$size, NA_integer_),
        file_size_mb = ifelse(file.exists(src_target), round(file.info(src_target)$size / 1024^2, 3), NA_real_),
        extraction_timestamp = Sys.time(),
        status = "read_error"
      ))
      next
    }

    info <- magick::image_info(img)
    dens <- info$density
    dpi_x <- suppressWarnings(as.numeric(strsplit(ifelse(is.null(dens), "", dens), "x")[[1]][1]))
    dpi_y <- suppressWarnings(as.numeric(strsplit(ifelse(is.null(dens), "", dens), "x")[[1]][2]))
    if (is.na(dpi_x)) dpi_x <- NA_real_
    if (is.na(dpi_y)) dpi_y <- dpi_x

    out_filename <- sprintf(paste0("%s_img_%0", pad_width, "d.", out_ext), pdf_base, idx)
    out_path <- file.path(output_dir, out_filename)

    if (!is.null(dens) && nzchar(dens)) {
      magick::image_write(img, out_path, format = out_ext, density = dens)
    } else {
      magick::image_write(img, out_path, format = out_ext)
    }

    fs <- file.info(out_path)$size
    orig_ext <- tolower(tools::file_ext(src_orig))
    status_val <- if (grepl("^jb2(e)?$|^jbig2$", orig_ext)) "converted_from_jbig2" else "ok"

    near_txt <- NA_character_
    if (!is.na(page_no) && page_no <= length(words_by_page)) {
      map_row <- NULL
      if (!is.null(row$img_num) && !is.na(row$img_num)) {
        map_row <- img_map %>% dplyr::filter(num == row$img_num) %>% dplyr::slice_head(n = 1)
      }
      if (!is.null(map_row) && nrow(map_row) && !any(is.na(map_row[1, c("x","y","dx","dy")]))) {
        x0 <- map_row$x[1]; y0 <- map_row$y[1]; dx <- map_row$dx[1]; dy <- map_row$dy[1]
        near_txt <- words_near_bbox(words_by_page[[page_no]], x0, y0, x0 + dx, y0 + dy, margin = text_margin)
      } else {
        near_txt <- page_texts[[page_no]]
      }
    }

    metadata <- bind_rows(metadata, tibble(
      source_pdf = basename(pdf_path),
      page_number = page_no,
      filename = basename(out_path),
      full_path = out_path,
      method = "embedded_extract_preserve_dpi",
      width_pixels = info$width, height_pixels = info$height,
      dpi_x = dpi_x, dpi_y = dpi_y,
      width_cm = if (!is.na(dpi_x)) round(info$width / dpi_x * 2.54, 2) else NA_real_,
      height_cm = if (!is.na(dpi_y)) round(info$height / dpi_y * 2.54, 2) else NA_real_,
      format = toupper(info$format),
      colorspace = info$colorspace,
      file_size_bytes = fs,
      file_size_mb = round(fs / 1024^2, 3),
      extraction_timestamp = Sys.time(),
      status = status_val,
      text_near_image = near_txt
    ))
  }

  # Record skipped JBIG2 entries (if any)
  if (length(skipped_jb2)) {
    for (s in skipped_jb2) {
      fn_orig <- basename(s)
      m <- regexpr("\\d+", fn_orig)
      img_num <- if (m[1] > 0) suppressWarnings(as.integer(regmatches(fn_orig, m))) else NA_integer_
      map_row <- if (!is.na(img_num)) dplyr::filter(img_map, num == img_num) %>% dplyr::slice_head(n = 1) else NULL
      page_no <- if (!is.null(map_row) && nrow(map_row)) map_row$page[1] else NA_integer_
      metadata <- bind_rows(metadata, tibble(
        source_pdf = basename(pdf_path),
        page_number = page_no,
        filename = basename(s),
        full_path = s,
        method = "embedded_extract_preserve_dpi",
        width_pixels = NA_integer_, height_pixels = NA_integer_,
        dpi_x = NA_real_, dpi_y = NA_real_,
        width_cm = NA_real_, height_cm = NA_real_,
        format = toupper(tools::file_ext(s)),
        colorspace = NA_character_,
        file_size_bytes = file.info(s)$size,
        file_size_mb = round(file.info(s)$size / 1024^2, 3),
        extraction_timestamp = Sys.time(),
        status = "skipped_unsupported_jbig2",
        text_near_image = if (!is.na(page_no) && page_no <= length(page_texts)) page_texts[[page_no]] else NA_character_
      ))
    }
  }

  unlink(temp_dir, recursive = TRUE)
  if (isTRUE(write_clean) && nrow(metadata) > 0) {
    write_clean_metadata_csv(metadata, file.path(output_dir, "extract_preserve_dpi_metadata.csv"))
  }
  metadata
}

#' Display summary of extracted images
#'
#' @param metadata Data frame returned by extract_pdf_images
display_extraction_summary <- function(metadata) {
  cat("=== IMAGE EXTRACTION SUMMARY ===\n")
  cat("Total images extracted:", nrow(metadata), "\n")
  cat("Total file size:", sum(metadata$file_size_mb), "MB\n")
  cat("Average dimensions:",
      round(mean(metadata$width_pixels)), "x",
      round(mean(metadata$height_pixels)), "pixels\n")
  cat("Formats:", paste(unique(metadata$format), collapse = ", "), "\n")
  cat("DPI:", unique(metadata$dpi_x)[1], "\n\n")

  # Display detailed table
  print(metadata %>%
    select(filename, width_pixels, height_pixels, dpi_x, format,
           file_size_mb, method) %>%
    arrange(filename))
}

#' Compare image quality between methods
#'
#' @param metadata_list List of metadata data frames from different methods
compare_extraction_methods <- function(metadata_list) {
  method_names <- names(metadata_list)
  if (is.null(method_names)) {
    method_names <- paste("Method", seq_along(metadata_list))
  }

  cat("=== METHOD COMPARISON ===\n")
  for (i in seq_along(metadata_list)) {
    meta <- metadata_list[[i]]
    if (nrow(meta) > 0) {
      cat(sprintf("%s:\n", method_names[i]))
      cat(sprintf("  Images: %d\n", nrow(meta)))
      cat(sprintf("  Avg size: %.2f MB\n", mean(meta$file_size_mb)))
      cat(sprintf("  Avg dimensions: %dx%d\n",
                  round(mean(meta$width_pixels)),
                  round(mean(meta$height_pixels))))
      cat("\n")
    }
  }
}

#' Batch process multiple PDFs
#'
#' @param pdf_directory Character. Directory containing PDF files
#' @param output_base_dir Character. Base directory for output
#' @param dpi Numeric. DPI for extraction
#' @param method Character. Extraction method
#' @return List of metadata data frames
batch_extract_pdfs <- function(pdf_directory,
                               output_base_dir = "batch_extracted",
                               dpi = 300,
                               method = "render") {

  # Find all PDF files
  pdf_files <- list.files(pdf_directory, pattern = "\\.pdf$",
                         full.names = TRUE, ignore.case = TRUE)

  if (length(pdf_files) == 0) {
    stop("No PDF files found in directory: ", pdf_directory)
  }

  cat("Found", length(pdf_files), "PDF files to process\n\n")

  # Create base output directory
  if (!dir.exists(output_base_dir)) {
    dir.create(output_base_dir, recursive = TRUE)
  }

  # Initialize results list
  all_metadata <- list()

  # Process each PDF
  for (i in seq_along(pdf_files)) {
    pdf_file <- pdf_files[i]
    pdf_name <- tools::file_path_sans_ext(basename(pdf_file))

    cat(sprintf("Processing %d/%d: %s\n", i, length(pdf_files), pdf_name))

    # Create individual output directory
    output_dir <- file.path(output_base_dir, pdf_name)

    tryCatch({
      # Extract images
      metadata <- extract_pdf_images(
        pdf_path = pdf_file,
        output_dir = output_dir,
        dpi = dpi,
        method = method
      )

      # Add PDF filename to metadata
      metadata$pdf_file <- pdf_name

      # Store in results
      all_metadata[[pdf_name]] <- metadata

      cat(sprintf("  Extracted %d images\n", nrow(metadata)))

    }, error = function(e) {
      cat(sprintf("  ERROR: %s\n", e$message))
      all_metadata[[pdf_name]] <- tibble()
    })

    cat("\n")
  }

  # Combine all metadata
  combined_metadata <- bind_rows(all_metadata, .id = "pdf_source")

  # Save combined metadata
  metadata_file <- file.path(output_base_dir, "batch_extraction_metadata.csv")
  write_clean_metadata_csv(combined_metadata, metadata_file)

  cat(sprintf("Batch processing complete!\n"))
  cat(sprintf("Combined metadata saved to: %s\n", metadata_file))
  cat(sprintf("Total images extracted: %d\n", nrow(combined_metadata)))

  return(all_metadata)
}

#' Generate extraction report
#'
#' @param metadata Data frame with extraction metadata
#' @param output_file Character. Path for HTML report (optional)
generate_extraction_report <- function(metadata, output_file = NULL) {

  # Calculate summary statistics
  total_images <- nrow(metadata)
  total_size_mb <- sum(metadata$file_size_mb)
  avg_width <- round(mean(metadata$width_pixels))
  avg_height <- round(mean(metadata$height_pixels))
  formats <- paste(unique(metadata$format), collapse = ", ")

  # Create report text
  report <- sprintf("
# PDF Image Extraction Report

Generated: %s

## Summary Statistics
- **Total Images Extracted:** %d
- **Total File Size:** %.2f MB
- **Average Dimensions:** %d x %d pixels
- **DPI:** %d
- **Formats:** %s
- **Extraction Method:** %s

## Size Distribution
- **Smallest Image:** %.3f MB (%s)
- **Largest Image:** %.3f MB (%s)
- **Median Size:** %.3f MB

## Detailed Results
",
    Sys.time(),
    total_images,
    total_size_mb,
    avg_width,
    avg_height,
    metadata$dpi_x[1],
    formats,
    metadata$method[1],
    min(metadata$file_size_mb),
    metadata$filename[which.min(metadata$file_size_mb)],
    max(metadata$file_size_mb),
    metadata$filename[which.max(metadata$file_size_mb)],
    median(metadata$file_size_mb)
  )

  # Print to console
  cat(report)

  # Optionally save to file
  if (!is.null(output_file)) {
    writeLines(report, output_file)
    cat(sprintf("\nReport saved to: %s\n", output_file))
  }

  # Return detailed table
  return(metadata %>%
    select(filename, width_pixels, height_pixels, file_size_mb, format, method) %>%
    arrange(desc(file_size_mb)))
}

# =============================================================================
# EXAMPLE USAGE AND DEMONSTRATIONS
# =============================================================================

cat("=== PDF IMAGE EXTRACTION TOOL ===\n")
cat("Complete R solution for extracting images from PDFs with detailed metadata\n\n")

# Example paths (replace with your actual files)
example_pdf <- "/Users/shaigordin/Dropbox/Git-projects/Cuneimorph/data/raw/images/EP2019-ScribOugaritPaleo.pdf"
output_directory <- "/Users/shaigordin/Dropbox/Git-projects/Cuneimorph/data/raw/images/EP_2019_Uga"

# Choose method
if (isTRUE(RUN_DEMOS)) {
  cat("EXAMPLE 1: Single PDF extraction (render method)\n")
  cat("Replace 'sample_document.pdf' with your PDF file path\n\n")

  metadata_render <- NULL
  metadata_extract <- NULL
  metadata_preserve_dpi <- NULL

  if (file.exists(example_pdf)) {
    # Extract using render method
    cat("Extracting images by rendering pages...\n")
    metadata_render <- extract_pdf_images(
      pdf_path = example_pdf,
      output_dir = file.path(output_directory, "render"),
      dpi = 300,
      format = "png",
      method = "render"
    )
    display_extraction_summary(metadata_render)
    cat("\n")
    generate_extraction_report(metadata_render)
    # Save metadata if not empty
    if (!is.null(metadata_render) && nrow(metadata_render) > 0) {
      write_clean_metadata_csv(metadata_render, file.path(output_directory, "render_metadata.csv"))
      cat("Metadata saved to:", file.path(output_directory, "render_metadata.csv"), "\n\n")
    }
  } else {
    cat("Sample PDF not found. Please update the 'example_pdf' variable with your PDF path.\n\n")
  }

  # Demonstration 2: Extract embedded images (if pdfimages available)
  cat("EXAMPLE 2: Extract embedded images (requires poppler-utils)\n")
  pdfimages_available <- has_pdfimages()

  if (file.exists(example_pdf) && pdfimages_available) {
    cat("pdfimages found - extracting embedded images...\n")
    metadata_extract <- extract_pdf_images(
      pdf_path = example_pdf,
      output_dir = file.path(output_directory, "extract"),
      dpi = 300,
      format = "png",
      method = "extract"
    )
    display_extraction_summary(metadata_extract)
    # Save metadata if not empty
    if (!is.null(metadata_extract) && nrow(metadata_extract) > 0) {
      write_clean_metadata_csv(metadata_extract, file.path(output_directory, "extract_metadata.csv"))
      cat("Metadata saved to:", file.path(output_directory, "extract_metadata.csv"), "\n\n")
    }
    # Compare methods if both were successful
    if (!is.null(metadata_render) && nrow(metadata_render) > 0 && nrow(metadata_extract) > 0) {
      cat("\n")
      compare_extraction_methods(list(
        "Render Method" = metadata_render,
        "Extract Method" = metadata_extract
      ))
    }
  } else if (!pdfimages_available) {
    cat("pdfimages not found. To use embedded extraction, install poppler-utils:\n")
    cat("  Ubuntu/Debian: sudo apt-get install poppler-utils\n")
    cat("  macOS: brew install poppler\n")
    cat("  Windows: Download from https://poppler.freedesktop.org/\n\n")
  }

  # Demonstration 3: Extract embedded images preserving original DPI
  cat("EXAMPLE 3: Extract embedded images preserving original DPI\n")
  # Ensure the flag exists even if this block runs alone
  pdfimages_available <- if (exists("pdfimages_available")) pdfimages_available else has_pdfimages()
  if (file.exists(example_pdf) && pdfimages_available) {
    cat("Extracting embedded images with original DPI...\n")
    metadata_preserve_dpi <- extract_embedded_images_preserve_dpi(
      pdf_path = example_pdf,
      output_dir = file.path(output_directory, "extract_preserve_dpi")
    )
    display_extraction_summary(metadata_preserve_dpi)
    # Save metadata if not empty
    if (!is.null(metadata_preserve_dpi) && nrow(metadata_preserve_dpi) > 0) {
      write_clean_metadata_csv(metadata_preserve_dpi, file.path(output_directory, "extract_preserve_dpi_metadata.csv"))
      cat("Metadata saved to:", file.path(output_directory, "extract_preserve_dpi_metadata.csv"), "\n\n")
    }
  } else if (!pdfimages_available) {
    cat("Cannot extract embedded images without pdfimages utility.\n")
  } else {
    cat("Sample PDF not found. Please update the 'example_pdf' variable with your PDF path.\n\n")
  }

  # Demonstration 4: Batch processing
  cat("EXAMPLE 4: Batch processing multiple PDFs\n")
  pdf_folder <- "pdf_documents"  # Replace with your folder path

  if (dir.exists(pdf_folder)) {
    cat("Processing all PDFs in folder:", pdf_folder, "\n")

    batch_results <- batch_extract_pdfs(
      pdf_directory = pdf_folder,
      output_base_dir = "batch_output",
      dpi = 300,
      method = "render"
    )

    # Display batch summary
    total_pdfs <- length(batch_results)
    total_images <- sum(sapply(batch_results, nrow))
    cat(sprintf("Batch complete: %d PDFs processed, %d images extracted\n", total_pdfs, total_images))

  } else {
    cat("Create a 'pdf_documents' folder with PDF files to test batch processing.\n")
  }
}

# Usage tips
cat("\n=== USAGE TIPS ===\n")
cat("1. For best quality: Use method='render' with dpi=300\n")
cat("2. For original images: Use method='extract' (requires poppler-utils)\n")
cat("3. Supported formats: 'png', 'jpg', 'tiff'\n")
cat("4. All metadata is automatically saved to CSV files\n")
cat("5. Use batch_extract_pdfs() for processing multiple files\n")
cat("6. Check the generated reports for detailed analysis\n\n")

cat("Script ready! Update the file paths and run the examples.\n")