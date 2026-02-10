#!/usr/bin/env Rscript
# filepath: analysis/scripts/hpm_konk_scrape.R
# install.packages(c("chromote", "rvest", "dplyr", "tidyr", "stringr", "purrr"))
# How to run: source("analysis/scripts/hpm_konk_scrape.R")

# Dependencies ----
if (!requireNamespace("chromote")) install.packages("chromote")
if (!requireNamespace("rvest")) install.packages("rvest")
if (!requireNamespace("dplyr")) install.packages("dplyr")
if (!requireNamespace("tidyr")) install.packages("tidyr")
if (!requireNamespace("stringr")) install.packages("stringr")
if (!requireNamespace("purrr")) install.packages("purrr")
library(chromote)
library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# ---- Auto-generate interactive prompt for all form fields ----
get_form_fields_and_prompt <- function(form_url) {
  doc <- read_html(form_url)
  form <- html_node(doc, "form")

  # Inputs (text, hidden, checkbox, radio, etc.)
  inputs <- html_nodes(form, "input")
  input_df <- tibble::tibble(
    name = html_attr(inputs, "name"),
    type = html_attr(inputs, "type"),
    value = html_attr(inputs, "value")
  ) %>% dplyr::filter(!is.na(name))

  # Selects
  selects <- html_nodes(form, "select")
  select_df <- tibble::tibble(
    name = html_attr(selects, "name"),
    options = purrr::map(selects, ~html_nodes(.x, "option") %>% html_attr("value")),
    labels = purrr::map(selects, ~html_nodes(.x, "option") %>% html_text(trim=TRUE))
  )

  # Textareas
  textareas <- html_nodes(form, "textarea")
  textarea_df <- tibble::tibble(
    name = html_attr(textareas, "name")
  )

  # Prompt user for each field
  cat("Enter your search criteria for the Hethiter Konkordanz form.\n(Leave blank to skip a field)\n")
  user_choices <- list()

  # Text/hidden inputs (skip submit/reset/button)
  for (i in seq_len(nrow(input_df))) {
    nm <- input_df$name[i]
    tp <- input_df$type[i]
    if (tp %in% c("submit", "reset", "button")) next
    if (tp == "checkbox") {
      ans <- readline(sprintf("%s (checkbox, y/n): ", nm))
      user_choices[[nm]] <- tolower(ans) %in% c("y", "yes", "1")
    } else if (tp == "radio") {
      # For radios, you may want to group by name and prompt once; here we prompt for value
      ans <- readline(sprintf("%s (radio, value): ", nm))
      user_choices[[nm]] <- ans
    } else {
      ans <- readline(sprintf("%s (type: %s): ", nm, tp))
      user_choices[[nm]] <- ans
    }
  }

  # Selects
  for (i in seq_len(nrow(select_df))) {
    nm <- select_df$name[i]
    opts <- select_df$options[[i]]
    labs <- select_df$labels[[i]]
    cat(sprintf("%s (dropdown):\n", nm))
    for (j in seq_along(opts)) {
      cat(sprintf("  [%d] %s (%s)\n", j, labs[j], opts[j]))
    }
    ans <- readline(sprintf("Choose option number for %s (blank to skip): ", nm))
    if (ans != "" && !is.na(as.integer(ans))) {
      idx <- as.integer(ans)
      if (idx >= 1 && idx <= length(opts)) {
        user_choices[[nm]] <- opts[idx]
      }
    }
  }

  # Textareas
  for (i in seq_len(nrow(textarea_df))) {
    nm <- textarea_df$name[i]
    ans <- readline(sprintf("%s (textarea): ", nm))
    user_choices[[nm]] <- ans
  }

  user_choices
}

# ---- Config ----
out_dir <- "data/raw/metadata/Hat"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
raw_html_path <- file.path(out_dir, "hpm_search_results.html")
clean_csv_path <- file.path(out_dir, "clean_hpm_search.csv")

# ---- Helper: Wait for selector ----
wait_for_selector <- function(session, selector, timeout = 120) {   # increased default timeout to 120s
  t0 <- Sys.time()
  repeat {
    res <- session$Runtime$evaluate(sprintf('!!document.querySelector("%s")', selector))
    if (isTRUE(res$result$value)) break
    if (as.numeric(Sys.time() - t0, units = "secs") > timeout)
      stop(sprintf("Timeout waiting for selector: %s", selector))
    Sys.sleep(0.5)  # slightly longer polling interval
  }
}

# ---- Step 1: Automated Navigation & Extraction ----
scrape_konkordanz_table <- function() {
  chrome <- ChromoteSession$new()
  on.exit(chrome$close(), add = TRUE)
  chrome$view()
  tab <- chrome$new_session()
  tab$view()

  # Navigate with an increased timeout (milliseconds)
  tab$Page$navigate("https://www.hethport.uni-wuerzburg.de/hetkonk/hetkonk_abfrageF.php", timeout = 120000)
  wait_for_selector(tab, "form")

  # Fill form fields from user_choices
  for (nm in names(user_choices)) {
    val <- user_choices[[nm]]
    if (isTRUE(val)) {
      tab$Runtime$evaluate(sprintf(
        'var cb = document.querySelector("[name=\'%s\']"); if (cb) cb.checked = true;', nm))
    } else if (isFALSE(val)) {
      tab$Runtime$evaluate(sprintf(
        'var cb = document.querySelector("[name=\'%s\']"); if (cb) cb.checked = false;', nm))
    } else if (!is.null(val) && val != "") {
      tab$Runtime$evaluate(sprintf(
        '
        var el = document.querySelector("[name=\'%s\']");
        if (el) {
          if (el.tagName === "SELECT") { el.value = "%s"; el.dispatchEvent(new Event("change", {bubbles:true})); }
          else if (el.type === "checkbox") { el.checked = %s; }
          else { el.value = "%s"; el.dispatchEvent(new Event("input", {bubbles:true})); }
        }
        ', nm, val, ifelse(val %in% c("y", "yes", "1", TRUE), "true", "false"), val))
    }
  }

  Sys.sleep(0.5)

  # Submit the form (first submit button)
  tab$Runtime$evaluate('
    var btn = document.querySelector("input[type=submit]");
    if (btn) btn.click();
  ')

  # Wait robustly for the results table to appear
  wait_for_selector(tab, "table", timeout = 120)
  Sys.sleep(2)

  # Extract the first table's HTML
  html <- tab$Runtime$evaluate('document.querySelector("table").outerHTML')$result$value
  writeLines(html, raw_html_path)

  # Clean up
  tab$close()
  chrome$close()

  html
}

# ---- Step 2: Parsing and Cleaning ----

# Mapping for partial links
base_urls <- list(
  "../hetskiz" = "https://www.hethport.uni-wuerzburg.de/hetskiz",
  "bildpraep.php" = "https://www.hethport.uni-wuerzburg.de/hetkonk/bildpraep.php",
  "3dprp.php" = "https://www.hethport.uni-wuerzburg.de/hetkonk/3dprp.php",
  "kbopraep.php" = "https://www.hethport.uni-wuerzburg.de/hetkonk/kbopraep.php",
  "../TLHdig" = "https://www.hethport.uni-wuerzburg.de/TLHdig",
  "hetkonk_abfrage.php" = "https://www.hethport.uni-wuerzburg.de/hetkonk/hetkonk_abfrage.php"
)

expand_links <- function(text, base_urls) {
  for (prefix in names(base_urls)) {
    pattern <- paste0("\\(", prefix, "([^)]*)\\)")
    replacement <- paste0("(", base_urls[[prefix]], "\\1)")
    text <- gsub(pattern, replacement, text, perl = TRUE)
  }
  text
}

extract_links <- function(cell) {
  links <- html_nodes(cell, "a")
  if (length(links) == 0) return(list(text = html_text(cell, trim = TRUE), links = character(0), titles = character(0)))
  link_texts <- html_text(links, trim = TRUE)
  link_hrefs <- html_attr(links, "href")
  link_titles <- html_attr(links, "title")
  list(
    text = html_text(cell, trim = TRUE),
    links = link_hrefs,
    link_texts = link_texts,
    link_titles = link_titles
  )
}

clean_publikation <- function(text) {
  if (grepl(" \\| ", text)) {
    parts <- unlist(strsplit(text, " \\| "))
    if (length(parts) >= 2 && startsWith(parts[2], parts[1])) {
      parts[2] <- sub(paste0("^", fixed(parts[1])), "", parts[2])
    }
    text <- paste(parts, collapse = " | ")
  }
  text
}

# ---- Improved Table Parsing: Extracts both text and links ----
parse_table <- function(html) {
  doc <- read_html(html, encoding = "UTF-8")
  table_node <- html_node(doc, "table")
  rows <- html_nodes(table_node, "tr")
  col_names <- c("Inventarnummer", "Joins", "Image_link", "Publikation", "CTH",
                 "Fundort", "Zeit", "TLHdig", "Anmerkungen")
  data_list <- rows[-1] %>% map(function(row) {
    cells <- html_nodes(row, "td")
    lapply(cells, extract_links)
  })
  # Build data.frames for text, links, and titles
  text_df <- as.data.frame(do.call(rbind, lapply(data_list, function(row) sapply(row, `[[`, "text"))), stringsAsFactors = FALSE)
  colnames(text_df) <- col_names
  links_df <- as.data.frame(do.call(rbind, lapply(data_list, function(row) sapply(row, function(x) paste(x$links, collapse = "; ")))), stringsAsFactors = FALSE)
  colnames(links_df) <- paste0(col_names, "_links")
  titles_df <- as.data.frame(do.call(rbind, lapply(data_list, function(row) sapply(row, function(x) paste(x$link_titles, collapse = "; ")))), stringsAsFactors = FALSE)
  colnames(titles_df) <- paste0(col_names, "_titles")
  cbind(text_df, links_df, titles_df)
}

# ---- Step 3: Improved Cleaning, Use extracted links ----

clean_konkordanz_df <- function(df) {
  # Clean Inventarnummer and CTH, fill down
  df <- df %>%
    mutate(
      Inventarnummer = ifelse(grepl("^\\s*$", Inventarnummer), NA, Inventarnummer),
      CTH = ifelse(grepl("^\\s*\\.?\\s*$", CTH), NA, CTH)
    ) %>%
    tidyr::fill(Inventarnummer, CTH) %>%
    mutate(across(everything(), str_trim))

  # Clean Publikation
  df$Publikation <- sapply(df$Publikation, clean_publikation)

  # Split Publikation links (from extracted links)
  df <- df %>%
    mutate(
      Publikation_link = ifelse(is.na(Publikation_links), NA, Publikation_links)
    )

  # Clean Joins and Image_link columns (use extracted links)
  df <- df %>%
    mutate(
      Joins_links = ifelse(is.na(Joins_links), "", Joins_links),
      Image_link_url = ifelse(is.na(Image_link_links), NA, Image_link_links)
    )

  # CTH: separate number and link (from extracted links)
  df <- df %>%
    mutate(
      CTH_num = str_extract(CTH, "^[0-9]+"),
      CTH_link = ifelse(is.na(CTH_links), NA, CTH_links)
    )

  # Fundort: split before/after colon
  df <- df %>%
    mutate(
      Fundort_room = str_trim(str_extract(Fundort, "^[^:]+")),
      Fundort_details = str_trim(str_replace(Fundort, "^[^:]+:", ""))
    )

  # TLHdig: clean link (from extracted links)
  df <- df %>%
    mutate(
      TLHdig_link = ifelse(is.na(TLHdig_links), NA, TLHdig_links)
    )

  df
}

# ---- Main pipeline ----

form_url <- "https://www.hethport.uni-wuerzburg.de/hetkonk/hetkonk_abfrageF.php"
user_choices <- get_form_fields_and_prompt(form_url)
html <- scrape_konkordanz_table()
raw_df <- parse_table(html)
clean_df <- clean_konkordanz_df(raw_df)

# --- Improved link normalization and splitting ---

normalize_links <- function(link, base_map = NULL) {
  if (is.null(base_map)) {
    base_map <- list(
      "^bildpraep\\.php" = "https://www.hethport.uni-wuerzburg.de/hetkonk/",
      "^3dprp\\.php" = "https://www.hethport.uni-wuerzburg.de/hetkonk/",
      "^kbopraep\\.php" = "https://www.hethport.uni-wuerzburg.de/hetkonk/",
      "^../TLHdig" = "https://www.hethport.uni-wuerzburg.de/",
      "^hetkonk_abfrage\\.php" = "https://www.hethport.uni-wuerzburg.de/hetkonk/",
      "^../hetskiz" = "https://www.hethport.uni-wuerzburg.de/",
      "^javascript:inf\\('\\.\\./CTH/(cthfix\\.php\\?c=[^']+)'\\s*,\\s*'CTH'\\)" = "https://www.hethport.uni-wuerzburg.de/CTH/"
    )
  }
  links <- unlist(strsplit(link, ";\\s*"))
  normed <- sapply(links, function(l) {
    l <- trimws(l)
    if (is.na(l) || l == "") return(NA)
    if (grepl("^https?://", l)) return(l)
    for (pat in names(base_map)) {
      if (grepl(pat, l, perl = TRUE)) {
        if (pat == "^javascript:inf\\('\\.\\./CTH/(cthfix\\.php\\?c=[^']+)'\\s*,\\s*'CTH'\\)") {
          m <- regmatches(l, regexec(pat, l, perl = TRUE))[[1]]
          if (length(m) > 1 && grepl("^cthfix\\.php\\?c=[0-9]+", m[2])) {
            return(paste0(base_map[[pat]], m[2]))
          } else {
            return(NA)
          }
        } else {
          l2 <- sub("^\\.\\./", "", l)
          return(paste0(base_map[[pat]], l2))
        }
      }
    }
    l
  }, USE.NAMES = FALSE)
  paste(normed[!is.na(normed)], collapse = "; ")
}

# Normalize all link columns
link_cols <- grep("(_links|_link|_url)$", names(clean_df), value = TRUE)
for (col in link_cols) {
  clean_df[[col]] <- sapply(clean_df[[col]], function(x) if (!is.na(x) && x != "") normalize_links(x) else x)
}

# --- Split semicolon-separated links into correct columns ---
split_links <- function(links, pattern) {
  if (is.na(links) || links == "") return(NA)
  links <- unlist(strsplit(links, ";\\s*"))
  links <- links[grepl(pattern, links)]
  if (length(links) == 0) return(NA)
  paste(links, collapse = "; ")
}

clean_df <- clean_df %>%
  mutate(
    Image_URL = sapply(Image_link_url, split_links, pattern = "bildpraep\\.php"),
    `3D_image_URL` = sapply(Image_link_url, split_links, pattern = "3dprp\\.php"),
    handcopy_URL = sapply(Publikation_links, split_links, pattern = "kbopraep\\.php"),
    hetskiz_URL = sapply(Joins_links, split_links, pattern = "hetskiz")
  )

# --- Clean CTH_URL: remove NA, unwanted HTML, and keep only valid links ---
clean_df <- clean_df %>%
  mutate(
    CTH_URL = ifelse(!is.na(CTH_link) & grepl("^https?://", CTH_link), CTH_link, NA),
    CTH_URL = ifelse(grepl("<font", CTH_URL, fixed = TRUE), NA, CTH_URL)
  )

# Only encode the value after ?d= in TLHdig_link
clean_df$TLHdig_link <- vapply(clean_df$TLHdig_link, function(link) {
  if (is.na(link) || !grepl("\\?d=", link)) return(link)
  parts <- strsplit(link, "\\?d=", fixed = FALSE)[[1]]
  if (length(parts) != 2) return(link)
  base <- parts[1]
  docid <- parts[2]
  docid_enc <- URLencode(docid, reserved = TRUE)
  paste0(base, "?d=", docid_enc)
}, character(1))

# Remove TLHdig links that contain HTML tags (e.g. <font ...>)
clean_df$TLHdig_link[grepl("<font", clean_df$TLHdig_link, fixed = TRUE) | grepl("%3Cfont", clean_df$TLHdig_link, fixed = TRUE)] <- NA

# --- Final field selection: prompt user for columns from the cleaned set only ---
final_fields <- c(
  "Inventarnummer", "Joins", "hetskiz_URL", "handcopy_URL", "Image_URL", "3D_image_URL",
  "Publikation", "CTH_full", "CTH_URL", "Fundort_room", "Fundort_details",
  "Zeit", "TLHdig_link", "Anmerkungen"
)
final_fields <- final_fields[final_fields %in% names(clean_df)]

cat("Available columns for final CSV:\n")
cat(paste(seq_along(final_fields), final_fields, sep = ": "), sep = "\n")
cols_input <- readline("Enter comma-separated numbers of columns to keep (or leave blank for all): ")
if (cols_input != "") {
  keep_idx <- as.integer(unlist(strsplit(cols_input, ",")))
  final_fields <- final_fields[keep_idx]
}
clean_df_final <- clean_df[, final_fields, drop = FALSE]

# Write final CSV
final_csv_path <- file.path(out_dir, "clean_hpm_search_final.csv")
write.csv(clean_df_final, final_csv_path, row.names = FALSE)
cat("Wrote final CSV to:", final_csv_path, "\n")