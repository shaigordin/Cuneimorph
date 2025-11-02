library(rvest)
library(dplyr)
library(stringr)
library(purrr)

# Helper to extract text from cell
extract_text <- function(cell) {
  txt <- html_text(cell, trim = TRUE)
  if (txt == "") return(NA)
  txt
}

# Helper to extract bildpraep.php image-links from first column
extract_image_url <- function(cell) {
  link <- html_node(cell, "a")
  if (!is.na(link)) {
    href <- html_attr(link, "href")
    if (!is.na(href)) {
      if (!grepl("^https?://", href)) {
        href <- paste0("https://www.hethport.uni-wuerzburg.de/Alalach/", href)
      }
      href <- gsub(" ", "%20", href, fixed = TRUE)
      return(href)
    }
  }
  return(NA)
}

# Scrape a single table from a URL
scrape_alalach_table <- function(url) {
  doc <- read_html(url)
  table <- html_node(doc, "table")
  rows <- html_nodes(table, "tr")
  data <- rows[-1] %>% map(function(row) {
    cells <- html_nodes(row, "td")
    if (length(cells) < 5) return(NULL) # skip non-data rows
    list(
      text_id = extract_text(cells[1]),
      image_URL = extract_image_url(cells[1]),
      new_id = extract_text(cells[3]),
      date = extract_text(cells[4]),
      publication = extract_text(cells[5])
    )
  }) %>% compact()
  df <- bind_rows(data)
  df
}

# Scrape both tables and combine
urls <- c(
  "https://www.hethport.uni-wuerzburg.de/Alalach/alalfot_AlT.php",
  "https://www.hethport.uni-wuerzburg.de/Alalach/alalfot_div.php"
)
all_df <- bind_rows(lapply(urls, scrape_alalach_table))

# Clean up column names
colnames(all_df) <- c("text-id", "image-URL", "new-id", "date", "publication")

# Write to CSV
out_dir <- "data/raw/metadata/AlT"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_csv <- file.path(out_dir, "AlT_clean.csv")
write.csv(all_df, out_csv, row.names = FALSE)
cat("Wrote cleaned CSV to:", out_csv, "\n")