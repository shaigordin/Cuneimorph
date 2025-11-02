# Make sure to install Chrome or Chromium browser before running this script!

# Dependency ----

# We also need a few R packages to interact with the browser, scrape, and clean
# the data. Please install them if you haven't already by uncommenting and
# running:

install.packages(c("tidyr", "purrr", "chromote", "rvest", "stringr", "dplyr"))

# Load the packages
library(tidyr)
library(rvest)
library(purrr)
library(dplyr)
library(stringr)
library(chromote)

# Create a new browser session
chrome_session <- ChromoteSession$new()

# Launch chrome to view actions taken in the browser
chrome_session$view()

# Get the browser's version
chrome_session$Browser$getVersion()

# Create a new tab session for HPM website ----

# Start a new browser tab session
chrome_session_hpm <- chrome_session$new_session()

# Open a new tab in the current browser
chrome_session_hpm$view()

## Navigate to HPM ----

# Navigate to hethiter portal
chrome_session_hpm$Page$navigate("https://www.hethport.uni-wuerzburg.de")

# Wait for the page to load
Sys.sleep(0.5)

# Highlight an element on the page
chrome_session_hpm$Runtime$evaluate(
  expression = "
  // Find the element
  element = document.querySelector('li:nth-child(3) > a');

  // Highlight it
  element.style.backgroundColor = 'yellow';
  element.style.border = '2px solid red';
  "
)

# Wait for the action to complete
Sys.sleep(0.5)

## Search for a location ----

# First focus the input field
chrome_session_hpm$Runtime$evaluate('
  document.querySelector("li:nth-child(3) > a").focus();
')

# Brief pause to ensure focus is complete
Sys.sleep(0.5)

# Wait for and, then, click the result
Sys.sleep(0.5)
chrome_session_hpm$Runtime$evaluate('
    document.querySelector("li:nth-child(3) > a").click();
  ')

# Enter search term
search_field_selector <- 'body > div > div > div:nth-child(4) > form > div > div:nth-child(14) > div.seven.columns > select'
search_query <- 'III. DAS RECHT (CTH 291-298)'
chrome_session_hpm$Runtime$evaluate(
  expression = sprintf('(function() {
    // Locate the dropdown element using the provided selector
    const dropdown = document.querySelector("body > div > div > div:nth-child(4) > form > div > div:nth-child(14) > div.seven.columns > select");
    if (!dropdown) {
      console.error("Dropdown element not found!");
      return; // This return is now allowed inside the IIFE
    }

    // Option 1: Select by index (0-based, so 1 selects the second option)
    dropdown.selectedIndex = 3;

    // Option 2: Alternatively, select by matching text
    // const options = Array.from(dropdown.options);
    // const desiredOption = options.find(opt => opt.text.trim() === "III. DAS RECHT (CTH 291-298)");
    // if (desiredOption) {
    //   desiredOption.selected = true;
    // } else {
    //   console.error("Desired option not found!");
    //   return;
    // }

    // Dispatch a change event so that the site registers the new selection
    const changeEvent = new Event("change", { bubbles: true });
    dropdown.dispatchEvent(changeEvent);
  })();')
)
Sys.sleep(0.5)

# Trigger search
chrome_session_hpm$Runtime$evaluate('
    document.querySelector("input[type=submit]:nth-child(1)").click();
  ')
Sys.sleep(15)


## Extract text metadata ----

# Wait for and, then, extract the weather data table
Sys.sleep(5)
html <- chrome_session_hpm$Runtime$evaluate('
    document.querySelector("table").outerHTML
  ')$result$value

head(html)
writeLines(html, "data/raw/metadata/hpm_search_results.html")

# Clean up the browser sessions ----

chrome_session_hpm$close()
chrome_session$close()

# Preprocess and clean Konkordanz metadata table

## Define a mapping for partial link prefixes to their full base URLs.
# Update these values to match the actual base URLs for your project.
base_urls <- list(
  "../hetskiz" = "https://www.hethport.uni-wuerzburg.de/hetskiz",
  "bildpraep.php" = "https://www.hethport.uni-wuerzburg.de/hetkonk/bildpraep.php",
  "3dprp.php" = "https://www.hethport.uni-wuerzburg.de/hetkonk/3dprp.php",
  "kbopraep.php" = "https://www.hethport.uni-wuerzburg.de/hetkonk/kbopraep.php",
  "../TLHdig" = "https://www.hethport.uni-wuerzburg.de/TLHdig",
  "hetkonk_abfrage.php" = "https://www.hethport.uni-wuerzburg.de/hetkonk/hetkonk_abfrage.php"
)

## Function to expand partial links in a cell's text.
expand_links <- function(text, base_urls) {
  for(prefix in names(base_urls)) {
    # Look for links that start with the given prefix inside parentheses
    pattern <- paste0("\\(", prefix, "([^)]*)\\)")
    replacement <- paste0("(", base_urls[[prefix]], "\\1)")
    text <- gsub(pattern, replacement, text, perl = TRUE)
  }
  return(text)
}

## Function to remove redundant duplicate text in the Publikation column.
clean_publikation <- function(text) {
  if(grepl(" \\| ", text)) {
    parts <- unlist(strsplit(text, " \\| "))
    if(length(parts) >= 2 && startsWith(parts[2], parts[1])) {
      parts[2] <- sub(paste0("^", fixed(parts[1])), "", parts[2])
    }
    text <- paste(parts, collapse = " | ")
  }
  return(text)
}

## Extraction function for a table cell.
# It gets the plain text, appends any link info (as "link_text (href)"),
# and then expands partial links using our mapping.
extract_cell <- function(cell) {
  cell_text <- html_text(cell, trim = TRUE)
  links <- html_nodes(cell, "a")
  if(length(links) > 0) {
    link_texts <- html_text(links, trim = TRUE)
    link_hrefs <- html_attr(links, "href")
    link_info <- paste0(link_texts, " (", link_hrefs, ")")
    cell_text <- paste(cell_text, paste(link_info, collapse = "; "), sep = " | ")
  }
  cell_text <- expand_links(cell_text, base_urls)
  return(cell_text)
}

# Read the HTML file with proper encoding for Unicode
html_doc <- read_html(html, encoding = "UTF-8")
table_node <- html_node(html_doc, "table")
rows <- html_nodes(table_node, "tr")

# Define column names.
# Note: The first header row spans three columns for Inventarnummer,
# so we assign our own names for all 9 columns.
col_names <- c("Inventarnummer", "Joins", "Image-link", "Publikation", "CTH",
               "Fundort", "Zeit", "TLHdig", "Anmerkungen")

# Process each row (skipping the header) and extract cell contents.
data_list <- rows[-1] %>% map(function(row) {
  cells <- html_nodes(row, "td")
  sapply(cells, extract_cell, USE.NAMES = FALSE)
})

# Combine rows into a data frame and assign column names.
raw_df <- as.data.frame(do.call(rbind, data_list), stringsAsFactors = FALSE)
colnames(raw_df) <- col_names

# Post-process the Publikation column to remove redundant text.
raw_df$Publikation <- sapply(raw_df$Publikation, clean_publikation)

# Clean up the data:
# - Treat cells as missing in Inventarnummer if they are empty,
#   and in CTH if they are blank or contain only a dot.
# - Fill down missing values in Inventarnummer and CTH.
# - Trim extra whitespace from all columns.
clean_df <- raw_df %>%
  mutate(
    Inventarnummer = ifelse(grepl("^\\s*$", Inventarnummer), NA, Inventarnummer),
    CTH = ifelse(grepl("^\\s*\\.?\\s*$", CTH), NA, CTH)
  ) %>%
  tidyr::fill(Inventarnummer, CTH) %>%
  mutate(across(everything(), str_trim))

# Write the final cleaned data frame to CSV.
write.csv(clean_df, "data/raw/metadata/clean_hpm_search.csv", row.names = FALSE)

## Things to add / still necessary
# In 'Joins' column just remove redundant symbols before and after link; do the same in 'Image-link';
# In 'Publikation' make separate link column;
# In 'CTH' separate into extra column with corrected link, remove extra CTH number, and fill down number in empty cells.
# 'Fundort' separate into before ':' and after ':' - columns 'room' and 'details' or similar.
# 'TLHdig' clean link
# 'Anmerkung' for now leave as is
# separate functions into separate files

