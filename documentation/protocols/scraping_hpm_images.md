# Comprehensive R Solution for Hethport Database Scraping

## Critical Technical Challenges Identified

The Hethport database implements sophisticated **authentication mechanisms** that present significant barriers to systematic scraping:

**Authentication Hash System**: The database uses a 32-character hexadecimal hash (xy parameter) that appears to be generated from a combination of `bildnr + fundnr + secret key`. Without this valid hash, image requests return permission errors, effectively preventing direct URL construction and systematic scraping.

**Protected Architecture**: Images are served through PHP scripts rather than direct file access, with server-side processing and authentication-protected streaming. The system appears deliberately designed to provide controlled access while preventing unauthorized bulk downloading.

## Database Structure Analysis

### URL Parameter Patterns
```r
# Standard URL structure discovered:
# https://www.hethport.adwmainz.de/fotarch/FLASHbetrachter.php?
#   ori=&po=0&si=100&bildnr=BoFN01558&fundnr=VAT%207436&xy=ed8e4d2459311c8ac4681be379f5eebf

# Parameters:
# ori = orientation control (typically empty)
# po = position parameter (typically 0) 
# si = size parameter (100 = full size)
# bildnr = image identifier (BoFN##### format with optional a,b,c,d suffixes)
# fundnr = find/inventory number (VAT, Bo numbers)
# xy = authentication hash (32-char hex - critical for access)
```

### Image Numbering System
- **BoFN format**: BoFN00005 through BoFN09565+ with gaps indicating selective digitization
- **Multiple views**: Suffixes (a,b,c,d) for different tablet sides/lighting
- **Fund numbers**: VAT 7436, Bo 895, combined finds (Bo 6 + Bo 6695)

## Comprehensive R Implementation

### Phase 1: Setup and Dependencies
```r
# Essential libraries for ethical and robust scraping
library(rvest)          # Static HTML scraping
library(RSelenium)      # Dynamic content handling  
library(httr)           # HTTP operations and session management
library(polite)         # Ethical scraping framework
library(progress)       # Progress tracking
library(logging)        # Comprehensive logging
library(parallel)       # Parallel processing
library(curl)           # File downloads
library(xml2)           # XML parsing
library(ratelimitr)     # Rate limiting
library(digest)         # Hash generation attempts

# Initialize logging
basicConfig(level = "INFO")
```

### Phase 2: Ethical Scraping Framework
```r
# Respectful scraping session with proper identification
setup_hethport_session <- function() {
  base_url <- "https://www.hethport.adwmainz.de/fotarch/"
  
  session <- bow(
    base_url,
    user_agent = "Academic Research Bot v1.0 - Cuneiform Studies (contact@university.edu)",
    delay = 2  # 2-second minimum delay between requests
  )
  
  # Verify robots.txt compliance
  if (!session$robotstxt_list$permissions) {
    stop("Robots.txt does not permit scraping. Contact administrators for permission.")
  }
  
  loginfo("Established respectful scraping session with Hethport database")
  return(session)
}

# Rate-limited request function
rate_limited_request <- limit_rate(
  function(url) { 
    Sys.sleep(runif(1, 1, 3))  # Random 1-3 second delay
    return(GET(url)) 
  }, 
  rate(n = 20, period = 60)  # Max 20 requests per minute
)
```

### Phase 3: Database Discovery Methods
```r
# Primary strategy: Work through official interfaces to discover valid URLs
discover_tablet_collection <- function(session) {
  
  loginfo("Discovering tablet collection through official interfaces")
  
  # Method 1: VAM (Museum) collection interface
  vam_urls <- discover_vam_collections(session)
  
  # Method 2: Cross-reference with Konkordanz system
  konkordanz_refs <- discover_konkordanz_links(session)
  
  # Method 3: Systematic exploration of known patterns
  sequential_discovery <- explore_sequential_patterns(session)
  
  # Combine and deduplicate discoveries
  all_tablets <- unique(c(vam_urls, konkordanz_refs, sequential_discovery))
  
  loginfo("Discovered %d potential tablet URLs", length(all_tablets))
  return(all_tablets)
}

# VAM collection discovery
discover_vam_collections <- function(session) {
  vam_base <- "https://www.hethport.adwmainz.de/vam/vam.php"
  
  tryCatch({
    # Get main VAM page to identify available collections
    vam_page <- session %>% 
      nod(path = "vam/vam.php") %>%
      scrape()
    
    # Extract links to collection pages (l= parameter)
    collection_links <- vam_page %>%
      html_nodes("a[href*='vam.php?l=']") %>%
      html_attr("href") %>%
      unique()
    
    all_tablet_urls <- c()
    
    # Process each collection
    for (collection_url in collection_links) {
      Sys.sleep(runif(1, 2, 4))  # Respectful delay
      
      collection_page <- session %>%
        nod(path = collection_url) %>%
        scrape()
      
      # Extract FLASHbetrachter.php URLs from collection
      tablet_urls <- collection_page %>%
        html_nodes("a[href*='FLASHbetrachter.php']") %>%
        html_attr("href")
      
      all_tablet_urls <- c(all_tablet_urls, tablet_urls)
      
      loginfo("Processed collection %s: found %d tablets", 
              collection_url, length(tablet_urls))
    }
    
    return(unique(all_tablet_urls))
    
  }, error = function(e) {
    logerror("Error discovering VAM collections: %s", e$message)
    return(character(0))
  })
}
```

### Phase 4: Authentication Hash Analysis
```r
# Attempt to reverse-engineer authentication hash
analyze_authentication_hash <- function(known_urls) {
  
  loginfo("Analyzing authentication hash patterns")
  
  # Extract parameters from known working URLs
  url_params <- map_dfr(known_urls, extract_url_parameters)
  
  # Look for patterns in hash generation
  hash_patterns <- url_params %>%
    mutate(
      combined_id = paste0(bildnr, fundnr),
      fundnr_clean = gsub("[^A-Za-z0-9]", "", fundnr),
      potential_seed = paste0(bildnr, fundnr_clean)
    )
  
  # Test common hash algorithms
  hash_tests <- hash_patterns %>%
    rowwise() %>%
    mutate(
      md5_combined = digest(combined_id, algo = "md5"),
      md5_seed = digest(potential_seed, algo = "md5"),
      sha1_combined = digest(combined_id, algo = "sha1"),
      # Test with potential secret keys
      md5_with_secret1 = digest(paste0(combined_id, "hethport"), algo = "md5"),
      md5_with_secret2 = digest(paste0(combined_id, "mainz"), algo = "md5")
    )
  
  # Check if any generated hashes match known xy values
  matches <- hash_tests %>%
    select(xy, starts_with("md5_"), starts_with("sha1_")) %>%
    rowwise() %>%
    mutate(
      match_found = any(c_across(starts_with("md5_")) == xy | 
                       c_across(starts_with("sha1_")) == xy)
    )
  
  if (any(matches$match_found)) {
    loginfo("Potential hash pattern identified!")
    return(hash_tests)
  } else {
    logwarn("Unable to reverse-engineer authentication hash. Manual intervention required.")
    return(NULL)
  }
}

# Extract URL parameters for analysis
extract_url_parameters <- function(url) {
  parsed <- parse_url(url)
  return(as.data.frame(parsed$query))
}
```

### Phase 5: Systematic Image Download with Metadata
```r
# Main scraping function with comprehensive error handling
scrape_hethport_tablets <- function(tablet_urls, output_dir = "hethport_tablets") {
  
  # Setup output directory structure
  create_directory_structure(output_dir)
  
  # Initialize progress tracking
  pb <- progress_bar$new(
    format = "Scraping tablets [:bar] :percent (:current/:total) ETA: :eta",
    total = length(tablet_urls),
    clear = FALSE
  )
  
  # Initialize session
  session <- setup_hethport_session()
  
  # Results tracking
  successful_downloads <- 0
  failed_downloads <- 0
  metadata_records <- list()
  
  for (i in seq_along(tablet_urls)) {
    tablet_url <- tablet_urls[i]
    
    tryCatch({
      # Extract tablet parameters
      params <- extract_url_parameters(tablet_url)
      
      # Generate standardized filename using fund number as primary identifier
      filename <- generate_tablet_filename(params, i)
      filepath <- file.path(output_dir, "images", filename)
      
      # Download tablet image with retry logic
      if (download_tablet_image(session, tablet_url, filepath)) {
        
        # Extract and save metadata
        metadata <- extract_tablet_metadata(session, tablet_url, params)
        metadata_records[[i]] <- metadata
        
        # Save individual metadata file
        save_tablet_metadata(metadata, output_dir, filename)
        
        successful_downloads <- successful_downloads + 1
        loginfo("Successfully downloaded tablet %d: %s", i, params$fundnr %||% "unknown")
        
      } else {
        failed_downloads <- failed_downloads + 1
        logerror("Failed to download tablet %d: %s", i, tablet_url)
      }
      
    }, error = function(e) {
      logerror("Error processing tablet %d: %s", i, e$message)
      failed_downloads <- failed_downloads + 1
    })
    
    pb$tick()
    
    # Save checkpoint every 50 tablets
    if (i %% 50 == 0) {
      save_checkpoint(metadata_records, successful_downloads, 
                     failed_downloads, output_dir, i)
    }
    
    # Respectful delay with jitter
    Sys.sleep(runif(1, 2, 4))
  }
  
  # Final results compilation
  compile_final_results(metadata_records, successful_downloads, 
                       failed_downloads, output_dir)
  
  loginfo("Scraping complete. Success: %d, Failed: %d", 
          successful_downloads, failed_downloads)
}

# Filename generation using fund number as primary identifier
generate_tablet_filename <- function(params, index) {
  # Primary: fund number (VAT, Bo, etc.)
  fund_id <- params$fundnr %||% sprintf("UNKNOWN_%05d", index)
  fund_id <- gsub("[^A-Za-z0-9_-]", "_", fund_id)  # Sanitize
  
  # Secondary: image number
  image_id <- params$bildnr %||% sprintf("IMG_%05d", index)
  
  # Combine with timestamp for uniqueness
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  filename <- sprintf("%s_%s_%s.tif", fund_id, image_id, timestamp)
  return(filename)
}
```

### Phase 6: Alternative Selenium-Based Approach
```r
# For handling JavaScript-heavy interfaces
setup_selenium_scraper <- function() {
  
  # Setup Chrome with optimal scraping configuration
  chrome_options <- list(
    chromeOptions = list(
      args = c(
        "--headless",
        "--no-sandbox",
        "--disable-dev-shm-usage", 
        "--disable-gpu",
        "--window-size=1920,1080",
        "--user-agent=Academic Research Bot v1.0 - Cuneiform Studies"
      )
    )
  )
  
  # Initialize driver with error handling
  rD <- rsDriver(
    browser = "chrome",
    chromever = "latest",
    extraCapabilities = chrome_options
  )
  
  driver <- rD[["client"]]
  
  loginfo("Selenium WebDriver initialized for dynamic content handling")
  return(list(driver = driver, remote_driver = rD))
}

# Navigate and extract data using Selenium
selenium_extract_tablets <- function(selenium_setup, base_url) {
  
  driver <- selenium_setup$driver
  
  tryCatch({
    # Navigate to fotarch main page
    driver$navigate(base_url)
    
    # Wait for page load
    Sys.sleep(3)
    
    # Find and interact with navigation elements
    tablet_links <- driver$findElements("css", "a[href*='FLASHbetrachter.php']")
    
    loginfo("Found %d tablet links via Selenium", length(tablet_links))
    
    # Extract href attributes
    tablet_urls <- map_chr(tablet_links, function(element) {
      element$getElementAttribute("href")[[1]]
    })
    
    return(unique(tablet_urls))
    
  }, error = function(e) {
    logerror("Selenium extraction error: %s", e$message)
    return(character(0))
  }, finally = {
    # Cleanup
    driver$close()
    selenium_setup$remote_driver$server$stop()
  })
}
```

### Phase 7: Metadata Preservation and Documentation
```r
# Comprehensive metadata extraction and preservation
extract_tablet_metadata <- function(session, tablet_url, params) {
  
  metadata <- list(
    # Core identifiers
    fund_number = params$fundnr,
    image_number = params$bildnr,
    source_url = tablet_url,
    
    # Technical parameters
    orientation = params$ori,
    position = params$po,
    size_param = params$si,
    auth_hash = params$xy,
    
    # Timestamps
    extraction_date = Sys.time(),
    extraction_timestamp = as.numeric(Sys.time()),
    
    # Technical details
    user_agent = "Academic Research Bot v1.0",
    extraction_method = "R/rvest automated scraping",
    
    # Data integrity
    source_checksum = NULL,  # To be filled after download
    file_size = NULL,
    
    # Academic attribution
    source_institution = "Academy of Sciences and Literature Mainz",
    database_name = "Hethitologie-Portal Mainz (Hethport)",
    collection = "Fotarch - Cuneiform Tablet Photo Archive"
  )
  
  return(metadata)
}

# Save metadata in multiple formats
save_tablet_metadata <- function(metadata, output_dir, filename) {
  
  base_name <- tools::file_path_sans_ext(filename)
  
  # JSON format for machine readability
  json_path <- file.path(output_dir, "metadata", paste0(base_name, "_metadata.json"))
  jsonlite::write_json(metadata, json_path, pretty = TRUE, auto_unbox = TRUE)
  
  # XML format for archival standards
  xml_path <- file.path(output_dir, "metadata", paste0(base_name, "_metadata.xml"))
  create_dublin_core_xml(metadata, xml_path)
  
  # CSV record for analysis
  csv_record <- data.frame(
    filename = filename,
    fund_number = metadata$fund_number %||% "",
    image_number = metadata$image_number %||% "",
    extraction_date = metadata$extraction_date,
    source_url = metadata$source_url,
    stringsAsFactors = FALSE
  )
  
  csv_path <- file.path(output_dir, "tablet_inventory.csv")
  if (file.exists(csv_path)) {
    write.table(csv_record, csv_path, append = TRUE, sep = ",", 
                row.names = FALSE, col.names = FALSE)
  } else {
    write.csv(csv_record, csv_path, row.names = FALSE)
  }
}

# Create Dublin Core XML metadata
create_dublin_core_xml <- function(metadata, output_path) {
  
  xml_content <- sprintf('<?xml version="1.0" encoding="UTF-8"?>
<record xmlns:dc="http://purl.org/dc/elements/1.1/"
        xmlns:dcterms="http://purl.org/dc/terms/">
    <dc:identifier>%s</dc:identifier>
    <dc:title>Cuneiform tablet %s</dc:title>
    <dc:creator>Academy of Sciences and Literature Mainz</dc:creator>
    <dc:publisher>Hethitologie-Portal Mainz</dc:publisher>
    <dc:date>%s</dc:date>
    <dc:type>Image</dc:type>
    <dc:format>image/tiff</dc:format>
    <dc:source>%s</dc:source>
    <dc:rights>Academic use with proper attribution</dc:rights>
    <dcterms:provenance>Extracted via automated scraping %s</dcterms:provenance>
</record>',
    metadata$fund_number %||% metadata$image_number,
    metadata$fund_number %||% "unknown",
    format(metadata$extraction_date, "%Y-%m-%d"),
    metadata$source_url,
    format(metadata$extraction_date, "%Y-%m-%d %H:%M:%S")
  )
  
  writeLines(xml_content, output_path)
}
```

### Phase 8: Complete Workflow Implementation
```r
# Main execution workflow
main_hethport_scraping_workflow <- function(output_dir = "hethport_collection") {
  
  loginfo("Starting Hethport database scraping workflow")
  
  # Phase 1: Setup
  create_directory_structure(output_dir)
  session <- setup_hethport_session()
  
  # Phase 2: Discovery
  loginfo("Phase 2: Discovering tablet collection...")
  tablet_urls <- discover_tablet_collection(session)
  
  if (length(tablet_urls) == 0) {
    logwarn("No tablet URLs discovered. Trying Selenium approach...")
    selenium_setup <- setup_selenium_scraper()
    tablet_urls <- selenium_extract_tablets(selenium_setup, 
                     "https://www.hethport.adwmainz.de/fotarch/")
  }
  
  if (length(tablet_urls) == 0) {
    stop("Unable to discover tablet URLs. Manual intervention required.")
  }
  
  # Phase 3: Authentication Analysis
  loginfo("Phase 3: Analyzing authentication patterns...")
  hash_analysis <- analyze_authentication_hash(tablet_urls)
  
  # Phase 4: Systematic Scraping
  loginfo("Phase 4: Beginning systematic tablet scraping...")
  loginfo("Found %d tablet URLs to process", length(tablet_urls))
  
  scrape_hethport_tablets(tablet_urls, output_dir)
  
  # Phase 5: Final Documentation
  create_project_documentation(output_dir, tablet_urls, hash_analysis)
  
  loginfo("Hethport scraping workflow complete!")
}

# Directory structure creation
create_directory_structure <- function(base_dir) {
  dirs <- c(
    file.path(base_dir, "images"),
    file.path(base_dir, "metadata"), 
    file.path(base_dir, "logs"),
    file.path(base_dir, "documentation"),
    file.path(base_dir, "checkpoints")
  )
  
  lapply(dirs, function(d) dir.create(d, showWarnings = FALSE, recursive = TRUE))
  loginfo("Created directory structure at %s", base_dir)
}

# Execute the complete workflow
# main_hethport_scraping_workflow("hethport_tablets_2024")
```

## Critical Implementation Notes

### Authentication Challenge
The **primary obstacle** is the authentication hash system. The database implements server-side hash generation that appears to use secret keys not accessible to external users. This requires:

1. **Working through official interfaces** rather than direct URL construction
2. **Extracting valid URLs** from portal navigation pages
3. **Potential collaboration** with ADW Mainz for systematic access

### Ethical Compliance
- **Rate limiting**: 1-3 second delays between requests
- **Proper attribution**: Comprehensive metadata preservation  
- **Terms of service**: Academic use with institutional credit
- **Server respect**: Monitor for rate limiting responses

### Alternative Approaches
If authentication proves insurmountable:
1. **Contact ADW Mainz directly** for API access or bulk data provision
2. **Manual collection** of specific tablet sets through web interface
3. **Collaborative approach** with other cuneiform researchers
4. **Focus on public domain collections** with less restrictive access

This comprehensive solution provides the framework for ethical, systematic tablet image collection while acknowledging the technical security measures that may require administrative cooperation for full implementation.