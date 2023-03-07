#############################################
# Title: 00_scrape_property_data
# Author: Abhinav Krishnan
# Date: 04 December 2022
# Inputs: 
# Outputs: 
#############################################

# =============================================================================
# =============================== Description =================================
# =============================================================================



# =============================================================================
# ================================= Setup =====================================
# =============================================================================

  # clear working environment
  rm(list = ls())
  
  # ============== #
  # Load Libraries
  # ============== #
  
    library(data.table)
    library(tidyverse)
    library(ggplot2)
    library(stringr)
    library(readxl)
    library(rvest)      # xml2 is a required package for rvest, so you only need to install rvest
    library(xml2)
    library(httr)

    library(RSelenium)
    library(netstat)
    library(selectr)
    library(wdman)

    library(sf)
    library(tidygeocoder)
  
  # ============== #
  # Set Parameters
  # ============== #
  
    # set export toggle
    p_export <- F
    
    # set timestamp
    p_timestamp <- Sys.time()
    
    # set output directory
    p_dir_out <- ""
    
    # set input directory
    p_dir_in_base <- ""
    
  # ============== #
  # URL parameters
  # ============== #
  
    # Okay so we want to create code that will enable us to scrape each page of this webpage indexed by the search page number
    # e.g.: https://www.padctn.org/prc/#/search/1?searchType=owner&searchTerm=vanderbilt&searchNumber=
    
    # set test url
    test_url <- "https://www.padctn.org/prc/#/search/1?searchType=owner&searchTerm=vanderbilt&searchNumber="
    
    # set base url
    p_url_base <- "https://www.padctn.org"

# =============================================================================
# ============================= Server Setup ==================================
# =============================================================================
    
    # using wdman to start a selenium server
    selServ <- selenium(
      port = 8002L,
      version = 'latest',
      chromever = NULL
    )
    
    # using RSelenium to start firefox on the selenium server
    rD <- rsDriver(port = free_port(),
                     browser = c("firefox"), # ugh so it only works with firefox, not chrome
                     version = "latest",
                     chromever = "latest",
                     geckover = "latest",
                     iedrver = NULL,
                     phantomver = "2.1.1",
                     verbose = TRUE,
                     check = TRUE,
                     extraCapabilities = list(acceptInsecureCerts = TRUE))
    
    remDr <- rD[["client"]]

# =============================================================================
# =========================== Page Processing ===============================
# =============================================================================
    
  # Description:
    # This section creates a function that will navigate to each listing by url 
    # The function will then extract the list nodes and remove the html tags
    # The resulting strings are split into a wide datatable of variables and values 
    
    scrape_page <- function(link_suffix) {
      
      # Create test url using base url and first link suffix
      url.listing <- paste0(p_url_base, link_suffix)
      
        # Sample: 
      
      # Navigate to test url
      remDr$navigate(url.listing)
      
      Sys.sleep(0.5)
      
      # Scrape page source code
      page.source <- remDr$getPageSource()[[1]]
      
      # Create character vector of nodes with <li> tag using page source code
      v.nodes.li <- read_html(page.source) %>% 
        html_nodes("li") %>% 
        as.character()
      
      # Filter <li> nodes to only nodes with links (designated by href)
      v.nodes.li.filtered <- v.nodes.li[!(v.nodes.li %like% "href")]
      
      # Clean data
      # Create empty vector 
      v.holding <- c()
      
      # Strip unnecessary characters from nodes
      for (i in 1:length(v.nodes.li.filtered)) {
        
        # NOTE: strong nodes don't match up with filtered nodes
        # v.holding[i] <- gsub(v.nodes.li.str[i], "", v.nodes.li.filtered[i])
        v.holding[i] <- gsub("<li>\\n|</li>|<strong>|</strong>|<br>\\n", "", v.nodes.li.filtered[i]) # Remove tags
        v.holding[i] <- str_trim(v.holding[i], side = "both") # Trim whitespace
        
      }
      
      # Remove "Overlays" and "Base Layers" elements
      v.holding <- v.holding[!(v.holding %like% "<label>")]
      
      # Split vector at colon to separate variables from values
      v.split <- str_split_fixed(v.holding, ":", n = 2)
      
      # subset columns of matrix
      v.variables <- v.split[,1]
      v.values <- v.split[,2]
      
      # Create datatable with variables and values
      dt.temp <- data.table(v.variables, v.values)
      
      # Melt values to wide
      # dt.temp.wide <- dcast(dt.temp, formula = ) # Doesn't work well
      dt.temp.wide <- pivot_wider(dt.temp, names_from = v.variables, values_from = v.values)
      
      return(dt.temp.wide)
      
    }
    
    # QUESTION: Should I geolocate when scraping the dataset or after the culminating dataset
    # ANSWER: Probably geolocate the larger dataset
      
# =============================================================================
# ========================== Parse Property Data =================================
# =============================================================================
      
      # Create url for current page of listings
      landing.page <- paste0(p_url_base, "/prc/#/search/")
      
      # Navigate to landing page
      remDr$navigate(landing.page)
      
      # Let page load for one second
      Sys.sleep(1)
      
      # Find agree disclaimer and click Agree
      remDr$findElement(using = "id", value = "AgreeDisclaimer")$clickElement()
        # Navigates to: https://www.padctn.org/prc/#/search/1
        
      # Search for Vanderbilt University
      # remDr$findElement(using = "id", value = "searchTerm")$sendKeysToElement(list("Vanderbilt University"))
        # Navigates to: https://www.padctn.org/prc/#/search/1?searchType=owner&searchTerm=Vanderbilt%20University&searchNumber=
        # This code has been made defunct by including the search type and search term in the url and accessing the page directly
        
    # ============== #
    # Create vector of all pages
    # ============== #
  
      # Create empty vector
      v.urls.holding <- c()  
      
      # loop through each index and create list of urls
      for (i in 1:10) {
        # NOTE: If the number of pages change, this method is no longer robust. 
        # May need to scrape the number of pages from the website to pass to for loop index
        
        # Create url for current page of listings
        current.page <- paste0(p_url_base, "/prc/#/search/", i, "?searchType=owner&searchTerm=Vanderbilt University&searchNumber=")
        
        # Add url to holding vector
        v.urls.holding <- c(v.urls.holding, current.page)
        
      }
      
      # Create empty list
      ls.page.holding <- list()
      
      # Loop through the index of pages
      for (page in v.urls.holding) {
        
        remDr$navigate(page)
        Sys.sleep(1)
        
        # Set page source code to object
        page.source <- remDr$getPageSource()[[1]]
        
        # Scrape all 'a' class elements into a character vector
        listings <- read_html(page.source) %>% 
          # html_nodes("panel panel-primary") %>% 
          html_nodes("a") %>% 
          as.character()
        
        # Keep only listings with VANDERBILT in name
        listing.links <- listings[listings %like% 'VANDERBILT']
        
        # Extract link suffix using position of characters
          # link.suffixes <- str_sub(listing.links, 10, 35)
        
          # WARNING: POSITIONAL REFERENCES ARE NOT A ROBUST METHOD
            # This method has become a problem, because the link suffixes are inconsistently formatted. 
            # Some properties have 6 digit codes while others have 5 digit codes. This causes the positional reference to fail at times
            # Lets write a regex for these suffixes:
              # regex: /prc/property/(\\d{5,6})/card/1
        
        # Extract link suffix using regular expression
        link.suffixes <- str_extract(listing.links, "/prc/property/(\\d{5,6})/card/1")
        
        # Loop through link suffixes to scrape filings on current page
        for (suffix in link.suffixes) {
          
          # Pass the suffix to the scrape_page function
          temp.dt <- scrape_page(suffix)
          
          # assign each datatable of parsed data to a list item
          ls.page.holding[[suffix]] <- temp.dt
          
        }
        
      }
      
      # Convert list to datatable for faster vectorized operations
      dt.page.holding <- as.data.table(rbindlist(ls.page.holding))
      
    # Add Latitude and Longitude data
    dt.geocodes <- geocode(dt.page.holding,
                          address = Location,
                          method = "arcgis",
                          full_results = FALSE # Disabling this feature increases the search speed
                          )
    
    
    
# =============================================================================
# =============================== WORKSPACE ===================================
# =============================================================================

    # setting this if statement means this workspace will never run when I run the entire file
    if (TRUE == FALSE) {
    
      
      # set search type, term, number
      search_type <- "searchType=owner"
      search_term <- "searchTerm=vanderbilt"
      search_number <- "searchNumber="
        
      driver <- rsDriver(browser=c("chrome"))
      remote_driver <- driver[["client"]]
      remote_driver$open()
      
      # Spoof user agent
      page.spoof <- GET(test_url, add_headers('user-agent' = 'Property-Scraper ([[abhinav.s.krishnan@vanderbilt.edu]])'))
      
      page.spoof <- GET("https://www.padctn.org/prc/#/search/1", 
                        add_headers('user-agent' = 'Property-Scraper ([[abhinav.s.krishnan@vanderbilt.edu]])',
                                    'X-Amzn-Trace-Id' = "Root=1-63f51eae-5e060f2645d955435548fda6"
                                    )
                        )
      
      # Extract all hyperlinks from webpage
      page.html <- test_url %>% 
        read_html() %>% 
        html_elements('a') %>% # here, using the html class works better than trying to use the xpath
        html_attrs()
  
      test_page <- "https://www.padctn.org/prc/property/236260/card/1/historical"
      
      page.html <- test_page %>% 
        read_html() %>% 
        html_elements('a') %>% # here, using the html class works better than trying to use the xpath
        html_attrs()
    
    }
