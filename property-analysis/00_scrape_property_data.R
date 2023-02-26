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
    p_url_base <- "https://www.padctn.org/prc/#/search/"
    
    # set search type, term, number
    search_type <- "searchType=owner"
    search_term <- "searchTerm=vanderbilt"
    search_number <- "searchNumber="

# =============================================================================
# =============================== Section ====================================
# =============================================================================

    driver <- rsDriver(browser=c("chrome"))
    remote_driver <- driver[["client"]]
    remote_driver$open()
    
    library(wdman)
    
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
    
    remDr$navigate(p_url_base)
    
    # Find agree disclaimer and click
    remDr$findElement(using = "id", value = "AgreeDisclaimer")$clickElement()
    # Search for Vanderbilt University
    remDr$findElement(using = "id", value = "searchTerm")$sendKeysToElement(list("Vanderbilt University"))
    
    # Let page load
    Sys.sleep(2)
    
    # Set page html code to object
    page.source <- remDr$getPageSource()[[1]]
    
    listings <- read_html(page.source) %>% 
      html_nodes("a")
    
    
    
    
# =============================================================================
# ======================== Scrape Property URLs =============================
# =============================================================================

    # Okay so this section needs to go through each page of the search for Vanderbilt and scrape the urls for each property record
    
    # For loop for recursive search
    
    # set number of search pages
    n_pages <- c(10)
    
    for (i in 1:n_pages) {
      
      page_url <- paste0(p_)
      
      
      
      
    }
    
    
# =============================================================================
# =============================== WORKSPACE ===================================
# =============================================================================

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
    

