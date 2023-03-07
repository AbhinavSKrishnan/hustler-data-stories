#############################################
# Title: 
# Author: 
# Date: 
# Inputs: 
# Outputs: 
#############################################

# =============================================================================
# =============================== Description =================================
# =============================================================================

  # TO DO
    # 1. Add zip code
    # 2. Convert Property Use column to full sentences
    # 3. Research Zone codes
    # 4. add neighborhood descriptions
    # 5. Add column for common name

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
  # Load Data
  # ============== #
  
    dt.propertydata <- as.data.table(read.csv(file = "property-analysis/00_output/00_propertydata"))
    
# =============================================================================
# ============================ Data Processing  =================================
# =============================================================================
    
    # Add "Nashville, TN, USA" to Location
    dt.propertydata[, Location := str_c(Location, ", Nashville, TN")]
    
    # Add Latitude and Longitude data
    dt.geocodes <- geocode(dt.propertydata,
                           address = Location,
                           method = "arcgis",
                           full_results = FALSE # Disabling this feature increases the search speed
    )
    # Typically takes ~45 seconds
    
    # Convert to JSON
    json.geocodes <- toJSON(dt.geocodes)
    
    write(json.geocodes, file = paste0("/Users/abhinavskrishnan/vanderbilt-property-visualstory/_data/vanderbilt_properties.json"))