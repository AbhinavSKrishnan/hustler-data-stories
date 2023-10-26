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
  
    library(tidyverse)
    library(ggplot2)
    library(stringr)
    library(tidygeocoder)
    library(jsonlite)
    library(janitor)
  
  # ============== #
  # Set Parameters
  # ============== #
  
    # set export toggle
    p_export <- F
    
    # set timestamp
    p_timestamp <- Sys.time()
    
    # set output directory
    p_dir_out <- "~/Documents/Projects/hustler-data-stories/property-analysis/00_output/"
    
    # set input directory
    p_dir_in_base <- ""
  
  # ============== #
  # Load Data
  # ============== #
  
    in.properties <- as.data.table(read.csv(file = "property-analysis/00_output/00_propertydata"))
    
# =============================================================================
# ============================ Geocoding  =================================
# =============================================================================
    
    # Add "Nashville, TN, USA" to Location
    in.properties <- in.properties %>% 
        mutate(Location = str_c(Location, ", Nashville, TN"))
    
    # Add Latitude and Longitude data
    properties.geocoded <- geocode(in.properties,
                           address = Location,
                           method = "arcgis",
                           full_results = FALSE # Disabling this feature increases the search speed
    )
    # Typically takes ~45 seconds
    
    # rename longitude column to fit datawrapper format
    properties.geocoded <- properties.geocoded %>% rename(lng = long)
    
    # Convert to JSON
    json.geocodes <- toJSON(properties.geocoded)
    
    write.csv(properties.geocoded, file = paste0(p_dir_out, "properties_geocodes.csv"))
    
    write(json.geocodes, file = paste0("/Users/abhinavskrishnan/vanderbilt-property-visualstory/_data/", p_timestamp, "vanderbilt_properties.json"))
    
    
    # ============================================================================ #
    # ================================= Exploration ================================== #
    # ============================================================================ #
    
    # Create function to set tax status based on assessment ratio
    case_property_class <- function(a_assessment_ratio, a_property_type, a_property_use){
      
      case_when(
        assessment_ratio == 0 ~ "Unclear",
        assessment_ratio == 0.25 ~ "Residential/Farm",
        assessment_ratio == 0.3 ~ "Personalty",
        assessment_ratio == 0.4 | property_use %like% "APARTMENT" ~ "Commercial/Industry",
        assessment_ratio > 0 ~ "Other",
        .default = "Unclear"
      )
      
    }
    
    
    p_tax_rate_current <- 3.254
    
    # Cleaning and formatting dataset
    properties <- properties.geocoded %>% 
      clean_names() %>% 
      mutate(num_sale_date = mdy(sale_date),
             num_sale_price = as.numeric(parse_number(sale_price)),
             num_appraisal_value = as.numeric(parse_number(total_appraisal_value)),
             num_assessed_value = as.numeric(parse_number(assessed_value)),
             assessment_ratio = num_assessed_value/num_appraisal_value,
             property_class = case_property_class(assessment_ratio, property_type, property_use),
             tax_status = ifelse(assessment_ratio == 0, "Exempt", "Taxed"),
             tax_amount = num_assessed_value * (p_tax_rate_current/100)
               # NOTE: Vanderbilt falls within the Urban Services District ($3.254 in 2022)
             ) %>% 
      separate(col = num_sale_date, into = c("Year", "Month", "Date"), sep = "-")
    
    # 
    properties_exempt <- properties %>% 
      filter(tax_status == "Exempt") %>% 
      mutate(
        hyp_assessment_ratio = 0.4, 
          # Assume all properties would otherwise be taxed at a commercial rate
        hyp_assessed_value = num_appraisal_value * hyp_assessment_ratio,
        hyp_tax_amount = hyp_assessed_value * (p_tax_rate_current/100)
      )
      
    
    # What are tax exempt properties being used for? and on what type of property are they/
    properties_exempt %>% count(property_use, property_class, property_type) %>% arrange(desc(n)) %>% print(n = 28)
      # Most of these are parking lots, followed by office buildings and "school or college"
      # I think it would be reasonable to calculate the amount of tax forgone by assuming everything is taxed at a commercial rate
    
    # How much money could Nashville earn through property tax if all tax exempt properties were taxed at a commercial rate?
    sum(properties_exempt$hyp_tax_amount)
      # Phew. $30.5 million per year in potential property taxes.
      # with this money, Nashville could double the amount it has allocated to affordable housing
        # source: https://www.wsmv.com/2023/05/01/mayor-cooper-files-2024-fiscal-year-budget-nashville/
      # This is also over half of what the city has allocated to making cost of living adjustments for MNPS teachers
        # source: 
    
      
      # Holy shit. You can figure out which properties are tax exempt by calculating the assessment ratio.
      # Residential properties are taxed at 25% of appraised value and Commercial/industrial properties are taxed at 40% of appraised value
      # Anything that is at 0% is not taxed at all!
    
    # How many properties are exempt and what are the others taxed
    properties %>% count(assessment_ratio, property_class)
      # 65 of 99 properties (65%) are tax exempt
    
    # What if these properties were taxed at their appropriate rate?
    
    
    # Some summary statistics
    properties %>% 
      count(Year) %>% arrange(desc(n)) %>% print(n = 39)
    
    # Years when the most properties were purchased: 2015 (10), 2021 (8), 2016 (7), 2018 (5), 2008, 1989, 2023
    
    properties %>% 
      count(Num_Sale_Price) %>% arrange(desc(n)) %>% print()
    
    # 61 properties list the sale price as $0 - this is close to 30% of all observations
    # Question: are these sale prices in real, inflation-adjusted dollars or in nominal dollars?
    
    
    # Question: what is the median assessed value (not including the $0 sales)
    properties %>% 
      filter(Num_Assessed_Value != 0) %>% count(Num_Assessed_Value) %>% arrange(desc(Num_Assessed_Value)) %>% print(n = 34)
    # Answer: $1,240,000
    
    # All but one sale was greater than $100,000 - what was this 75k sale?
    properties %>% filter(Num_Sale_Price <= 100000 & Num_Sale_Price != 0) %>% select(Location, Sale_Date, Property_Use, Assessed_Value, Square_Footage, Num_Sale_Price) %>% View()
    
      # 27 sales were greater than $1,000,000
      # One sale is $13 million - what the hell is this?
    properties %>% filter(Num_Assessed_Value >= 1000000) %>% select(Location, Sale_Date, Property_Use, Assessed_Value, Square_Footage, lat, lng) %>% View()
        
        # The property at 2609 WEST END AVE Unit 101, Nashville, TN was resubbed i.e. an existing parcel was subdivided into two different parcels (the adjacent parcel is still currently owned by Vanderbilt University). This property currently houses the Redlands Grill and the adjacent property houses the Vanderbilt Allergy Institute
    
    # So Question: how many parcels are subdivided? Were they all sold for $0?
    properties %>% filter(Legal_Description %like% "RESUB" & Num_Sale_Price == 0) %>% count(Num_Sale_Price, Year, Property_Use, Location) %>% View()
    # Answer: No, only 10 resub properties were sold for $0 and, of these, 8 were in the last six years
    
    # Does this materially affect the inclusion of the subdivided properties?
    
    # How many $0 sales are also assessed at $0?
    properties %>% filter(Num_Sale_Price == 0 & Num_Assessed_Value == 0) %>% count(Property_Use, Sale_Date, Assessed_Value, Sale_Price, Location) %>% View()
    
    # Question: when did these $0 sales occur?
    properties %>% 
      filter(Num_Sale_Price == 0) %>% count(Year) %>% arrange(desc(n))
    # Answer: Most sales occured in 2015 (10), 2021 (8), 1973, and 2023 i.e. mostly within the last two decades
    
  
    
# ============================================================================ #
# ============================== Data Formatting ============================== #
# ============================================================================ #
    
  yrs.1 <- c(2013:2023)  
  yrs.2 <- c(2003:2013)
  yrs.3 <- c(1983:2003) # roughly corresponds to Joe B. Wyatt's tenure as Chancellor
  yrs.4 <- c(min(properties$Year):1983)
  
  properties.1 <- properties %>% filter(Year %in% yrs.1)
  write.csv(properties.1, file = "~/Documents/Projects/hustler-data-stories/property-analysis/00_output/properties_2013_2023.csv")
  properties.2 <- properties %>% filter(Year %in% yrs.2)
  write.csv(properties.2, file = "~/Documents/Projects/hustler-data-stories/property-analysis/00_output/properties_2003_2013.csv")
  properties.3 <- properties %>% filter(Year %in% yrs.3)
  write.csv(properties.3, file = "~/Documents/Projects/hustler-data-stories/property-analysis/00_output/properties_1983_2003.csv")
  properties.4 <- properties %>% filter(Year %in% yrs.4)
  write.csv(properties.4, file = "~/Documents/Projects/hustler-data-stories/property-analysis/00_output/properties_min_1983.csv")
  