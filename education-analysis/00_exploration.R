#############################################
# Title: Teacher Salary Data Analysis
# Author: Abhinav Krishnan
# Date: 01 March 2023
# Inputs: 
# Outputs: 
#############################################

# =============================================================================
# =============================== Description =================================
# =============================================================================

  # Teacher Salary Data Sources:
    # TN Department of Education: https://www.tn.gov/education/finance-and-monitoring/differentiated-pay.html
    # Tennessean: https://data.tennessean.com/2021-tennessee-average-teacher-salaries/
  
  # County Data
    # Shapefiles: https://catalog.data.gov/dataset/tiger-line-shapefile-2016-state-tennessee-current-county-subdivision-state-based
    # 

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
