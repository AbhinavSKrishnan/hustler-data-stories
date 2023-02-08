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

  # Analyze trends in the compensation of Vanderbilt's highest paid employees

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
    p_dir_out <- "compensation-trends/00_output"
    
    # set input directory
    p_dir_in_base <- "compensation-trends/00_raw_data/"
  
  # ============== #
  # Load Data
  # ============== #
    
    # Load data as datatable
    in_comp <- as.data.table(read.csv(file = "compensation-trends/00_raw_data/Form990-ScheduleJ-PartII.csv", strip.white = TRUE))
    
# =============================================================================
# =============================== Cleaning ====================================
# =============================================================================
    
    # Questions:
      # 1. Need to ask for definitions of Compensation Types
    
    # drop empty rows
    dt_comp <- in_comp[!is.na(Year)]
    
    # Rename columns with '.' to '_'
      setnames(dt_comp, colnames(dt_comp), gsub("\\.", "_", colnames(dt_comp)))
      # NOTE: going forward, we're going to create a dataset for analysis and then reformat the column names at the END to make it presentable for sharing
    
    # Change column classes
      # revert Year to character
      dt_comp[, Year := as.character(Year)] 
      # identify monetary columns to be converted to numeric
      monetary.columns <- colnames(dt_comp)[10:15]
      # parse dollar signs and commas out of values
        # create new function for str_remove  
        remove_format <- function(x) {
          
          clean_dollar <- str_remove(x, "\\$")
          clean_comma <- str_remove_all(clean_dollar, "\\,")
          
          return(clean_comma)
        }
        
      # Remove dollar signs and commas from monetary amounts
      dt_comp[, (monetary.columns) := lapply(.SD, remove_format), .SDcols = monetary.columns]
      
      # Convert to numeric class
      dt_comp[, (monetary.columns) := lapply(.SD, as.numeric), .SDcols = monetary.columns]
      
      
# =============================================================================
# =============================== Exploration ====================================
# =============================================================================
      
      # calculate total salary as sum of all columns
      dt_comp[, Total_Compensation := lapply(.SD, sum, na.rm = TRUE), .SDcols = monetary.columns]
      dt_comp[, Total_Compensation := sum(dt_comp[, list(monetary.columns)], na.rm = TRUE)]
      
      dt_comp[, Total_Compensation := Base_Compensation + Bonus___Incentive_Compensation + Other_Reportable_Compensation + Retirement_and_other_deferred_compensation + Nontaxable_Benefits + Employee_Benefit_Plan_Contribution]
      
      # Explore base compensation by job type
      mean_base_comp <- setorder(dt_comp[, mean(Base_Compensation, na.rm = TRUE), by = list(Position, Department)], -V1)
      
      # Explore highest compensation type for chancellor
      
        # set idvars & comp vars
        v.idvars <- c("Position", "Year", "Name")
        v.compvars <- c("Base_Compensation", "Bonus___Incentive_Compensation", "Other_Reportable_Compensation", "Retirement_and_other_deferred_compensation", "Nontaxable_Benefits",  "Employee_Benefit_Plan_Contribution")
      
      chanc.comp <- dt_comp[Position == "Chancellor", list(v.idvars, v.compvars)]
      
      melt(chanc.comp, id.vars = v.idvars, measure.vars = v.compvars, variable.name = "Compensation Type", value.name = "Amount")
      