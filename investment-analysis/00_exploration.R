#############################################
# Title: Investment Data Analysis
# Author: Abhinav Krishnan
# Date: 7 February 2023
# Inputs: 
# Outputs: 
#############################################

# =============================================================================
# =============================== Description =================================
# =============================================================================

  # Analyze, explore, and visualize data on Vanderbilt's investment companies


  # ============== #
  # To-Do
  # ============== #
    # February 8
      # DONE - Create rank variable and set order using fct_reorder(var, sortvar)
      # DONE - Reposition plot.title (plot.title.position = "plot")
      # DONE - Remove "Year" axis label and yaxis label
      # Explanation of colors using ggtext
        # plot.subtitle = element_markdown(<span style = 'color:#hexcode'>)
      # OR could use a legend at (0,0)
      # REVISIT - add theme_void()
      # REVISIT - OPTIONAL: Create theme function
      # DONE - Add major gridlines
      # DONE - Remove yaxis ticks & add yaxis line
      # DONE - Replace caption note with "Source:"
      
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
    library(gganimate)
    library(forcats)
    library(extrafont)
    library(waffle)
  
  # ============== #
  # Set Parameters
  # ============== #
  
    # set export toggle
    p_export <- F
    
    # set timestamp
    p_timestamp <- Sys.time()
    
    # set output directory
    p_dir_out <- "investment-analysis/00_output_data"
    
    # set graphics directory
    p_dir_graphics <- "investment-analysis/graphics/"
    
    # set input directory
    p_dir_in_base <- "investment-analysis/00_input_data/"
  
  # ============== #
  # Load Data
  # ============== #

    in_schr <- as.data.table(read.csv("investment-analysis/raw-Form990-ScheduleR.csv", fill = TRUE))
    
  # ============== #
  # Ggplot stuff
  # ============== #
  
    # Define colors
    hex.silver <- "#F2EEE3" #F2EEE3 - light gold
    hex.gold   <- "#DDBB63" #DDBB63 - medium gold
    hex.brown  <- "#8E6600" #8E6600 - dark gold
    hex.black  <- "#000000" #000000 - black  
    
    
# =============================================================================
# =============================== Analysis ====================================
# =============================================================================

    # Creating a graphic that illustrates the lifespan of investment companies
    dt_schr <- in_schr[, c("Year", "Entity.Name") := list(as.numeric(Year), as.factor(Entity.Name))]
    
    # create another dataset with only investment-related companies and generate new variables
    rangeyear <- dt_schr[Purpose %in% c("Investments", "Investment", "Holding Company", "Investment Holding"), 
                         c("Last_Year", "First_Year", "Age") := list(max(Year), min(Year), max(Year) - min(Year)),
                         by = Entity.Name]
    
    # Reorder Entity.Name as a factor based on the first year the entity appears
    rangeyear[, Entity.Name := fct_reorder(rangeyear[, Entity.Name], -rangeyear[, First_Year])]
    
    # change row order in dataset
    setorder(rangeyear, cols = First_Year)
    
# =============================================================================
# ============================ Visualizations =================================
# =============================================================================

    # ============== #
    # Set ggplot theme
    # ============== #
    
      # Define base theme
      theme_hustler_ts <- function() {
        theme_classic() %+replace%
          theme(
            axis.ticks.y      = element_blank(),
            axis.title        = element_text(face = "bold"),
            axis.line = element_line(),
            axis.text = element_text(),
            
            plot.title        = element_text(face = "bold", size = 20, family = "serif"),
            plot.subtitle     = element_text(color = "#4f4f4f", size = 13),
            plot.caption      = element_text(vjust = -1),
            plot.background   = element_rect(fill = "#FAFAFA"),
            plot.title.position = "plot",
            
            # panel.background  = element_rect(fill = "#FAFAFA"),
            panel.grid.major = element_line(color = "black", size = 0.5, linetype = 1),
            
            legend.title      = element_blank(),
            legend.text       = element_text(size = 12),
            legend.background = element_rect(),
            legend.position   = c(0.1,0.9)
          )
      }
    
    # ============== #
    # Plots
    # ============== #
    
    # Figure 1 - number and lifespan of Vanderbilt's Investment Companies
    
      gg <- ggplot(rangeyear[Purpose %in% c("Investments", "Investment", "Holding Company", "Investment Holding") & Age != 0], aes(x = Entity.Name, y = Year))
      
      p.entity.lifespan <- gg + 
          geom_path() +
          geom_point(aes(x = Entity.Name, y = First_Year, color = "First Appearance")) +
          geom_point(aes(x = Entity.Name, y = Last_Year, color = "Most Recent Appearance")) + 
          scale_color_manual(values = c("#DDBB63", "#000000")) +
          labs(
            title = "Lifespan of Vanderbilt's Investment Companies, 2001 - 2020",
            caption = "Source: Vanderbilt IRS Form 990"
          ) + 
          scale_y_continuous(limits = c(2000, 2020), breaks = seq(2000, 2020, 4), expand = expansion(add = 2)) +
          theme_classic() +
          theme(
            axis.ticks.y      = element_blank(),
            axis.title.x       = element_blank(), # for some reason, just using axis.title = element_blank() doesn't work
            axis.title.y       = element_blank(),
            axis.line = element_line(linetype = 1),
            axis.line.x = element_blank(),
            axis.text = element_text(family = "Arial Narrow"),
            
            plot.title        = element_text(face = "bold", size = 15, family = "Georgia", 
                                             margin = margin(t = 5, b = 5), hjust = 0.1),
            plot.subtitle     = element_text(color = "#4f4f4f", size = 13, family = "Arial Narrow"),
            plot.caption      = element_text(vjust = -1, family = "Arial Narrow"),
            plot.background   = element_rect(fill = "#ffffff"),
            plot.title.position = "plot",
            
            # panel.background  = element_rect(fill = "#FAFAFA"),
            panel.grid.major.x = element_line(color = "black", size = 0.1, linetype = 1),
            
            legend.title      = element_blank(),
            legend.text       = element_text(size = 9, family = "Arial Narrow"),
            legend.background = element_rect(fill = "#ffffff", color = "white"),
            legend.position   = c(0.2,0.1)
          ) +
          coord_flip()
      
      p.entity.lifespan
    
# =============================================================================
# =============================== Workspace ====================================
# =============================================================================

    
    
    
    
    # rangeyear[, Entity.Name := (Entity.Name, Min_Year)]
    
    
    
    
    
# =============================================================================
# =============================== Export ====================================
# =============================================================================

    
    if (p_export == TRUE) {
      
      ggsave(filename = "entity_lifespan", p.entity.lifespan, device = "png", path = p_dir_graphics)
      
    }
    