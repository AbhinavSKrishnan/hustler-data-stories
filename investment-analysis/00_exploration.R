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

    # February 9
      # DONE - Increase font size of legends
      # Increase size of dots (a little)
      # DONE - (try) Add horizontal gridlines (very light and thin)
      # DONE - REMOVE axis.ticks.x
      # Adjust margin below and to the right of caption
       
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
    p_export <- TRUE
    
    # set timestamp
    p_timestamp <- Sys.time()
    
    # set output directory
    p_dir_out      <- "investment-analysis/output-data"
    
    # set graphics directory
    p_dir_graphics <- "investment-analysis/graphics/"
    
    # set input directory
    p_dir_in_base  <- "investment-analysis/raw-data/"
  
  # ============== #
  # Load Data
  # ============== #

    in_schr <- as.data.table(read.csv("investment-analysis/raw-data/raw-Form990-ScheduleR.csv", fill = TRUE))
    
    in_fees <- as.data.table(read.csv("investment-analysis/raw-data/raw-Form990-PartIX-Line11f.csv", fill = TRUE))
    
    in_cpi <- as.data.table(read.csv("investment-analysis/raw-data/raw-CPIAUCSL.csv", fill = TRUE))
      # Source: St. Louis Fed
    
  # ============== #
  # Ggplot stuff
  # ============== #
  
    # Define colors
    hex.gold.l   <- "#F2EEE3" #F2EEE3 - light gold
    hex.gold.m   <- "#DDBB63" #DDBB63 - medium gold
    hex.gold.d   <- "#8E6600" #8E6600 - dark gold
    hex.black    <- "#000000" #000000 - black
    hex.blue.gr  <- "#406063" # - greyish blue
    hex.blue.d   <- "#0D3847" # - dark blue
    hex.grey     <- "#696E7A" # - dark grey
    
    
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
          panel.grid.major = element_line(color = "black", linewidth = 0.5, linetype = 1),
          
          legend.title      = element_blank(),
          legend.text       = element_text(size = 12),
          legend.background = element_rect(),
          legend.position   = c(0.1,0.9)
        )
    }
    
    # Create custom plot save function
    custom_save <- function() {
      if(p_export == TRUE) {ggsave(filename = temp.filename, device = "png", path = p_dir_graphics, width = 11.9, height = 7)}
    }
    

# =============================================================================
# Figure 2: Fees paid to investment managers
# =============================================================================

  # ============== #
  # Processing
  # ============== #
  
    # Rename columns with '.' to '_'
    setnames(in_fees, colnames(in_fees), gsub("\\.", "_", colnames(in_fees)))
    
    # Parse dollar signs and commas out of values
      # create new function for str_remove  
      remove_format <- function(x) {
        
        clean_dollar <- str_remove(x, "\\$")
        clean_comma <- str_remove_all(clean_dollar, "\\,")
        
        return(clean_comma)
      }
  
    # Reclassify Investment_Management_Fees as numeric and remove monetary formatting
    dt_fees <- in_fees[, c("Investment_Management_Fees", "Investment_Income") := list(as.numeric(remove_format(Investment_Management_Fees)), as.numeric(remove_format(Investment_Income)))]
    
    
    # Average CPI data for each calendar year
    for (year in seq(2000, 2020, 1)) {
      
      in_cpi[DATE %like% year, c("Tax_Year", "Avg_CPI") := list(year, mean(CPIAUCSL))]
      
    }
    
    # Remove DATE and CPIAUCSL columns
    dt_cpi <- in_cpi[, c("DATE", "CPIAUCSL") := NULL]
    
    dt_cpi_nodups <- unique(dt_cpi[Tax_Year %in% seq(2000, 2020, 1)], by = "Tax_Year")
    
    # Calculate inflation-adjusted management fees and investment income
      
      # Merge fees data with cpi data
      dt_merged <- merge(dt_fees, dt_cpi_nodups, by = "Tax_Year")
      
      # Calculate inflation-adjusted income and fees
      dt_adjusted <- dt_merged[, c("Fees_Adjusted", "Income_Adjusted", "Notes") := list(Investment_Management_Fees/Avg_CPI, Investment_Income/Avg_CPI, NULL)]
    
      # Scale fees and income down by 1000 i.e. convert to integer thousands
      dt_adjusted[, c("Fees_Scaled", "Income_Scaled") := list(Fees_Adjusted/1000, Income_Adjusted/1000)]
      
  # ============== #
  # Visualization
  # ============== #
  
    # create theme object for these charts
    bar.theme <- theme_classic() +
                  theme(
                    axis.ticks.y      = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.title.x       = element_blank(), # for some reason, just using axis.title = element_blank() doesn't work
                    axis.title.y       = element_text(),
                    axis.line = element_line(linetype = 1),
                    axis.line.x = element_blank(),
                    axis.text = element_text(family = "Arial Narrow"),
                    
                    plot.title        = element_text(face = "bold", size = 15, family = "Georgia", 
                                                     margin = margin(t = 5, b = 5, l = 10)),
                    plot.subtitle     = element_text(color = "dark grey", size = 13, family = "Arial Narrow",
                                                     margin = margin(b = 5, l = 10)),
                    plot.caption      = element_text(color = "dark grey", vjust = -1, family = "Arial Narrow",
                                                     margin = margin(t = 5, b = 5)),
                    plot.background   = element_rect(fill = "#ffffff"),
                    plot.title.position = "plot",
                    
                    # panel.background  = element_rect(fill = "#FAFAFA"),
                    panel.grid.major.x = element_line(color = "black", linewidth = 0.1, linetype = 1),
                    panel.grid.major.y = element_line(color = "light grey", linewidth = 0.075, linetype = 1),
                    panel.grid.minor.x = element_line(color = "grey", linewidth = 0.075, linetype = 1),
                    
                    legend.title      = element_blank(),
                    legend.text       = element_text(size = 11, family = "Arial Narrow"),
                    legend.background = element_rect(fill = "#ffffff", color = "white"),
                    legend.position   = c(0.2,0.1)
                  )
      
    # Figure 2.1: Inflation-adjusted Fees paid to investment managers
    
      # set plot axis limits
      x.min <- 2000
      x.max <- 2021
      y.min <- 0
      y.max <- 1500000
    
    ggplot(dt_adjusted) + 
      geom_col(aes(x = Tax_Year, y = Fees_Adjusted), fill = hex.blue.d) +
      geom_rect(aes(xmin = 2000, xmax = 2008, ymin = y.min, ymax = y.max), fill = hex.grey, alpha = 0.01) +
      geom_text(aes(x = 2004, y = (0.6*y.max), label = "No Data Available")) +
      geom_vline(aes(xintercept = 2019.5), color = hex.gold.m) + 
      geom_label(aes(x = 2019.5, y = (0.9*y.max), label = "Daniel Diermeier\nbecomes Chancellor")) +
      # geom_col(aes(x = Tax_Year, y = Income_Adjusted), fill = hex.gold.l) +
      scale_x_continuous(limits = c(x.min, x.max), breaks = seq(x.min, x.max, 1), expand = expansion(add = 2)) +
      scale_y_continuous(limits = c(y.min, y.max), breaks = seq(y.min, y.max, (0.1*y.max)), minor_breaks = seq(y.min, y.max, (0.05*y.max))) + 
      labs(
        title = "Fees Paid to Vanderbilt's Investment Managers",
        subtitle = "Inflation adjusted dollars",
        caption = "Source: Vanderbilt IRS Form 990" # or maybe I say "Source: Internal Revenue Service"
      ) +
      ylab("Thousands of Dollars") +
      bar.theme
      
    temp.filename <- "year_fees_adjusted"
    
    custom_save()
    
    # Figure 2.2: Inflation-adjusted Investment Income
    
      # set plot axis limits
      x.min <- 2000
      x.max <- 2021
      y.min <- 0
      y.max <- 5500000
    
    ggplot(dt_adjusted) + 
      # geom_col(aes(x = Tax_Year, y = Fees_Adjusted), fill = hex.blue.d) +
      geom_col(aes(x = Tax_Year, y = Income_Adjusted), fill = hex.gold.l) +
      geom_vline(aes(xintercept = 2019.5), color = hex.gold.m) + 
      geom_label(aes(x = 2019.5, y = (0.9*y.max), label = "Daniel Diermeier\nbecomes Chancellor")) +
      scale_x_continuous(limits = c(x.min, x.max), breaks = seq(x.min, x.max, 1), expand = expansion(add = 2)) +
      scale_y_continuous(limits = c(y.min, y.max), breaks = seq(y.min, y.max, (0.1*y.max)), minor_breaks = seq(y.min, y.max, (0.05*y.max))) + 
      labs(
        title = "Vanderbilt's Investment Income",
        subtitle = "Inflation adjusted dollars",
        caption = "Source: Vanderbilt IRS Form 990" # or maybe I say "Source: Internal Revenue Service"
      ) +
      ylab("Thousands of Dollars") +
      bar.theme
    
    temp.filename <- "year_income_adjusted"
    
    custom_save()
    
    # Figure 2.3: Mixed graph, same axis
    ggplot(dt_adjusted) + 
      geom_col(aes(x = Tax_Year, y = Income_Adjusted), fill = hex.gold.l) +
      geom_col(aes(x = Tax_Year, y = Fees_Adjusted), fill = hex.blue.d) +
      scale_x_continuous(limits = c(2000, 2021), breaks = seq(2000, 2021, 1), expand = expansion(add = 2)) +
      scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, 0.1), minor_breaks = seq(0, 6, 0.05)) + 
      labs(
        title = "Vanderbilt's Investment Expenses",
        subtitle = "Inflation adjusted dollars",
        caption = "Source: Vanderbilt IRS Form 990" # or maybe I say "Source: Internal Revenue Service"
      ) +
      ylab("Millions of Dollars") +
      bar.theme
    
    temp.filename <- "income_fees_combined"
    
    custom_save()
    
    # Figure 2.4: Mixed graph, separate axes - INCOMPLETE
    ggplot(dt_adjusted) + 
      geom_col(aes(x = Tax_Year, y = Income_Adjusted), fill = hex.gold.l) +
      geom_col(aes(x = Tax_Year, y = Fees_Adjusted), fill = hex.blue.d) +
      scale_x_continuous(limits = c(2000, 2021), breaks = seq(2000, 2021, 1), expand = expansion(add = 2)) +
      scale_y_continuous(name = "First Axis", sec.axis = sec_axis(trans = ~./5, name = "Second Axis"), limits = c(0, 6), breaks = seq(0, 6, 0.1), minor_breaks = seq(0, 6, 0.05)) + 
      labs(
        title = "Fees Paid to Vanderbilt's Investment Managers",
        subtitle = "Inflation adjusted dollars",
        caption = "Source: Vanderbilt IRS Form 990" # or maybe I say "Source: Internal Revenue Service"
      ) +
      ylab("Millions of Dollars") +
      bar.theme
    
    temp.filename <- "income_fees_combined_diffaxis"
    
    custom_save()
    
    
# =============================================================================
# Figure 1: number and lifespan of Vanderbilt's Investment Companies 
# =============================================================================
    
    # ============== #
    # Processing
    # ============== #
    
    # Reclassifying Year and Entity.Name
    dt_schr <- in_schr[, c("Year", "Entity.Name") := list(as.numeric(Year), as.factor(Entity.Name))]
    
    # create another dataset with only investment-related companies and generate new variables
    rangeyear <- dt_schr[Purpose %in% c("Investments", "Investment", "Holding Company", "Investment Holding"), 
                         c("Last_Year", "First_Year", "Age") := list(max(Year), min(Year), max(Year) - min(Year)),
                         by = Entity.Name]
    
    # Reorder Entity.Name as a factor based on the first year the entity appears
    rangeyear[, Entity.Name := fct_reorder(rangeyear[, Entity.Name], -rangeyear[, First_Year])]
    
    # change row order in dataset
    setorder(rangeyear, cols = First_Year)
    
    # ============== #
    # Visualization
    # ============== #
    
    # Create base ggplot object with filtered dataset
    gg <- ggplot(rangeyear[Purpose %in% c("Investments", "Investment", "Holding Company", "Investment Holding") & Age != 0], aes(x = Entity.Name, y = Year))
    
    # Create dumbell chart of investment companies
    p.entity.lifespan <- gg + 
      geom_path() +
      geom_point(aes(x = Entity.Name, y = First_Year, color = "First Appearance")) +
      geom_point(aes(x = Entity.Name, y = Last_Year, color = "Most Recent Appearance")) + 
      scale_color_manual(values = c("#DDBB63", "#000000")) +
      labs(
        title = "Vanderbilt's Investment Organizations, 2000 - 2020",
        subtitle = "Companies used by Vanderbilt for investment purposes",
        caption = "Source: Vanderbilt IRS Form 990"
      ) + 
      scale_y_continuous(limits = c(2000, 2020), breaks = seq(2000, 2020, 2), expand = expansion(add = 2)) +
      theme_classic() +
      theme(
        axis.ticks.y      = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x       = element_blank(), # for some reason, just using axis.title = element_blank() doesn't work
        axis.title.y       = element_blank(),
        axis.line = element_line(linetype = 1),
        axis.line.x = element_blank(),
        axis.text = element_text(family = "Arial Narrow"),
        
        plot.title        = element_text(face = "bold", size = 15, family = "Georgia", 
                                         margin = margin(t = 5, b = 5, l = 10)),
        plot.subtitle     = element_text(color = "dark grey", size = 13, family = "Arial Narrow",
                                         margin = margin(b = 5, l = 10)),
        plot.caption      = element_text(color = "dark grey", vjust = -1, family = "Arial Narrow",
                                         margin = margin(t = 5, b = 5)),
        plot.background   = element_rect(fill = "#ffffff"),
        plot.title.position = "plot",
        
        # panel.background  = element_rect(fill = "#FAFAFA"),
        panel.grid.major.x = element_line(color = "black", linewidth = 0.1, linetype = 1),
        panel.grid.major.y = element_line(color = "light grey", linewidth = 0.075, linetype = 1),
        panel.grid.minor.x = element_line(color = "light grey", linewidth = 0.075, linetype = 1),
        
        legend.title      = element_blank(),
        legend.text       = element_text(size = 11, family = "Arial Narrow"),
        legend.background = element_rect(fill = "#ffffff", color = "white"),
        legend.position   = c(0.2,0.1)
        
        
      ) +
      coord_flip()
    
    # Display plot
    p.entity.lifespan
    
    temp.filename <- "entity_lifespan"
    
    # Save plot
    custom_save()
    
# =============================================================================
# =============================== Workspace ====================================
# =============================================================================

    
  custom.theme <- theme(
                    title,
                    aspect.ratio,
                    
                    
                    axis.title,
                    axis.title.x,
                    axis.title.x.top,
                    axis.title.x.bottom,
                    axis.title.y,
                    axis.title.y.left,
                    axis.title.y.right,
                    
                    axis.text,
                    axis.text.x,
                    axis.text.x.top,
                    axis.text.x.bottom,
                    axis.text.y,
                    axis.text.y.left,
                    axis.text.y.right,
                    
                    axis.ticks,
                    axis.ticks.x,
                    axis.ticks.x.top,
                    axis.ticks.x.bottom,
                    axis.ticks.y,
                    axis.ticks.y.left,
                    axis.ticks.y.right,
                    axis.ticks.length,
                    axis.ticks.length.x,
                    axis.ticks.length.x.top,
                    axis.ticks.length.x.bottom,
                    axis.ticks.length.y,
                    axis.ticks.length.y.left,
                    axis.ticks.length.y.right,
                    
                    axis.line,
                    axis.line.x,
                    axis.line.x.top,
                    axis.line.x.bottom,
                    axis.line.y,
                    axis.line.y.left,
                    axis.line.y.right,
                    
                    
                    legend.background,
                    legend.margin,
                    
                    legend.spacing,
                    legend.spacing.x,
                    legend.spacing.y,
                    
                    legend.key,
                    legend.key.size,
                    legend.key.height,
                    legend.key.width,
                    
                    legend.text,
                    legend.text.align,
                    
                    legend.title,
                    legend.title.align,
                    legend.position,
                    legend.direction,
                    legend.justification,
                    
                    legend.box,
                    legend.box.just,
                    legend.box.margin,
                    legend.box.background,
                    legend.box.spacing,
                    
                    
                    panel.background,
                    panel.border,
                    
                    panel.spacing,
                    panel.spacing.x,
                    panel.spacing.y,
                    
                    panel.grid,
                    panel.grid.major,
                    panel.grid.minor,
                    panel.grid.major.x,
                    panel.grid.major.y,
                    panel.grid.minor.x,
                    panel.grid.minor.y,
                    panel.ontop,
                    
                    
                    plot.background,
                    plot.title,
                    plot.title.position,
                    plot.subtitle,
                    plot.caption,
                    plot.caption.position,
                    plot.tag,
                    plot.tag.position,
                    plot.margin,
                    
                    
                    strip.background,
                    strip.background.x,
                    strip.background.y,
                    strip.clip,
                    strip.placement,
                    strip.text,
                    strip.text.x,
                    strip.text.y,
                    strip.switch.pad.grid,
                    strip.switch.pad.wrap
                  )
                    

# =============================================================================
# =============================== Export ====================================
# =============================================================================

    # NOTE: exporting graphs is handled individually immediately after generation
    