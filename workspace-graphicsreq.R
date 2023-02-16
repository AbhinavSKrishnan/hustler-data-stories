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
  # Set ggplot options
  # ============== #
  
    theme.hustler <- theme_classic() +
      theme(
        axis.ticks.x       = element_blank(),
        axis.ticks.y       = element_blank(),
        axis.title.y       = element_blank(), # for some reason, just using axis.title = element_blank() doesn't work
        axis.title.x       = element_blank(),
        axis.line          = element_line(linetype = 1),
        axis.line.y        = element_blank(),
        axis.text          = element_text(size = 12, family = "Arial Narrow", margin = margin(r = 0)),
        
        plot.title        = element_text(color = "black", face = "bold", size = 20, family = "Georgia", 
                                         margin = margin(t = 5, 
                                                         b = 5, 
                                                         l = 10)),
        plot.subtitle     = element_text(color = "#696E7A", size = 13, family = "Arial Narrow", 
                                         margin = margin(b = 5, 
                                                         l = 10)),
        plot.caption      = element_text(color = "dark grey", vjust = -1, family = "Arial Narrow",
                                         margin = margin(t = 5, 
                                                         b = 5)),
        plot.background     = element_rect(fill = "#ffffff"),
        plot.title.position = "plot",
        
        # panel.background  = element_rect(fill = "#FAFAFA"),
        panel.grid.major.y  = element_line(color = "black", linewidth = 0.1, linetype = 1),
        panel.grid.major.x  = element_line(color = "light grey", linewidth = 0.075, linetype = 1),
        
        legend.title        = element_blank(),
        legend.text         = element_text(size = 13, family = "Arial Narrow"),
        legend.background   = element_rect(fill = "#ffffff", color = "white"),
        legend.position     = c(0.2,0.1)
      )
  
    
    
# =============================================================================
# ============================ Greek Life Rush ================================
# =============================================================================
  
    # Create data.table with values
    dt.glrush <- data.table(c("Panhellenic Council", "Interfraternity Council"), c(292, 201), c(264, 163))
    setnames(dt.glrush, c("V1", "V2", "V3"), c("Conference", "2023", "2022"))
    
    # Melt long
    dt.glrush.long <- melt(dt.glrush, id.vars = "Conference", measure.vars = c("2023", "2022"), variable.name = "Year", value.name = "Enrollment")
    
    dt.glrush.long[, Year := factor(Year, levels = c("2022", "2023"))]
    
    ggplot(dt.glrush.long) + 
      geom_col(aes(x = Conference, y = Enrollment, fill = Year), position = "dodge") +
      # geom_text(aes(x = Conference, y = Enrollment, label = Enrollment), color = "white") + 
        # can't seem to get this to work
      scale_fill_manual(values = c("#DDBB63", "#0D3847")) +
      scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50), minor_breaks = seq(0, 300, 5)) +
      labs(
        title    = "Rush Enrollment in Greek Life at Vanderbilt",
        subtitle = "Transfers and upperclassmen are not included",
        caption  = "Source: Vanderbilt Office of Greek Life"
      ) + 
      theme.hustler + 
        theme(
          axis.line.x = element_blank(),
          axis.text.x = element_text(face = "bold"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_line(color = "dark grey", linewidth = 0.1, linetype = 1),
          legend.position = c(0.12, 0.88)
        )
    
    ggsave(filename = "gl-rush-enrollment", path = "/Users/abhinavskrishnan/Library/CloudStorage/GoogleDrive-abhinav.s.krishnan@vanderbilt.edu/.shortcut-targets-by-id/16L9lFy_3QQbFyta0cYDOLE_M9fFsDx_I/Vanderbilt Hustler 2022-23 Editor Access/Vanderbilt Hustler 2022-23/News/In Progress/", device = "png")
    