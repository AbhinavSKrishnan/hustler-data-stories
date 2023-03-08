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
    library(tidyverse)
    library(stringr)
    library(purrr)
    library(ggthemes)
    library(gganimate)
    library(gifski)
    library(av)
    library(ggiraph)
    library(htmlwidgets)
  
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
  
    # Define colors
      hex.gold.l   <- "#F2EEE3" #F2EEE3 - light gold
      hex.gold.m   <- "#DDBB63" #DDBB63 - medium gold
      hex.gold.d   <- "#8E6600" #8E6600 - dark gold
      hex.black    <- "#000000" #000000 - black
      hex.blue.gr  <- "#406063" # - greyish blue
      hex.blue.d   <- "#0D3847" # - dark blue
      hex.grey     <- "#696E7A" # - dark grey
      hex.white <- "#ffffff" # - white
    
    # Create hustler theme object
    theme.hustler <- theme_classic() +
      theme(
        axis.ticks.x       = element_blank(),
        axis.ticks.y       = element_blank(),
        axis.title.y       = element_blank(), # for some reason, just using axis.title = element_blank() doesn't work
        axis.title.x       = element_blank(),
        axis.line          = element_line(linetype = 1),
        axis.line.y        = element_blank(),
        axis.text          = element_text(size = 12, family = "Arial Narrow", margin = margin(r = 0)),
        
        plot.title        = element_text(color = "black", face = "bold", size = 25, family = "Georgia", 
                                         margin = margin(t = 5, 
                                                         b = 5, 
                                                         l = 10)),
        plot.subtitle     = element_text(color = "#696E7A", size = 17, family = "Arial Narrow", 
                                         margin = margin(b = 5, 
                                                         l = 10)),
        plot.caption      = element_text(color = "dark grey", size = 12, vjust = -1, family = "Arial Narrow",
                                         margin = margin(t = 5, 
                                                         b = 5)),
        plot.background     = element_rect(fill = "#ffffff"),
        plot.title.position = "plot",
        
        # panel.background  = element_rect(fill = "#FAFAFA"),
        panel.grid.major.y  = element_line(color = "black", linewidth = 0.1, linetype = 1),
        panel.grid.major.x  = element_line(color = "light grey", linewidth = 0.075, linetype = 1),
        
        legend.title        = element_blank(),
        legend.text         = element_text(size = 15, family = "Arial Narrow"),
        legend.background   = element_rect(fill = "#ffffff", color = "white"),
        # legend.position     = c(0.2,0.1)
      )
  
    # Create custom plot save function
    custom_save <- function() {
      if(p_export == TRUE) {ggsave(filename = temp.filename, device = "png", path = p_dir_graphics, width = 11.9, height = 7)}
    }
    
# =============================================================================
# =========================== Anti-LGBTQ Bills =================================
# =============================================================================

    # ============== #
    # Data Processing
    # ============== #
    
    raw.legislation <- as.data.table(read.csv(file = "graphics-requests/raw_data/raw_long_Anti-LGBTQlegislation.csv"))
    raw.legislation[, Status := factor(Status, levels = c("Introduced", "Passed First Chamber", "Passed Second Chamber", "Signed into Law"))]
    
    raw.legislation <- raw.legislation %>%
      mutate(Combined_Bill_Name = str_c(Bill.Name, "/", Companian.Bill.Name)) %>% 
      mutate(Combined_Bill_Name = str_replace_all(Combined_Bill_Name, " ", ""))
    
    ggplot(raw.legislation) +
      geom_point(aes(x = Topic, y = Bill.Name, color = Status)) + 
      labs(
        title = "Status of Anti-LGBTQ Legislation in Tennessee",
        subtitle = "Fourteen bills have been introduced and two have been signed into law",
        ylab = "Bill Name"
      ) +
      coord_flip() +
      theme.hustler +
        theme(
          # panel.grid.major.x = 
        )
    
    write.csv(raw.legislation, file = "graphics-requests/processed_legislation.csv")
    
    
    
# =============================================================================
# =============================== EDI Story ====================================
# =============================================================================
    
    # ============== #
    # Data Processing
    # ============== #
    
    # create list of dataset filenames
    ls_in_names <- as.vector(list.files("edi-analysis/00_input/"))
    
    # create list of dataset filepaths
    ls_in_dirs <- paste0("edi-analysis/00_input/", ls_in_names)
    
    # create a function to import datasets, reshape, and output
    import.shape <- function(in_data){
      
      # Read csv and create datatable object
      temp_data <- as.data.table(read.csv(in_data))
      
      # Melt long
      long_data <- melt(temp_data, 
                        measure.vars = c(paste0("X", seq(2011, 2022, 1))), 
                        variable.name = "Year", value.name = "Percentage")
      
      # remove leading X from year column and trailing % from Percentage column
      #long_data[, .SD := .(gsub("X", "", Year), gsub("%", "", Percentage)), .SDcols = c('Year', "Percentage")]
      
      # going to brute force it for now
      long_data[, Year := gsub("X", "", Year)]
      long_data[, Percentage := as.numeric(gsub("%", "", Percentage))]
      
      # Create binary white nonwhite race variable
      
      # Vector of unique races
      v.race <- unique(long_data[, Race])
      v.white <- c("White")
      # v.unkno <- c("International", "Race Unknown")
      # NOTE: Vanderbilt does not provide more specific racial information about International students. As a result, we will classify them as "Other" to avoid including them in either of the other categories
      v.nonwhite <- c("Black", "Hispanic", "American Indian", "Asian/Pacific Islander", "Two or More Races")
      # NOTE: Biracial students were added to the nonwhite category since they would be at most 50% white
      
      long_data[, "Race_Category" := factor(ifelse(Race == "White", "White", 
                                                   ifelse(Race == "International", "International", 
                                                          ifelse(Race %in% v.nonwhite, "Nonwhite", "Unknown"))), levels = c("White", "Nonwhite", "International", "Unknown"))]
      
    }
    
    # apply function to list of dataset filenames
    ls_in_data <- map(ls_in_dirs, import.shape)
    
    # set names for datasets in list
    names(ls_in_data) <- c(ls_in_names)
    
    # set list items as objects
    race.ugp <- ls_in_data$`er-ugp.csv`
    race.all <- ls_in_data$`er-all.csv`
    race.firstyear <- ls_in_data$`er-firstyear.csv`
    
    # Create a new datatable with the sum of Race Categories
    race.ugp.category <- race.ugp[, sum(Percentage), by = list(Type, Year, Race_Category)]
    setnames(race.ugp.category, "V1", "Total_Percentage")
    
    # Create Total_Percentage character column
    race.ugp.category[, str_Total_Percentage := paste0(Total_Percentage, "%")]
    
    # Set levels of race category variable
    levels(race.ugp.category[, Race_Category])
    
    # placeholder variable
    race.firstyear[, Empty := NA]
    
    # ============== #
    # Visualization
    # ============== #
    
      # Create graph for undergraduate racial composition plot - STATIC
      ggplot(data = race.ugp.category[Type == "Undergraduate" & `Race_Category` != "Unknown"], mapping = aes(x = Year, y = `Total_Percentage`, color = `Race_Category`)) + 
        geom_point() + 
        geom_line(aes(group = `Race_Category`)) + 
        scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) + 
        scale_color_manual(values = c("#DDBB63", "#000000", "#8E6600")) +
        labs(
          title = "Change in Racial Composition of Undergraduate Students",
          subtitle = "Students with unknown race are not included",
          caption = "Source: Vanderbilt Office for Equity, Diversity, and Inclusion"
        ) +
        ylab("Percentage") +
        xlab("Academic Year") +
        theme.hustler +
          theme(
            legend.position     = c(0.9, 0.9),
            legend.background   = element_blank(),
            panel.grid.major.y  = element_line(color = "light grey", linewidth = 0.075, linetype = 1),
            panel.grid.major.x  = element_line(color = "black", linewidth = 0.01, linetype = 1)
            )
      
      temp.filename <- "race_ugp_static"
      
      custom_save()
      
      # Create graph for undergraduate racial composition plot - INTERACTIVE
      race.ugp.interactive <- ggplot(data = race.ugp.category[Type == "Undergraduate" & `Race_Category` != "Unknown"], mapping = aes(x = Year, y = `Total_Percentage`, color = `Race_Category`)) + 
        geom_line_interactive(aes(group = `Race_Category`, data_id = Race_Category), linewidth = 2) +
        geom_point_interactive(aes(tooltip = paste0(Race_Category, ": ", str_Total_Percentage), data_id = Race_Category), size = 3) + 
        scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) + 
        scale_color_manual(values = c(hex.blue.d, hex.gold.m, hex.grey)) +
        labs(
          title = "Change in Racial Composition of Undergraduate Students",
          subtitle = "The percentage of white undergraduates at Vanderbilt has decreased consistently since 2011 \n(Students with unknown race are not included)",
          caption = "Source: Vanderbilt Office for Equity, Diversity, and Inclusion"
        ) +
        ylab("Percentage") +
        xlab("Academic Year") +
        theme.hustler +
        theme(
          legend.position     = c(0.9, 0.9),
          legend.background   = element_blank(),
          panel.grid.major.y  = element_line(color = "light grey", linewidth = 0.075, linetype = 1),
          panel.grid.major.x  = element_line(color = "black", linewidth = 0.01, linetype = 1)
        )
      
      race.Widget <- girafe(ggobj = race.ugp.interactive, width_svg = 11.9, height_svg = 7) %>%
        girafe_options(
          opts_hover_inv(css = "opacity:0.1;"), # makes other interactive geoms disappear upon hover
          opts_hover(css = "fill:#fffffff") # 
          )
      
      race.Widget
      
      htmlwidgets::saveWidget(race.Widget, "race_widget_page.html", selfcontained = TRUE)
      
      
      # Create an a graph for first-year racial composition plot
      ggplot(data = race.firstyear[Race_Category != "Unknown"], mapping = aes(x = Year, y = Percentage, color = Race)) + 
        geom_point() + 
        geom_line(aes(group = `Race`)) + 
        scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) + 
        #scale_color_manual(values = c("#DDBB63", "#000000", "#8E6600")) +
        labs(
          title = "Change in Racial Composition of First-Year Students",
          subtitle = "Students with unknown race are not included",
          caption = "Source: Vanderbilt Office for Equity, Diversity, and Inclusion"
        ) +
        ylab("Percentage") +
        xlab("Academic Year") +
        theme.hustler
      
      temp.filename <- "race_fy_static"
      
      custom_save()
    
# =============================================================================
# ============================ Greek Life Rush ================================
# =============================================================================
  
    # Create data.table with values
    dt.glrush <- data.table(c("Panhellenic Council", "Interfraternity Council"), c(292, 201), c(264, 163))
    setnames(dt.glrush, c("V1", "V2", "V3"), c("Conference", "2023", "2022"))
    
    # Melt long
    dt.glrush.long <- melt(dt.glrush, id.vars = "Conference", measure.vars = c("2023", "2022"), variable.name = "Year", value.name = "Enrollment")
    
    dt.glrush.long[, Year := factor(Year, levels = c("2022", "2023"))]
    
    # Create bar chart with enrollment data - INTERACTIVE
    ggplot(dt.glrush.long) + 
      geom_col_interactive(aes(x = Conference, y = Enrollment, fill = Year, tooltip = Enrollment, data_id = Year), position = "dodge") +
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
    
    # Convert to interactive girafe object  
    gl.enrollment <- girafe(ggobj = last_plot(), width_svg = 11.9, height_svg = 7) %>%
      girafe_options(
        opts_hover_inv(css = "opacity:0.1;") # makes other interactive geoms disappear upon hover
        # opts_hover(css = "fill:#696E7A")
      )
    
    gl.enrollment
    
    # Save as html file
    htmlwidgets::saveWidget(gl.enrollment, "gl_enrollment_interactive", selfcontained = TRUE)
    
    
    
    # Create bar chart with enrollment data - STATIC
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
    
    # Save to Hustler directory
    ggsave(filename = "gl-rush-enrollment", path = "/Users/abhinavskrishnan/Library/CloudStorage/GoogleDrive-abhinav.s.krishnan@vanderbilt.edu/.shortcut-targets-by-id/16L9lFy_3QQbFyta0cYDOLE_M9fFsDx_I/Vanderbilt Hustler 2022-23 Editor Access/Vanderbilt Hustler 2022-23/News/In Progress/", device = "png")
    
