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

  # okay this script needs to process the data (import, melt long and stuff) and create visualizations

  # do I need to do anything else with this data?

# source: https://www.vanderbilt.edu/edi-report/campus-community-data/

    # this is what I'm thinking:
      # definitely no pie charts lol
      # I need some way to demonstrate change over time compared to the U.S. census
      # NEED TO DOWNLOAD US CENSUS DEMOGRAPHIC DATA FOR OUR GENERATION
      # line chart/bar chart by year for black students

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
    library(purrr)
    library(ggthemes)
    library(gganimate)
    library(gifski)
    library(av)
  
  # ============== #
  # Set Parameters
  # ============== #
  
    # set export toggle
    p_export <- T
    
    # set timestamp
    p_timestamp <- Sys.time()
    
    # set output directory
    p_dir_out <- "edi-analysis/00_output/"
    
    # set graphics output
    p_graphics_out <- "edi-analysis/graphics/"
    
    # set input directory
    p_dir_in_base <- "edi-analysis/00_input/"
  

# =============================================================================
# =============================== Data Loading ====================================
# =============================================================================
    

    # create list of dataset filenames
    ls_in_names <- as.vector(list.files(p_dir_in_base))
    
    # create list of dataset filepaths
    ls_in_dirs <- paste0(p_dir_in_base, ls_in_names)
    
    # create a function to import datasets, reshape, and output
    import.shape <- function(in_data){
      
      temp_data <- as.data.table(read.csv(in_data))
      
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
        
        long_data[, "Race Category" := factor(ifelse(Race == "White", "White", 
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
    race.ugp.category <- race.ugp[, sum(Percentage), by = list(Type, Year, `Race Category`)]
    setnames(race.ugp.category, "V1", "Total Percentage")
    
    # Set levels of race category variable
    levels(race.ugp.category[, `Race Category`])
    
    # placeholder variable
    race.firstyear[, Empty := NA]
    
    
# =============================================================================
# =============================== Graphing ====================================
# =============================================================================

  # ============== #
  # GGplot2 Settings
  # ============== #
  
    # colors
      #F2EEE3 - silver
      #DDBB63 - gold
      #8E6600 - brown
      #000000 - black
    
    # set ggtheme
    # theme_set(theme_classic())
    
    # modify theme
    base.theme <- theme(
                  axis.line.y = element_blank(),
                  axis.title = element_text(face = "bold"),
                  axis.title.x = element_text(vjust = -1),
                  plot.title = element_text(face = "bold", size = 20, family = "serif"),
                  plot.subtitle = element_text(color = "#4f4f4f", size = 13),
                  plot.caption = element_text(vjust = -1),
                  plot.background = element_rect(fill = "#FAFAFA"),
                  panel.background = element_rect(fill = "#FAFAFA"),
                  legend.title = element_blank(),
                  legend.text = element_text(size = 12),
                  legend.background = element_rect(fill = "#FAFAFA"),
                  legend.position = c(0.9,0.8)
                )
    
    # Create graph for ug racial composition plot
        ggplot(data = race.ugp.category[Type == "Undergraduate" & `Race Category` != "Unknown"], mapping = aes(x = Year, y = `Total Percentage`, color = `Race Category`)) + 
        geom_point() + 
        geom_line(aes(group = `Race Category`)) + 
        scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) + 
        scale_color_manual(values = c("#DDBB63", "#000000", "#8E6600")) +
        labs(
          title = "Change in Racial Composition of Undergraduate Students",
          subtitle = "Students with unknown race are not included",
          caption = "Source: Vanderbilt Office for Equity, Diversity, and Inclusion"
        ) +
        ylab("Percentage") +
        xlab("Academic Year") +
        base.theme
    
    # Create an a graph for firstyear racial composition plot
        ggplot(data = race.firstyear[`Race Category` != "Unknown"], mapping = aes(x = Year, y = `Percentage`, color = Race)) + 
        geom_point() + 
        geom_line(aes(group = `Race`)) + 
        scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) + 
        #scale_color_manual(values = c("#DDBB63", "#000000", "#8E6600")) +
        labs(
          title = "Change in Racial Composition of Firstyear Students",
          subtitle = "Students with unknown race are not included",
          caption = "Source: Vanderbilt Office for Equity, Diversity, and Inclusion"
        ) +
        ylab("Percentage") +
        xlab("Academic Year") +
        base.theme
    
    # Create an animated graph
        
    anim.firstyear <- ggplot(race.firstyear[`Race Category` != "Unknown" & Race != "Race Unknown"]) +
                  geom_col(aes(x = Empty, y = Percentage, fill = Race), position = "fill") +
                  # scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) + 
                  labs(
                    title = "Change in Racial Composition of First Year Students",
                    subtitle = "Year: {frame_time}",
                    caption = "Source: Vanderbilt Office for Equity, Diversity, and Inclusion"
                  ) +
                  scale_color_manual(values = c("#DDBB63", "#9b8e45", "#63c2dd", "#45889b", "#dd63ac", "#000000", "#F2EEE3")) +
                  ylab("Proportion of Students") +
                  base.theme +
                    theme(axis.ticks.y = element_blank(),
                          axis.title.y = element_blank(),
                          axis.text.y = element_blank(),
                          axis.line.y = element_blank(),
                          legend.position = c(0.8,0.8)
                    ) +
                  coord_flip() + scale_y_reverse() +
                  transition_time(as.integer(Year)) +
                  ease_aes('linear')
    
    
# =============================================================================
# =============================== Export ====================================
# =============================================================================

    if (p_export == TRUE) {
      
      saveRDS(race.all, file = paste0(p_graphics_out, "out_er_all"))
      saveRDS(race.firstyear, file = paste0(p_graphics_out, "out_er_firstyear"))
      saveRDS(race.ugp, file = paste0(p_graphics_out, "out_er_ugp"))
      
      write.csv(race.all, file = paste0(p_graphics_out, "out-er-all.csv"))
      write.csv(race.ugp, file = paste0(p_graphics_out, "out-er-ugp.csv"))
      
      write.csv(race.ugp.category, file = paste0(p_graphics_out, "out-race-ugp-categorical.csv"))
      
      anim_save("anim-race-firstyear.gif", anim.firstyear, p_graphics_out)
      
    }
    
# =============================================================================
# =============================== WORKSPACE ====================================
# =============================================================================

