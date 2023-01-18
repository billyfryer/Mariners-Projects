# Libraries
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(devtools)
# devtools::install_github("camdenk/mlbplotR")
library(mlbplotR)
library(gridExtra)
library(png)
library(RCurl)
library(cowplot)
library(ggthemes)
library(grid)

# Read in Raw Data
raw_data <- read_csv(file = "Pitcher X Data 2023.csv")

unique_pitches <- unique(raw_data$PitchType)

# Mariners Colors
navy_blue <- "#0C2C56"
northwest_green <- "#005C5C"
silver <- "#C4CED4"
red <- "#D50032"

# Strike Zone Plot Set-Up
topKzone <- 3.5
botKzone <- 1.5
inKzone <- -.71
outKzone <- .71
kZone <- data.frame(
  x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
  y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
)

# Basic Stats Table
basic_stats_table <- function(pitchtype) {

  # By Pitch Table
  by_pitch_table <- raw_data %>% 
    filter(PitchType == str_to_upper(pitchtype)) %>% 
    group_by(PitchType) %>% 
    summarize(SampleSize = n(),
              AvgReleaseSpeed = mean(ReleaseSpeed, na.rm = TRUE),
              AvgSpinRate = mean(SpinRate, na.rm = TRUE),
              AvgInducedVertBreak = mean(InducedVertBreak, na.rm = TRUE),
              AvgHorzBreak = mean(HorzBreak, na.rm = TRUE),
              avg_swing_prob = mean(swing_prob, na.rm = TRUE),
              avg_whiff_prob = mean(whiff_prob, na.rm = TRUE)) %>% 
    ungroup() %>% 
    # Convert avg_swing_prob and avg_whiff_prob to percent
    mutate(`Sample Size` = SampleSize,
           `Avg Velocity` = round(AvgReleaseSpeed, 1),
           `Avg Spin Rate` = round(AvgSpinRate, 1),
           `Avg Induced\nVert Break` = round(AvgInducedVertBreak, 1),
           `Avg Horz Break` = round(AvgHorzBreak, 1),
           `Avg\nSwing Prob` = paste0(round(100*avg_swing_prob, 1), "%"),
           `Avg\nWhiff Prob` = paste0(round(100*avg_whiff_prob, 1), "%"),
           `Pitch Type` = str_to_sentence(PitchType),
           across(.cols = everything(),
                  .fns = as.character)) %>% 
    # Select only the clean columns
    select(`Pitch Type`, `Sample Size`, `Avg Horz Break`, `Avg\nSwing Prob`,
           `Avg Spin Rate`, `Avg Velocity`,`Avg Induced\nVert Break`,   `Avg\nWhiff Prob`)
  
  # Need to stack by_pitch_table into a 4x4 grid so that it fits in arrange
  top_half <- by_pitch_table[,1:4]
  bottom_half <- by_pitch_table[,5:8]
  
  # Save the colnames as a vector
  top_half_headers <- colnames(top_half)
  bottom_half_headers <- colnames(bottom_half)
  
  # Replace colnames with c("V1", "V2", "V3", "V4")
  # Need column names to be the same to combine
  names(top_half) <- c("V1", "V2", "V3", "V4")
  names(bottom_half) <- c("V1", "V2", "V3", "V4")
  names(top_half_headers) <- c("V1", "V2", "V3", "V4")
  names(bottom_half_headers) <- c("V1", "V2", "V3", "V4")
  
  # Bind 4 rows together
  stacked_table <- bind_rows(top_half_headers, top_half,
                             bottom_half_headers, bottom_half)
  
  # Make row and column names NULL
  row.names(stacked_table) <- NULL
  names(stacked_table) <- NULL
  
  base_table <-
    stacked_table %>%
    # Create ggtexttable
    ggtexttable( 
      rows = NULL,
      # Format Column Header
      theme = ttheme(base_style = "classic",
                     padding = unit(c(2, 2), "mm"))) %>% 
    # Title
    tab_add_title(text = paste(str_to_sentence(pitchtype), "Metrics"),
                  size = 14,
                  face = "bold",
                  # Not sure why this works but it does and
                  # I'm not going to touch it
                  just = -0.75) 
  
  # Color Cell Backgrounds
  final_table <-  table_cell_bg(base_table, 
                                row = c(2,4), 
                                column = 1:tab_ncol(base_table), 
                                linewidth = 5,
                                fill= northwest_green,
                                alpha = 0.5) %>% 
    # Change Font Size For Rest
    table_cell_font(row = 2:tab_nrow(base_table), 
                    column = 1:tab_ncol(base_table),
                    size = 8)
    
  return(final_table)
}

# Whiff Plot by Location Plot
whiff_plot_loc <- function(pitchtype) {
  
  # Filter Data to only that pitch type
  plotting_data <- raw_data %>% 
    filter(PitchType == str_to_upper(pitchtype)) %>% 
    arrange(whiff_prob) %>% 
    mutate(HitterStatus = case_when(BatterSide == "Right" ~ "RHH",
                                    TRUE ~ "LHH"))
  
  # Plot
  final_plot <- ggplot(plotting_data, aes(x = PlateSide,
                       y = PlateHeight)) +
    # Plot point
    geom_point(aes(color = whiff_prob),
               alpha = 0.8) +
    # Facet by Batter side
    facet_grid(~HitterStatus) +
    # Colors from silver to red
    # Scale Color from 0 to 0.4
    # Percents on scale rather than decimals
    scale_color_gradient(high = red, 
                         low = silver,
                         labels = scales::percent,
                         limits = c(0,0.4)) +
    # Plots Strike Zone
    geom_path(data=kZone, aes(x,y),  
              lwd=.5, col="black") +
    # Scale x and y
    scale_x_continuous(limits = c(-3,3)) +
    scale_y_continuous(limits = c(0,5)) +
    # Labels
    labs(title = paste0("Whiff Prob by Location", "\n", 
                        str_to_sentence(pitchtype)),
         subtitle = "From Pitchers Point of View",
         x = "",
         y = "",
         color = "Whiff Prob") +
    # Theme
    theme_foundation() +
    theme(axis.title = element_text(face = "bold"),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5,
                                    size = 14,
                                    face = "bold"),
          plot.subtitle = ggtext::element_markdown(hjust = 0.5,
                                                   size = 8,
                                                   face = "italic"),
          plot.background = element_rect(fill = "white",
                                         color = "white"),
          legend.key.size = unit(10, units = "points")
    ) +
    # Coord Fixed
    coord_fixed(ratio = 1)
  
  return(final_plot)
}