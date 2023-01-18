# Slide Graphics
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

# Potentially tipping pitches by arm slot?
ggplot(raw_data, aes(x = ReleaseSide,
                     y = ReleaseHeight)) +
    # Plot point
    geom_point(aes(color = PitchType),
               alpha = 0.5) +
    #facet_wrap() +
    #geom_smooth() +
  ylim(0,7) +
  xlim(-3,0) +
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
  labs(title = "Arm Slots",
       x = "",
       y = "",
       color = "Pitch Type")

ggsave("Images/Arm Slots.png")

### Change Up
#'
#'
#'
### Curveball
#'
#' Inc ReleaseSpeed = Inc Whiff Strong
#' Inc InducedVertBreak = Dec Whiff Prob (Less Break == Worse Pitch) Strong
#'
### Cutter
#'
#' Inc HorzBreak = Inc Whiff Prob Strong
#' Inc InducedVertBreak = Dec Whiff Prob (Less Break == Worse Pitch) Strong
#' 
### Fastball
#'
#' Inc HorzBreak = Inc Whiff Prob Weak
#' Inc ReleaseSpeed = Inc Whiff Strong
#' 

pitch_type <- "CURVEBALL"
metric <- "Spin Rate"

filtered_data <- plotting_data <- raw_data %>% 
  filter(PitchType == pitch_type) %>% 
  arrange(whiff_prob)

# variable by Whiff Prob Graph
ggplot(filtered_data, aes(x = SpinRate,
                     y = whiff_prob)) +
  # Plot point
  geom_point(color = navy_blue,
             alpha = 0.5) +
  # Scale y percent
  scale_y_continuous(labels = scales::percent) +
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
  labs(title = paste(metric, "by Whiff Prob"),
       x = metric,
       y = "Whiff Prob") +
  geom_smooth(color = red,
              se = FALSE)

image_name <- paste0("Images/", pitch_type, "-", metric, ".png")

ggsave(filename = image_name,
       width = 6,
       height = 4)
