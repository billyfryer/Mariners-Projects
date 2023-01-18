# Create Report

# Source Graphics-Code and other libraries not in Graphics-Code
source("Report-Graphics.R")

# Get Mariners Logo
logo <- load_mlb_teams() %>% 
  filter(team_abbr == "SEA") %>% 
  pull(team_logo_espn)

logo_grob <- rasterGrob(readPNG(getURLContent(logo)))

# Arrange Heading
heading_text_grob <- text_grob("Pitcher X Report",
                               face = "bold",
                               size = 30)
full_heading <- plot_grid(logo_grob, 
                             heading_text_grob, 
                             logo_grob, 
                             ncol = 3)

# Create Grob by Pitch
by_pitch_grob <- function(pitchtype_input) {

  # Make Graphics
  pitch_whiff_plot <- whiff_plot_loc(pitchtype_input)
  pitch_basic_stats <- basic_stats_table(pitchtype_input)
  pitch_body <- plot_grid(plotlist = list(pitch_whiff_plot, pitch_basic_stats), 
                          ncol = 2
                       )

  # Return body
  return(pitch_body)
}

# By Pitches
pitch_1 <- by_pitch_grob(str_sort(unique_pitches)[1])
pitch_2 <- by_pitch_grob(str_sort(unique_pitches)[2])
pitch_3 <- by_pitch_grob(str_sort(unique_pitches)[3])
pitch_4 <- by_pitch_grob(str_sort(unique_pitches)[4])


# Arrange all into 1 graphic
report <- list(plot_grid(full_heading, pitch_1, pitch_2, pitch_3, pitch_4,
                          ncol = 1,
                         rel_heights = c(1,rep(1.35,times = 4))
                         ))

# ggexport exports the document
# you can mess around with the height and width to see how much info 
# you wanna get on the page and when you print, you can just fit to page
ggexport(report, filename = "Pitcher X Report.pdf", width = 8.5, height = 11)
