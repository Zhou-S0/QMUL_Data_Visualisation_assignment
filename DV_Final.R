# Sammi Zhou's Code, Student ID 210928411

# Installing all the required packages needed 
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("RColorBrewer")
install.packages("patchwork")

# Loading all the packages needed
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(patchwork)
# tidyverse and ggplot2 for data manipulation and data visualization
# patchwork to arrange the layout of the graph

# Reading the dataset into a dataframe called 'spotify_songs'
spotify_songs <- tibble(readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv'))

# Filtering spotify_songs data for all the songs that are sung by Beyonce and Jay-Z and storing it in a variable callled 'jay_z_beyonce_songs'
# Selecting specific metrics that are needed to compare Beyonce and Jay-Z's overall artist profiles
jay_z_beyonce_songs <- spotify_songs %>%
  filter(track_artist %in% c("JAY-Z", "Beyoncé")) %>%
  select(track_name, track_artist, danceability, energy, speechiness, valence, tempo)

# Creating the artist profiles with density plots for the different metrics selected
# x axis is showing the value of the metrics, y axis is showing the density
artist_profiles_plot <- jay_z_beyonce_songs %>%
  gather(metric, value, -track_name, -track_artist) %>% # Reshaing data from wide to long format
  ggplot(aes(x = value, fill = track_artist, color = track_artist)) + # Mapping the data to aesthetics
  geom_density(alpha = 0.5) + # Adding density plots with transparency
  facet_wrap(~ metric, scales = "free") +
  labs(title = "Artist Profiles: Jay-Z vs Beyoncé", # Adding the title, substitle and the names for the legend, x, and y axis
       subtitle = "Comparing music metrics between Beyoncé and J-Zay", 
       x = "Value",
       y = "Density",
       fill = "Artist",
       color = "Artist") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") + # Setting fill colours using RColorBrewer packages
  scale_color_brewer(palette = "Set1") + # Setting line colours using RColorBrewer packages
  theme(axis.title.x = element_text(colour = "black", size = 10, face = 'bold', family = 'Arial'), # Using patchwork to set the font, size, and colour of the titles
        axis.title.y = element_text(colour = "black", size = 10, face = 'bold', family = 'Arial'),
        plot.title = element_text(hjust = 0.5, colour = "black", size = 15, face = 'bold', family = 'Arial'),
        plot.subtitle = element_text(hjust = 0.5, colour = "black", size = 12, face = 'bold', family = 'Arial'),
        legend.position = "top") # Positioning the legends to be at the top

# Printing the plot
print(artist_profiles_plot)
