#' ---
#' title: "Pollution Map"
#' author: "Darren Shoemaker"
#' ---

# Libraries ----

library(ggplot2)
library(maps)
library(sf)
library(mapproj)
library(ggspatial)
library(tidyverse)
library(here)

# Read data ----

dat <- read.csv(here('data/raw', 'res_pollution_v4.csv')) %>% 
  mutate(Poll_clus = factor(Poll_clus))
state <- map_data('state')
sites <- st_as_sf(dat, coords = c('Latitude', 'Longitude'),
                  crs = 3174, agr = 'constant')

# Make Plot ----

ggplot() +
  geom_polygon(
    data = state,
    aes(x = long, y = lat, group = group),
    fill = 'white',
    color = 'black'
  ) +
  geom_point(dat = dat, aes(x = Longitude, y = Latitude, color = Poll_clus), shape = 19, size = 1) +
  coord_map('albers', lat0 = 39, lat1 = 45) +
  labs(x = 'Longtude', y = 'Latitude') +
  theme(legend.title = element_blank(),
        text = element_text(family = 'Times New Roman'))