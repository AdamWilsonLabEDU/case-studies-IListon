#install packages if needed
#install.packages("sf")
#install.packages("spData")

#load necessary packages

library(spData)
library(sf)
library(tidyverse)

#load world and U.S. states data
data("world")
data("us_states")

#transform to albers equal area projection 

albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +
        ellps=GRS80 +datum=NAD83 +units=m +no_defs"

world_aea <- st_transform(world, crs = albers)

#filter for canada

canada <- world_aea %>%
            filter(name_long == "Canada")

#buffer canada

canada_buffer <- st_buffer(canada, 10000)

ggplot(canada_buffer)+
  geom_sf()

#filter for new york state

new_york_state <- us_states %>%
                    filter(NAME == "New York")

#transform new york state to albers

nys_albers <- st_transform(new_york_state, crs = albers)

ggplot(nys_albers)+
  geom_sf()

#find overlap between canada buffer and new york state

overlap <- st_intersection(canada_buffer, nys_albers)

#plot the border areas at "risk"

ggplot()+
  geom_sf(data = nys_albers, fill = "gray")+
  geom_sf(data = overlap, fill = "red")+
  labs(title = "Areas at Risk in New York",
       subtitle = "Less than 10km away from the Canadian Border")

ggsave("NYS_Areas_at_Risk.PNG")

#calculate the area of the buffer

area_of_overlap <- st_area(overlap)

area_of_overlap_sqkm <- area_of_overlap %>% 
                          set_units(km^2)
area_of_overlap_sqkm

