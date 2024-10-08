#install packages

#install.packages("Rtools43")
#install.packages("terra")
#install.packages("ncdf4")

#load libraries

library(terra)
library(spData)
library(tidyverse)
library(sf)
library(ncdf4)

#download the data

download.file("https://crudata.uea.ac.uk/cru/data/temperature/absolute.nc","crudata.nc", method = "curl")

#read in the data 

tmean <- rast("crudata.nc")
plot(tmean)

#calculate the maximum temperature for each month

max_temp <- tmean %>%
             max()
plot(max_temp)

#find the max in each country

max_temp_by_country <- terra::extract(x = max_temp, y = world, fun=max, 
                                      na.rm = T, small = T)

#add the temperature data to the world data set

world_clim <- bind_cols(world, max_temp_by_country)

#plot the results of combining the temperature data with the world data

ggplot(world_clim) +
  geom_sf(aes(fill = max)) +
  scale_fill_viridis_c(name="Maximum\nTemperature (C)") +
  theme(legend.position = 'bottom') +
  labs(title = "Maximum Temperature in Each Country",
       caption = "Source: Climate Research Unit, University of East Anglia")

ggsave("Max_Temp_Country.png", width = 12, height = 8)

#make hottest countries table

hottest_countries <- world_clim %>%
                        group_by(continent) %>%
                        top_n(1) %>%
                        select(name_long, continent, max) %>%
                        st_set_geometry(NULL) %>%
                        arrange(desc(max))
  
view(hottest_countries)
