
#load libraries

library(tidyverse)
library(spData)
library(sf)

## New Packages
#install.packages("mapview")
library(mapview) 
#install.packages("foreach")
library(foreach)
#install.packages("doParallel")
library(doParallel)
#install.packages("tidycensus")
library(tidycensus)
library(usethis)

#connect to census API
#usethis::edit_r_environ()
census_api_key(Sys.getenv("CENSUS_API_KEY"))

#get required census data
race_vars <- c("Total Population" = "P1_001N",
                "White alone" = "P1_003N",
                "Black or African American alone" = "P1_004N",
                "American Indian and Alaska Native alone" = "P1_005N",
                "Asian alone" = "P1_006N",
                "Native Hawaiian and Other Pacific Islander alone" = "P1_007N",
                "Some Other Race alone" = "P1_008N",
                "Two or More Races" = "P1_009N")

options(tigris_use_cache = TRUE)
erie <- get_decennial(geography = "block", variables = race_vars, year=2020,
                      state = "NY", county = "Erie County", geometry = TRUE,
                      sumfile = "pl", cache_table=T) 

#limit data to decrease computation time
erie_cropped <- erie %>%
                  st_crop(c(xmin=-78.9,xmax=-78.85,ymin=42.888,ymax=42.92))

variables <- factor(erie_cropped$variable)
view(variables)

#foreach loop

cores <- detectCores()
cl <- makeCluster(cores-4)
registerDoParallel(cl)

results <- foreach(race = levels(variables), 
                   .packages = c("dplyr", "sf"),
                   .combine = "rbind") %dopar% {
                  erie_cropped %>% 
                  filter(variable == race) %>%
                  st_sample(size = .$value, type = "random")%>%
                  st_as_sf()%>%
                  mutate(., variable = race)}

view(results)

stopCluster(cl)

#make dot map

mapview(results,
        cex = 1,
        stroke = FALSE,
        alpha = 0.75)
  



