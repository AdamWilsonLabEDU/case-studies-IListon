# load packages
library(tidyverse)
library(nycflights13)

#examine tables
view(flights)
view(airports)

#arrange by flight distance
flights_by_distance <- flights %>%
                          arrange(desc(distance))

view(flights_by_distance)            

#slice table to isolate farthest airport
flights_to_farthest_airport <- flights_by_distance %>%
                                  slice_max(distance)

view(flights_to_farthest_airport)

#join table with full airport names
flights_and_airports <- flights_to_farthest_airport %>%
                          left_join(airports, join_by(dest == faa))

view(flights_and_airports)

#select the name of the farthest airport 
name_col_of_farthest_airport <- flights_and_airports %>%
                                  select(name)

#convert name to a single character
farthest_airport <- name_col_of_farthest_airport %>%
                      slice_head()

farthest_airport

