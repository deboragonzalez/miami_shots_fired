
## Importing the initial data:

library(tidyverse)
library(tigris)
library(sf)
library(ggmap)
library(maps)

miami <- read_csv(file = "miamidadecounty_fl.csv",
                  col_types = cols(
                    `Complaint Datetime` = col_character(),
                    `Case Number` = col_character(),
                    `Signal Suffix` = col_character(),
                    `CAD Signal Code Description` = col_character(),
                    `Grid Code` = col_double(),
                    `Address String` = col_character(),
                    Latitude = col_double(),
                    Longitude = col_double(),
                    City = col_character(),
                    State = col_character(),
                    year = col_double(),
                    month = col_double(),
                    yearmonth = col_double())) %>% 
  filter(Latitude <= 27.0 & Latitude >= 24.0) %>% 
  filter(Longitude <= -79.0 & Longitude >= -82.0)

#GPS stuff
#NORTH: 27
#SOUTH: 24
#WEST: 



# We can also use the link if we are doing it on the app. 

raw_shapes_305 <- urban_areas(class = "sf")

shapes_305 <- raw_shapes_305 %>% 
              filter(NAME10 == "Miami, FL")
  
  
map_2 <- st_as_sf(miami, coords = c("Longitude", "Latitude"), 
                  crs = st_crs(shapes_305))


ggplot(data = shapes_305) + geom_sf(color = "black", fill = "lightgreen") +
  geom_sf(data = map_2, color = "black", alpha  = 0.4) + 
  theme_bw() + 
  theme(axis.text.x = element_blank())
  

# A look at the numbers:

incidents <- miami %>% 
  select(year, month) %>% 
  group_by(year) %>% 
  summarize(number = n()) %>% 
  spread(year, number)
  
  



