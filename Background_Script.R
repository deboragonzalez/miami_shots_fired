# Script for rough draft of Shotspotter shinyapp:


## Importing the initial data:

library(tidyverse)
library(tigris)
library(sf)
library(ggmap)
library(maps)
library(ggthemes)

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

# We can also use the link if we are doing it on the app.

# Now, let's get the shapefiles using tigris.

raw_shapes_305 <- urban_areas(class = "sf")

shapes_305 <- raw_shapes_305 %>% 
              filter(NAME10 == "Miami, FL")
  
# Converting the ShotSpotter data to sf --> in order to plot it on shape file.
  
miami_2 <- st_as_sf(miami, coords = c("Longitude", "Latitude"), 
                  crs = st_crs(shapes_305)) %>% 
            filter(year == 2018)

# Let's map it:

giph <- ggplot(data = shapes_305) + geom_sf(color = "black", fill = "lightgreen") +
  geom_sf(data = miami_2, color = "black", alpha  = 0.4) + 
  theme_bw() + 
  theme(axis.text.x = element_blank()) +
  theme(text = element_text(size = 8)) +
  labs(title = "Shots Spotted in 2018 by Month: {closest_state}") +
  transition_states(month, 20,30, wrap = TRUE)

anim_save("final_plot.gif", animate(giph))
  

# A look at the numbers:

      # # Other attempts at other formats of data tables
      # incidents <- miami %>%
      #   select(year, month) %>%
      #   group_by(year) %>%
      #   summarize(number = n()) %>%
      #   spread(year, number)
      # 
      # monthly_incidents <- miami %>%
      #   select(year, month) %>%
      #   group_by(month) %>%
      #   summarize(number = n()) %>%
      #   spread(month, number)

# Monthly incidents by year

incident_nice <- miami %>% 
  select(year, month) %>% 
  group_by(year, month) %>% 
  summarise(x = n()) %>% 
  spread(year, x, fill = 0)


# Let's graoh it: 

incident_nice %>% 
  ggplot() +
  geom_line(aes(x = month, y = `2012`), color = "red") +
  geom_line(aes(x = month, y = `2013`), color = "blue") +
  geom_line(aes(x = month, y = `2017`), color = "green") +
  geom_line(aes(x = month, y = `2018`), color = "purple") +
  labs(y = "Number of Incidences",
       x = "Month",
       title = "Monthly Shootings",
       subtitle = "Miami Urban Area - breakdown by year",
       caption = "Source: Justice Tech Lab - ShotSpotter Data") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                       labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                "Aug", "Sep", "Oct", "Nov", "Dec"),
                     limits = c(1,12)) +
  annotate(geom = "text", x = 10.5, y = 80, label = "2012") +
  annotate(geom = "text", x = 1, y = 110, label = "2013") +
  annotate(geom = "text", x = 10, y = 175, label = "2017") +
  annotate(geom = "text", x = 4, y = 200, label = "2018") + 
  theme_economist() +
  theme(text = element_text(size = 8))
  
# How we want to title this project
  # 'Shots "Spotted" in Miami  by Year'
  



