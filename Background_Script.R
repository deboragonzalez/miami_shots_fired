# Script for rough draft of Shotspotter shinyapp:


## Importing the initial data:

# choose your toolbox wisely

library(tidyverse)
library(tigris)
library(sf)
library(ggmap)
library(maps)
library(ggthemes)

# Read in the core data from J.Tech and incorporate the implicit parsing that is
# posted in the console. This allows for a clean run in the end and likely less
# margin for error.

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
  
  # Filter here to remove extraneous points in the ocean and elsewhere.
  
  filter(Latitude <= 27.0 & Latitude >= 24.0) %>% 
  filter(Longitude <= -79.0 & Longitude >= -82.0)

# We can also use the link if we are doing it on the app.

# Now, let's get the shapefiles using tigris.
# Step 1: download all of the shapefiles available.

raw_shapes_305 <- urban_areas(class = "sf")

# Step 2: grab only the Miami-specific shapefile information.

shapes_305 <- raw_shapes_305 %>% 
              filter(NAME10 == "Miami, FL")
  
# Converting the ShotSpotter data to sf --> in order to plot it on shape file.
  
miami_2 <- st_as_sf(miami, coords = c("Longitude", "Latitude"), 
                  crs = st_crs(shapes_305)) %>% 
  
            # In our animated portion we only want to see 2018, so we filtered
            # accordingly before we began to work with the data more intensely.
  
            filter(year == 2018)

# Let's map it: this chunk creates the base map (via the shapefile) and adds
# color and theme.

giph <- ggplot(data = shapes_305) + geom_sf(color = "black", fill = "lightgreen") +
  geom_sf(data = miami_2, color = "black", alpha  = 0.4) + 
  theme_bw() + 
  
  # We removed the bottom labels because they were too large and clustered and
  # subtracted from the general asthetic niceness of the presentation. 
  
  theme(axis.text.x = element_blank()) +
  theme(text = element_text(size = 8)) +
  
  # Adding a title with an indicator relevant to the month that is temporally
  # being displayed on the animated plot. Ensure that the loop has enough time
  # to run smoothly and wrap to avoid abrupt breaks.
  
  labs(title = "Shots Spotted in 2018 by Month: {closest_state}") +
  transition_states(month, 20,30, wrap = TRUE)

# Save the above-created gif to be read directly into the shiny app later. This
# saves the user time.

anim_save("final_plot.gif", animate(giph))
  
# In the process of poking around the dataset, we created a handful of useful
# tables if one were to return and take on other projects. We elected to leave
# the relevant code commented out for further use and creation!

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

# Monthly incidents by year This is the table that is used in the 'overview'
# chart. It contains the standard gov1005 workflow:
# select-group-summarize-spread

incident_nice <- miami %>% 
  select(year, month) %>% 
  group_by(year, month) %>% 
  summarise(x = n()) %>% 
  spread(year, x, fill = 0)


# Let's graph it: Plotting 4 lines, one for each year (you may have noticed that
# we excluded 2014 from the dataset, it only had 1 singular plot, so we deemed
# it faulty data). 

incident_nice %>% 
  ggplot() +
  geom_line(aes(x = month, y = `2012`), color = "red") +
  geom_line(aes(x = month, y = `2013`), color = "blue") +
  geom_line(aes(x = month, y = `2017`), color = "green") +
  geom_line(aes(x = month, y = `2018`), color = "purple") +
  
  # Labels and pretty things, added months along the bottom. 
  
  labs(y = "Number of Incidences",
       x = "Month",
       title = "Monthly Shootings",
       subtitle = "Miami Urban Area - breakdown by year",
       caption = "Source: Justice Tech Lab - ShotSpotter Data") +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),
                       labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                "Aug", "Sep", "Oct", "Nov", "Dec"),
                     
                     # There were a few points that appeared to bebefore 1-1 and
                     # after 12-31 of the years in question, for conciseness, we
                     # stuck to these years.
                     
                     limits = c(1,12)) +
  
  # Labeling the lines directly looked nicer than having a legend, as it keeps
  # from horizontally compressing the graph.
  
  annotate(geom = "text", x = 10.5, y = 80, label = "2012") +
  annotate(geom = "text", x = 1, y = 110, label = "2013") +
  annotate(geom = "text", x = 10, y = 175, label = "2017") +
  annotate(geom = "text", x = 4, y = 200, label = "2018") + 
  theme_economist() +
  theme(text = element_text(size = 8))
  



