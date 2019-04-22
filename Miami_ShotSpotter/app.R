# Libraries

library(tidyverse)
library(tigris)
library(sf)
library(ggmap)
library(maps)
library(ggthemes)
library(shiny)
library(gganimate)

# Data setup

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

# Shape files

raw_shapes_305 <- urban_areas(class = "sf")

shapes_305 <- raw_shapes_305 %>% 
  filter(NAME10 == "Miami, FL")


miami_2 <- st_as_sf(miami, coords = c("Longitude", "Latitude"), 
                  crs = st_crs(shapes_305))

# Data manipulation for the 2nd plot

incident_nice <- miami %>% 
  select(year, month) %>% 
  group_by(year, month) %>% 
  summarise(x = n()) %>% 
  spread(year, x, fill = 0)

# Define UI for application that draws the plots

ui <- fluidPage(
   
   # Application title
   titlePanel('Shots "Spotted" in the Miami Urban Area'),
   
   # Select years to show: 
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "year",
                     label = "Select Year:",
                     choices = c("2012", "2013", "2017", "2018"),
                     selected = NULL)),
      
      # Set up tabs
      mainPanel(
        tabsetPanel(
          tabPanel("Where do the Shootings Occur?",
                   plotOutput("MAP")),
          tabPanel("A Closer Look at the Numbers",
                   plotOutput("graph_lines")),
          tabPanel("Source and Background",
                   htmlOutput("text"))
                   )
                )
            ))


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$MAP <- renderPlot({
     miami_subset <- miami_2 %>% filter(year == input$year) %>% 
       sample_n(50)
     
     # Draw map
     ggplot(data = shapes_305) + geom_sf(color = "black", fill = "lightgreen") +
       geom_sf(data = miami_subset, color = "black", alpha  = 0.4) + 
       theme_bw() + 
       theme(axis.text.x = element_blank()) +
       labs(title = "Shots Spotted by Month: {closest_state}",
            subtitle = "per year selected") +
       transition_states(month, 30, 20, wrap = TRUE)
   })
   
   output$graph_lines <- renderPlot({
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
       annotate(geom = "text", x = 11, y = 80, label = "2012") +
       annotate(geom = "text", x = 1, y = 110, label = "2013") +
       annotate(geom = "text", x = 10, y = 175, label = "2017") +
       annotate(geom = "text", x = 4, y = 200, label = "2018") + 
       theme_economist()
                                      })
   
  output$text <- renderText({
'<h3><b>About this Project</b></h3>
<br/>
The data for this project comes from the ShotSpotter project at the Justice Tech Lab. Special thanks to Dr. Doleac and Justice Tech for allowing public access to the data.
<br/><br/>
<a href="http://justicetechlab.org">Learn more about the Justice Tech Lab</a>
<br/><br/>
<a href="https://github.com/deboragonzalez/miami_shots_fired">See the Github repository for this project</a>

<br/> <br/>Application Authors: D. Gonzalez & B. Meche'
                            })
}

# Run the application 
shinyApp(ui = ui, server = server)

