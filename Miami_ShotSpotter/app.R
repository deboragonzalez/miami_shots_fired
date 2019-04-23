# Libraries
# Choose your tools wisely:

library(tidyverse)
library(tigris)
library(sf)
library(ggmap)
library(maps)
library(ggthemes)
library(shiny)
library(gganimate)
library(plotly)

# Data setup
# Much of the data importing and parsing code comes from the "Background Script"

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
                                      

# Data manipulation for the 2nd plot
# Monthly incidents by year This is the table that is used in the 'overview'
# chart. It contains the standard gov1005 workflow:
# select-group-summarize-spread

incident_nice <- miami %>% 
  select(year, month) %>% 
  group_by(year, month) %>% 
  summarise(x = n()) %>% 
  spread(year, x, fill = 0)

# Define UI for application that draws the plots.
# No "data manipulation" for the
# animation because the 'gif' was created and saved in the background script.
# See "Background_Script.R" for the details of this.

ui <- fluidPage(
   
   # Application title
  
   titlePanel('Shots "Spotted" in the Miami Urban Area'),
   
   #  We originally had a reactive panel with options to show the geography of
   #  gunshots my selecting the year. This tab was replaced my the animation,
   #  but we are leaving the code here so that we may come back and add this
   #  feature back to the app.
   
             # Select years to show: 
             #  sidebarLayout(
                # sidebarPanel(
                #    selectInput(inputId = "year",
                #                label = "Select Year:",
                #                choices = c("2012", "2013", "2017", "2018"),
                #                selected = NULL)),
          
      # Set up tabs tabsetPanel allows for the creation of separate panels to be
      # accessed by tab clicking in the app. Cool!
   
      mainPanel(
        tabsetPanel(
          
          #Add a title to each ___Output
          
          tabPanel("Where do the Shootings Occur?",
                   imageOutput("final_animation")),
          tabPanel("A Closer Look at the Numbers",
                   plotOutput("graph_lines")),
          tabPanel("Source and Background",
                   htmlOutput("text"))
                   )
                )
            )


# Define server logic for the desired output types

server <- function(input, output) {
   
  # This is where we read in our previously-constructed gif. Specify that this
  # process should NOT delete your file after running... Why this is necessary I
  # do not know, but this function prefers 'single-use' gifs I suppose.
  
   output$final_animation <- renderImage({
     
     list(src = "final_plot.gif",
          contentType = 'image/gif')},
     deleteFile = FALSE)
  
   # This is the same code as was constructed in the script, just contained in
   # the appropriate braces, parentheses etc.
   
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
   
   # Here is my rusty attempt at HTML. Key idea here was to have a link to the
   # data provider and the GitHub for this project. This was / is accomplished.
   # Also included: gratitude and colleagues. Pro Tip: make normal HTML code and
   # then wrap the whole thing in ""s.
   
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

# Don't forget to actually make the app run, the world must see this
# masterpiece!
# Names for the components are quite unoriginal...

shinyApp(ui = ui, server = server)

