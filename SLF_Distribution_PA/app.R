# Load required libraries
library(shiny)
library(shinythemes)
library(readxl)
library(tigris)
library(leaflet)
library(dplyr)

# GBIF SLF data
occurrences <- read_excel("SLF_occurrences.xlsx")

# Pennsylvania geojson
pa_counties <- tigris::counties("PA", cb = TRUE)

# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Title & blurb
  titlePanel("Spotted Lanternfly Spread in Pennsylvania (2014-2023)."),
  HTML("<p>The Spotted Lanternfly (SLF) is an invasive insect that was first identified in the U.S. in 2014, 
specifically in Berks County, Pennsylvania. It is native to China, India, and Vietnam. The SLF has the 
potential to cause significant economic damage, with an estimated cost of $324 million to the state of 
Pennsylvania each year if not contained. Currently, the SLF has spread to 51 counties across Pennsylvania, 
New Jersey, New York, Ohio, Connecticut, Maryland, Delaware, Virginia, and West Virginia. This insect feeds 
on the sap of plants and excretes honeydew, which promotes the growth of fungus on its plant host, negatively 
affecting the plant's health and potentially leading to its demise. The SLF is known to feed on over 70 species 
of plants, including important crops such as grapevines, hops, and apple trees. It is advised by Penn State and 
the Pennsylvanian government that if the SLF is spotted in any phase of its life cycle, it should be eliminated 
to help slow its spread and minimize the damage it causes. For more information visit: https://extension.psu.edu/spotted-lanternfly-what-to-look-for</p>"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      
      # Dropdown menu for years
      selectInput("yearInput", "Select Year:",
                  choices = unique(arrange(occurrences, year)$year),
                  selected = min(occurrences$year)
      )
    ),
    
    
    mainPanel(
      leafletOutput("map")
    )
  )
)

# Server info
server <- function(input, output, session) {
  
  # Filter for selected years
  filtered_data <- reactive({
    filter(occurrences, year == input$yearInput)
  })
  
  zoom_level <- reactive({
    occurrences_count <- nrow(filtered_data())
    if (occurrences_count < 10) {
      return(7)
    } else if (occurrences_count < 50) {
      return(6)
    } else {
      return(5)
    }
  })
  
  # Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -77.8, lat = 40.9, zoom = zoom_level()) %>%
      addPolygons(data = pa_counties, fillColor = "#b5a134", color = "black", fillOpacity = 0.5) %>%
      addCircles(data = filtered_data(), 
                 lng = ~Longitude, lat = ~Latitude,
                 radius = 500, color = "#a31714", fillOpacity = 0.8,
                 popup = paste("Scientific Name: ", filtered_data()$ScientificName,
                               "<br>State: ", filtered_data()$State,
                               "<br>Date: ", paste(filtered_data()$month, filtered_data()$day, filtered_data()$year, sep = "/")))
  })
}




# Run the Shiny app
shinyApp(ui, server)