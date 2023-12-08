# Load required libraries
library(shiny)
library(shinythemes)
library(readxl)
library(tigris)
library(leaflet)
library(dplyr)

# GBIF SLF data
occurrences <- read_excel("SLF_occurrences.xlsx")

# Pennsylvania geojson (confirmed in metadata file)
pa_counties <- tigris::counties("PA", cb = TRUE)

# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # Title & blurb
  titlePanel(HTML("<em>Spotted Lanternfly Spread in Pennsylvania (2014-2023)</em>")),
  HTML("<p>The Spotted Lanternfly (SLF), (<em>Lycorma delicatula</em>), is an invasive insect that was first identified in the U.S. in 2014, 
       specifically in Berks County, Pennsylvania. It is native to China, India, and Vietnam. The SLF has the 
       potential to cause significant economic damage, with an estimated cost of $324 million to the state of 
       Pennsylvania each year if it is not contained, according to Penn State. Currently, the SLF has spread to 51 counties across Pennsylvania, 
       New Jersey, New York, Ohio, Connecticut, Maryland, Delaware, Rhode Island, Michigan, North Carolina, Virginia, and West Virginia. 
       This insect feeds 
       on the sap of plants and excretes honeydew, which promotes growth of fungus on the plant host -- negatively 
       affecting the plant's health and potentially leading it to its demise. The SLF is known to feed on over 70 species 
       of plants, including important crops such as grapevines, hops, and apple trees. It is advised by Penn State and 
       the Pennsylvanian government that if the SLF is spotted in any phase of its life cycle, it should be eliminated 
       to help slow its spread and minimize the damage it causes. 
       For more information visit: https://extension.psu.edu/spotted-lanternfly-what-to-look-for</p>
      <p><b>Use the dropdown menu below to view the spread of Spotted Lanternfly occurrences in Pennsylvania from 2014-2023.</b></p>
       <p>GBIF.org (30 November 2023) GBIF Occurrence Download https://doi.org/10.15468/dl.nk6afv</p>"),
  
  # Layout
  fluidRow(
    column(6, 
           leafletOutput("map")
    ),
    
    column(6,
           # Dropdown menu for years
           selectInput("yearInput", "Select a year to view occurrences on the map:",
                       choices = unique(arrange(occurrences, year)$year),
                       selected = min(occurrences$year)
           ),
           # Image in the main panel
           imageOutput("SLF_image"),
           # Description under the image
           textOutput("image_description")
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
  
  # Leaflet map -- couldn't get a tmap code to work (confirmed lat long with "Metadata Confirmation" file)
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -80.51939, lat = 39.71986, zoom = zoom_level()) %>%
      addPolygons(data = pa_counties, fillColor = "#b5a134", color = "black", fillOpacity = 0.5) %>%
      addCircles(data = filtered_data(), 
                 lng = ~Longitude, lat = ~Latitude,
                 radius = 500, color = "#a31714", fillOpacity = 0.8,
                 popup = paste("Scientific Name: ", filtered_data()$ScientificName,
                               "<br>State: ", filtered_data()$State,
                               "<br>Date: ", paste(filtered_data()$month, filtered_data()$day, filtered_data()$year, sep = "/")))
  })
  
  
  # SLF image & decription -- couldn't get imageOutput code to work
  output$SLF_image <- renderImage({
    list(src = "www/SLF_image.jpeg",
         width = "90%",
         height = 375)
  }, deleteFile = FALSE)
  
  # image description (not working, overlays on the image for some reason)
  output$image_description <- renderText({
    " "
  })
  
}

# Run the Shiny app
shinyApp(ui, server)