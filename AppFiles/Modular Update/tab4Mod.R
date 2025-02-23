
tab4UI <- function(id){
  
  ns <- NS(id)
  
  page_sidebar(
    
    sidebar = sidebar(
      
      title = "Options",
      
      textInput(ns("Address"), "Enter Address:", value = NULL),
      
      uiOutput(ns("CoordError")),
      
      sliderInput(ns("SelectRad"), "Radius (miles)", 
                  min = 0,
                  max = 100, 
                  value = 50,
                  step = 5),
      
      actionButton(ns("SearchButton"), "Search")
      
    ),
    
    card(
      
      full_screen = TRUE,
      
      card_body(
        
        leafletOutput(ns("Map")) %>%
          withSpinner(type = 4,
                      size = 3,
                      color = "#000000") %>%
          as_fill_carrier()
        
      )
      
    )
    
  )
  
  
}

tab4Server <- function(id, Decade_Data) {
  
  moduleServer(
    
    id,
    
    function(input, output, session){
      
      coords <- eventReactive(input$SearchButton,{
        
        coordtable <- geo(input$Address, full_results = TRUE)
        
      })
        
      output$CoordError <- renderUI({
        
        if (is.na(coords()$lat)){
          
          message <- tags$span(paste("Empty or Invalid Address"),
                               style = "color: red;")
          
          return(message)
          
        } else if (!grepl("United States", coords()$display_name)) {
          
          message <- tags$span(paste("Not a U.S. Address"),
                               style = "color: red;")
          
        } else {
          
          return(NULL)
          
        }
        
      })
      
      
      output$Map <- renderLeaflet({
        
        if (is.na(coords()$lat)){
          
          return(NULL)
          
        } else {
          
        # Convert coords to a geometry column
        new_coords <- st_as_sf(coords(), #data
                               coords = c("long", "lat"), # coord cols
                               crs = 4326) # Coordinate system
        
        # Convert coords to geometry col
        facilities <- st_as_sf(Decade_Data,
                               coords = c("13. LONGITUDE", "12. LATITUDE"),
                               crs = 4326) # coordinate system
        
        # Use sf package to calculate distance from chosen facility
        distances <- st_distance(facilities, new_coords)
        
        units(distances) <- as_units("US_survey_mile")
        
        # Set radius to 50km
        radius <- set_units(input$SelectRad, "US_survey_mile")
        
        # Filter to facilities within 50km of chosen facility
        nearby_facilities <- facilities %>%
          mutate(distance = distances) %>% 
          filter(distance <= radius)%>%      
          group_by(`4. FACILITY NAME`, `23. INDUSTRY SECTOR`, geometry) %>% # Group by facility and coords
          summarize(total_release = sum(total_release))
        
        # Leaflet map
        leaflet() %>%
          addTiles() %>%
          # Add marker for the facility of interest
          addAwesomeMarkers(
            data = new_coords,
            icon = awesomeIcons(icon = "star",
                                iconColor = "yellow",
                                markerColor = "black") # Add a customizable icon for facility
            
          ) %>%
          # Heatmap of nearby facility releases
          addHeatmap(
            data = nearby_facilities,
            intensity = ~total_release, # Set intensity of heatmap
            minOpacity = 20,
            blur = 35,
          ) %>%
          addLegend(
            position = "bottomleft", # legend position
            values = nearby_facilities$total_release, # Values
            title = HTML(paste("Toxic Release Over <br>10 Years (Pounds)")), # Plot/legend title
            pal = colorNumeric(c("blue","green","yellow", "orange","red"), # Color scale to match heatmap
                               domain = nearby_facilities$total_release) # Domain of legend
          ) %>%
          addCircleMarkers(data = nearby_facilities,
                           radius = 0.5,
                           popup = ~paste0("<b>Facility:</b> ", `4. FACILITY NAME`,
                                           "<br><b>Industry:</b> ", `23. INDUSTRY SECTOR`,
                                           "<br><b>Chemical Release:</b> ", comma(round(total_release, 2)), 
                                           " lbs"))
        }
      })
    }
    
  )
  
}