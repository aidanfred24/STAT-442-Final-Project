
tab4UI <- function(id){
  
  ns <- NS(id)
  
  page_sidebar(
    
    sidebar = sidebar(
      
      title = "Options",
      
      textInput(ns("Address"), "Enter Address:", value = NULL),
      
      actionButton(ns("SearchButton"), "Search")
      
    ),
    
    card(
      
      full_screen = TRUE,
      
      card_body(
        
        leafletOutput(ns("Map"))
        
      )
      
    )
    
  )
  
  
}

tab4Server <- function(id, Decade_Data) {
  
  moduleServer(
    
    id,
    
    function(input, output, session){
      
      coords <- eventReactive(input$SearchButton,{
        
        coordtable <- geo(input$Address)
        
        # Convert coords to a geometry column
        st_as_sf(coordtable, #data
                           coords = c("long", "lat"), # coord cols
                           crs = 4326) # Coordinate system
        
      })
        
        output$Map <- renderLeaflet({
          
          # Convert coords to geometry col
          facilities <- st_as_sf(Decade_Data,
                                 coords = c("13. LONGITUDE", "12. LATITUDE"),
                                 crs = 4326) # coordinate system
          
          # Use sf package to calculate distance from chosen facility
          distances <- st_distance(facilities, coords())
          
          # Set radius to 50km
          radius <- set_units(50, "km")
          
          # Filter to facilities within 50km of chosen facility
          nearby_facilities <- facilities %>%
            mutate(distance = distances) %>% 
            filter(distance <= radius)%>%      
            group_by(`4. FACILITY NAME`, geometry) %>% # Group by facility and coords
            summarize(total_release = sum(total_release))
          
          # Leaflet map
          leaflet() %>%
            addTiles() %>%
            # Add marker for the facility of interest
            addAwesomeMarkers(
              data = coords(),
              icon = awesomeIcons(icon = "star",
                                  iconColor = "yellow",
                                  markerColor = "black") # Add a customizable icon for facility
              
            ) %>%
            # Heatmap of nearby facility releases
            addHeatmap(
              data = nearby_facilities,
              intensity = nearby_facilities$total_release, # Set intensity of heatmap
              minOpacity = 20,
              blur = 35,
            ) %>%
            addLegend(
              position = "bottomleft", # legend position
              values = nearby_facilities$total_release, # Values
              title = HTML(paste("Toxic Release Over <br>10 Years (Pounds)")), # Plot/legend title
              pal = colorNumeric(c("blue","green","yellow", "orange","red"), # Color scale to match heatmap
                                 domain = nearby_facilities$total_release) # Domain of legend
            )
          
        })
    }
    
  )
  
}