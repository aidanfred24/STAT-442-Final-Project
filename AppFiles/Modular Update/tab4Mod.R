
tab4UI <- function(id){
  
  ns <- NS(id)
  
  page_sidebar(
    
    sidebar = sidebar(
      
      title = "Options",
      
      textInput(ns("Address"), "Enter Address:", 
                value = NULL, 
                placeholder = "[# and Street], [City], [State]"),
      
      uiOutput(ns("CoordError")),
      
      sliderInput(ns("SelectRad"), "Radius (miles)", 
                  min = 0,
                  max = 100, 
                  value = 50,
                  step = 5),
      
      actionButton(ns("SearchButton"), "Search/Refresh")
      
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

tab4Server <- function(id, Decade_Data, tab3vars) {
  
  moduleServer(
    
    id,
    
    function(input, output, session){
      
      coords <- eventReactive(input$SearchButton, {
        
        if(input$Address != tab3vars$add()[3]){
          
          geo(input$Address, full_results = TRUE)
          
        } else {
          
          tibble(lat = tab3vars$add()[1], 
                 long = tab3vars$add()[2],
                 display_name = tab3vars$add()[3])
          
        }
        })
      
      label <- eventReactive(input$SearchButton,{
        
        if(input$Address != tab3vars$add()[3]){
          
          paste0("<b>Address: </b>", input$Address)
        
        } else {
          
          label_data <- Decade_Data %>%
            filter(`12. LATITUDE` == as.numeric(tab3vars$add()[1]),
                   `13. LONGITUDE` == as.numeric(tab3vars$add()[2]),
                   `23. INDUSTRY SECTOR` == as.character(tab3vars$add()[4])) %>%
            group_by(`4. FACILITY NAME`, `23. INDUSTRY SECTOR`) %>%
            summarize(avg_release = round(mean(total_release), 2)) %>%
            slice(1)
          
          paste0("<b>Facility:</b> ", label_data$`4. FACILITY NAME`,
                 "<br><b>Industry: </b>", label_data$`23. INDUSTRY SECTOR`,
                 "<br><b>Yearly Average Release: </b>", comma(label_data$avg_release), " lbs")
          
        }
      })
      
        
      output$CoordError <- renderUI({
        
        if (is.na(coords()$long)){
          
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
                                markerColor = "black"), # Add a customizable icon for facility
            popup = ~HTML(label())
            
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
      
      observeEvent(tab3vars$add(), {
        
        req(tab3vars$add()[3])
        
        updateTextInput(session, inputId = "Address", value = as.character(tab3vars$add()[3]))
        
      })
      
    }
    
  )
  
}