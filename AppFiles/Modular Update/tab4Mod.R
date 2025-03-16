
tab4UI <- function(id){
  
  ns <- NS(id)
  
  # Sidebar layout
  page_sidebar(
    
    sidebar = sidebar( #define sidebar
      
      title = "Options",
      
      # Input box for address
      textInput(ns("Address"), "Enter Address:", 
                value = NULL, 
                placeholder = "[# and Street], [City], [State]"), # address format for user
      
      # Output for errors if any
      uiOutput(ns("CoordError")),
      
      # Radius selection in miles
      sliderInput(ns("SelectRad"), "Radius (miles)", 
                  min = 0,
                  max = 100, 
                  value = 50,
                  step = 5),
      
      # search button
      actionButton(ns("SearchButton"), "Search/Refresh"),
      
      # Data info at bottom of sidebar
      div(
        style = "position: absolute; bottom: 10px; left: 10px; right: 10px; font-size: 0.6em; color: grey;",
        HTML("Using 2013-2023 release data, sourced from the <br>Environmental Protection Agency.")
      )
      
    ),
    
    # card for map
    card(
      
      # enable full screen
      full_screen = TRUE,
      
      card_body(
        
        # output leaflet map
        leafletOutput(ns("Map")) %>%
          withSpinner(type = 4, # loading animation
                      size = 3,
                      color = "#000000") %>%
          as_fill_carrier() # fill panel with loader (overflow possible without)
        
      )
      
    )
    
  )
  
  
}

tab4Server <- function(id, Decade_Data, tab3vars) {
  
  moduleServer(
    
    id,
    
    function(input, output, session){
      
      # Find coordinates on search button press
      coords <- reactiveVal({NULL})
        
        # If tab4 search is used, use normal geo method
        observeEvent(input$SearchButton,{

          coords(geo(input$Address, full_results = TRUE))
          
        })

        # If tab3 search is used, use facility info passed in
        observeEvent(tab3vars$add(),{  
          
          # Use tab3 facility info instead
          coords(tibble(lat = tab3vars$add()[1], 
                 long = tab3vars$add()[2],
                 display_name = tab3vars$add()[3]))
          
          })

      
      # Dynamic label/popup generation
      label <- reactiveVal({NULL})
      
      #Use plain address if user enters address
      observeEvent(input$SearchButton,{
          
          # Label = user-entered address
          label(paste0("<b>Address: </b>", input$Address))
        
      })
      
      # Use facility info for label if tab3 search is used  
      observeEvent(tab3vars$add(), {
          
          # Get data for selected facility location
          label_data <- Decade_Data %>%
            filter(`12. LATITUDE` == as.numeric(tab3vars$add()[1]),
                   `13. LONGITUDE` == as.numeric(tab3vars$add()[2]),
                   `23. INDUSTRY SECTOR` == as.character(tab3vars$add()[4])) %>%
            group_by(`4. FACILITY NAME`, `23. INDUSTRY SECTOR`) %>% # narrow to specific location
            summarize(avg_release = round(mean(total_release), 2)) %>% # calculate average location release
            slice(1) # ensure only 1 entry
          
          #Location info
         label(paste0("<b>Facility:</b> ", label_data$`4. FACILITY NAME`, # Display name, industry, and yearly average
                 "<br><b>Industry: </b>", label_data$`23. INDUSTRY SECTOR`,
                 "<br><b>Yearly Average Release: </b>", comma(label_data$avg_release), " lbs"))
          
        })
      
      # Output for potential errors
      output$CoordError <- renderUI({
        
        # Check if address is invalid (no coordinates found)
        if (is.null(coords()$display_name)){
          
          # notify of invalid address
          message <- tags$span(paste("Empty or Invalid Address"),
                               style = "color: red;")
          
          return(message)
          
        # Check if not a US address
        } else if (!grepl("United States", coords()$display_name) & coords()$display_name != "") {
          
          # Notify of error (map will generate anyway)
          message <- tags$span(paste("Not a U.S. Address"),
                               style = "color: red;")
          
        } else {
          
          # no error
          return(NULL)
          
        }
        
      })
      
      # Map output
      output$Map <- renderLeaflet({
        
        # Don't return map if address is invalid
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
          group_by(`4. FACILITY NAME`, `23. INDUSTRY SECTOR`, geometry) %>% # Group by facility, industry, and coords
          summarize(avg_release = round(mean(total_release, na.rm = TRUE), 2))
        
        pal1 <- colorNumeric(c("blue","green","yellow", "orange","red"), # Color scale
                             domain = nearby_facilities$avg_release)
        
        # Leaflet map
        leaflet() %>%
          addTiles() %>%
          # Add marker for the facility of interest
          addAwesomeMarkers(
            data = new_coords,
            icon = awesomeIcons(icon = "star",
                                iconColor = "yellow",
                                markerColor = "black"), # Add a customizable icon for facility
            popup = ~HTML(label()), # dynamic labels/popups
            label = ~HTML(label())
            
          ) %>%
          # Heatmap of nearby facility releases
          addHeatmap(
            data = nearby_facilities,
            intensity = ~avg_release, # Set intensity of heatmap (doesn't function how I hoped)
            minOpacity = 20,
            blur = 35,
          ) %>%
          addLegend(
            position = "bottomleft", # legend position
            values = nearby_facilities$avg_release, # Values
            title = HTML(paste("Average Toxic <br>Release (Pounds)")), # Plot/legend title
            pal = colorNumeric(c("blue","green","yellow", "orange","red"), # Color scale to match heatmap
                               domain = nearby_facilities$avg_release) # Domain of legend
          ) %>%
          addCircleMarkers(data = nearby_facilities, # add circles for nearly facilities
                           radius = 3,
                           color = "black", # circle outline
                           fill = TRUE,
                           fillColor = ~pal1(avg_release), # fill circles by custom palette
                           stroke = TRUE, # Allow outline
                           weight = 1, # Width of stroke
                           fillOpacity = 1, #solid fill
                           opacity = 1, # solid outline
                           popup = ~paste0("<b>Facility:</b> ", `4. FACILITY NAME`, # info for facilities when clicked
                                           "<br><b>Industry:</b> ", `23. INDUSTRY SECTOR`,
                                           "<br><b>Yearly Average Release:</b> ", comma(avg_release), 
                                           " lbs"))
        }
      })
      
      #Observe when address is searched from tab3
      observeEvent(tab3vars$add(), {
        
        req(tab3vars$add()[3]) # require non-null
        
        #Fill address input with facility address
        updateTextInput(session, inputId = "Address", value = as.character(tab3vars$add()[3]))
        
      })
      
    }
    
  )
  
}