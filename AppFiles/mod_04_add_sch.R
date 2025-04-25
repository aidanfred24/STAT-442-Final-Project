mod_04_add_sch_ui <- function(id) {
  
  ns <- NS(id)
  
  # Sidebar layout
  page_sidebar(
    
    sidebar = sidebar(  # define sidebar
      
      title = "Enter Address:",
      bg = "#EDEDED",
      
      # Address input
      textInput(
        ns("Address"),
        label = NULL,
        value = "Washington, DC",
        placeholder = "[# and Street], [City], [State]"
      ),
      
      # Error output
      uiOutput(ns("CoordError")),
      
      # Search button
      actionButton(
        ns("SearchButton"), 
        "Search",
        icon = icon("magnifying-glass")
      ),
      
      div(style = "margin-top: 50px;",
      # Radius selection
      sliderInput(
        ns("SelectRad"), 
        "Radius (miles)", 
        min = 0,
        max = 100, 
        value = 50,
        step = 5
      )
      ),
      
      # Data source info
      div(
        style = paste(
          "position: absolute; bottom: 10px; left: 10px;",
          "right: 10px; font-size: 0.6em; color: grey;"
        ),
        HTML(paste(
          "Using 2013-2023 release data, sourced from the <br>",
          "Environmental Protection Agency."
        ))
      )
    ),
    
    # Map card
    card(
      full_screen = TRUE,
      card_body(
        uiOutput(
          ns("PlaceHolder"),
          fill = TRUE
        )
      )
    )
  )
}

mod_04_add_sch_server <- function(id, Decade_Data, tab3vars) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # Coordinates reactive value
      coords <- reactiveVal({
        tidygeocoder::geo(
          "Washington, DC",
          full_results = TRUE
          )
      })
      
      # Handle direct address search
      observeEvent(input$SearchButton, {
        coords(
          tidygeocoder::geo(
            input$Address, 
            full_results = TRUE
          )
        )
      })
      
      # Handle search from tab3
      observeEvent(tab3vars$add(), {
        coords(tibble(
          lat = tab3vars$add()[1], 
          long = tab3vars$add()[2],
          display_name = tab3vars$add()[3]
        ))
      })
      
      # Dynamic label/popup content
      label <- reactiveVal({"Address: Washington, DC"})
      
      # Set label for user-entered address
      observeEvent(input$SearchButton, {
        label(paste0("<b>Address: </b>", input$Address))
      })
      
      # Set label for facility from tab3
      observeEvent(tab3vars$add(), {
        # Get facility data
        label_data <- Decade_Data %>%
          filter(
            `12. LATITUDE` == as.numeric(tab3vars$add()[1]),
            `13. LONGITUDE` == as.numeric(tab3vars$add()[2]),
            `23. INDUSTRY SECTOR` == as.character(tab3vars$add()[4])
          ) %>%
          group_by(`4. FACILITY NAME`, `23. INDUSTRY SECTOR`) %>%
          summarize(avg_release = round(mean(total_release), 2),
                    avg_recycprop = round(mean(recyc_prop, na.rm = TRUE), 2))
        
        # Create HTML label
        label(
          paste0(
          "<b>Facility:</b> ", label_data$`4. FACILITY NAME`,
          "<br><b>Industry: </b>", label_data$`23. INDUSTRY SECTOR`,
          "<br><b>Yearly Average Release: </b>", 
          comma(label_data$avg_release), " lbs",
          "<br><b>Average Percent Recycled: </b>",
          label_data$avg_recycprop, " %"
          )
        )
      })
      
      # Show coordinate errors
      output$CoordError <- renderUI({
        if (is.null(coords())) {
          return(NULL)
          # Check for invalid address
        } else if (!is.null(coords()) && 
                   is.null(coords()$display_name)) {
          return(tags$span(
            paste("Empty or Invalid Address"),
            style = "color: red;"
          ))
          # Check for non-US address
        } else if (
          !grepl(
            "United States", 
            coords()$display_name) && coords()$display_name != "") {
          return(tags$span(
            paste("Not a U.S. Address"),
            style = "color: red;"
          ))
        } else {
          return(NULL)
        }
      })
      
      output$PlaceHolder <- renderUI({
        
        ns <- session$ns
          
        if (!is.null(coords()) && 
            !is.null(coords()$display_name) && 
            !is.null(coords()$lat) && 
            !is.na(coords()$lat) &&
            grepl(
              "United States", 
              coords()$display_name)
          ) {

          leaflet::leafletOutput(
            ns("Map")
          )%>% 
            shinycssloaders::withSpinner(
              type = 4,
              size = 3,
              color = "#000000"
            ) %>%
            as_fill_carrier()
  
        } else {
          
          uiOutput(
            ns("popup")
          )%>% 
            shinycssloaders::withSpinner(
              type = 4,
              size = 3,
              color = "#000000"
            ) %>%
            as_fill_carrier()
          
        }

      })
      
      output$popup <- renderUI({
        div(
          tags$img(
            src = "Logo2.png",
            width = "70%",
            style = "display: block; margin: 0 auto; padding: 0px;"
          ),
          h1(
            "Are There Chemical Releases Near You?",
            style = "text-align: center;"
          ),
          br(),
          h5(
            "Enter an address and search to find out...",
            style = "text-align: center; color: grey;"
          )
        )
      })
      
      # Generate map
      output$Map <- leaflet::renderLeaflet({
        
        req(!is.na(coords()$lat))
          # Convert search coordinates to spatial object
          new_coords <- sf::st_as_sf(
            coords(),
            coords = c("long", "lat"),
            crs = 4326
          )
          
          # Convert facility data to spatial object
          facilities <- sf::st_as_sf(
            Decade_Data,
            coords = c("13. LONGITUDE", "12. LATITUDE"),
            crs = 4326
          )
          
          # Calculate distances
          distances <- sf::st_distance(facilities, new_coords)
          units(distances) <- units::as_units("US_survey_mile")
          
          # Set search radius
          radius <- units::set_units(input$SelectRad, "US_survey_mile")
          
          # Find nearby facilities
          nearby_facilities <- facilities %>%
            mutate(distance = distances) %>% 
            filter(distance <= radius) %>%      
            group_by(`4. FACILITY NAME`, `23. INDUSTRY SECTOR`, geometry) %>%
            summarize(avg_release = round(mean(total_release, na.rm = TRUE), 2),
                      avg_recycprop = round(mean(recyc_prop, na.rm = TRUE), 2))
          
          # Color palette for release values
          pal1 <- leaflet::colorNumeric(
            c("blue", "green", "yellow", "orange", "red"),
            domain = nearby_facilities$avg_release
          )
          
          # Color palette for release values
          pal2 <- leaflet::colorNumeric(
            c("red", "orange", "yellow", "green"),
            domain = nearby_facilities$avg_recycprop
          )
          
          # Create map
          leaflet::leaflet() %>%
            leaflet::addTiles() %>%
            # Add marker for search location
            leaflet::addAwesomeMarkers(
              data = new_coords,
              icon = leaflet::awesomeIcons(
                icon = "star",
                iconColor = "yellow",
                markerColor = "black"
              ),
              popup = ~HTML(label()),
              label = ~HTML(label())
            ) %>%
            # Add heatmap layer
            leaflet.extras::addHeatmap(
              data = nearby_facilities,
              intensity = ~avg_release,
              minOpacity = 20,
              blur = 35,
              group = "Heatmap"
            ) %>%
            # Add legend
            leaflet::addLegend(
              value = nearby_facilities$avg_release,
              group = "Release",
              position = "bottomleft",
              title = HTML(paste("Average Toxic <br>Release (Pounds)")),
              pal = pal1
            ) %>%
            leaflet::addLegend(
              value = nearby_facilities$avg_recycprop,
              group = "Recycling",
              position = "bottomleft",
              title = HTML(paste("Average Percent <br>Recycled")),
              pal = pal2,
              labFormat = leaflet::labelFormat(suffix = "%")
            ) %>%
            # Add markers for nearby facilities
            leaflet::addCircleMarkers(
              data = nearby_facilities,
              radius = 3.5,
              color = "black",
              fill = TRUE,
              fillColor = ~pal1(avg_release),
              stroke = TRUE,
              weight = 1,
              fillOpacity = 1,
              opacity = 1,
              popup = ~paste0(
                "<b>Facility:</b> ", `4. FACILITY NAME`,
                "<br><b>Industry:</b> ", `23. INDUSTRY SECTOR`,
                "<br><b>Yearly Average Release:</b> ", 
                comma(avg_release), " lbs"
              ),
              group = "Release" 
            ) %>%
            # Add markers for nearby facilities
            leaflet::addCircleMarkers(
              data = nearby_facilities,
              radius = 3.5,
              color = "black",
              fill = TRUE,
              fillColor = ~pal2(avg_recycprop),
              stroke = TRUE,
              weight = 1,
              fillOpacity = 1,
              opacity = 1,
              popup = ~paste0(
                "<b>Facility:</b> ", `4. FACILITY NAME`,
                "<br><b>Industry:</b> ", `23. INDUSTRY SECTOR`,
                "<br><b>Yearly Average Recycling:</b> ", 
                comma(avg_recycprop), " %"
              ),
              group = "Recycling" 
            ) %>%
            leaflet::addLayersControl(
              overlayGroups = c("Release", "Recycling", "Heatmap")
              ) %>%
            leaflet::hideGroup(group = "Recycling")
      })
      
      # Update address input when coming from tab3
      observeEvent(tab3vars$add(), {
        req(tab3vars$add()[3])
        updateTextInput(
          session, 
          inputId = "Address", 
          value = as.character(tab3vars$add()[3])
        )
      })
    }
  )
}