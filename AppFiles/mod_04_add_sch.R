mod_04_add_sch_ui <- function(id) {
  
  ns <- NS(id)
  
  # Sidebar layout
  page_sidebar(
    
    sidebar = sidebar(  # define sidebar
      
      title = "Search by Address:",
      bg = "#EDEDED",
      
      # Address input
      textInput(
        ns("Address"),
        label = NULL,
        value = "1600 Pennsylvania Avenue NW, Washington, D.C. 20500",
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
        leaflet::leafletOutput(
          ns("Map")
        ) %>% 
          shinycssloaders::withSpinner(
            size = 3,
            color = "#000000",
            type = 4
          ) %>% 
          as_fill_carrier()
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
          "1600 Pennsylvania Avenue NW, Washington, D.C. 20500",
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
      label <- reactiveVal({
        paste0("<table style='width:100%; 
                              border-collapse: collapse;'><tr>
                  <td style='padding:4px;'><b>Address: </b></td>
                  <td style='padding:4px; text-align: center;'>
               1600 Pennsylvania Avenue NW, Washington, D.C.
               </td></tr></table>"
        )
      })
      
      # Set label for user-entered address
      observeEvent(input$SearchButton, {
        label(
          paste0("<table style='width:100%; 
                  border-collapse: collapse;'><tr>
                  <td style='padding:4px;'><b>Address: </b></td>
                  <td style='padding:4px; text-align: center;'>", 
                 input$Address, "</td></tr></table>")
        )
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
          group_by(`4. FACILITY NAME`,
                   `23. INDUSTRY SECTOR`,
                   `12. LATITUDE`,
                   `13. LONGITUDE`) %>%
          summarize(avg_release = round(mean(total_release), 2),
                    avg_recycprop = round(mean(recyc_prop, na.rm = TRUE), 2),
                    carcin_sum = sum(carcin_count, na.rm = TRUE),
                    CAAC_sum = sum(CAAC_count, na.rm = TRUE),
                    pbt_sum = sum(pbt_count, na.rm = TRUE),
                    pfas_sum = sum(pfas_count, na.rm = TRUE),
                    min_year = median(min_year),
                    max_year = median(max_year))

        # Create HTML label
        label(
          paste0(
            "<h6 style='text-align:center; font-weight:600; color:#333; 
                            margin:5px 0; padding:5px; 
                            font-family:Helvetica;'>",
            label_data$`4. FACILITY NAME`, "</h6>",
            "<table style='width:100%; border-collapse: collapse;'>",
            "<tr style='background-color:#f2f2f2;'>
                  <td style='padding:4px;'><b>Industry</b></td>
                  <td style='padding:4px; text-align:right;'>", 
            label_data$`23. INDUSTRY SECTOR`, "</td></tr>",
            "<tr>
                  <td style='padding:4px;'><b>Years Operated</b></td>
                  <td style='padding:4px;text-align:right;'>",
            label_data$min_year, "-", label_data$max_year,
            "<tr style='background-color:#f2f2f2;'>
                <td style='padding:4px;'><b>Average Release</b></td>
                <td style='padding:4px; text-align:right;'>", 
            comma(label_data$avg_release), " lbs</td></tr>",
            "</td></tr>
            <tr>
                  <td style='padding:4px;'><b>Percent Recycled</b></td>
                  <td style='padding:4px; text-align:right;'>", 
            comma(label_data$avg_recycprop), " %</td></tr>",
            "<tr style = 'background-color:#f2f2f2;'><td style='padding:4px;'>
            <b>Chemicals</b></td><td style='padding:4px;'>", 
            ifelse(
              label_data$carcin_sum > 0, 
              "✓ Carcinogens<br>", 
              "✗ Carcinogens<br>"),
            ifelse(
              label_data$CAAC_sum > 0, 
              "✓ CAA Hazardous<br>", 
              "✗ CAA Hazardous<br>"),
            ifelse(
              label_data$pbt_sum > 0, 
              "✓ PBT Chemical<br>", 
              "✗ PBT Chemical<br>"),
            ifelse(
              label_data$pfas_sum > 0, 
              "✓ PFAS Chemical", 
              "✗ PFAS Chemical"), "</td></tr>",
            "</table>"
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
          
          NULL
  
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
      
      # Generate map
      output$Map <- leaflet::renderLeaflet({
        
        req(!is.na(coords()$lat))
        
        if (!is.null(coords()) && 
            !is.null(coords()$display_name) && 
            !is.null(coords()$lat) && 
            !is.na(coords()$lat) &&
            grepl(
              "United States", 
              coords()$display_name)
        ) {
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
            group_by(
              `4. FACILITY NAME`,
              `23. INDUSTRY SECTOR`,
              `8. ST`,
              geometry) %>%
            summarize(
              avg_release = round(mean(total_release, na.rm = TRUE), 2),
              avg_recycprop = round(mean(recyc_prop, na.rm = TRUE), 2),
              carcin_sum = sum(carcin_count, na.rm = TRUE),
              CAAC_sum = sum(CAAC_count, na.rm = TRUE),
              pbt_sum = sum(pbt_count, na.rm = TRUE),
              pfas_sum = sum(pfas_count, na.rm = TRUE),
              min_year = median(min_year),
              max_year = median(max_year),
              fac_label = as.factor(cur_group_id()))
          
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
                markerColor = "black",
              ),
              group = "Primary",
              layerId = "Primary",
              popup = ~HTML(as.character(label())),
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
              layerId = ~paste(
                `4. FACILITY NAME`,
                `8. ST`, 
                fac_label, 
                sep = "___"),
              popup = ~paste0(
                "<h6 style='text-align:center; font-weight:600; color:#333; 
                            margin:5px 0; padding:5px; 
                            font-family:Helvetica;'>",
                `4. FACILITY NAME`, "</h6>",
                "<table style='width:100%; border-collapse: collapse;'>",
                "<tr style='background-color:#f2f2f2;'>
                <td style='padding:4px;'><b>Industry</b></td>
                <td style='padding:4px; text-align:right;'>", 
                `23. INDUSTRY SECTOR`, "</td></tr>",
                "<tr>
                <td style='padding:4px;'><b>Years Operated</b></td>
                <td style='padding:4px;text-align:right;'>",
                min_year, "-", max_year,
                "</td></tr>
                <tr style='background-color:#f2f2f2;'>
                <td style='padding:4px;'><b>Average Release</b></td>
                <td style='padding:4px; text-align:right;'>", 
                comma(avg_release), " lbs</td></tr>
                <tr>
                  <td style='padding:4px;'><b>Percent Recycled</b></td>
                  <td style='padding:4px; text-align:right;'>", 
                comma(avg_recycprop), " %</td></tr>
                <tr style='background-color:#f2f2f2;'>
                <td style='padding:4px;'><b>Chemicals</b></td>
                <td style='padding:4px;'>", 
                ifelse(
                  carcin_sum > 0, 
                  "✓ Carcinogens<br>", 
                  "✗ Carcinogens<br>"),
                ifelse(
                  CAAC_sum > 0, 
                  "✓ CAA Hazardous<br>", 
                  "✗ CAA Hazardous<br>"),
                ifelse(
                  pbt_sum > 0, 
                  "✓ PBT Chemical<br>", 
                  "✗ PBT Chemical<br>"),
                ifelse(
                  pfas_sum > 0, 
                  "✓ PFAS Chemical", 
                  "✗ PFAS Chemical"), "</td></tr>",
                "</table>"
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
              layerId = ~paste(
                `4. FACILITY NAME`,
                `8. ST`, 
                fac_label, 
                sep = "____"),
              popup = ~paste0(
                "<h6 style='text-align:center; font-weight:600; color:#333; 
                            margin:5px 0; padding:5px; 
                            font-family:Helvetica;'>",
                `4. FACILITY NAME`, "</h6>",
                "<table style='width:100%; border-collapse: collapse;'>",
                "<tr style='background-color:#f2f2f2;'>
                  <td style='padding:4px;'><b>Industry</b></td>
                  <td style='padding:4px; text-align:right;'>", 
                `23. INDUSTRY SECTOR`, "</td></tr>",
                "<tr>
                  <td style='padding:4px;'><b>Years Operated</b></td>
                  <td style='padding:4px;text-align:right;'>",
                min_year, "-", max_year,
                "</td></tr>
                <tr style='background-color:#f2f2f2;'>
                <td style='padding:4px;'><b>Average Release</b></td>
                <td style='padding:4px; text-align:right;'>", 
                comma(avg_release), " lbs</td></tr>
                <tr>
                  <td style='padding:4px;'><b>Percent Recycled</b></td>
                  <td style='padding:4px; text-align:right;'>", 
                comma(avg_recycprop), " %</td></tr>
                <tr style='background-color:#f2f2f2;'>
                <td style='padding:4px;'><b>Chemicals</b></td>
                <td style='padding:4px;'>", 
                ifelse(
                  carcin_sum > 0, 
                  "✓ Carcinogens<br>", 
                  "✗ Carcinogens<br>"),
                ifelse(
                  CAAC_sum > 0, 
                  "✓ CAA Hazardous<br>", 
                  "✗ CAA Hazardous<br>"),
                ifelse(
                  pbt_sum > 0, 
                  "✓ PBT Chemical<br>", 
                  "✗ PBT Chemical<br>"),
                ifelse(
                  pfas_sum > 0, 
                  "✓ PFAS Chemical", 
                  "✗ PFAS Chemical"), "</td></tr>",
                "</table>"
              ),
              group = "Recycling" 
            ) %>%
            leaflet::addLayersControl(
              overlayGroups = c("Release", "Recycling", "Heatmap")
              ) %>%
            leaflet::hideGroup(group = "Recycling")
          
        } else {NULL}
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
      
      state <- reactiveVal({NULL})
      fac <- reactiveVal({NULL})
      
      observeEvent(input$Map_marker_click, {
        
        click <- input$Map_marker_click
        
        full_id <- click$id
        
        if (click$group == "Recycling"){
          
          # Separate the combined id back into Facility and State
          parts <- strsplit(full_id, "____")[[1]]
          clicked_fac <- parts[1]
          clicked_state <- parts[2]
          
          state(clicked_state)
          fac(clicked_fac)
          
        } else if (click$group == "Release"){
          
          # Separate the combined id back into Facility and State
          parts <- strsplit(full_id, "___")[[1]]
          clicked_fac <- parts[1]
          clicked_state <- parts[2]

          state(clicked_state)
          fac(clicked_fac)
          
        } else {NULL}
        
      })
      
      return(
        list(
          state = state,
          fac = fac
        )
      )
    }
  )
}