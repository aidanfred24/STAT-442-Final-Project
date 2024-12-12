
#basic shiny functions
library(shiny)

#fancy loading symbols
library(shinycssloaders)

#for statewise plots and timeline
library(ggplot2)

#make ggplots interactive
library(ggiraph)

#for fancy themes and formatting
library(bslib)

# UI grid layout with cards (page 2)
library(gridlayout)

#data manipulation/pipelining
library(tidyverse)

#"simple features" to read shapefiles and plot maps
library(sf)
library(units) # format data with units attached (helps with sf objects)

# text editing (commas and case of letters)
library(stringr)
library(scales)

#color palettes
library(RColorBrewer)

# read in data files
library(readr)

# libraries for fancy geographic maps (page 2)
library(leaflet)
library(leaflet.extras)

# load in state/county-wise data
TRI23_counties <- read_sf("TRI23_counties.shp")
TRI23_states <- read_sf("TRI23_states.shp")

# round all release and waste values to nearest unit
TRI23_counties$prd_wst <- round(TRI23_counties$prd_wst, 0)
TRI23_states$prd_wst <- round(TRI23_states$prd_wst, 0)
TRI23_counties$ttl_rls <- round(TRI23_counties$ttl_rls, 0)
TRI23_states$ttl_rls <- round(TRI23_states$ttl_rls, 0)

# read in most recent decade of data
TRIDecade <- read_csv("TRIDecade.csv")

# calculate important proportions for the decade
TRIDecade <- TRIDecade %>%
  mutate(recyc_prop = round((recycled / prod_waste) * 100, 2),
         treat_prop = round((treated / prod_waste) * 100, 2),
         rls_prop = round((total_release / prod_waste) * 100, 2)) %>%
  mutate(across(everything(), ~replace_na(., 0)))    #replace NA with 0

#Use all unique state names as choices
state_choices <- unique(TRI23_states$NAME)

#state abbr. with full name as name in list
state_choices2 <- setNames(TRI23_states$STUSPS, TRI23_states$NAME)

# Metric options for the first page
metric_options <- setNames(colnames(st_drop_geometry(TRI23_states[,c(18, 19, 2, 21, 22, 31:46)])),
                           c("Total Production Waste", 
                             "Total Chemical Release",
                             "Facility Count",
                             "Top Chemical",
                             "Top Industry",
                             "% Hazarous Chemicals",
                             "% Toxic Metals",
                             "% Carcinogens",
                             "% PBT Chemicals",
                             "% PFAS Chemicals",
                             "% Fugitive Air Release",
                             "% Stack Air Release",
                             "% Water Release",
                             "% Underground Release",
                             "% Landfill Release",
                             "% Misc. Release",
                             "% On-Site Release",
                             "% Off-Site Release",
                             "% Waste Recycled",
                             "% Waste Treated",
                             "% Waste Release"))

# Metric options for the timeline on page 2
metric_options2 <- setNames(colnames(TRIDecade[ ,c(24, 21:23, 25:27)]),
                            c("Total Released",
                              "Total Recycled",
                              "Total Treated",
                              "Total Production Waste",
                              "Proportion of Toxics Recycled",
                              "Proportion of Toxics Treated",
                              "Proportion of Toxics Released"))

# Dynamic y-axis labels for timeline
y_axis <- setNames(metric_options2,
                   c(rep("Pounds", 4),
                     rep("Percent", 3)))

# Dynamic tooltip labels for each metric
tooltips <- setNames(metric_options, 
                     c("Pounds", 
                       "Pounds",
                       "Facilities",
                       "Chemical",
                       "Industry",
                       rep("% of Facilities", 5),
                       rep("% of Release", 8),
                       rep("% of Waste", 3)))

# Dynamic captions for complex metrics
captions <- setNames(metric_options, 
                     c("", "", "",
                       rep("Rated by Total Release", 2),
                       "Hazardous Chemicals - Chemicals deemed hazardous by the Clean Air Act",
                       "Toxic Metal - Metals deemed toxic, required to be reported in the TRI",
                       "Carcinogen - Chemical that has been linked to cancer development, determined by OSHA",
                       "PBT - Chemicals that are persistent, bioaccumulative, and toxic (PBT)",
                       "PFAS - Long lasting per- and polyfluoroalkyl substances that are linked to harmful health effects",
                       "Fugitive Air - Unintentional releases of gases or particulates into the air (leaking equipment, livestock operations, etc.)",
                       "Stack Air - Chemicals emitted/released from a point source",
                       rep("", 3),
                       "Misc. - Chemicals released to other disposal units/impoundments",
                       rep("", 3),
                       "Chemicals that were cleaned at treatment plants",
                       ""))

# Names for each method column
methods <- setNames(colnames(TRIDecade[,13:18]), 
                    c("Fugitive Air Emissions",
                      "Stack Air Emissions",
                      "Surface Water Discharge",
                      "Underground Injection",
                      "Landfill Release",
                      "Other Disposal Units"))

ui <- page_navbar(
  title = "Toxic Release Inventory ",      # App title
  selected = "Statewise",                  # Selected tab
  collapsible = TRUE,
  theme = bs_theme(preset = "materia",     # Custom theme, "materia" bs preset
                   primary = "#000000"),
  
  # Define first panel
  nav_panel(
    title = "Statewise",    #Tab title
    
    page_sidebar(           # Allows for sidebar unique to this tab
      sidebar = sidebar(    # Define sidebar
        title = "Options",
        
        # Input for state selection or national
        selectInput("State", 
                    label = "Select State(s):",
                    choices = c("United States Mainland",
                                "United States (Mainland & Alaska)", 
                                state_choices),
                    selected = "United States (Mainland)"),  #set default value
        
        # Metric selection
        selectInput("Stat",
                    label = "Select Metric",
                    choices = metric_options)
        
        ),
      
      # Define info card for graph in the main panel
      card(
        full_screen = TRUE,       # Optional: Allow full-screen toggle
        card_body(
          # render interactive map with loading symbol
          withSpinner(girafeOutput(outputId = "StateMaps", 
                                   height = "80vh"),  # small height adj.
                      type = 4,             #spinner type, color, size
                      color = "#000000",
                      size = 3)
        ),
      )
    )
  )
  ,
  # UI for profile panel
  nav_panel(
    title = "Facility Profile",     # Tab title
    
    page_sidebar(               # Allows for sidebar unique to this tab
      
      sidebar = sidebar(        # Define sidebar
        title = "Options",
        
        #Input for state selection
        selectInput("States2",
                    label = "State",
                    choices = state_choices2,
                    selected = "AK"),
       
        # Input for facility choice (updated in server for better performance)
        selectizeInput("Facility",
                    label = "Facility",
                    choices = NULL,       #Starts with no choices, updated by state
                    multiple = FALSE,
                    options = list(maxOptions = 300000)),  #Change max num of options
        
        # Metric choice for timeline
        selectInput("TimeMetric",
                    label = "Timeline Metric",
                    choices = metric_options2)
         
      ),
      
    # Define grid layout for 2nd page
    grid_container(
      # Overall layout, 8 areas, right half is like a "sub-area"
      layout = c(
        "area1 area0",
        "area2 area0",
        "area3 area0",
        "area4 area0"
      ),
      row_sizes = c(     #Equal sizes for each row of the grid
        "1fr",
        "1fr",
        "1fr",
        "1fr"
      ),
      col_sizes = c(    # Larger right column (area0) for bigger plots
        "0.6fr",
        "1.4fr"
      ),
      gap_size = "10px",  # Gaps between boxes and border
      # Define card on grid space
      grid_card(
        area = "area0", # select "sub-area" from above
        # Define content for card body
        card_body(
          # Nested grid container for 2 plots
          grid_container(
            layout = c(
              "area0",
              "area1"
            ),
            row_sizes = c( # Equal rows (same height plots)
              "1fr",
              "1fr"
            ),
            col_sizes = c( # Only 1 column
              "1fr"
            ),
            gap_size = "10px",
            
            # Grid card for bottom area1
            grid_card(
              area = "area1",
              # Content for grid card
              card_body(
                # render timeline in area1
                girafeOutput("timeline",
                             height = "100vh") %>% # Adjust height
                  withSpinner(type = 4,                  # Pipeline the spinner onto the visual!
                              color = "#000000") %>% 
                  as_fill_carrier()),              # Tell the graph to stay within the card
              
              full_screen = TRUE    # Full screen option
            ),
            
            # card for top area0
            grid_card(
              area = "area0",
              card_body(
                # Fancy geographic heatmap
                leafletOutput("Map") %>% 
                  withSpinner(type = 4,   # Pipeline the spinner!
                              color = "#000000") %>% 
                  as_fill_carrier()),     # Tell graph to fill the card
              
              full_screen = TRUE
            )
          )
        )
      ),
      # Move to the left side of the grid
      grid_card(
        area = "area1", # top left area
        card_body(
          # Box to display values in cards
          value_box(
            title = "Primary Industry",  # title for value box
            value = uiOutput("Industry"),  # text output for top industry
            showcase = bsicons::bs_icon("shop",
                                        color = "#000000") # Shop icon for the box
          )
        )
      ),
      # card 2nd from the top
      grid_card(
        area = "area2",
        card_body(
          value_box(
            title = "Primary Release Method",
            value = uiOutput("Method"),     # Text output for top method
            showcase = bsicons::bs_icon("globe-americas",
                                        color = "#000000") # globe icon
          )
        )
      ),
      # 3rd card down
      grid_card(
        area = "area3",
        card_body(
          value_box(
            title = "Average Yearly Release",
            value = uiOutput("Average"),      # Text output for avg yearly release
            showcase = bsicons::bs_icon("bar-chart-line-fill",
                                        color = "#000000")  # bar chart icon
          )
        )
      ),
      
      # Bottom left card
      grid_card(
        area = "area4",
        card_header("Chemical Checklist"), # text header for the card
        card_body(
          # custom css style for the text body (less whitespace)
          style = "overflow: hidden; padding-left: 20px; padding-top: 0px; padding-bottom: 0px",
          
          tags$div( # custom arrangement of objects
            style = "display: flex; align-items: center;", # Center items, start from left
            
            # card_body doesn't have "showcase" option, so we must add an icon
            bsicons::bs_icon("radioactive", 
                             size = 70,
                             color = "#000000"),
            
            uiOutput("checklist") # Add dynamic checklist
          )
        )
      )
    )
    )
  )
 )


server <- function(input, output, session) {
  
  
# PAGE 1 
#====================================================================================
  output$StateMaps <- renderGirafe({
    
    #Dynamic Text options for plots
    title_text <- names(metric_options[which(metric_options == input$Stat)])
    tooltip_text <- names(tooltips[which(tooltips == input$Stat)])
    caption_text <- names(captions[which(captions == input$Stat)])
    
    # Function for dynamic color scale in ggplot
    get_scale_fill <- function(data, variable) {
      
      if (is.numeric(data[[variable]])) {
        
        # Continuous scale
        scale_fill_viridis_c(option = "C",
                             name = tooltip_text,  # Match legend title to units
                             label = function(x){
                               
                               format(x, big.mark = ",", scientific = FALSE)  # function to add commas to legend labels
                               
                               })

      } else {
        # Discrete scale
        scale_fill_discrete(name = tooltip_text)  #Match legend title to units
      }
    }
  
    # if/else structure for input cases
    if (input$State == "United States Mainland"){
        
        #Filter out non-mainland states
        TRI23_snew <- TRI23_states %>%
          filter(!STUSPS %in% c("AK", "AS", "GU", "HI", "MP", "PR", "VI"))
        
        #Mainland plot
        p2 <- ggplot(data = TRI23_snew)+
          geom_sf_interactive(mapping = aes(fill = .data[[input$Stat]],    # Fill by input
                                            data_id = NAME,                #Id for interactive layer in ggiraph
                                            tooltip = paste("State: ", NAME,       # Text Displayed on hover
                                                            "\n", tooltip_text,    # Use our dynamic text
                                                            ": ", format(.data[[input$Stat]], big.mark = ","))),  # Format numbers with commas
                              color = "black")+  # Color of state outline
          theme_void()+  #remove all unnecessary plot elements
          get_scale_fill(TRI23_states, input$Stat)+  # Use or custom function for color scale
          labs(title = paste(title_text, " by State in ", input$State),  # Dynamic title
               caption = caption_text,  # Dynamic caption
               fill= tooltip_text)+     # Dynamic legend title
          theme(plot.caption = element_text(color = "#555666"))  # Change caption color
        
        # Make the plot interactive
        girafe(ggobj = p2,
               options = list(opts_hover(css = "fill:green;stroke:black"),  # Add css hover options
                              opts_zoom(min = 1, max = 20, duration = 300), # Allow user to zoom
                              opts_selection(type = "none")),  # Disable lasso option (useless)
               width_svg = 10, # Change plot aspect ratio
               height_svg = 6)
      
      # Case to include Alaska
    } else if (input$State == "United States (Mainland & Alaska)"){
      
      # Exclude territories (and Hawaii)
      TRI23_snew <- TRI23_states %>%
        filter(!STUSPS %in% c("AS", "GU", "HI", "MP", "PR", "VI"))
      
      # Alaska plot
      p2 <- ggplot(data = TRI23_snew)+
        geom_sf_interactive(mapping = aes(fill = .data[[input$Stat]], # fill by metric
                                          data_id = NAME,             # ID for interactivity
                                          tooltip = paste("State: ", NAME, # Text on hover
                                                          "\n", tooltip_text,
                                                          ": ", format(.data[[input$Stat]], big.mark = ","))),
                            color = "black")+
        coord_sf(xlim = c(-180, -60))+  # Edit map bounds to better fit Alaska
        theme_void()+
        get_scale_fill(TRI23_states, input$Stat)+ # Use custom color scale function
        labs(title = paste(title_text, " by State in ", input$State),  # Dynamic labels
             caption = caption_text,
             fill = tooltip_text)
      
      # Interactive conversion
      girafe(ggobj = p2,
             options = list(opts_hover(css = "fill:green;stroke:black"), # Add hover, zoom, remove selection
                            opts_zoom(min = 1, max = 20, duration = 300),
                            opts_selection(type = "none")),
             width_svg = 10, # Aspect ratio change
             height_svg = 5)
    
      # Case for individual state selection  
    } else {
        
      # Custom bounds for Alaska and American Samoa
        if (input$State == "Alaska"){
          
          xlim1 <- c(-180, -120)
          
          ylim1 <- c(NA, NA)
          
        } else if (input$State == "American Samoa"){
          
          xlim1 <- c(NA, NA)
          
          ylim1 <- c(-15, -13.8)
          
        } else {
          
          xlim1 <- c(NA, NA)
          
          ylim1 <- c(NA, NA)
          
        }
    
      # Filter only selected state from county data, replace ' 
        TRI23_cnew <- TRI23_counties %>%
          filter(STATENA == input$State) %>%
          mutate(NAME = gsub("'", "", NAME))
        
        # Plot of counties in selected state
        p1 <- ggplot(data = TRI23_cnew)+
          geom_sf_interactive(mapping = aes(fill = .data[[input$Stat]], 
                                            data_id = NAME,
                                            tooltip = paste("State: ", input$State,  # State name
                                                            "\nCounty: ", str_to_title(NAME), # County name
                                                            "\n", tooltip_text,
                                                            ": ", format(.data[[input$Stat]], big.mark = ","))),
                              color = "black")+
          coord_sf(xlim = xlim1,   # Custom bounds from above (AK, AS)
                   ylim = ylim1)+
          theme_void()+
          get_scale_fill(TRI23_states, input$Stat)+  # Dynamic color scale
          labs(title = paste(title_text, " by County in ", input$State), # Dynamic labels
               caption = caption_text,
               fill = tooltip_text)
        
        # Interactivity
        girafe(ggobj = p1,
               options = list(opts_hover(css = "fill:green;stroke:black"), # Add hover, zoom, remove selection
                              opts_zoom(min = 1, max = 20, duration = 300),
                              opts_selection(type = "none")),
               width_svg = 10,  #adjust width and height of plot
               height_svg = 6)
    
    }
  })
  
#====================================================================================

# PAGE 2
#====================================================================================
  
  # Dynamically populate selectize input with server-side processing
  observe({
    
    # Filter data to state selected (improves performance)
    TRIDecade1 <- TRIDecade %>%
      filter(`8. ST` == input$States2)
    
    # Find facilities
    facility_choices1 <- unique(TRIDecade1$`4. FACILITY NAME`)
    
    # Update user input with facility options after resetting to NULL
    updateSelectizeInput(
      
      session,
      "Facility",
      choices = NULL,
      server = TRUE
      
    )
    
    updateSelectizeInput(
      
      session,
      "Facility", # Input ID
      choices = facility_choices1, # New choices
      server = TRUE
      
    )
  })
  
  # Timeline for facilties
  output$timeline <- renderGirafe({
    
    # Require non-NULL facility name before executing
    req(input$Facility %in% TRIDecade$`4. FACILITY NAME`)
    
    #Dynamic Text options for plots
    title_text <- names(metric_options2[which(metric_options2 == input$TimeMetric)])
    axis_text <- names(y_axis[which(y_axis == input$TimeMetric)])
    
    # Filter out chosen facility from chose state
    TRIDecade2 <- TRIDecade %>%
      filter(`8. ST` == input$States2,
             `4. FACILITY NAME` == input$Facility)%>%
      mutate(`1. YEAR` = as.character(`1. YEAR`)) %>%  #format year as a character
      group_by(`12. LATITUDE`, `13. LONGITUDE`) %>%    # group by unique location
      mutate(facility_label = as.factor(cur_group_id())) %>% # Add label for each facility location
      ungroup() # ungroup, keeping the labels
      
    # Timeline for facility
    p3 <- ggplot(data = TRIDecade2)+
      aes(x = `1. YEAR`, 
          y = .data[[input$TimeMetric]])+ # Selected metric on y-axis
      geom_line(mapping = aes(group = `12. LATITUDE`,  # Group them by coordinates (could also use location label)
                              color = facility_label), # Color by location label 
                linewidth = 2)+ # Thicker line for readability
      geom_point_interactive(mapping = aes(data_id = interaction(`12. LATITUDE`, # Interactive layer, data_id is both the group and year
                                                                 `1. YEAR`),
                                           tooltip = paste("Year: ", `1. YEAR`,  # Year and dynamic tooltip text
                                                           "\nLocation #: ", facility_label,
                                                           "\n", title_text, ": ", comma(round(.data[[input$TimeMetric]], 0)))), # Add commas to numbers
                             size = 5)+ # Bigger point size
      scale_y_continuous(label = comma, # add commas to y-labels
                         limits = c(0, NA))+ # Start from 0
      theme_minimal()+ # minimal theme
      labs(title = paste(title_text, " by ", input$Facility), # Dynamic labels
           x = "Year",
           y = axis_text,
           color = "Location #")+
      theme(axis.title = element_text(size = 18), # Adjust size of plot elements
            plot.title = element_text(size = 22),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14))
    
    # Make timeline interactive
    girafe(ggobj = p3,
           width_svg = 18, # Change aspect ratio
           height_svg = 6,
           options = list(opts_hover(css = "fill:green;stroke:black"))) # Add hover
    
  })
  
  # Fancy heatmapppppppp
  output$Map <- renderLeaflet({
    
    # Require facility selected before generation
    req(input$Facility %in% TRIDecade$`4. FACILITY NAME`)
    
    # Filter to chosen facility
    fac_of_int <- TRIDecade %>%
      filter(`8. ST` == input$States2,
             `4. FACILITY NAME` == input$Facility) %>%
      group_by(`4. FACILITY NAME`, `12. LATITUDE`, `13. LONGITUDE`) %>% # Group by facility and coords
      summarize(total_release = sum(total_release)) # Add up releases
    
    # Convert coords to a geometry column
    fac_of_int <- st_as_sf(fac_of_int, #data
                           coords = c("13. LONGITUDE", "12. LATITUDE"), # coord cols
                           crs = 4326) # Coordinate system
    
    # Filter to all facilities in chosen state
    facilities <- TRIDecade %>%
      filter(`8. ST` == input$States2) 
    
    # Convert coords to geometry col
    facilities <- st_as_sf(facilities,
                           coords = c("13. LONGITUDE", "12. LATITUDE"),
                           crs = 4326) # coordinate system
    
    # Use sf package to calculate distance from chosen facility
    distances <- st_distance(facilities, fac_of_int[1, ])
    
    # Set radius to 50km
    radius <- set_units(50, "km")
    
    # Filter to facilities within 50km of chosen facility
    nearby_facilities <- facilities %>%
      mutate(distance = distances) %>%
      filter(distance <= radius | `4. FACILITY NAME` == input$Facility)
    
    # Leaflet map
    leaflet() %>%
      addTiles() %>%
      # Add marker for the facility of interest
      addAwesomeMarkers(
        data = fac_of_int,
        label = ~paste("Facility: ", `4. FACILITY NAME`, "|", # Label for hover effect
                      "Total Release: ",
                      comma(round(total_release, 0)),
                      "Pounds"),
        popup = ~paste("Facility: ", `4. FACILITY NAME`, "|", # Popup on click
                      "Total Release: ",
                      comma(round(total_release, 0)),
                      "Pounds"),
        icon = awesomeIcons(  # Add a customizable icon for facility
          icon = "star",
          iconColor = "yellow",
          markerColor = "black"
        )
      ) %>%
      # Heatmap of nearby facility releases
      addHeatmap(
        data = nearby_facilities,
        intensity = ~total_release, # Set intensity of heatmap
        blur = 40,  # How much each point blurs with others
        minOpacity = 20 # Minimum opacity of heat visual
      ) %>%
      addLegend(
        position = "bottomleft", # legend position
        values = nearby_facilities$total_release, # Values
        title = HTML(paste("Toxic Release Over <br>10 Years (Pounds)")), # Plot/legend title
        pal = colorNumeric(c("blue","green","yellow", "orange","red"), # Color scale to match heatmap
                           domain = nearby_facilities$total_release) # Domain of legend
      )
    
  })
  
  # Text output for primary industry
  output$Industry <- renderUI({
    
    # filter to chosen facility
    Industry1 <- TRIDecade %>%
      filter(`8. ST` == input$States2,
             `4. FACILITY NAME` == input$Facility) %>%
      group_by(`23. INDUSTRY SECTOR`) %>% # Group by industry
      summarize(total_prod = sum(prod_waste)) %>% # Add up production
      arrange(`23. INDUSTRY SECTOR`, desc(total_prod)) %>% # Arrange in descending
      slice(1) # Take top industry
    
    tags$span(paste(Industry1$`23. INDUSTRY SECTOR`), # convert to text with custom size
              style = "font-size: 20px;")
    
  })
  
  # Find top release method
  output$Method <- renderUI({
    
    # Filter to chosen facility
    FindMethod <- TRIDecade %>%
      filter(`8. ST` == input$States2,
             `4. FACILITY NAME` == input$Facility)
    
    # find column name with the highest releases
    top_method <- colnames(FindMethod[, 13:18])[which.max(colSums(FindMethod[,13:18]))]
    
    tags$span(names(methods[which(methods == top_method)]), # Use dynamic labels and adjust size
              style = "font-size: 20px;")
    
  })
  
  # Average Yearly Releases
  output$Average <- renderUI({
    
    # Filter to chosen facility
    Yearly <- TRIDecade %>%
      filter(`8. ST` == input$States2,
             `4. FACILITY NAME` == input$Facility) %>%
      group_by(`1. YEAR`, `4. FACILITY NAME`) %>% # group by year and facility
      summarize(total_release = sum(total_release)) # add up releases
    
    yearly_avg <- comma(round(mean(Yearly$total_release), 0)) # round and add commas
    
    tags$span(paste(yearly_avg, "lbs"),   # Convert to text with label and custom size
              style = "font-size: 20px;")
    
  })
  
  # checklist for chemical flags
  output$checklist <- renderUI({
    
    # Filter to facility
    CheckChem <- TRIDecade %>%
      filter(`8. ST` == input$States2,
             `4. FACILITY NAME` == input$Facility)
    
    # Named list with names assigned to flag values for facility chemicals
    checklist_items <- list(
      "CAA Hazardous Chemical" = (sum(CheckChem$CAAC_count) != 0),
      "Carcinogen (Cancer-Linked)" = (sum(CheckChem$carcin_count) != 0),
      "PBT Chemical" = (sum(CheckChem$pbt_count) != 0),
      "PFAS Chemical" = (sum(CheckChem$pfas_count) != 0)
    )
    
    # Tag for unordered list (ul)
    tags$ul(
      # custom css style: removes bullet points and margin; Edits padding
      style = "list-style-type: none; padding-left: 20px; margin: 0;",
      
      # Apply if/else across the list
      lapply(names(checklist_items), function(item) {
        
        if (checklist_items[[item]]) { # If chemical type is present
          
          # list format with check mark
          tags$li(tags$span(style = "color: green;", "✔ "), item) 
          
        } else {
          
          tags$li(tags$span(style = "color: red;", "✘ "), item) # red X if not present
          
        }
      })
    )
  })
#====================================================================================
}

shinyApp(ui, server)
  

