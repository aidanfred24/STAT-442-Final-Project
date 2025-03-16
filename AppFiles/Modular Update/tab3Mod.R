
tab3UI <- function(id, state_choices, metric_options) {
  
  ns <- NS(id)
  
  page_sidebar(               # Allows for sidebar unique to this tab
    
    sidebar = sidebar(        # Define sidebar
      title = "Options",
      
      # Custom class for numeric input (make it smaller)
      tags$head(
        tags$style(HTML("
        .custom-numeric-input .form-control {
        padding: 0px !important; /* Adjust inner spacing */
        }"
        ))
      ),
      
      #Input for state selection
      selectInput(ns("State"),
                  label = "State",
                  choices = state_choices,
                  selected = "AK"),
      
      # Input for facility choice (updated in server for better performance)
      selectizeInput(ns("Facility"),
                     label = "Facility",
                     choices = NULL,       #Starts with no choices, updated by state
                     multiple = FALSE,
                     options = list(maxOptions = 300000)),  #Change max num of options
      
      # Metric choice for timeline
      selectInput(ns("TimeMetric"),
                  label = "Timeline Metric",
                  choices = metric_options),
      
      # Insert text between input sections
      tags$p("Search Address"),
      
      # Put contents side-by-side
      fluidRow(
        # 1st element, half of row
        column(6,
               # input location number if facility has >1
               numericInput(inputId = ns("Location"), 
                            label = "Location #", 
                            value = 1, # Default value
                            min = 1, # Can't go below 1
                            step = 1) |>
                 tagAppendAttributes(class = "custom-numeric-input")), #add custom class
        
        # 2nd element, half of row
        column(6,
               # search button for facility
               actionButton(ns("SearchButton"),
                            label = "Search"))
        
      ),
      
      # Data info at bottom of sidebar
      div(
        style = "position: absolute; bottom: 10px; left: 10px; right: 10px; font-size: 0.6em; color: grey;",
        HTML("Using 2013-2023 release data, sourced from the <br>Environmental Protection Agency.")
      )
      
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
        
        # Content for grid card
        card_body(
          # render timeline in area1
          girafeOutput(ns("timeline"),
                       width = "100%") %>% # Adjust height
            withSpinner(type = 4,                  # Pipeline the spinner onto the visual!
                        color = "#000000") %>% 
            as_fill_carrier()),              # Tell the graph to stay within the card
        
        full_screen = TRUE    # Full screen option
      ),
      # Move to the left side of the grid
      grid_card(
        area = "area1", # top left area
        card_body(
          style = "background-color: #434854;",
          # Box to display values in cards
          value_box(
            style = "overflow:hidden; margin: -6px;",
            title = "Primary Industry",  # title for value box
            value = uiOutput(ns("Industry")),  # text output for top industry
            showcase = bsicons::bs_icon("shop",
                                        color = "#000000") # Shop icon for the box
          )
        )
      ),
      # card 2nd from the top
      grid_card(
        area = "area2",
        card_body(
          style = "background-color: #434854;",
          value_box(
            style = "overflow:hidden; margin: -6px;",
            title = "Primary Release Method",
            value = uiOutput(ns("Method")),     # Text output for top method
            showcase = bsicons::bs_icon("globe-americas",
                                        color = "#000000") # globe icon
          )
        )
      ),
      # 3rd card down
      grid_card(
        area = "area3",
        card_body(
          style = "background-color: #434854;",
          value_box(
            style = "overflow:hidden; margin: -6px;",
            title = "Average Yearly Release",
            value = uiOutput(ns("Average")),      # Text output for avg yearly release
            showcase = bsicons::bs_icon("bar-chart-line-fill",
                                        color = "#000000")  # bar chart icon
          )
        )
      ),
      
      # Bottom left card
      grid_card(
        area = "area4",
        card_header("Chemical Checklist",
                    style = "color: white; background-color: #434854; margin-bottom: 10px"), # text header for the card
        card_body(
          # custom css style for the text body (less whitespace)
          style = "overflow: hidden; padding-left: 20px; padding-top: 0px; padding-bottom: 0px",
          
          tags$div( # custom arrangement of objects
            style = "display: flex; align-items: center;", # Center items, start from left
            
            # card_body doesn't have "showcase" option, so we must add an icon
            bsicons::bs_icon("radioactive", 
                             size = 70,
                             color = "#000000"),
            
            uiOutput(ns("checklist")) # Add dynamic checklist
          )
        )
      )
    )
  )
  
}

tab3Server <- function(id, metric_options, axis_options, method_options, 
                       Decade_Data, tab2vars, tab1vars) {
  
  moduleServer(
    
    id, 
    
    function(input, output, session) {
      
      # Dynamically populate selectize input with server-side processing
      observe({
        
        # Filter data to state selected (improves performance)
        TRIDecade1 <- Decade_Data %>%
          filter(`8. ST` == input$State)
        
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
      
      # Filter out chosen facility from chose state
      TRIDecade2 <- reactive({
        
        # Require non-NULL facility name before executing
        req(input$Facility %in% Decade_Data$`4. FACILITY NAME`)
        
        Decade_Data %>%
          filter(`8. ST` == input$State,
                 `4. FACILITY NAME` == input$Facility)%>%
          mutate(`1. YEAR` = as.character(`1. YEAR`)) %>%  #format year as a character
          group_by(`12. LATITUDE`, `13. LONGITUDE`, `23. INDUSTRY SECTOR`) %>%    # group by unique location
          mutate(facility_label = as.factor(cur_group_id())) %>% # Add label for each facility location
          ungroup() # ungroup, keeping the labels
        
      })
      
      # Timeline for facilties
      output$timeline <- renderGirafe({
        
        # Require non-NULL facility name before executing
        req(input$Facility %in% Decade_Data$`4. FACILITY NAME`)
        
        #Dynamic Text options for plots
        title_text <- names(metric_options[which(metric_options == input$TimeMetric)])
        axis_text <- names(axis_options[which(axis_options == input$TimeMetric)])
        
        # Timeline for facility
        p3 <- ggplot(data = TRIDecade2())+
          aes(x = `1. YEAR`, 
              y = .data[[input$TimeMetric]])+ # Selected metric on y-axis
          geom_line(mapping = aes(group = facility_label,  # Group them by coordinates (could also use location label)
                                  color = facility_label), # Color by location label 
                    linewidth = .75)+ # Thicker line for readability
          geom_point_interactive(mapping = aes(data_id = interaction(facility_label, # Interactive layer, data_id is both the group and year
                                                                     `1. YEAR`),
                                               tooltip = paste("Year: ", `1. YEAR`,  # Year and dynamic tooltip text
                                                               "\nLocation #: ", facility_label,
                                                               "\n", title_text, ": ", comma(round(.data[[input$TimeMetric]], 0)))), # Add commas to numbers
                                 size = 2)+ # Bigger point size
          scale_y_continuous(label = comma, # add commas to y-labels
                             limits = c(0, NA))+ # Start from 0
          theme_minimal()+ # minimal theme
          labs(title = paste(title_text), # Dynamic labels
               x = "Year",
               y = axis_text,
               color = "Location #")+
          theme(axis.title = element_text(size = 10), # Adjust size of plot elements
                plot.title = element_text(size = 12, hjust = 0.5),
                axis.text.x = element_text(size = 8),
                axis.text.y = element_text(size = 8),
                legend.position = "top",
                legend.text = element_text(size = 8),
                legend.title = element_text(size = 8),
                plot.margin = margin(t = 5, r = 0, b = 2, l = 2, unit = "pt"))
        
        # Make timeline interactive
        girafe(ggobj = p3,
               height_svg = 5,
               width_svg = 7,
               options = list(opts_hover(css = "fill:green;stroke:black;r: 5px; transition: all 0.1s ease;"),
                              opts_selection(type = "none"),
                              opts_sizing(rescale = TRUE))) # Add hover, no selection
        
      })
      
      # Text output for primary industry
      output$Industry <- renderUI({
        
        # filter to chosen facility
        Industry1 <- Decade_Data %>%
          filter(`8. ST` == input$State,
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
        FindMethod <- Decade_Data %>%
          filter(`8. ST` == input$State,
                 `4. FACILITY NAME` == input$Facility)
        
        # find column name with the highest releases
        top_method <- colnames(FindMethod[, 13:18])[which.max(colSums(FindMethod[,13:18]))]
        
        tags$span(names(method_options[which(method_options == top_method)]), # Use dynamic labels and adjust size
                  style = "font-size: 20px;")
        
      })
      
      # Average Yearly Releases
      output$Average <- renderUI({
        
        # Filter to chosen facility
        Yearly <- Decade_Data %>%
          filter(`8. ST` == input$State,
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
        CheckChem <- Decade_Data %>%
          filter(`8. ST` == input$State,
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
              tags$li(tags$span(style = "color: green;", "✔ "), item,
                      style = " font-size: 14px;") 
              
            } else {
              
              tags$li(tags$span(style = "color: red;", "✘ "), item,
                      style = " font-size: 14px;") # red X if not present
              
            }
          })
        )
      })
      
      # Watch changes in facility selection
      observeEvent(input$Facility, {
        
        # update maximum value of facility locations, set to max of selected facility
        updateNumericInput(session, "Location",
                           value = 1,
                           max = max(unique(as.numeric(TRIDecade2()$facility_label))))
        
      })
      
      # Observe change in tab2 state selection
      observeEvent(tab2vars$state(),{
        
        # Update tab3 state selection
        updateSelectInput(session, "State", selected = tab2vars$state())
        
      })
      
      # Observe tab2 facility clicked
      observeEvent(tab2vars$fac(),{
        
        # update to clicked facility on tab 2
        updateSelectInput(session, "Facility", selected = tab2vars$fac())
        
      }
      )
      
      # Default (empty) address/location
      add <- reactiveVal({
        tibble(lat = NA, long = NA, display_name = "", ind = "")
      })
      
      # Observe search button press
      observeEvent(input$SearchButton, {
        
        # Filter state data to selected location number
        coord_data <- TRIDecade2() %>%
          filter(facility_label == input$Location) %>% 
          group_by(`12. LATITUDE`, `13. LONGITUDE`, `23. INDUSTRY SECTOR`) %>% # group by location (redundant, but ok) and industry
          summarize(total_release = sum(total_release)) # total release for facility (not used, just allows grouping to take place)
        
        # table of facility coordinates, address, and industry
        table1 <- tibble(reverse_geo(lat = coord_data$`12. LATITUDE`,
                                     long = coord_data$`13. LONGITUDE`),
                         ind = coord_data$`23. INDUSTRY SECTOR`)
        
        # Change reactive value to this
        add(table1)
        
      })
      
      # reactive value for tab switch on search button click
      tab <- eventReactive(input$SearchButton, {
        
        "addsch"
        
      })
      
      # return address table and tab switch variable
      return(
        
        list(
          
          add = add,
          tab = tab
          
          
        )
        
      )
      
    }
    
  )
  
}