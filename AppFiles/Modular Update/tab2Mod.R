tab2UI <- function(id, state_choices, metric_options){
  
  ns <- NS(id)
  
  # Sidebar layout
  page_sidebar(
    
    #Define sidebar
    sidebar = sidebar(
      
      title = "Options",
      
      # Input for state selection
      selectInput(ns("State"), 
                  label = "State/Territory:", 
                  choices = state_choices),
      
      # Input for metric selection
      selectizeInput(
        inputId = ns("BarMetric"),
        label = "Plot Metric:",
        choices = metric_options
      ),
      
      # Data info at bottom of text bar
      div(
        style = "position: absolute; bottom: 10px; left: 10px; right: 10px; font-size: 0.6em; color: grey;",
        HTML("Using 2013-2023 release data, sourced from the <br>Environmental Protection Agency.")
      )
      
    ),
    
    # Specify grid layout for tab
    grid_container(
      layout = c(
        "area1 area2", # Split area on top, joined on bottom
        "area3 area3"
      ),
      row_sizes = c(
        "1.47fr",     # Larger top half for plots
        "0.53fr"
      ),
      col_sizes = c(
        "1fr",
        "1fr"
      ),
      gap_size = "10px", # 10 pixels between cards
      #top left card
      grid_card(
                area = "area1", # Specify top right area
                card_body(
                          # output facility rank plot
                          girafeOutput(ns("FacilityRank")) %>%
                            withSpinner(type = 4, # Loading animation 4
                                        size = 1,
                                        color = "#000000") %>%
                            as_fill_carrier(), # keeps output constrained to the card
                          
                          # add checkbox below plot for logarithmic scale
                          checkboxInput(
                            inputId = ns("LogCheck"),
                            label = "Log Scale",
                            value = FALSE
                          )
                          
                )
                
      ),
      # Top right plot
      grid_card(
        area = "area2", # Specify top right area
        card_body(girafeOutput(ns("IndustryPie")) %>% #output pie chart for industry
                    withSpinner(type = 4, #Another loading animation
                                size = 1,
                                color = "#000000") %>%
                    as_fill_carrier()) # fill card when loading
      ),
      
      # Lower area cards
      grid_card(
        area = "area3", # Specify bottom area
        # new grid layout for bottom
        grid_container(
          layout = c(
            "area0 area1 area2" # 3 areas, horizontal
          ),
          row_sizes = c(
            "1fr"
          ),
          col_sizes = c( # Equal sizes
            "1fr",
            "1fr",
            "1fr"
          ),
          gap_size = "10px",
          # Bottom left card
          grid_card(
            # Expand card vertically, default margins are restrictive
            style = "margin-top: -12px; margin-bottom: -12px;",
            area = "area0", # Specify left area
            # Body of card, with value box
            card_body(
              value_box(
                title = "Average Yearly Release",
                showcase = bsicons::bs_icon("bar-chart-line-fill", #Custom bootstrap icon
                                            color = "#000000"),
                value = uiOutput(ns("StateAvg")), # Output for state average release
                style = "border: none; box-shadow: none; background: transparent;" # remove card borders
              )
            )
          ),
          
          # Middle card
          grid_card(
            #Expand card vertically
            style = "margin-top: -12px; margin-bottom: -12px;",
            area = "area1", # Specify middle area
            card_body(
              value_box(
                title = "State vs. National Median Facility Release",
                showcase = uiOutput(ns("Icon")), # Dynamic icon output
                value = uiOutput(ns("StatevsNat")), # State vs. National Median output
                style = "border: none; box-shadow: none; background: transparent;"
              )
            )
          ),
          
          # Bottom right
          grid_card(
            # Expand vertically
            style = "margin-top: -12px; margin-bottom: -12px;",
            area = "area2", # Specify right area
            card_body(
              value_box(
                title = "Facility Count",
                showcase = bsicons::bs_icon("shop",
                                            color = "#000000"), # Custom bootstrap icon
                value = uiOutput(ns("FacCount")), # Facility count output
                style = "border: none; box-shadow: none; background: transparent;"
              )
            )
          )
        )
      )
    )
  )
  
}

tab2Server <- function(id, StateData, metric_options, tab1vars, state_choices){
  
  moduleServer(
    
    id,
    
    function(input, output, session){
      
      # Facility rank bar chart
      output$FacilityRank <- renderGirafe({
        
        # Filter data to state selected
        TRIState <- StateData %>%
          filter(`8. ST` == input$State) %>%
          group_by(`4. FACILITY NAME`) %>%
          summarize(total = sum(.data[[input$BarMetric]]))%>% # Summarize to total metric by facility
          arrange(desc(total)) %>% # Arrange in descending order
          slice(1:10) # Select top 10
        
        # Check for logarithmic checkbox selection
        if (input$LogCheck){
          
          # Scale total (in tons) by natural log
          TRIState <- TRIState %>%
            mutate(yscaled = log(round(total / 2000, 2)))
          
          # Dynamic label for log axis
          ylabel <- paste0("Log(",
                           names(metric_options[which(metric_options == input$BarMetric)]),
                           ")")
          
        } else {
          
          # No log transformation
          TRIState <- TRIState %>%
            mutate(yscaled = round(total / 2000, 2))
          
          # Regular dynamic label, no log
          ylabel <- paste(names(metric_options[which(metric_options == input$BarMetric)]),
                          "(Tons)")
          
        }
        
        # Actual bar plot
        p1 <- ggplot(data = TRIState)+
          aes(x = reorder(`4. FACILITY NAME`, -total), # Plot in descending order
              y = yscaled,
              fill = reorder(`4. FACILITY NAME`, -total), # fill with specified order
              data_id = reorder(`4. FACILITY NAME`, -total), # ID follows order
              tooltip = paste0("Facility: ", `4. FACILITY NAME`, # tooltip for facility
                               "\n", names(metric_options[which(metric_options == input$BarMetric)]), # Dynamic label
                               ": ",format(round(total / 2000, 2), big.mark = ","), # format values with commas
                               " Tons"))+
          geom_bar_interactive(stat = "identity")+ #Interactive bar geom 
          scale_fill_paletteer_d("palettetown::barboach")+ # custom palette
          theme_minimal()+ #minimal theme
          scale_x_discrete(labels = seq(1:10))+ # Label x axis with rank
          labs(x = "Facility Rank", #Custom/dynamic labels
               y = ylabel,
               title = "Top 10 Facilities")+
          theme(axis.title = element_text(size = 18), # Adjust size of plot elements
                plot.title = element_text(size = 22),
                axis.text.x = element_text(size = 10),
                axis.text.y = element_text(size = 10),
                legend.position = "none") # No legend, use tooltips for info
        
        # generate interactive output
        girafe(ggobj = p1,
               width_svg = 8, #adjust aspect ratio
               height_svg = 5,
               options = list(opts_hover(css = "fill: green; stroke: black;"), #add hover options
                              opts_selection(css = "fill:#90EE90; stroke:black;", #add user selection of facilities
                                             type = "single"),
                              opts_zoom(min = 1, max = 20, duration = 300), #zoom option (log transform is better)
                              opts_tooltip(css = "background-color: black;color: white;padding:10px;border-radius:10px 10px 10px 10px;")))
                              #custom tooltip
      })
      
      # Pie chart
      output$IndustryPie <- renderGirafe({
        
        # Filter by state selection, group by industry
        ByIndustry <- StateData %>%
          filter(`8. ST` == input$State) %>%
          group_by(`23. INDUSTRY SECTOR`) %>%
          summarize(total = sum(.data[[input$BarMetric]])) %>% # total of metric
          mutate(prop = (total / sum(total)) * 100) %>% # proportion of state total
          arrange(desc(total)) %>% #arrange descending
          slice(1:10) # take top 10
        
        # Pie chart
        p2 <- ggplot(data = ByIndustry)+
          aes(x = "", #no x axis
              y = total,
              fill = reorder(`23. INDUSTRY SECTOR`, total), # fill ascending by total (counterclockwise)
              data_id = reorder(`23. INDUSTRY SECTOR`, total), # ID by order
              tooltip = paste0("Industry: ", `23. INDUSTRY SECTOR`, # tooltip with proportion by industry
                               "\nPercent: ", round(prop, 2), "%"))+
          geom_bar_interactive(stat = "identity",
                               width = 1, #Limit width
                               color = "white", # Color lines in between sections
                               linewidth = 0.5)+ # Width of lines between
          scale_fill_paletteer_d("palettetown::barboach")+ # Match palette to facility rank
          coord_polar(theta = "y")+ # polar coordinates where angle is y variable
          theme_minimal()+ # minimal theme
          theme(plot.title = element_text(size = 16, hjust = 0.5), # Adjust title, remove legend and axes
                legend.position = "none",
                axis.title = element_blank(),
                axis.text = element_blank())+
          labs(title = paste("Percent of", # Dynamic title
                             names(metric_options[which(metric_options == input$BarMetric)]),
                             "by Industry (Top 10)"),
               caption = "Note: Percent shown is the percent across all industries") # footnote
        
        # make pie chart interactive
        girafe(ggobj = p2,
               options = list(opts_hover(css = "fill: green; stroke: black;"), # hover settings
                              opts_selection(type = "none"))) # No selectionS
      })
      
      # State average output
      output$StateAvg <- renderUI({
        
        # filter to state selection, group by year
        StateData2 <- StateData %>%
          filter(`8. ST` == input$State) %>%
          group_by(`1. YEAR`)%>%
          summarize(total_release = sum(total_release)) # average over each year
        
        # Combine into HTML output (text output can also work)
        tags$span(paste(format(round(mean(StateData2$total_release, # round, convert to tons, add commas
                                          na.rm = TRUE) / 2000,
                                     0),
                               big.mark = ",")
                        , "Tons"))
        
      })
      
      # Reactive percent difference between state and national
      perc <- reactive({
        
        # filter by facility name across US
        Stat1 <- StateData %>%
          group_by(`4. FACILITY NAME`) %>%
          summarize(total_release = sum(total_release)) # total release by facility
        
        # National median release in tons
        nat_med <- median(Stat1$total_release) / 2000
        
        # Filter by state, group by facility
        Stat2 <- StateData %>%
          filter(`8. ST` == input$State) %>%
          group_by(`4. FACILITY NAME`)%>%
          summarize(total_release = sum(total_release)) # total release by facility
        
        # State median in tons
        state_med <- median(Stat2$total_release) / 2000
        
        # Percent difference between national and state, 2 decimal places
        round(((state_med / nat_med) - 1) * 100, 2)
        
      })
      
      # output for State vs. National Median
      output$StatevsNat <- renderUI({
        
        # Check if state is above or below national median using perc from above
        if (perc() <= 0){
          
          # Tag for below national
          status <- "Below"
          
        } else if (perc() > 0) {
          
          # Tag for above national
          status <- "Above"
          
        }
        
        # Combine dynamic tag and percent difference, return to UI
        tags$span(paste0(abs(perc()), "%", " ", status),
                  style = "font-size: 25px;") #Set font size
        
      })
      
      # Dynamic Icon for Median Comparison
      output$Icon <- renderUI({
        
        # Check if state median release is above or below national
        if (perc() <= 0){
          
          # Below = good, so assign green down arrow
          bsicons::bs_icon("chevron-double-down", color = "darkgreen")
          
        } else if (perc() > 0) {
          
          # above = bad, so assign red up arrow
          bsicons::bs_icon("chevron-double-up", color = "darkred")
          
        }
        
      })
      
      # Facility count output
      output$FacCount <- renderUI({
        
        # filter by state selection, group by facility and location
        StatFac <- StateData %>%
          filter(`8. ST` == input$State) %>%
          group_by(`4. FACILITY NAME`, `12. LATITUDE`, `13. LONGITUDE`)
        
        # number of rows = number of unique facility names
        num_fac <- nrow(StatFac)
        
        # return HTML as output (text would work too)
        tags$span(paste(format(num_fac, big.mark = ",")))
        
      })
      
      # Observe changes in tab1 state selection
      observeEvent(tab1vars$state(), {
        
        # Update tab2 state selection to match tab1
        updateSelectInput(session, "State", selected = state_choices[which(names(state_choices) == tab1vars$state())])
        
      })
      
      # return state selection and facility selection (if clicked) for tab3 use
      return(
        
        list(
          fac = reactive(input$FacilityRank_selected),
          state = reactive(input$State)
        )
      )
      
    }
    
  )
  
}