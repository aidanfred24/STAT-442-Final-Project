
tab1UI <- function(id, state_choices, metric_options){
  
  ns <- NS(id)
  
  page_sidebar(           # Allows for sidebar unique to this tab
    sidebar = sidebar(    # Define sidebar
      title = "Options",
      
      # Input for state selection or national
      selectInput(ns("State"), 
                  label = "Select State(s):",
                  choices = c("United States Mainland",
                              "United States (Mainland & Alaska)", 
                              state_choices),
                  selected = "United States (Mainland)"),  #set default value
      
      # Metric selection
      selectInput(ns("Stat"),
                  label = "Select Metric",
                  choices = metric_options),
      
      div(
        style = "position: absolute; bottom: 10px; left: 10px; right: 10px; font-size: 0.6em; color: grey;",
        HTML("Using 2023 release data, sourced from the <br>Environmental Protection Agency.")
      )
    ),
    
    # Define info card for graph in the main panel
    card(
      full_screen = TRUE,       # Optional: Allow full-screen toggle
      card_body(
        # render interactive map with loading symbol
        withSpinner(girafeOutput(outputId = ns("StateMaps"), 
                                 height = "80vh"),  # small height adj.
                    type = 4,             #spinner type, color, size
                    color = "#000000",
                    size = 3)
      )
    )
  )
  
}

tab1Server <- function(id, metric_options, tooltips, captions,
                       State_Data, County_Data){
  
  moduleServer(
    
    id,
    
    function(input, output, session){
      
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
          TRI23_snew <- State_Data %>%
            filter(!STUSPS %in% c("AK", "AS", "GU", "HI", "MP", "PR", "VI"))
          
          #Mainland plot
          p2 <- ggplot(data = TRI23_snew)+
            geom_sf_interactive(mapping = aes(fill = .data[[input$Stat]],    # Fill by input
                                              data_id = NAME,                #Id for interactive layer in ggiraph
                                              tooltip = paste0("State: ", NAME,       # Text Displayed on hover
                                                               "\n", tooltip_text,    # Use our dynamic text
                                                               ": ", format(.data[[input$Stat]], big.mark = ","))),  # Format numbers with commas
                                color = "black")+  # Color of state outline
            theme_void()+  #remove all unnecessary plot elements
            get_scale_fill(State_Data, input$Stat)+  # Use or custom function for color scale
            labs(title = paste(title_text, " by State in ", input$State),  # Dynamic title
                 caption = caption_text,  # Dynamic caption
                 fill= tooltip_text)+     # Dynamic legend title
            theme(plot.caption = element_text(color = "#555666"),
                  plot.margin = margin(t = 0, r = 15, b = 0, l = 0, unit = "pt"))  # Change caption color
          
          # Make the plot interactive
          girafe(ggobj = p2,
                 options = list(opts_hover(css = "fill:green;stroke:black; transition: 0.2s ease-in-out;"),  # Add css hover options
                                opts_zoom(min = 1, max = 20, duration = 300), # Allow user to zoom
                                opts_selection(type = "none"),
                                opts_tooltip(css = "background-color: black;color: white;padding:10px;border-radius:10px 10px 10px 10px;")),# Disable lasso option (useless)
                 width_svg = 11, # Change plot aspect ratio
                 height_svg = 6)
          
          # Case to include Alaska
        } else if (input$State == "United States (Mainland & Alaska)"){
          
          # Exclude territories (and Hawaii)
          TRI23_snew <- State_Data %>%
            filter(!STUSPS %in% c("AS", "GU", "HI", "MP", "PR", "VI"))
          
          # Alaska plot
          p2 <- ggplot(data = TRI23_snew)+
            geom_sf_interactive(mapping = aes(fill = .data[[input$Stat]], # fill by metric
                                              data_id = NAME,             # ID for interactivity
                                              tooltip = paste0("State: ", NAME, # Text on hover
                                                               "\n", tooltip_text,
                                                               ": ", format(.data[[input$Stat]], big.mark = ","))),
                                color = "black")+
            coord_sf(xlim = c(-180, -60))+  # Edit map bounds to better fit Alaska
            theme_void()+
            get_scale_fill(State_Data, input$Stat)+ # Use custom color scale function
            labs(title = paste(title_text, " by State in ", input$State),  # Dynamic labels
                 caption = caption_text,
                 fill = tooltip_text)
          
          # Interactive conversion
          girafe(ggobj = p2,
                 options = list(opts_hover(css = "fill:green;stroke:black; transition: 0.2s ease-in-out;"), # Add hover, zoom, remove selection
                                opts_zoom(min = 1, max = 20, duration = 300),
                                opts_selection(type = "single", css = "fill:#90EE90; stroke:black;"),
                                opts_tooltip(css = "background-color: black;color: white;padding:10px;border-radius:10px 10px 10px 10px;")),
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
          TRI23_cnew <- County_Data %>%
            filter(STATENA == input$State) %>%
            mutate(NAME = gsub("'", "", NAME))
          
          # Plot of counties in selected state
          p1 <- ggplot(data = TRI23_cnew)+
            geom_sf_interactive(mapping = aes(fill = .data[[input$Stat]], 
                                              data_id = NAME,
                                              tooltip = paste0("State: ", input$State,  # State name
                                                               "\nCounty: ", str_to_title(NAME), # County name
                                                               "\n", tooltip_text,
                                                               ": ", format(.data[[input$Stat]], big.mark = ","))),
                                color = "black")+
            coord_sf(xlim = xlim1,   # Custom bounds from above (AK, AS)
                     ylim = ylim1)+
            theme_void()+
            get_scale_fill(State_Data, input$Stat)+  # Dynamic color scale
            labs(title = paste(title_text, " by County in ", input$State), # Dynamic labels
                 caption = caption_text,
                 fill = tooltip_text)
          
          # Interactivity
          girafe(ggobj = p1,
                 options = list(opts_hover(css = "fill:green;stroke:black; transition: 0.2s ease-in-out;"), # Add hover, zoom, remove selection
                                opts_zoom(min = 1, max = 20, duration = 300),
                                opts_selection(type = "none"),
                                opts_tooltip(css = "background-color: black;color: white;padding:10px;border-radius:10px 10px 10px 10px;")),
                 width_svg = 10,  #adjust width and height of plot
                 height_svg = 6)
          
        }
      })
     
      return(list(
        
        state <- reactive(input$StateMaps_selected)
        
      ))
         
    }
    
  )
  
}