
mod_01_statewise_ui <- function(id, state_choices, metric_options) {
  
  ns <- NS(id)
  
  page_sidebar(
    sidebar = sidebar(
      title = "Options",
      
      # Input for state selection
      selectInput(
        ns("State"), 
        label = "Select State(s):",
        choices = c(
          "United States Mainland",
          "United States (Mainland & Alaska)", 
          state_choices
        ),
        selected = "United States (Mainland)"
      ),
      
      # Metric selection
      selectInput(
        ns("Stat"),
        label = "Select Metric",
        choices = metric_options
      ),
      
      # Data source info
      div(
        style = paste0(
          "position: absolute; bottom: 10px; left: 10px; right: 10px; ",
          "font-size: 0.6em; color: grey;"
        ),
        HTML(paste0(
          "Using 2023 release data, sourced from the <br>",
          "Environmental Protection Agency."
        ))
      )
    ),
    
    # Map visualization card
    card(
      full_screen = TRUE,
      card_body(
        shinycssloaders::withSpinner(
          girafeOutput(
            outputId = ns("StateMaps"),
            height = "100%"
          ),
          type = 4,
          color = "#000000",
          size = 3
        ) %>% as_fill_carrier()
      )
    )
  )
}

mod_01_statewise_server <- function(id, metric_options, tooltips, 
                                    captions, State_Data, County_Data) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      output$StateMaps <- renderGirafe({
        
        # Dynamic text options for plots
        title_text <- names(metric_options[which(metric_options == input$Stat)])
        tooltip_text <- names(tooltips[which(tooltips == input$Stat)])
        caption_text <- names(captions[which(captions == input$Stat)])
        
        # Function for dynamic color scale
        get_scale_fill <- function(data, variable) {
          if (is.numeric(data[[variable]])) {
            # Continuous scale
            scale_fill_viridis_c(
              option = "C",
              name = tooltip_text,
              label = function(x) {
                format(x, big.mark = ",", scientific = FALSE)
              }
            )
          } else {
            # Discrete scale
            scale_fill_discrete(name = tooltip_text)
          }
        }
        
        # Handle different map view selections
        if (input$State == "United States Mainland") {
          
          # Filter out non-mainland states/territories
          TRI23_snew <- State_Data %>%
            filter(!STUSPS %in% c("AK", "AS", "GU", "HI", "MP", "PR", "VI"))
          
          # Mainland plot
          p2 <- ggplot(data = TRI23_snew) +
            geom_sf_interactive(
              mapping = aes(
                fill = .data[[input$Stat]],
                data_id = NAME,
                tooltip = paste0(
                  "State: ", 
                  NAME,
                  "\n", 
                  tooltip_text,
                  ": ", 
                  format(.data[[input$Stat]], big.mark = ",")
                )
              ),
              color = "black"
            ) +
            theme_void() +
            get_scale_fill(State_Data, input$Stat) +
            labs(
              title = paste(title_text, " by State in ", input$State),
              caption = caption_text,
              fill = tooltip_text
            ) +
            theme(
              plot.caption = element_text(color = "#555666"),
              plot.margin = margin(t = 0, r = 15, b = 0, l = 0, unit = "pt")
            )
          
          # Interactive map settings
          girafe(
            ggobj = p2,
            options = list(
              opts_hover(
                css = "fill:green;stroke:black; transition: 0.2s ease-in-out;"
              ),
              opts_zoom(min = 1, max = 20, duration = 300),
              opts_selection(type = "single", css = "fill:#90EE90; stroke:black;"),
              opts_tooltip(
                css = paste0(
                  "background-color: black;color: white;padding:10px;",
                  "border-radius:10px 10px 10px 10px;"
                )
              )
            ),
            width_svg = 11,
            height_svg = 5
          )
          
        } else if (input$State == "United States (Mainland & Alaska)") {
          
          # Exclude territories (and Hawaii)
          TRI23_snew <- State_Data %>%
            filter(!STUSPS %in% c("AS", "GU", "HI", "MP", "PR", "VI"))
          
          # Alaska included plot
          p2 <- ggplot(data = TRI23_snew) +
            geom_sf_interactive(
              mapping = aes(
                fill = .data[[input$Stat]],
                data_id = NAME,
                tooltip = paste0(
                  "State: ", 
                  NAME,
                  "\n", 
                  tooltip_text,
                  ": ", 
                  format(.data[[input$Stat]], big.mark = ",")
                )
              ),
              color = "black"
            ) +
            coord_sf(xlim = c(-180, -60)) +
            theme_void() +
            get_scale_fill(State_Data, input$Stat) +
            labs(
              title = paste(title_text, " by State in ", input$State),
              caption = caption_text,
              fill = tooltip_text
            )
          
          # Interactive settings
          girafe(
            ggobj = p2,
            options = list(
              opts_hover(
                css = "fill:green;stroke:black; transition: 0.2s ease-in-out;"
              ),
              opts_zoom(min = 1, max = 20, duration = 300),
              opts_selection(
                type = "single", 
                css = "fill:#90EE90; stroke:black;"
              ),
              opts_tooltip(
                css = paste0(
                  "background-color: black;color: white;padding:10px;",
                  "border-radius:10px 10px 10px 10px;"
                )
              )
            ),
            width_svg = 11,
            height_svg = 5
          )
          
        } else {
          
          # Custom bounds for special cases
          if (input$State == "Alaska") {
            xlim1 <- c(-180, -120)
            ylim1 <- c(NA, NA)
          } else if (input$State == "American Samoa") {
            xlim1 <- c(NA, NA)
            ylim1 <- c(-15, -13.8)
          } else {
            xlim1 <- c(NA, NA)
            ylim1 <- c(NA, NA)
          }
          
          # Filter for selected state counties
          TRI23_cnew <- County_Data %>%
            filter(STATENA == input$State) %>%
            mutate(NAME = gsub("'", "", NAME))
          
          # County-level plot
          p1 <- ggplot(data = TRI23_cnew) +
            geom_sf_interactive(
              mapping = aes(
                fill = .data[[input$Stat]],
                data_id = NAME,
                tooltip = paste0(
                  "State: ", 
                  input$State,
                  "\nCounty: ", 
                  str_to_title(NAME),
                  "\n", 
                  tooltip_text,
                  ": ", 
                  format(.data[[input$Stat]], big.mark = ",")
                )
              ),
              color = "black"
            ) +
            coord_sf(xlim = xlim1, ylim = ylim1) +
            theme_void() +
            get_scale_fill(State_Data, input$Stat) +
            labs(
              title = paste(title_text, " by County in ", input$State),
              caption = caption_text,
              fill = tooltip_text
            )
          
          # Interactive settings
          girafe(
            ggobj = p1,
            options = list(
              opts_hover(
                css = "fill:green;stroke:black; transition: 0.2s ease-in-out;"
              ),
              opts_zoom(min = 1, max = 20, duration = 300),
              opts_selection(type = "none"),
              opts_tooltip(
                css = paste0(
                  "background-color: black;color: white;padding:10px;",
                  "border-radius:10px 10px 10px 10px;"
                )
              )
            ),
            width_svg = 11,
            height_svg = 5.125
          )
        }
      })
      
      # Reactive value for state selection
      state <- reactive({
        if (
          input$State == "United States (Mainland & Alaska)" | 
          input$State == "United States Mainland"
        ) {
          input$StateMaps_selected
        } else {
          input$State
        }
      })
      
      # Return state selection
      return(list(state = state))
    }
  )
}