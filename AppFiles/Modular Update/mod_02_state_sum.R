
mod_02_state_sum_ui <- function(id, state_choices, metric_options) {
  
  ns <- NS(id)
  
  # Sidebar layout
  page_sidebar(
    
    # Define sidebar
    sidebar = sidebar(
      
      title = "Options",
      bg = "#EDEDED",
      
      # Input for state selection
      selectInput(
        ns("State"), 
        label = "State/Territory:", 
        choices = state_choices
      ),
      
      # Input for metric selection
      selectizeInput(
        inputId = ns("BarMetric"),
        label = "Plot Metric:",
        choices = metric_options
      ),
      
      # Data info at bottom
      div(
        style = paste0(
          "position: absolute; bottom: 10px; left: 10px; right: 10px; ",
          "font-size: 0.6em; color: grey;"
        ),
        HTML(paste0(
          "Using 2013-2023 release data, sourced from the <br>",
          "Environmental Protection Agency."
        ))
      )
    ),
    
    layout_column_wrap(
      width = 1,
      heights_equal = "row",
      fillable = TRUE, 
      fill = TRUE,
      
      layout_column_wrap(
        height = "80%",
        width = 1/2,
        # Top left card
        card(
          card_body(
            # Output facility rank plot
            girafeOutput(ns("FacilityRank")) %>%
              shinycssloaders::withSpinner(
                type = 4,
                size = 1,
                color = "#000000"
              ) %>%
              as_fill_carrier(),
            
            # Checkbox for logarithmic scale
            checkboxInput(
              inputId = ns("LogCheck"),
              label = "Log Scale",
              value = FALSE
            )
          )
        ),
        # Top right plot
        card(
          card_body(
            girafeOutput(ns("IndustryPie")) %>%
              shinycssloaders::withSpinner(
                type = 4,
                size = 1,
                color = "#000000"
              ) %>%
              as_fill_carrier()
          )
        )
      ),
      
      layout_column_wrap(
        height = "20%",
        width = 1/3,
        # Bottom left card
        value_box(
          title = "Average Yearly Release",
          showcase = bsicons::bs_icon(
            "bar-chart-line-fill",
            color = "#000000"
          ),
          value = uiOutput(ns("StateAvg"))
        ),
        
        # Middle card
        value_box(
          title = "State vs. National Median Facility Release",
          showcase = uiOutput(ns("Icon")),
          value = uiOutput(ns("StatevsNat"))
        ),
        
        # Bottom right
        value_box(
          title = "Facility Count",
          showcase = bsicons::bs_icon(
            "shop",
            color = "#000000"
          ),
          value = uiOutput(ns("FacCount"))
        )
      )
    )
  )
}

mod_02_state_sum_server <- function(id, StateData, metric_options, 
                                    tab1vars, state_choices, tab3vars) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
      # Facility rank bar chart
      output$FacilityRank <- renderGirafe({
        
        # Filter data to state selected
        TRIState <- StateData %>%
          filter(`8. ST` == input$State) %>%
          group_by(`4. FACILITY NAME`) %>%
          summarize(total = sum(.data[[input$BarMetric]])) %>%
          arrange(desc(total)) %>%
          slice(1:10)
        
        # Check for logarithmic checkbox selection
        if (input$LogCheck) {
          
          # Scale total (in tons) by natural log
          TRIState <- TRIState %>%
            mutate(yscaled = log(round(total / 2000, 2)))
          
          # Dynamic label for log axis
          ylabel <- paste0(
            "Log(",
            names(metric_options[which(metric_options == input$BarMetric)]),
            ")"
          )
          
        } else {
          
          # No log transformation
          TRIState <- TRIState %>%
            mutate(yscaled = round(total / 2000, 2))
          
          # Regular dynamic label, no log
          ylabel <- paste(
            names(metric_options[which(metric_options == input$BarMetric)]),
            "(Tons)"
          )
        }
        
        # Actual bar plot
        p1 <- ggplot(data = TRIState) +
          aes(
            x = reorder(`4. FACILITY NAME`, -total),
            y = yscaled,
            fill = reorder(`4. FACILITY NAME`, -total),
            data_id = reorder(`4. FACILITY NAME`, -total),
            tooltip = paste0(
              "Facility: ", 
              `4. FACILITY NAME`,
              "\n", 
              names(metric_options[which(metric_options == input$BarMetric)]),
              ": ",
              format(round(total / 2000, 2), big.mark = ","),
              " Tons"
            )
          ) +
          geom_bar_interactive(stat = "identity") +
          paletteer::scale_fill_paletteer_d("palettetown::barboach") +
          theme_minimal() +
          scale_x_discrete(labels = seq(1:10)) +
          labs(
            x = "Facility Rank",
            y = ylabel,
            title = "Top 10 Facilities"
          ) +
          theme(
            axis.title = element_text(size = 18),
            plot.title = element_text(size = 22),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            legend.position = "none"
          )
        
        # Generate interactive output
        girafe(
          ggobj = p1,
          width_svg = 8,
          height_svg = 5,
          options = list(
            opts_hover(css = "fill: green; stroke: black;"),
            opts_selection(
              css = "fill:#90EE90; stroke:black;",
              type = "single"
            ),
            opts_zoom(min = 1, max = 20, duration = 300),
            opts_tooltip(
              css = paste0(
                "background-color: black;color: white;padding:10px;",
                "border-radius:10px 10px 10px 10px;"
              )
            )
          )
        )
      })
      
      # Pie chart
      output$IndustryPie <- renderGirafe({
        
        # Filter by state selection, group by industry
        ByIndustry <- StateData %>%
          filter(`8. ST` == input$State) %>%
          group_by(`23. INDUSTRY SECTOR`) %>%
          summarize(total = sum(.data[[input$BarMetric]])) %>%
          mutate(prop = (total / sum(total)) * 100) %>%
          arrange(desc(total)) %>%
          slice(1:10)
        
        # Pie chart
        p2 <- ggplot(data = ByIndustry) +
          aes(
            x = "",
            y = total,
            fill = reorder(`23. INDUSTRY SECTOR`, total),
            data_id = reorder(`23. INDUSTRY SECTOR`, total),
            tooltip = paste0(
              "Industry: ", 
              `23. INDUSTRY SECTOR`,
              "\nPercent: ", 
              round(prop, 2), 
              "%"
            )
          ) +
          geom_bar_interactive(
            stat = "identity",
            width = 1,
            color = "white",
            linewidth = 0.5
          ) +
          paletteer::scale_fill_paletteer_d("palettetown::barboach") +
          coord_polar(theta = "y") +
          theme_minimal() +
          theme(
            plot.title = element_text(size = 16, hjust = 0.5),
            legend.position = "none",
            axis.title = element_blank(),
            axis.text = element_blank()
          ) +
          labs(
            title = paste(
              "Percent of",
              names(metric_options[which(metric_options == input$BarMetric)]),
              "by Industry (Top 10)"
            ),
            caption = "Note: Percent shown is the percent across all industries"
          )
        
        # Make pie chart interactive
        girafe(
          ggobj = p2,
          options = list(
            opts_hover(css = "fill: green; stroke: black;"),
            opts_selection(type = "none")
          )
        )
      })
      
      # State average output
      output$StateAvg <- renderUI({
        
        # Filter to state selection, group by year
        StateData2 <- StateData %>%
          filter(`8. ST` == input$State) %>%
          group_by(`1. YEAR`) %>%
          summarize(total_release = sum(total_release))
        
        # Combine into HTML output
        tags$span(
          paste(
            format(
              round(
                mean(
                  StateData2$total_release,
                  na.rm = TRUE
                ) / 2000,
                0
              ),
              big.mark = ","
            ),
            "Tons"
          )
        )
      })
      
      # Reactive percent difference between state and national
      perc <- reactive({
        
        # Filter by facility name across US
        Stat1 <- StateData %>%
          group_by(`4. FACILITY NAME`) %>%
          summarize(total_release = sum(total_release))
        
        # National median release in tons
        nat_med <- median(Stat1$total_release) / 2000
        
        # Filter by state, group by facility
        Stat2 <- StateData %>%
          filter(`8. ST` == input$State) %>%
          group_by(`4. FACILITY NAME`) %>%
          summarize(total_release = sum(total_release))
        
        # State median in tons
        state_med <- median(Stat2$total_release) / 2000
        
        # Percent difference between national and state
        round(((state_med / nat_med) - 1) * 100, 2)
      })
      
      # Output for State vs. National Median
      output$StatevsNat <- renderUI({
        
        # Check if state is above or below national median
        if (perc() <= 0) {
          status <- "Below"
        } else if (perc() > 0) {
          status <- "Above"
        }
        
        # Combine tag and percent difference
        tags$span(
          paste0(abs(perc()), "%", " ", status),
          style = "font-size: 25px;"
        )
      })
      
      # Dynamic Icon for Median Comparison
      output$Icon <- renderUI({
        
        # Check if state median release is above or below national
        if (perc() <= 0) {
          bsicons::bs_icon("chevron-double-down", color = "darkgreen")
        } else if (perc() > 0) {
          bsicons::bs_icon("chevron-double-up", color = "darkred")
        }
      })
      
      # Facility count output
      output$FacCount <- renderUI({
        
        # Filter by state selection, group by facility and location
        StatFac <- StateData %>%
          filter(`8. ST` == input$State) %>%
          group_by(
            `4. FACILITY NAME`, 
            `12. LATITUDE`, 
            `13. LONGITUDE`
          ) %>%
          summarize(num = n())
        
        # Number of rows = number of unique facility names
        num_fac <- nrow(StatFac)
        
        # Return HTML as output
        tags$span(paste(format(num_fac, big.mark = ",")))
      })
      
      # Observe changes in tab1 state selection
      observeEvent(tab1vars$state(), {
        
        # Update tab2 state selection to match tab1
        updateSelectInput(
          session, 
          "State", 
          selected = state_choices[which(
            names(state_choices) == tab1vars$state()
          )]
        )
      })
      
      # Sync with tab3 state selection
      observeEvent(tab3vars$state(), {
        updateSelectInput(
          session, 
          "State", 
          selected = tab3vars$state()
        )
      })
      
      # Return state selection and facility selection for tab3 use
      return(
        list(
          fac = reactive(input$FacilityRank_selected),
          state = reactive(input$State)
        )
      )
    }
  )
}