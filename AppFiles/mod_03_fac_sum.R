
mod_03_fac_sum_ui <- function(id, state_choices, metric_options) {
  
  ns <- NS(id)
  
  page_sidebar(  # Sidebar unique to this tab
    
    sidebar = sidebar(  # Define sidebar
      title = "Options",
      bg = "#EDEDED",
      
      # Custom class for smaller numeric input
      tags$head(
        tags$style(HTML("
        .custom-numeric-input .form-control {
        padding: 0px !important;
        }"
        ))
      ),
      
      # State selection
      selectInput(
        ns("State"),
        label = "State",
        choices = state_choices,
        selected = "AK"
      ),
      
      # Facility choice (updated in server)
      selectizeInput(
        ns("Facility"),
        label = "Facility",
        choices = NULL,  # No initial choices, updated by state
        multiple = FALSE
      ),
      
      # Timeline metric choice
      selectInput(
        ns("TimeMetric"),
        label = "Timeline Metric",
        choices = metric_options
      ),
      
      # Address search section
      tags$p("Search Address"),
      
      # Side-by-side inputs
      fluidRow(
        # Left column - location number
        column(
          6,
          numericInput(
            inputId = ns("Location"), 
            label = "Location #", 
            value = 1,  # Default value
            min = 1,  # Minimum 1
            step = 1
          ) |> tagAppendAttributes(class = "custom-numeric-input")
        ),
        
        # Right column - search button
        column(
          6,
          actionButton(
            ns("SearchButton"),
            label = "Search"
          )
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
    
    layout_columns(
      width = 1,
      col_widths = c(4, 8),
      
      layout_column_wrap(
        width = 1,
        # Industry value box
        value_box(
          title = "Primary Industry",
          value = uiOutput(ns("Industry")),
          showcase = bsicons::bs_icon(
            "shop",
            color = "#000000"
          )
        ),
        
        # Release method value box
        value_box(
          title = "Primary Release Method",
          value = uiOutput(ns("Method")),
          showcase = bsicons::bs_icon(
            "globe-americas",
            color = "#000000"
          )
        ),
        
        # Average release value box
        value_box(
          title = "Average Yearly Release",
          value = uiOutput(ns("Average")),
          showcase = bsicons::bs_icon(
            "bar-chart-line-fill",
            color = "#000000"
          )
        ),
        
        # Chemical checklist card
        card(
          card_header(
            "Chemical Checklist",
            style = paste(
              "color: white; background-color: #434854;",
              "margin-bottom: 10px"
            )
          ),
          card_body(
            style = paste(
              "overflow: hidden; padding-left: 20px;",
              "padding-top: 0px; padding-bottom: 0px"
            ),
            
            tags$div(
              style = "display: flex; align-items: center;",
              bsicons::bs_icon(
                "radioactive", 
                size = 70,
                color = "#000000"
              ),
              uiOutput(ns("checklist"))
            )
          )
        )
      ),
      
      # Timeline card
      card(
        card_body(
          girafeOutput(
            ns("timeline"),
            width = "100%"
          ) %>% 
            shinycssloaders::withSpinner(
              type = 4,
              color = "#000000"
            ) %>% 
            as_fill_carrier()
        ),
        full_screen = TRUE
      )
    )
  )
}

mod_03_fac_sum_server <- function(id, metric_options, axis_options, 
                                  label_options, method_options, 
                                  Decade_Data, tab2vars, tab1vars) {
  
  moduleServer(
    id, 
    function(input, output, session) {
      
      # Update facility choices when state changes
      observe({
        # Filter by selected state
        TRIDecade1 <- Decade_Data %>%
          filter(`8. ST` == input$State)
        
        # Get unique facilities
        facility_choices1 <- unique(TRIDecade1$`4. FACILITY NAME`)
        
        # Initialize selectize
        updateSelectizeInput(
          session,
          "Facility",
          choices = NULL,
          server = TRUE
        )
        
        # Update facility choices
        updateSelectizeInput(
          session,
          "Facility",
          choices = facility_choices1,
          server = TRUE,
          options = list(maxOptions = 10000)
        )
      })
      
      # Filter data for selected facility
      TRIDecade2 <- reactive({
        # Require valid facility selection
        req(input$Facility %in% Decade_Data$`4. FACILITY NAME`)
        
        Decade_Data %>%
          filter(
            `8. ST` == input$State,
            `4. FACILITY NAME` == input$Facility
          ) %>%
          mutate(`1. YEAR` = as.character(`1. YEAR`)) %>%
          group_by(`12. LATITUDE`, `13. LONGITUDE`, `23. INDUSTRY SECTOR`) %>%
          mutate(facility_label = as.factor(cur_group_id())) %>%
          ungroup()
      })
      
      # Generate timeline
      output$timeline <- renderGirafe({
        # Require valid facility
        req(!is.null(input$Facility))
        req(!is.na(input$Facility))
        req(nzchar(input$Facility))
        
        # Dynamic text options
        title_text <- names(metric_options[which(metric_options == input$TimeMetric)])
        axis_text <- names(axis_options[which(axis_options == input$TimeMetric)])
        label_text <- names(label_options[which(label_options == input$TimeMetric)])
        
        # Create timeline plot
        p3 <- ggplot(data = TRIDecade2()) +
          aes(
            x = `1. YEAR`, 
            y = .data[[input$TimeMetric]]
          ) + 
          geom_line(
            mapping = aes(
              group = facility_label,
              color = facility_label
            ),
            linewidth = .75
          ) + 
          geom_point_interactive(
            mapping = aes(
              data_id = interaction(
                facility_label,
                `1. YEAR`
              ),
              tooltip = paste(
                "Year: ", `1. YEAR`,
                "\nLocation #: ", facility_label,
                "\n", title_text, ": ", 
                comma(round(.data[[input$TimeMetric]], 0)),
                label_text
              )
            ),
            size = 2
          ) + 
          scale_y_continuous(
            label = comma,
            limits = c(0, NA)
          ) + 
          theme_minimal() + 
          labs(
            title = paste(title_text),
            x = "Year",
            y = axis_text,
            color = "Location #"
          ) +
          theme(
            axis.title = element_text(size = 10),
            plot.title = element_text(size = 12, hjust = 0.5),
            axis.text.x = element_text(size = 8),
            axis.text.y = element_text(size = 8),
            legend.position = "top",
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 8),
            plot.margin = margin(t = 5, r = 0, b = 2, l = 2, unit = "pt")
          )
        
        # Make interactive
        girafe(
          ggobj = p3,
          height_svg = 5,
          width_svg = 7,
          options = list(
            opts_hover(
              css = "fill:green;stroke:black;r: 5px; transition: all 0.1s ease;"
            ),
            opts_selection(type = "none"),
            opts_sizing(rescale = TRUE),
            opts_tooltip(
              css = paste(
                "background-color: black;color: white;padding:10px;",
                "border-radius:10px 10px 10px 10px;"
              )
            )
          )
        )
      })
      
      # Show primary industry
      output$Industry <- renderUI({
        req(!is.null(input$Facility))
        req(!is.na(input$Facility))
        req(nzchar(input$Facility))
        
        Industry1 <- Decade_Data %>%
          filter(
            `8. ST` == input$State,
            `4. FACILITY NAME` == input$Facility
          ) %>%
          group_by(`23. INDUSTRY SECTOR`) %>%
          summarize(total_prod = sum(prod_waste)) %>%
          arrange(`23. INDUSTRY SECTOR`, desc(total_prod)) %>%
          slice(1)
        
        tags$span(
          paste(Industry1$`23. INDUSTRY SECTOR`),
          style = "font-size: 20px;"
        )
      })
      
      # Show top release method
      output$Method <- renderUI({
        req(!is.null(input$Facility), !is.na(input$Facility), nzchar(input$Facility))
        
        FindMethod <- Decade_Data %>%
          filter(
            `8. ST` == input$State,
            `4. FACILITY NAME` == input$Facility
          )
        
        top_method <- colnames(FindMethod[, 13:18])[
          which.max(colSums(FindMethod[,13:18]))
        ]
        
        tags$span(
          names(method_options[which(method_options == top_method)]),
          style = "font-size: 20px;"
        )
      })
      
      # Show average yearly releases
      output$Average <- renderUI({
        req(!is.null(input$Facility))
        req(!is.na(input$Facility))
        req(nzchar(input$Facility))
        
        Yearly <- Decade_Data %>%
          filter(
            `8. ST` == input$State,
            `4. FACILITY NAME` == input$Facility
          ) %>%
          group_by(`1. YEAR`, `4. FACILITY NAME`) %>%
          summarize(total_release = sum(total_release))
        
        yearly_avg <- comma(round(mean(Yearly$total_release), 0))
        
        tags$span(
          paste(yearly_avg, "lbs"),
          style = "font-size: 20px;"
        )
      })
      
      # Generate chemical checklist
      output$checklist <- renderUI({
        req(!is.null(input$Facility), !is.na(input$Facility), nzchar(input$Facility))
        
        CheckChem <- Decade_Data %>%
          filter(
            `8. ST` == input$State,
            `4. FACILITY NAME` == input$Facility
          )
        
        # Check for chemical flags
        checklist_items <- list(
          "CAA Hazardous Chemical" = (sum(CheckChem$CAAC_count) != 0),
          "Carcinogen (Cancer-Linked)" = (sum(CheckChem$carcin_count) != 0),
          "PBT Chemical" = (sum(CheckChem$pbt_count) != 0),
          "PFAS Chemical" = (sum(CheckChem$pfas_count) != 0)
        )
        
        tags$ul(
          style = "list-style-type: none; padding-left: 20px; margin: 0;",
          lapply(names(checklist_items), function(item) {
            if (checklist_items[[item]]) {
              tags$li(
                tags$span(style = "color: green;", "✔ "),
                item,
                style = " font-size: 14px;"
              )
            } else {
              tags$li(
                tags$span(style = "color: red;", "✘ "),
                item,
                style = " font-size: 14px;"
              )
            }
          })
        )
      })
      
      # Update location input when facility changes
      observeEvent(input$Facility, {
        updateNumericInput(
          session, 
          "Location",
          value = 1,
          max = max(unique(as.numeric(TRIDecade2()$facility_label)))
        )
      })
      
      # Sync with tab2 state selection
      observeEvent(tab2vars$state(), {
        updateSelectInput(
          session, 
          "State", 
          selected = tab2vars$state()
        )
      })
      
      # Sync with tab2 facility selection
      observeEvent(tab2vars$fac(), {
        updateSelectizeInput(
          session, 
          "Facility",
          selected = tab2vars$fac()
        )
      })
      
      # Initialize empty address/location
      add <- reactiveVal({
        NULL
      })
      
      # Handle search button click
      observeEvent(input$SearchButton, {
        # Get coordinates for selected location
        coord_data <- TRIDecade2() %>%
          filter(facility_label == input$Location) %>% 
          group_by(`12. LATITUDE`, `13. LONGITUDE`, `23. INDUSTRY SECTOR`) %>%
          summarize()
        
        # Create location table
        table1 <- tibble(
          tidygeocoder::reverse_geo(
            long = coord_data$`13. LONGITUDE`,
            lat = coord_data$`12. LATITUDE`
          ),
          ind = coord_data$`23. INDUSTRY SECTOR`
        )
        
        add(table1)
      })
      
      # Tab switch on search
      tab <- eventReactive(input$SearchButton, {
        "addsch"
      })
      
      # Return values
      return(list(
        add = add,
        tab = tab,
        state = reactive(input$State)
      ))
    }
  )
}