tab2UI <- function(id, state_choices, metric_options){
  
  ns <- NS(id)
  
  page_sidebar(
    
    sidebar = sidebar(
      
      title = "Options",
      
      selectInput(ns("State"), label = "State/Territory:", choices = state_choices),
      
      selectizeInput(
        inputId = ns("BarMetric"),
        label = "Plot Metric:",
        choices = metric_options
      ),
      
      div(
        style = "position: absolute; bottom: 10px; left: 10px; right: 10px; font-size: 0.9em; color: grey;",
        HTML("Using 2013-2023 release data, <br>sourced from the Environmental <br>Protection Agency.")
      )
      
    ),
            grid_container(
              layout = c(
                "area1 area2",
                "area3 area3"
              ),
              row_sizes = c(
                "1.47fr",
                "0.53fr"
              ),
              col_sizes = c(
                "1fr",
                "1fr"
              ),
            gap_size = "10px",
            grid_card(style = "overflow:visible;",
              area = "area1",
              card_body(style = "overflow:visible;",
                        girafeOutput(ns("FacilityRank")) %>%
                          withSpinner(type = 4,
                                      size = 1,
                                      color = "#000000") %>%
                          as_fill_carrier(),
                          
                            checkboxInput(
                              inputId = ns("LogCheck"),
                              label = "Log Scale",
                              value = FALSE
                            )
                          
                      )

            ),
            grid_card(
              area = "area2",
              card_body(girafeOutput(ns("IndustryPie")) %>%
                          withSpinner(type = 4,
                                      size = 1,
                                      color = "#000000") %>%
                          as_fill_carrier())
            ),
            
            grid_card(
              area = "area3",
                grid_container(
                  layout = c(
                    "area0 area1 area2"
                  ),
                  row_sizes = c(
                    "1.5fr"
                  ),
                  col_sizes = c(
                    "1fr",
                    "1fr",
                    "1fr"
                  ),
                  gap_size = "10px",
                  grid_card(
                    area = "area0",
                    card_body(
                      value_box(
                        title = "Average Yearly Release",
                        showcase = bsicons::bs_icon("bar-chart-line-fill",
                                        color = "#000000"),
                        value = uiOutput(ns("StateAvg")),
                        style = "border: none; box-shadow: none; background: transparent;"
                      )
                    )
                  ),
                  grid_card(
                    style = "min-height: 110px;",
                    area = "area1",
                    card_body(
                      value_box(
                        title = "State vs. National Median Facility Release",
                        showcase = uiOutput(ns("Icon"),
                                            fill = TRUE),
                        value = uiOutput(ns("StatevsNat")),
                        style = "border: none; box-shadow: none; background: transparent;"
                      )
                      )
                  ),
                  grid_card(
                    area = "area2",
                    card_body(
                      value_box(
                        title = "Facility Count",
                        showcase = bsicons::bs_icon("shop",
                                                    color = "#000000"),
                        value = uiOutput(ns("FacCount")),
                        style = "border: none; box-shadow: none; background: transparent;"
                      )
                    )
                  )
                )
              )
            )
  )
  
}

tab2Server <- function(id, StateData, metric_options, tab1vars){
  
  moduleServer(
    
    id,
    
    function(input, output, session){
      
      output$FacilityRank <- renderGirafe({
        
        TRIState <- StateData %>%
          filter(`8. ST` == input$State) %>%
          group_by(`4. FACILITY NAME`) %>%
          summarize(total = sum(.data[[input$BarMetric]]))%>%
          arrange(desc(total)) %>%
          slice(1:10)
        
        if (input$LogCheck){

          TRIState <- TRIState %>%
            mutate(yscaled = log(round(total / 2000, 2)))
          
          ylabel <- paste0("Log(",
                           names(metric_options[which(metric_options == input$BarMetric)]),
                           ")")
          
        } else {
          
          TRIState <- TRIState %>%
            mutate(yscaled = round(total / 2000, 2))
          
          ylabel <- paste(names(metric_options[which(metric_options == input$BarMetric)]),
                          "(Tons)")
          
        }
        
        p1 <- ggplot(data = TRIState)+
          aes(x = reorder(`4. FACILITY NAME`, -total), 
              y = yscaled,
              fill = reorder(`4. FACILITY NAME`, -total),
              data_id = reorder(`4. FACILITY NAME`, -total),
              tooltip = paste0("Facility: ", `4. FACILITY NAME`, 
                              "\n", names(metric_options[which(metric_options == input$BarMetric)]),
                              ": ",format(round(total / 2000, 2), big.mark = ","),
                              " Tons"))+
          geom_bar_interactive(stat = "identity")+
          scale_fill_paletteer_d("palettetown::barboach")+
          theme_minimal()+
          scale_x_discrete(labels = seq(1:10))+
          labs(x = "Facility Rank",
               y = ylabel,
               title = "Top 10 Facilities")+
          theme(axis.title = element_text(size = 18), # Adjust size of plot elements
                plot.title = element_text(size = 22),
                axis.text.x = element_text(size = 10),
                axis.text.y = element_text(size = 10),
                legend.position = "none")
        
        girafe(ggobj = p1,
               width_svg = 8,
               height_svg = 5,
               options = list(opts_hover(css = "fill: green; stroke: black;"),
                              opts_selection(css = "fill:#90EE90; stroke:black;",
                                             type = "single"),
                              opts_zoom(min = 1, max = 20, duration = 300),
                              opts_tooltip(css = "background-color: black;color: white;padding:10px;border-radius:10px 10px 10px 10px;")))
        
      })
      
      output$IndustryPie <- renderGirafe({
        
        ByIndustry <- StateData %>%
          filter(`8. ST` == input$State) %>%
          group_by(`23. INDUSTRY SECTOR`) %>%
          summarize(total = sum(.data[[input$BarMetric]])) %>%
          mutate(prop = (total / sum(total)) * 100) %>%
          arrange(desc(total)) %>%
          slice(1:10)
        
        p2 <- ggplot(data = ByIndustry)+
          aes(x = "",
              y = total,
              fill = reorder(`23. INDUSTRY SECTOR`, total),
              data_id = reorder(`23. INDUSTRY SECTOR`, total),
              tooltip = paste0("Industry: ", `23. INDUSTRY SECTOR`,
                              "\nPercent: ", round(prop, 2), "%"))+
          geom_bar_interactive(stat = "identity",
                               width = 1,
                               color = "white",
                               linewidth = 0.5)+
          scale_fill_paletteer_d("palettetown::barboach")+
          coord_polar(theta = "y")+
          theme_minimal()+
          theme(plot.title = element_text(size = 16, hjust = 0.5),
                legend.position = "none",
                axis.title = element_blank(),
                axis.text = element_blank())+
          labs(title = paste("Percent of",
                             names(metric_options[which(metric_options == input$BarMetric)]),
                             "by Industry (Top 10)"),
               caption = "Note: Percent shown is the percent across all industries")
        
        girafe(ggobj = p2,
               options = list(opts_hover(css = "fill: green; stroke: black;"),
                              opts_selection(type = "none")))
      })
      
      output$StateAvg <- renderUI({
        
        StateData2 <- StateData %>%
          filter(`8. ST` == input$State) %>%
          group_by(`1. YEAR`)%>%
          summarize(total_release = sum(total_release))
        
        tags$span(paste(format(round(mean(StateData2$total_release,
                                          na.rm = TRUE) / 2000,
                                     0),
                               big.mark = ",")
                        , "Tons"))
        
      })
      
      perc <- reactive({
        
        Stat1 <- StateData %>%
          group_by(`4. FACILITY NAME`) %>%
          summarize(total_release = sum(total_release))
        
        nat_med <- median(Stat1$total_release) / 2000
        
        Stat2 <- StateData %>%
          filter(`8. ST` == input$State) %>%
          group_by(`4. FACILITY NAME`)%>%
          summarize(total_release = sum(total_release))
        
        state_med <- median(Stat2$total_release) / 2000
        
        round(((state_med / nat_med) - 1) * 100, 2)
        
      })
      
      output$StatevsNat <- renderUI({
        
        if (perc() <= 0){
          
          status <- "Below"
          
        } else if (perc() > 0) {
          
          status <- "Above"
          
        }
        
        tags$span(paste0(abs(perc()), "%", " ", status),
                  style = "font-size: 25px;")
        
      })
      
      output$Icon <- renderUI({
        
        if (perc() <= 0){
          
          bsicons::bs_icon("chevron-double-down", color = "darkgreen")
          
        } else if (perc() > 0) {
          
          bsicons::bs_icon("chevron-double-up", color = "darkred")
          
        }
        
      })
      
      output$FacCount <- renderUI({
        
        StatFac <- StateData %>%
          filter(`8. ST` == input$State) %>%
          group_by(`4. FACILITY NAME`)
        
        num_fac <- nrow(StatFac)
        
        tags$span(paste(format(num_fac, big.mark = ",")))
        
      })
      
      return(
        
        list(
          fac = reactive(input$FacilityRank_selected),
          state = reactive(input$State)
        )
        )
      
    }
    
  )
  
}