tab2UI <- function(id, state_choices, metric_options){
  
  ns <- NS(id)
  
  page_sidebar(
    
    sidebar = sidebar(
      
      title = "Options",
      
      selectInput(ns("State"), label = "State/Territory:", choices = state_choices),
      
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
                        
                        fluidRow(
                          column(6,
                            selectizeInput(
                              inputId = ns("BarMetric"),
                              label = "Select Metric",
                              choices = metric_options,
                              width = "200px"
                            )
                          ),
                          
                          column(6,
                            checkboxInput(
                              inputId = ns("LogCheck"),
                              label = "Log Scale",
                              value = FALSE
                            )
                          )
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
                    "1fr"
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
                        style = "border: none; box-shadow: none; background: transparent;",
                        fill = TRUE
                      )
                    )
                  ),
                  grid_card(
                    area = "area1",
                    card_body(
                      value_box(
                        title = "State vs. National Median Facility Release",
                        showcase = uiOutput(ns("Icon"),
                                            fill = TRUE),
                        value = uiOutput(ns("StatevsNat")),
                        style = "border: none; box-shadow: none; background: transparent;",
                        fill = TRUE
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
                        style = "border: none; box-shadow: none; background: transparent;",
                        fill = TRUE
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
          summarize(total_release = sum(total_release),
                    recycled = sum(recycled),
                    treated = sum(treated),
                    prod_waste = sum(prod_waste))%>%
          arrange(desc(.data[[input$BarMetric]])) %>%
          slice(1:10)
        
        if (input$LogCheck){

          TRIState <- TRIState %>%
            mutate(yscaled = log(round(.data[[input$BarMetric]] / 2000, 2)))
          
          ylabel <- paste0("Log(",
                           names(metric_options[which(metric_options == input$BarMetric)]),
                           ")")
          
        } else {
          
          TRIState <- TRIState %>%
            mutate(yscaled = round(.data[[input$BarMetric]] / 2000, 2))
          
          ylabel <- names(metric_options[which(metric_options == input$BarMetric)])
          
        }
        
        p1 <- ggplot(data = TRIState)+
          aes(x = reorder(`4. FACILITY NAME`, -.data[[input$BarMetric]]), 
              y = yscaled,
              fill = reorder(`4. FACILITY NAME`, -.data[[input$BarMetric]]),
              data_id = reorder(`4. FACILITY NAME`, -.data[[input$BarMetric]]),
              tooltip = paste("Facility:", `4. FACILITY NAME`, 
                              "\nTotal Release:", format(round(.data[[input$BarMetric]] / 2000, 2), big.mark = ","), "Tons"))+
          geom_bar_interactive(stat = "identity")+
          scale_fill_paletteer_d("palettetown::barboach")+
          theme_minimal()+
          scale_x_discrete(labels = seq(1:10))+
          labs(x = "Facility Rank",
               y = ylabel,
               title = "Top 10 Facilities by Total Chemical Release (Tons)")+
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
          summarize(total_release = sum(total_release),
                    recycled = sum(recycled),
                    treated = sum(treated),
                    prod_waste = sum(prod_waste)) %>%
          mutate(prop = (total_release / sum(total_release)) * 100) %>%
          arrange(desc(total_release)) %>%
          slice(1:10)
        
        p2 <- ggplot(data = ByIndustry)+
          aes(x = "",
              y = total_release,
              fill = reorder(`23. INDUSTRY SECTOR`, total_release),
              data_id = reorder(`23. INDUSTRY SECTOR`, total_release),
              tooltip = paste("Industry:", `23. INDUSTRY SECTOR`,
                              "\n Percent of Release:", round(prop, 2), "%"))+
          geom_bar_interactive(stat = "identity",
                               width = 1,
                               color = "white",
                               linewidth = 0.5)+
          scale_fill_paletteer_d("palettetown::barboach")+
          coord_polar(theta = "y")+
          theme_minimal()+
          theme(plot.title = element_text(size = 18),
                legend.position = "none",
                axis.title = element_blank(),
                axis.text = element_blank())+
          labs(title = paste("Proportion of Release By Industry (Top 10)"),
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