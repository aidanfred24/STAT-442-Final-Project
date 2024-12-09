library(shiny)
library(ggplot2)
library(bslib)
library(gridlayout)
library(ggiraph)
library(dplyr)
library(sf)
library(stringr)
library(scales)

TRI23_counties <- read_sf("TRI23_counties.shp")
TRI23_states <- read_sf("TRI23_states.shp")

TRI23_counties$prd_wst <- round(TRI23_counties$prd_wst, 0)
TRI23_states$prd_wst <- round(TRI23_states$prd_wst, 0)

TRI23_counties$ttl_rls <- round(TRI23_counties$ttl_rls, 0)
TRI23_states$ttl_rls <- round(TRI23_states$ttl_rls, 0)

state_choices <- unique(TRI23_states$NAME)

metric_options <- setNames(colnames(st_drop_geometry(TRI23_states[,c(18, 19, 21, 22, 31:46)])),
                           c("Total Production Waste", 
                             "Total Chemical Release",
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

tooltips <- setNames(metric_options, 
                     c("Pounds", 
                     "Pounds",
                     "Chemical",
                     "Chemical",
                     rep("% of Facilities", 5),
                     rep("% of Release", 8),
                     rep("% of Waste", 3)))

ui <- page_navbar(
  title = "Toxic Release Inventory ",
  selected = "Statewise",
  collapsible = TRUE,
  theme = bslib::bs_theme(),
  
  nav_panel(
    title = "Statewise",
    
    page_sidebar(
      sidebar = sidebar(
        title = "Sidebar Title",
        
        selectInput("State", 
                    label = "Select State(s):",
                    choices = c("United States (Mainland)",
                                "United States (Mainland & Alaska)", 
                                state_choices),
                    selected = "United States (Mainland)"),
        
        selectInput("Stat",
                    label = "Select Metric",
                    choices = metric_options)
        
        ),
      
      card(
        full_screen = TRUE,       # Optional: Allow full-screen toggle
        card_header = "State Map", # Optional: Add a header to the card
        card_body(
          girafeOutput(outputId = "StateMaps", height = "80vh")
        ),
        card_footer = NULL          # Optional: Add a footer (e.g., notes or links)
      )
    )
  )
#   ,
#   nav_panel(
#     title = "Facility Profile",
#     grid_container(
#       layout = c(
#         "area1 area0",
#         "area2 area0",
#         "area3 area0",
#         "area4 area0"
#       ),
#       row_sizes = c(
#         "1fr",
#         "1fr",
#         "1fr",
#         "1fr"
#       ),
#       col_sizes = c(
#         "0.46fr",
#         "1.54fr"
#       ),
#       gap_size = "10px",
#       grid_card(
#         area = "area0",
#         card_body(
#           grid_container(
#             layout = c(
#               "area0",
#               "area1"
#             ),
#             row_sizes = c(
#               "1fr",
#               "1fr"
#             ),
#             col_sizes = c(
#               "1fr"
#             ),
#             gap_size = "10px",
#             grid_card(
#               area = "area0",
#               card_body(plotlyOutput(outputId = "plot"))
#             ),
#             grid_card(
#               area = "area1",
#               card_body(plotlyOutput(outputId = "plot"))
#             )
#           )
#         )
#       ),
#       grid_card(
#         area = "area1",
#         card_body(
#           value_box(
#             title = "Look at me!",
#             showcase = bsicons::bs_icon("database")
#           )
#         )
#       ),
#       grid_card(
#         area = "area2",
#         card_body(
#           value_box(
#             title = "Look at me!",
#             showcase = bsicons::bs_icon("database")
#           )
#         )
#       ),
#       grid_card(
#         area = "area3",
#         card_body(
#           value_box(
#             title = "Look at me!",
#             showcase = bsicons::bs_icon("database")
#           )
#         )
#       ),
#       grid_card(
#         area = "area4",
#         card_body(
#           value_box(
#             title = "Look at me!",
#             showcase = bsicons::bs_icon("database")
#           )
#         )
#       )
#     )
#   )
 )


server <- function(input, output) {
  
  
  output$StateMaps <- renderGirafe({
  
    if (input$State == "United States (Mainland)") {
        
        TRI23_snew <- TRI23_states %>%
          filter(!STUSPS %in% c("AK", "AS", "GU", "HI", "MP", "PR", "VI"))
        
        tooltip_text <- names(tooltips[which(tooltips == input$Stat)])
        
        p2 <- ggplot(data = TRI23_snew)+
          geom_sf_interactive(mapping = aes(fill = .data[[input$Stat]], 
                                            data_id = NAME,
                                            tooltip = paste("State: ", NAME,
                                                            "\n", tooltip_text,
                                                            ": ", format(.data[[input$Stat]], big.mark = ","))),
                              color = "black")+
          theme_void()+
          labs()
        
        girafe(ggobj = p2,
               options = list(opts_hover(css = "fill:green;stroke:black"),
                              opts_zoom(min = 1, max = 20, duration = 300),
                              opts_selection(type = "none")),
               width_svg = 10,
               height_svg = 6)
      
      
    } else if (input$State == "United States (Mainland & Alaska)"){
      
      TRI23_snew <- TRI23_states %>%
        filter(!STUSPS %in% c("AS", "GU", "HI", "MP", "PR", "VI"))
      
      tooltip_text <- names(tooltips[which(tooltips == input$Stat)])
      
      p2 <- ggplot(data = TRI23_snew)+
        geom_sf_interactive(mapping = aes(fill = .data[[input$Stat]], 
                                          data_id = NAME,
                                          tooltip = paste("State: ", NAME,
                                                          "\n", tooltip_text,
                                                          ": ", format(.data[[input$Stat]], big.mark = ","))),
                            color = "black")+
        coord_sf(xlim = c(-180, -60))+
        theme_void()+
        labs()
      
      girafe(ggobj = p2,
             options = list(opts_hover(css = "fill:green;stroke:black"),
                            opts_zoom(min = 1, max = 20, duration = 300),
                            opts_selection(type = "none")),
             width_svg = 10,
             height_svg = 5)
      
    } else {
        
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
    
        TRI23_cnew <- TRI23_counties %>%
          filter(STATENA == input$State) %>%
          mutate(NAME = gsub("'", "", NAME))
        
        tooltip_text <- names(tooltips[which(tooltips == input$Stat)])
        
        p1 <- ggplot(data = TRI23_cnew)+
          geom_sf_interactive(mapping = aes(fill = .data[[input$Stat]], 
                                            data_id = NAME,
                                            tooltip = paste("State: ", input$State,
                                                            "\nCounty: ", str_to_title(NAME),
                                                            "\n", tooltip_text,
                                                            ": ", format(.data[[input$Stat]], big.mark = ","))),
                              color = "black")+
          coord_sf(xlim = xlim1,
                   ylim = ylim1)+
          theme_void()+
          labs(title = paste(input$State))
        
        girafe(ggobj = p1,
               options = list(opts_hover(css = "fill:green;stroke:black"),
                              opts_zoom(min = 1, max = 20, duration = 300),
                              opts_selection(type = "none")),
               width_svg = 10,
               height_svg = 6)
    
    }
  })
}

shinyApp(ui, server)
  

