library(shiny)
library(shinycssloaders)
library(ggplot2)
library(bslib)
library(gridlayout)
library(ggiraph)
library(dplyr)
library(sf)
library(stringr)
library(scales)
library(RColorBrewer)

TRI23_counties <- read_sf("TRI23_counties.shp")
TRI23_states <- read_sf("TRI23_states.shp")

TRI23_counties$prd_wst <- round(TRI23_counties$prd_wst, 0)
TRI23_states$prd_wst <- round(TRI23_states$prd_wst, 0)

TRI23_counties$ttl_rls <- round(TRI23_counties$ttl_rls, 0)
TRI23_states$ttl_rls <- round(TRI23_states$ttl_rls, 0)

state_choices <- unique(TRI23_states$NAME)

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

tooltips <- setNames(metric_options, 
                     c("Pounds", 
                       "Pounds",
                       "Facilities",
                       "Chemical",
                       "Chemical",
                       rep("% of Facilities", 5),
                       rep("% of Release", 8),
                       rep("% of Waste", 3)))

captions <- setNames(metric_options, 
                     c("", "", "",
                       rep("Rated by Total Release", 2),
                       "Hazardous Chemicals - Chemicals deemed hazardous by the Clean Air Act",
                       "Toxic Metal - Metals deemed toxic, required to be reported in the TRI",
                       "Carcinogen - Chemical that has been linked to cancer development, determined by OSHA",
                       "PBT - Chemicals that are persistent, bioaccumulative, and toxic (PBT)",
                       "PFAS - Long lasting per- and polyfluoroalkyl substances that linked to harmful health effects",
                       "Fugitive Air - Leaks and other irregular emissions",
                       "Stack Air - Chemicals emitted/releases from smoke stacks",
                       rep("", 3),
                       "Misc. - Chemicals released to other disposal units/impoundments",
                       rep("", 3),
                       "Chemicals that were cleaned at treatment plants",
                       ""))

ui <- page_navbar(
  title = "Toxic Release Inventory ",
  selected = "Statewise",
  collapsible = TRUE,
  theme = bs_theme(preset = "materia",
                   primary = "#000000"),
  
  nav_panel(
    title = "Statewise",
    
    page_sidebar(
      sidebar = sidebar(
        title = "Sidebar Title",
        
        selectInput("State", 
                    label = "Select State(s):",
                    choices = c("United States Mainland",
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
          withSpinner(girafeOutput(outputId = "StateMaps", height = "80vh"),
                      type = 4)
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
  
    if (input$State == "United States Mainland"){
        
        #Filter out non-mainland states
        TRI23_snew <- TRI23_states %>%
          filter(!STUSPS %in% c("AK", "AS", "GU", "HI", "MP", "PR", "VI"))
        
        #Mainland plot
        p2 <- ggplot(data = TRI23_snew)+
          geom_sf_interactive(mapping = aes(fill = .data[[input$Stat]],    # Fill by input
                                            data_id = NAME,                #Id for interactive layer in ggiraph
                                            tooltip = paste("State: ", NAME,       # Text Displayed on hover
                                                            "\n", tooltip_text,
                                                            ": ", format(.data[[input$Stat]], big.mark = ","))),
                              color = "black")+
          theme_void()+
          get_scale_fill(TRI23_states, input$Stat)+
          labs(title = paste(title_text, " by State in ", input$State),
               caption = caption_text,
               fill= tooltip_text)+
          theme(plot.caption = element_text(color = "#555666"))
        
        girafe(ggobj = p2,
               options = list(opts_hover(css = "fill:green;stroke:black"),
                              opts_zoom(min = 1, max = 20, duration = 300),
                              opts_selection(type = "none")),
               width_svg = 10,
               height_svg = 6)
      
      
    } else if (input$State == "United States (Mainland & Alaska)"){
      
      TRI23_snew <- TRI23_states %>%
        filter(!STUSPS %in% c("AS", "GU", "HI", "MP", "PR", "VI"))
      
      p2 <- ggplot(data = TRI23_snew)+
        geom_sf_interactive(mapping = aes(fill = .data[[input$Stat]], 
                                          data_id = NAME,
                                          tooltip = paste("State: ", NAME,
                                                          "\n", tooltip_text,
                                                          ": ", format(.data[[input$Stat]], big.mark = ","))),
                            color = "black")+
        coord_sf(xlim = c(-180, -60))+
        theme_void()+
        get_scale_fill(TRI23_states, input$Stat)+
        labs(title = paste(title_text, " by State in ", input$State),
             caption = caption_text,
             fill = tooltip_text)
      
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
          get_scale_fill(TRI23_states, input$Stat)+
          labs(title = paste(title_text, " by County in ", input$State),
               caption = caption_text,
               fill = tooltip_text)
        
        girafe(ggobj = p1,
               options = list(opts_hover(css = "fill:green;stroke:black"),
                              opts_zoom(min = 1, max = 20, duration = 300),
                              opts_selection(type = "none")),
               width_svg = 10,  #adjust width and height of plot
               height_svg = 6)
    
    }
  })
  
#====================================================================================

}

shinyApp(ui, server)
  

