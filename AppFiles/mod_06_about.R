
mod_06_about_ui <- function(id) {
  
  ns <- NS(id)
  
  fluidPage(
    
    div(card(
    tags$img(src = "Logo1.png",
             style = "display: block; margin-left: 40%; margin-right: 40%;",
             width = "20%"),
    tags$h1("About The App", style = "text-align: center;"),
    hr(),
    
    p('The US Environmental Protection Agency makes much of its data available 
       to the US public. Of this data, one of the most interesting is their 
       "Toxics Release Inventory" (TRI). This inventory is an method of tracking
       the toxic waste management processes and amounts in US facilities. Data 
       is reported annually by individual facilities. Chemicals that are 
       required to be reported are generally those that cause:', 
      style = "text-align: center; margin-left: 10%; margin-right: 10%;"),
    
    tags$ul(
      
      tags$li("Cancer or other chronic human health effects"),
      tags$li("Significant adverse acute human health effects"),
      tags$li("Significant adverse environmental effects"),
      style = "margin-left: 10%;"
      
    ),
    p("ToxOnline offers a way to access, view, and analyze this TRI data in a 
       way that is user friendly and efficient.",
      style = "text-align: center; margin-left: 10%; margin-right: 10%;"),
    p("This app originally started as a final project for an Exploratory Data 
       Analysis course (STAT 442) at South Dakota State University. However, 
       since then it has seen many improvements, including better functionality,
       user friendliness, and advanced features. Now it serves as a fully 
       operational environmental data analysis tool.",
      style = "text-align: center; margin-left: 10%; margin-right: 10%;")
    
  ), 
  style = "margin-left: 5%; margin-right: 5%;"),
  
  div(
    fluidRow(
      column(
        6,
        card(
          h3(
            "Important Links",
            style = "margin-bottom: 0px;"
          ),
          hr(),
          tags$ul(
            tags$li(
              "Visit the",
              tags$a(
                href = paste0("https://www.epa.gov/",
                              "toxics-release-inventory-tri-program/",
                              "tri-basic-data-files-calendar-years-1987-present"
                       ),
                target = "_blank",
                "TRI Data Site",
                style = "color: #309676;"
              )
            ),
            tags$li(
              "View the",
              tags$a(
                href = "https://www.epa.gov/",
                target = "_blank",
                "EPA Homepage",
                style = "color: #309676;"
              )
            ),
            tags$li(
              "TRI",
              tags$a(
                href = paste0("https://www.epa.gov/",
                              "toxics-release-inventory-tri-program/",
                              "tri-laws-and-regulatory-activities"
                       ),
                target = "_blank",
                "Laws and Regulations",
                style = "color: #309676;"
              )
            ),
            tags$li(
              "Github page for the app",
              tags$a(
                href = "https://github.com/aidanfred24/STAT-442-Final-Project",
                target = "_blank",
                "here",
                style = "color: #309676;"
              )
            )
          ),
          height = "95%"
        )
      ),
      column(
        6,
        card(
          h3(
            "Additional Information",
            style = "margin-bottom: 0px;"
          ),
          hr(),
          tags$ul(
            tags$li(
              "Most tabs, except for the National tab, utilize the past decade 
               (2013-2023) of the EPA's TRI data. The National tab only uses 
               the 2023 data from the TRI to give the most recent statistics for
               the United States"
            ),
            tags$li(
              "For more in-depth information on how to use other tabs, press the
               HELP button in the upper right corner!"
            ),
            tags$li(
              "Any dropdown menu can be used as a searchbar if cleared 
               beforehand using the backspace key"
            )
          )
        )
      )
    ),
    style = "margin-left: 5%; margin-right: 5%;"
  )
  )
}

mod_06_about_server <- function(id) {
  
  moduleServer(
    id,
    function(input, output, session) {
      
    }
  )
  
}
