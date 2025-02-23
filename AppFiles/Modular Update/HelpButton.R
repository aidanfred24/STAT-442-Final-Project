
HelpButtonUI <- function(id) {
  
  ns <- NS(id)
  
    nav_item(
      
    actionButton(ns("HelpButton"), # new namespace for button
                 label = "Help", #Button  label and icons
                 class = "btn btn-primary",
                 icon = icon("circle-info", class = "fa-lg"), #select icon, increase size
                 style = "position: absolute; top: 15px; right: 5px;") # position of button
  )
  
}

HelpButtonServer <- function(id, currentTab) {
  
  moduleServer(
    
    id,
    
    function(input, output, session){
      
      # Show modal dialog on button press
      observeEvent(input$HelpButton, {
        
        if (currentTab() == "statewise") {
          
          text <- "<div>This tab is meant for generating large-scale statistics at the 
                   national or state level. The options panel allows you to select between
                   the Mainland US, US including Alaska, and individual states/territories.
                   The national maps will display these statistics by state, while the state/territory
                   maps with display them by county.<br>You may also select which statistic is
                   displayed using the lower-most dropdown menu. Each Plot maybe downloaded for 
                   future use.</div>"
          
        } else {
          
          text <- "Other Tab"
          
        }
        
        showModal(modalDialog(title = "Help",
                              tagList(
                                div(renderImage({list(src = "www/Logo2.png", width = "80%")},
                                            deleteFile = FALSE),
                                    style = "text-align: center;"),
                                HTML(text)
                                ),
                              footer = modalButton("Close"),
                              easyClose = TRUE)) # button to close modal
      })
    
       
    }
    
  )
  
}