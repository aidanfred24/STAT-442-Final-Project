
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

HelpButtonServer <- function(id) {
  
  moduleServer(
    
    id,
    
    function(input, output, session){
      
      # Show modal dialog on button press
      observeEvent(input$HelpButton, {
                   
                   showModal(modalDialog(title = "Welcome",
                                                 HTML("<div style='text-align: center;'>Test Message <br> Coming Soon!</div>"),
                                                 footer = modalButton("Close"))) # button to close modal
      })
    
       
    }
    
  )
  
}