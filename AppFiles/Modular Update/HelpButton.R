
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
          
          img <- "Guide1.png"
          
          text <- "<div style = 'align-text: center;'>This tab is meant for generating large-scale statistics at the 
                   national or state level.</div><div><br><b>The options panel allows you to:</b><br><ul>
                   <li><b>#1:</b> Select between the Mainland US, US including Alaska, and individual states/territories.
                   The national maps will display these statistics by state, while the state/territory
                   maps with display them by county.</li>
                   <li><b>#2:</b> Select which statistic is displayed. Statistics include:<ul>
                   <li>Toxic Chemical Totals</li>
                   <li>Release Method Proportions</li>
                   <li>Facility Counts by Chemical Type</li>
                   <li>More</li></ul></li></ul></div>
                   <div style = 'text-align: center;'><br>Each plot maybe downloaded for future use.</div>"
          
        } else if (currentTab() == "statesum"){
          
          img <- "Guide2.png"
          
          text <- "<div style = 'align-text: center;'> The State Summary tab provides more in-depth statistics and measurements
                   for any given US state or territory. The two main plots display the top 10 facilities and industries
                   by a chosen metric or release method.</div>
                   <div><br><b>Useful Info for Highlighted Areas:</b><br>
                      <ul><li><b>#1:</b> Select a state or US territory to view in the tab</li>
                      <li><b>#2:</b> Select a metric to rank by in the plots shown in this tab. These metrics include:
                        <ul><li>Overall totals for chemical release and individual release methods</li>
                        <li>Totals for waste recycling, waste treatment, and overall waste</li>
                      </ul></li>
                      <li><b>#3:</b> Activates a logarithmic scale for the facility ranking. This makes the graph much easier
                      to read when there are large differences between facilities.</li>
                      <li><b>#4:</b> This card displays the percent difference between the median chemical
                      release of the selected state and the national median release.</li></ul>
                   <br><br><b>Tip:</b> Clicking on a Facility in the <b>top left</b> plot will automatically
                   load that facility in the Facility Profile tab!</div>"
          
        } else {
          
          img <- NULL
          
          text <- "Coming Soon!"
          
        }
        
        showModal(modalDialog(title = "Help",
                              tagList(
                                div(
                                  tags$img(src = img, width = "100%", style = "display: block; margin: 0 auto; padding: 0px;"),
                                  style = "text-align: center; padding: 0px; margin-bottom: 0px;"
                                ),
                                HTML(text),
                                ),
                              footer = modalButton("Close"), # button to close modal
                              easyClose = TRUE, # Click anywhere to close
                              size = "l")) # large size
      })
    
       
    }
    
  )
  
}