
mod_05_help_button_ui <- function(id) {
  ns <- NS(id)
  
  # Create new item on navigation bar
  nav_item(
    actionButton(
      ns("HelpButton"),
      label = "Help",
      class = "btn btn-primary",
      icon = icon("circle-info", class = "fa-lg"),
      style = "position: absolute; top: 15px; right: 5px;"
    )
  )
}

mod_05_help_button_server <- function(id, currentTab) {
  moduleServer(
    id,
    function(input, output, session) {
      # Show modal dialog on button press
      observeEvent(
        input$HelpButton,
        {
          # If/else structure that checks the current tab value
          if (currentTab() == "statewise") {
            img <- "Guide1.png"
            text <- "<div style = 'text-align: center;'>This tab is meant 
                     for generating large-scale statistics at the national or
                     state level.</div><div><br><b>The options panel allows you
                     to:</b><br><ul>
                     <li><b>#1:</b> Select between the Mainland US, US including 
                     Alaska, and individual states/territories. The national 
                     maps will display these statistics by state, while the 
                     state/territory maps with display them by county.</li>
                     <li><b>#2:</b> Select which statistic is displayed. 
                     Statistics include:
                     <ul><li>Toxic Chemical Totals</li>
                     <li>Release Method Proportions</li>
                     <li>Facility Counts by Chemical Type</li>
                     <li>More</li></ul></li></ul>
                     <b>Tip: </b> Click on any state while in the nationwide 
                     view to automatically select it in the State and Facility
                     Profile tabs! The same selection occurs when viewing a 
                     state/territory individually!</div>
                     <div style = 'text-align: center;'><br>Each plot maybe 
                     downloaded for future use.</div>"
          } else if (currentTab() == "statesum") {
            img <- "Guide2.png"
            text <- "<div style = 'text-align: center;'> The State Summary tab 
                     provides more in-depth statistics and measurements for any 
                     given US state or territory. The two main plots display 
                     the top 10 facilities and industries by a chosen metric or 
                     release method.</div>
                     <div><br><b>Useful Info for Highlighted Areas:</b><br>
                     <ul><li><b>#1:</b> Select a state or US territory to view 
                     in the tab</li>
                     <li><b>#2:</b> Select a metric to rank by in the plots 
                     shown in this tab. These metrics include:
                     <ul><li>Overall totals for chemical release and individual 
                     release methods</li>
                     <li>Totals for waste recycling, waste treatment, and 
                     overall waste</li>
                     </ul></li>
                     <li><b>#3:</b> Activates a logarithmic scale for the 
                     facility ranking. This makes the graph much easier to read 
                     when there are large differences between facilities.</li>
                     <li><b>#4:</b> This card displays the percent difference 
                     between the median chemical release of the selected state 
                     and the national median release.</li></ul>
                     <br><b>Tip:</b> Clicking on a Facility in the 
                     <b>top left</b> plot will automatically load that facility 
                     in the Facility Profile tab!</div>"
            
          } else if (currentTab() == "facprof") {
            img <- "Guide3.png"
            text <- "<div style = 'text-align: center;'> The Facility Profile 
                     tab attempts to give a broad overview of a selected 
                     facility, showing important metrics of how they operate, 
                     what they operate on, and the volume of their operations.
                     </div> <div><br><b>Useful Info for Highlighted Areas:
                     </b><br>
                     <ul><li><b>#1: </b>Select a state/territory of interest, 
                     which must be selected prior to selecting a facility</li>
                     <li><b>#2: </b>Select a facility of interest. Some 
                     facilities report for multiple locations, which can be 
                     seen on the timeline if that is the case</li>
                     <li><b>#3: </b>Select a metric for the timeline on the 
                     far right. Metrics include totals and proportions of 
                     release, recycling, waste, etc.</li>
                     <li><b>#4: </b>Perform address search for selected 
                     facility. After selecting one of the facility's locations 
                     (left), press the search button (right) to open the Address
                     Search tab and fill the address box with the location's 
                     address</li>
                     <li><b>#5: </b>Chemical Checklist. Displays which chemical 
                     types the selected facility releases. Chemical 
                     Descriptions: <ul><li><b>CAA Hazardous: </b>Declared 
                     hazardous by the Clean Air Act</li>
                     <li><b>Carcinogen: </b>Chemicals linked to cancer 
                     development</li>
                     <li><b>PBT Chemical: </b>Persistent, bioaccumulative, and 
                     toxic</li>
                     <li><b>PFAS Chemical: </b> Long lasting per- and 
                     polyfluoroalkyl substances linked to harmful health effects
                     </li></ul></li></ul></div>"
            
          } else if (currentTab() == "addsch") {
            img <- "Guide4.png"
            text <- "<div style = 'text-align: center;'>The Address Search tab 
                     allows for users to enter in any US address in order to 
                     gain a geographic understanding of the area surrounding 
                     the given address. This tab also interacts with the 
                     Facility Profile tab, allowing you to search for any 
                     given facility's location (see Facility Profile tab Help 
                     button for more details).
                     <br><br>Each point on the map represents an individual 
                     facility, while the large black marker represents either 
                     the address or facility that a user searches. Each point 
                     can be clicked to reveal more information about the 
                     facility it represents.</div>
                  <div><br><b>Tab Elements: </b>
                      <ul><li><b>#1: </b>Address input section. A good format to
                      follow for entering an address is '[# and Street], [City],
                      [State]', but it will also work if only a city and state 
                      is entered</li>
                      <li><b>#2: </b> Adjust radius of search. Radius can be set
                      from 0 to 100 US survey miles from the given address</li>
                      <li><b>#3: </b>Legend for generated plot. Measures the 
                      average yearly chemical release/recycling of facilities
                      </li>
                      <li><b>#4: </b>Select the layers displayed on the map. 
                      Currently, users can choose between facility releases, 
                      recycling, and toggling the heatmap feature. It is 
                      recommended that only one of the release metrics be 
                      selected at once, as points overlap</li></ul></div>
                      <div><b>Warning: </b>It is <b>IMPORTANT</b> not to confuse
                      what the legend applies to. The legend on the plot 
                      corresponds to the color of the points that represent each 
                      facility. The color of the heatmap element (the blended 
                      colors) should be used only as a reference for the density 
                      of facilities in a given area and the potential area of 
                      immediate affect surrounding those facilities.</div>"
            
          } else if (currentTab() == "about") {
            img <- "Logo2.png"
            text <- "<div style = 'text-align: center'> 
                     <h4>Welcome to Tox HelpR!</h4>
                     <br> Check the <b>About</b> tab to learn more about what 
                     this app can do and where it came from! </div>"
          } else {
            img <- NULL
            text <- "Error in tab values: Please contact app admin regarding 
                     this issue."
          }
          
          showModal(
            modalDialog(
              title = "Help",
              tagList(
                div(
                  tags$img(
                    src = img,
                    width = "100%",
                    style = "display: block; margin: 0 auto; padding: 0px;"
                  ),
                  style = "text-align: center; padding: 0px; 
                           margin-bottom: 0px;"
                ),
                HTML(text)
              ),
              footer = modalButton("Close"),
              easyClose = TRUE,
              size = "l"
            )
          )
        }
      )
    }
  )
}