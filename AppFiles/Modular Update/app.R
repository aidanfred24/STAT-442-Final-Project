# Basic shiny functions
library(shiny)
library(ggplot2)   # For statewise plots and timeline
library(ggiraph)   # Make ggplots interactive
library(bslib)     # Fancy themes and grid formatting
library(dplyr)     # Data manipulation/pipelining
library(stringr)   # Text editing (commas and case of letters)
library(scales)

source("mod_01_statewise.R")
source("mod_02_state_sum.R")
source("mod_03_fac_sum.R")
source("mod_04_add_sch.R")
source("mod_05_help_button.R")
source("mod_06_about.R")

# Load in state/county-wise data
TRI23_counties <- sf::read_sf("TRI23_counties.shp")
TRI23_states <- sf::read_sf("TRI23_states.shp")

# Read in most recent decade of data
TRIDecade <- feather::read_feather("TRIDecade.feather")

# Read in label data
state_labels <- feather::read_feather("state_labels.feather")
metric_data1 <- feather::read_feather("metric_data1.feather")
metric_data2 <- feather::read_feather("metric_data2.feather")
method_data <- feather::read_feather("method_data.feather")

# Use all unique state names as choices
state_choices <- state_labels$statefull

# State abbr. with full name as name in list
state_choices2 <- setNames(
  state_labels$stateabb,
  state_labels$statefull
)

# Metric options for the first page
metric_options <- setNames(
  metric_data1$options,
  metric_data1$names1
)

# Metric options for the timeline on page 2
metric_options2 <- setNames(
  metric_data2$options,
  metric_data2$names1
)

# Dynamic y-axis labels for timeline
y_axis <- setNames(
  metric_options2,
  metric_data2$y_axis
)

# Dynamic tooltip labels for each metric
tooltips <- setNames(
  metric_options,
  metric_data1$tooltips
)

# Dynamic captions for complex metrics
captions <- setNames(
  metric_options,
  metric_data1$captions
)

# Names for each method column
methods <- setNames(
  method_data$options,
  method_data$names1
)

labels <- setNames(
  metric_options2,
  metric_data2$labels
)

ui <- page_navbar(
  id = "tab",
  title = "Tox HelpR",  # App title
  selected = "addsch",
  collapsible = TRUE,
  theme = bs_theme(
    preset = "materia",
    primary = "#000000"
  ),
  
  # Define first panel
  nav_panel(
    value = "addsch",
    title = "Address Search",
    mod_04_add_sch_ui(
      "add_sch"
    )
  ),
  
  nav_panel(
    value = "statewise",
    title = "National Statistics",
    mod_01_statewise_ui(
      "statewise",
      state_choices,
      metric_options
    )
  ),
  
  nav_panel(
    value = "statesum",
    title = "State Summary",
    mod_02_state_sum_ui(
      "state_sum",
      state_choices2,
      c(metric_options2[1:4], methods)
    )
  ),
  
  nav_panel(
    value = "facprof",
    title = "Facility Profile",
    mod_03_fac_sum_ui(
      "fac_sum",
      state_choices2,
      metric_options2
    )
  ),
  
  nav_panel(
    value = "about",
    title = "About",
    mod_06_about_ui(
      "about"
    )
  ),
  
  mod_05_help_button_ui(
    "help_button"
  )
)

server <- function(input, output, session) {

  current_tab <- reactive(
    input$tab
  )
  
  mod_05_help_button_server(
    "help_button",
    current_tab
  )
  
  # PAGE 1
  tab1vars <- mod_01_statewise_server(
    "statewise",
    metric_options,
    tooltips,
    captions,
    TRI23_states,
    TRI23_counties
  )
  
  # PAGE 2
  tab2vars <- mod_02_state_sum_server(
    "state_sum",
    TRIDecade,
    c(metric_options2[1:4], methods),
    tab1vars,
    state_choices2
  )
  
  # PAGE 3
  tab3vars <- mod_03_fac_sum_server(
    "fac_sum",
    metric_options2,
    y_axis,
    labels,
    methods,
    TRIDecade,
    tab2vars,
    tab1vars
  )
  
  observeEvent(
    tab3vars$tab(),
    {
      updateNavbarPage(
        session,
        "tab",
        selected = "addsch"
      )
    }
  )
  
  # PAGE 4
  mod_04_add_sch_server(
    "add_sch",
    TRIDecade,
    tab3vars
  )
}

shinyApp(
  ui,
  server
)


