
#basic shiny functions
library(shiny)

#fancy loading symbols
library(shinycssloaders)

#for statewise plots and timeline
library(ggplot2)

#make ggplots interactive
library(ggiraph)

#for fancy themes and formatting
library(bslib)

# UI grid layout with cards (page 2)
library(gridlayout)

#data manipulation/pipelining
library(tidyverse)

#"simple features" to read shapefiles and plot maps
library(sf)
library(units) # format data with units attached (helps with sf objects)

# text editing (commas and case of letters)
library(stringr)
library(scales)

#color palettes
library(RColorBrewer)
library(paletteer)

# read in data files
library(readr)

# libraries for fancy geographic maps (page 2)
library(leaflet)
library(leaflet.extras)
library(tidygeocoder)

source("tab1Mod.R")
source("tab2Mod.R")
source("tab3Mod.R")
source("tab4Mod.R")
source("About.R")
source("HelpButton.R")

# load in state/county-wise data
TRI23_counties <- read_sf("TRI23_counties.shp")
TRI23_states <- read_sf("TRI23_states.shp")

# round all release and waste values to nearest unit
TRI23_counties$prd_wst <- round(TRI23_counties$prd_wst, 0)
TRI23_states$prd_wst <- round(TRI23_states$prd_wst, 0)
TRI23_counties$ttl_rls <- round(TRI23_counties$ttl_rls, 0)
TRI23_states$ttl_rls <- round(TRI23_states$ttl_rls, 0)

# read in most recent decade of data
TRIDecade <- read_csv("TRIDecade.csv")

# calculate important proportions for the decade
TRIDecade <- TRIDecade %>%
  mutate(recyc_prop = round((recycled / prod_waste) * 100, 2),
         treat_prop = round((treated / prod_waste) * 100, 2),
         rls_prop = round((total_release / prod_waste) * 100, 2)) %>%
  mutate(across(everything(), ~replace_na(., 0)))    #replace NA with 0

#Use all unique state names as choices
state_choices <- unique(TRI23_states$NAME)

#state abbr. with full name as name in list
state_choices2 <- setNames(TRI23_states$STUSPS, TRI23_states$NAME)

# Metric options for the first page
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

# Metric options for the timeline on page 2
metric_options2 <- setNames(colnames(TRIDecade[ ,c(24, 21:23, 25:27)]),
                            c("Total Released",
                              "Total Recycled",
                              "Total Treated",
                              "Total Production Waste",
                              "Proportion of Toxics Recycled",
                              "Proportion of Toxics Treated",
                              "Proportion of Toxics Released"))

# Dynamic y-axis labels for timeline
y_axis <- setNames(metric_options2,
                   c(rep("Pounds", 4),
                     rep("Percent", 3)))

# Dynamic tooltip labels for each metric
tooltips <- setNames(metric_options, 
                     c("Pounds", 
                       "Pounds",
                       "Facilities",
                       "Chemical",
                       "Industry",
                       rep("% of Facilities", 5),
                       rep("% of Release", 8),
                       rep("% of Waste", 3)))

# Dynamic captions for complex metrics
captions <- setNames(metric_options, 
                     c("", "", "",
                       rep("Rated by Total Release", 2),
                       "Hazardous Chemicals - Chemicals deemed hazardous by the Clean Air Act",
                       "Toxic Metal - Metals deemed toxic, required to be reported in the TRI",
                       "Carcinogen - Chemical that has been linked to cancer development, determined by OSHA",
                       "PBT - Chemicals that are persistent, bioaccumulative, and toxic (PBT)",
                       "PFAS - Long lasting per- and polyfluoroalkyl substances that are linked to harmful health effects",
                       "Fugitive Air - Unintentional releases of gases or particulates into the air (leaking equipment, livestock operations, etc.)",
                       "Stack Air - Chemicals emitted/released from a point source",
                       rep("", 3),
                       "Misc. - Chemicals released to other disposal units/impoundments",
                       rep("", 3),
                       "Chemicals that were cleaned at treatment plants",
                       ""))

# Names for each method column
methods <- setNames(colnames(TRIDecade[,13:18]), 
                    c("Fugitive Air Emissions",
                      "Stack Air Emissions",
                      "Surface Water Discharge",
                      "Underground Injection",
                      "Landfill Release",
                      "Other Disposal Units"))

ui <- page_navbar(

  id = "tab",
  title ="Tox HelpR", # App title
  selected = "statewise",                  # Selected tab
  collapsible = TRUE,
  theme = bs_theme(preset = "materia",     # Custom theme, "materia" bs preset
                   primary = "#000000"),
  
  # Define first panel
  nav_panel(
    
    value = "statewise",
    title = "Statewise",    #Tab title
    
    tab1UI("myModule", state_choices, metric_options)
  )
  ,
  
  nav_panel(
    
    value = "statesum",
    title = "State Summary",
    
    tab2UI("myModule2", state_choices2, c(metric_options2[1:4], methods))
    
  ),
  
  # UI for profile panel
  nav_panel(
    
    value = "facprof",
    title = "Facility Profile",     # Tab title
    
    tab3UI("myModule3", state_choices2, metric_options2)
    
  ),
  
  nav_panel(
    
    value = "addsch",
    title = "Address Search",
    
    tab4UI("myModule5")
    
  ),
  
  nav_panel(
    
    value = "about",
    title = "About",
    
    AboutUI("aboutModule")
    
  ),
    
  HelpButtonUI("myModule4")
  
 )


server <- function(input, output, session) {
  
  current_tab <- reactive(input$tab)
  
  HelpButtonServer("myModule4", current_tab)
    
  # PAGE 1 
  #====================================================================================
  tab1vars <- tab1Server("myModule", metric_options, tooltips, captions, TRI23_states, TRI23_counties)
  #====================================================================================
  
  # PAGE 2
  #====================================================================================
  tab2vars <- tab2Server("myModule2", TRIDecade, c(metric_options2[1:4], methods), tab1vars, state_choices2)
  #==================================================================================== 
   
  # PAGE 3
  #====================================================================================
  tab3vars <- tab3Server("myModule3", metric_options2, y_axis, methods, TRIDecade, tab2vars, tab1vars)
  
  observeEvent(tab3vars$tab(),{
    
    updateNavbarPage(session, "tab", selected = "addsch")
    
    })
  #====================================================================================
  
  # PAGE 4
  #====================================================================================
  tab4Server("myModule5", TRIDecade, tab3vars)
  #====================================================================================
  
}

shinyApp(ui, server)
  

