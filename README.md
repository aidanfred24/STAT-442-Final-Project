# STAT-442-Final-Project
## Overview
Analysis of Toxics Release Inventory Data from the US Environmental Protection Agency

The US Environmental Protection Agency makes much of its data available to the US public. Of this data, one of the most interesting
is their "Toxics Release Inventory" (TRI). This inventory is an method of tracking the toxic waste management processes and amounts in 
US facilities. Data is reported annually by individual facilities. Chemicals that are required to be reported are generally those that
cause:

* Cancer or other chronic human health effects
* Significant adverse acute human health effects
* Significant adverse environmental effects

## This Repo

This repository contains:

* The original TRI data used in the analysis ("Datasets" Folder)
* The reports and figures generated for the original project ("Final Project Documents")
* The R scripts/modules, shapefiles, cleaned data, and images used for the Shiny App ("AppFiles")
    * The main focus of this repo
    * Shapefiles include .dbf, .prj, .shx, .shp files
    * "Data Prep For App" file shows many tests and processes for cleaning data and exploring visualization methods
    * "Original Design" folder contains the app prior to conversion to a modular format, "Modular Update" contains the most recent app files

This originally started as a final project for an Exploratory Data Analysis course (STAT 442) at South Dakota State University. 
However, since then it has narrowed its focus to the Shiny App, now called Tox HelpR, and seen many improvements, including 
better functionality, user friendliness, and advanced features. Now it serves as a fully operational environmental data analysis tool.

Link to Shiny App
https://aidanfred24.shinyapps.io/FinalProjectApp/

Link to Original Data:
https://www.epa.gov/toxics-release-inventory-tri-program/tri-basic-data-files-calendar-years-1987-present
