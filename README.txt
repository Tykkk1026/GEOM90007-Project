# GEOM90007-Project Group72


This document provides the steps required to run the Shiny application.

1. Install the Required R Packages

Ensure you have the following R packages installed:
- shiny
- golem
- dplyr
- tidyr
- fullPage
- magrittr
- shinythemes
- mapboxer
- sf
- geojsonsf
- httr
- shinyjs

If you haven't installed the above packages, open R or RStudio and run the following command to install them:

install.packages(c("shiny", "golem", "dplyr", "tidyr", "fullPage", "magrittr", "shinythemes", "mapboxer", "sf", "geojsonsf", "httr", "shinyjs"))



2. Open "run_dev.R" file in "dev" folder in RStudio.
Source this file, when all data are read completely, run this file in R studio / input app::run_app() in command line

Then it will launch the Shiny application, and it should open in your default web browser.