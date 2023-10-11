#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
library(mapboxer)
mod_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Melbourne Map", class = "light"),
    actionButton(ns("btn_map1"), "Map 1"),
    actionButton(ns("btn_map2"), "Map 2"),
    # Add more buttons for more maps as needed
    mapboxer::mapboxerOutput(ns("map"), height = "50vh"),
    uiOutput(ns("desc"))
  )
}

    
#' map Server Function
#'
#' @noRd 
mod_map_server <- function(input, output, session){

  observeEvent(input$btn_map1, {
    output$map <- mapboxer::renderMapboxer({
      mapboxer::mapboxer(
        center = c(144.9631,-37.8136),  # Center around Melbourne for Map 1
        zoom = 10
      )
    })
  })

  observeEvent(input$btn_map2, {
    output$map <- mapboxer::renderMapboxer({
      mapboxer::mapboxer(
        center = c(145.9631,-37.8136),  # Different center for Map 2
        zoom = 10
      )
    })
  })

  # Add more observers for more maps as needed
}

    
## To be copied in the UI
# mod_map_ui("map")
    
## To be copied in the server
# callModule(mod_map_server, "map")
 