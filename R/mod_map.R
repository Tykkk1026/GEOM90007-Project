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
library(sf)
# read in gdb data and convert shape info into longitude and latitude
bus_regional_rt <- st_read("inst/data/BusRoute.gdb")
bus_data <- sf::st_transform(bus_regional_rt, 4326)

tram_rt <- st_read("inst/data/TramRoute.gdb")
tram_data <- sf::st_transform(tram_rt, 4326)

train_rt <- st_read("inst/data/Public.gdb", layer = "PTV_TRAIN_CORRIDOR_CENTRELINE")
train_data <- sf::st_transform(train_rt, 4326)

bus_stop_rt <- st_read("inst/data/Public.gdb", layer = "PTV_METRO_BUS_STOP")
bus_stop <- sf::st_transform(bus_stop_rt, 4326)

tram_stop_rt <- st_read("inst/data/TramStop.gdb")
tram_stop <- sf::st_transform(tram_stop_rt, 4326)

train_stop_rt <- st_read("inst/data/Public.gdb", layer = "PTV_METRO_TRAIN_STATION")
train_stop <- sf::st_transform(train_stop_rt, 4326)

# Create a convex hull around train stops
melbourne_train_hull <- st_union(train_stop) %>% st_convex_hull()
train_data <- st_intersection(train_data, melbourne_train_hull)



mod_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Melbourne Map", class = "light"),
    actionButton(ns("btn_map1"), "Public Transport"),
    actionButton(ns("btn_map2"), "Map 2"),
    tags$div(
      style = "display: flex; align-items: center; color: white;",
      uiOutput(ns("dynamicCheckboxBus")),
      uiOutput(ns("dynamicCheckboxTram")),
      uiOutput(ns("dynamicCheckboxTrain"))
    ),
    div(id = "weather"),
    mapboxer::mapboxerOutput(ns("map"), height = "50vh")
  )
}



#' map Server Function
#'
#' @noRd
mod_map_server <- function(input, output, session) {
  observeEvent(input$btn_map1, {
    output$map <- mapboxer::renderMapboxer({
      mapboxer::mapboxer(
        center = c(144.9631, -37.8136),
        zoom = 15,
      ) %>%
        # Bus routes and stops
        mapboxer::add_line_layer(
          source = as_mapbox_source(bus_data),
          line_color = "#1E90FF", # Bright blue for bus route
          line_width = 3,
          line_opacity = 0.8,
          id = "bus_route",
          visibility = FALSE,
          popup = paste0(
            "<strong>Route Name:</strong> {{ROUTE_SHORT_NAME}}<br>",
            "<strong>First Stop:</strong> {{FIRST_STOP_NAME}}<br>",
            "<strong>Last Stop:</strong> {{LAST_STOP_NAME}}<br>"
          )
        ) %>%
        mapboxer::add_circle_layer(
          source = as_mapbox_source(bus_stop),
          circle_color = "#1E90FF",
          circle_radius = 6,
          circle_stroke_color = "white",
          circle_stroke_width = 1.5,
          id = "bus_stop",
          visibility = FALSE,
          popup = paste0("<strong>Stop Name:</strong> {{STOP_NAME}}")
        ) %>%
        # Tram routes and stops
        mapboxer::add_line_layer(
          source = as_mapbox_source(tram_data),
          line_color = "#3CB371", # Medium sea green for tram route
          line_width = 3,
          id = "tram_route",
          visibility = FALSE,
          popup = paste0(
            "<strong>Route Name:</strong> {{ROUTE_SHORT_NAME}}<br>",
            "<strong>First Stop:</strong> {{FIRST_STOP_NAME}}<br>",
            "<strong>Last Stop:</strong> {{LAST_STOP_NAME}}<br>"
          )
        ) %>%
        mapboxer::add_circle_layer(
          source = as_mapbox_source(tram_stop),
          circle_color = "#3CB371",
          circle_radius = 6,
          circle_stroke_color = "white",
          circle_stroke_width = 1.5,
          id = "tram_stop",
          visibility = FALSE,
          popup = paste0("<strong>Stop Name:</strong> {{STOP_NAME}}")
        ) %>%
        # Train routes and stops
        mapboxer::add_line_layer(
          source = as_mapbox_source(train_data),
          line_color = "#FF4500", # Orange-red for train route
          line_width = 3,
          id = "train_route",
          visibility = FALSE,
          popup = paste0(
            "<strong>Route Name:</strong> {{SEGMENT_NAME}}<br>"
          )
        ) %>%
        mapboxer::add_circle_layer(
          source = as_mapbox_source(train_stop),
          circle_color = "#FF4500",
          circle_radius = 6,
          circle_stroke_color = "white",
          circle_stroke_width = 1.5,
          id = "train_stop",
          visibility = FALSE,
          popup = paste0("<strong>Stop Name:</strong> {{STOP_NAME}}")
        )
    })

    output$dynamicCheckboxBus <- renderUI({
      checkboxInput(session$ns("show_bus_route"), "Show Bus Route", value = FALSE)
    })

    output$dynamicCheckboxTram <- renderUI({
      checkboxInput(session$ns("show_tram_route"), "Show Tram Route", value = FALSE)
    })

    output$dynamicCheckboxTrain <- renderUI({
      checkboxInput(session$ns("show_train_route"), "Show Train Route", value = FALSE)
    })
  })


  observe({
    proxy <- mapboxer_proxy(session$ns("map"))
    if (is.null(input$show_bus_route)) {
      return()
    }

    if (input$show_bus_route) {
      proxy <- set_layout_property(proxy, "bus_route", "visibility", TRUE)
      proxy <- set_layout_property(proxy, "bus_stop", "visibility", TRUE)
      update_mapboxer(proxy, session$ns("map"))
    } else {
      proxy <- set_layout_property(proxy, "bus_route", "visibility", FALSE)
      proxy <- set_layout_property(proxy, "bus_stop", "visibility", FALSE)
      update_mapboxer(proxy, session$ns("map"))
    }
    if (input$show_tram_route) {
      proxy <- set_layout_property(proxy, "tram_route", "visibility", TRUE)
      proxy <- set_layout_property(proxy, "tram_stop", "visibility", TRUE)
      update_mapboxer(proxy, session$ns("map"))
    } else {
      proxy <- set_layout_property(proxy, "tram_route", "visibility", FALSE)
      proxy <- set_layout_property(proxy, "tram_stop", "visibility", FALSE)
      update_mapboxer(proxy, session$ns("map"))
    }
    if (input$show_train_route) {
      proxy <- set_layout_property(proxy, "train_route", "visibility", TRUE)
      proxy <- set_layout_property(proxy, "train_stop", "visibility", TRUE)
      update_mapboxer(proxy, session$ns("map"))
    } else {
      proxy <- set_layout_property(proxy, "train_route", "visibility", FALSE)
      proxy <- set_layout_property(proxy, "train_stop", "visibility", FALSE)
      update_mapboxer(proxy, session$ns("map"))
    }
  })



  observeEvent(input$btn_map2, {
    output$map <- mapboxer::renderMapboxer({
      mapboxer::mapboxer(
        center = c(145.9631, -37.8136), # Different center for Map 2
        zoom = 10
      )
    })

    output$dynamicCheckboxBus <- renderUI({})
    output$dynamicCheckboxTram <- renderUI({})
    output$dynamicCheckboxTrain <- renderUI({})
  })
}




## To be copied in the UI
# mod_map_ui("map")

## To be copied in the server
# callModule(mod_map_server, "map")
