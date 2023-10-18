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
library(shiny)
library(dplyr)

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

bicycle_rt <- st_read("inst/data/GEOM90007_Assignment3.gdb",
  layer = "Melbourne_Bicycle_Routes_MGA"
)
bicycle_rt <- st_transform(bicycle_rt, 4326)

m_bound <- st_read("inst/data/GEOM90007_Assignment3.gdb",
  layer = "Melbourne_Municipal_Boundary_MGA"
)
m_bound <- st_transform(m_bound, 4326)

cafe <- read.csv("inst/data/cafes-and-restaurants-with-seating-capacity.csv")
cafe <- subset(cafe, cafe$Industry..ANZSIC4..description == "Cafes and Restaurants" |
  cafe$Industry..ANZSIC4..description == "Takeaway Food Services" |
  cafe$Industry..ANZSIC4..code == "Other Store-Based Retailing n.e.c." |
  cafe$Industry..ANZSIC4..description == "Other Specialised Food Retailing" |
  cafe$Industry..ANZSIC4..description == "Bakery Product Manufacturing (Non-factory based)" |
  cafe$Industry..ANZSIC4..description == "Convenience Store")
cafe <- cbind(rep("cafe", nrow(cafe)), cafe)
names(cafe)[1] <- "facility"
for (i in 1:nrow(cafe)) {
  if (cafe$Industry..ANZSIC4..description[i] == "Convenience Store") {
    cafe$facility[i] <- "convenience"
  }
}
cafe <- cafe[, c("facility", "Trading.name", "Longitude", "Latitude")]
cafe <- subset(cafe, !is.na(cafe$Longitude))
names(cafe)[3] <- "lon"
names(cafe)[4] <- "lat"
names(cafe)[2] <- "name"
convenience <- subset(cafe, cafe$facility == "convenience")
cafe <- subset(cafe, cafe$facility == "cafe")
# cafe$icon <- icon(class="fa-sharp fa-solid fa-burger-soda", lib = 'fa')
# cafe$icon <- "fa-sharp fa-solid fa-burger-soda"

toilet <- read.csv("inst/data/public-toilets.csv")
toilet <- cbind(rep("toilet", nrow(toilet)), toilet)
names(toilet)[1] <- "facility"
toilet <- toilet[, c("facility", "female", "male", "lon", "lat")]
toilet <- subset(toilet, !is.na(toilet$lon))
sex <- list(NA, nrow(toilet))
for (i in 1:nrow(toilet)) {
  if ((toilet$female[i] == "no") & (toilet$male[i] == "yes")) {
    sex[i] <- "Male"
  } else if ((toilet$female[i] == "yes") & (toilet$male[i] == "no")) {
    sex[i] <- "Female"
  } else if ((toilet$female[i] == "yes") & (toilet$male[i] == "yes")) {
    sex[i] <- "Both Gender"
  }
}
sex <- strsplit(toString(sex), ", ")
toilet <- cbind(toilet, sex)
toilet[, 2] <- toilet[, 6]
names(toilet)[2] <- "name"
toilet <- toilet[, c("facility", "name", "lon", "lat")]
# toilet$icon <- icon(class="fa-solid fa-person", lib = 'fa')
# toilet$icon <- "fa-solid fa-person"

# Separate different type bicycle routes
on_road_bicy <- st_as_sf(bicycle_rt) %>% filter(type == "On-Road Bike Lane")
off_road_bicy <- st_as_sf(bicycle_rt) %>% filter(type == "Off-Road Bike Route")
informal_bicy <- st_as_sf(bicycle_rt) %>% filter(type == "Informal Bike Route")
corridor_bicy <- st_as_sf(bicycle_rt) %>% filter(type == "Corridor")

# Create a convex hull around train stops
melbourne_train_hull <- st_union(train_stop) %>% st_convex_hull()
train_data <- st_intersection(train_data, melbourne_train_hull)


# 车流量
traffic_geo <- st_read("inst/data/Traffic_Volume.geojson")

# 路边停车位
parking_sensors <- st_read("inst/data/on-street-parking-bay-sensors.geojson")

# 施工
city_work <- st_read("inst/data/city-activities-and-planned-works.geojson")
city_work <- st_transform(city_work, 4326)




mod_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Melbourne Map", class = "light"),
    actionButton(ns("btn_map1"), "Public Transport"),
    actionButton(ns("btn_map2"), "Bicycle Route"),
    actionButton(ns("btn_map3"), "Traffic and Pedestrian"),
    tags$div(
      style = "display: flex; align-items: center; color: white;",
      uiOutput(ns("dynamicCheckboxBus")),
      uiOutput(ns("dynamicCheckboxTram")),
      uiOutput(ns("dynamicCheckboxTrain"))
    ),
    tags$div(
      style = "display: flex; align-items: center; color: white;",
      uiOutput(ns("dynamicCheckboxToilet")),
      uiOutput(ns("dynamicCheckboxCafe")),
      uiOutput(ns("dynamicCheckboxConvenience"))
    ),
    tags$div(
      style = "display: flex; align-items: center; color: white;",
      uiOutput(ns("dynamicCheckboxTraffic")),
      uiOutput(ns("dynamicCheckboxCity"))
    ),
    tags$div(
      style = "position: absolute; bottom: 0; left: -100px; z-index: 1000; background-color: rgba(255,255,255,0); padding: 10px; color: white;",
      uiOutput(ns("dynamicCheckboxCorridor")),
      uiOutput(ns("dynamicCheckboxOnRoad")),
      uiOutput(ns("dynamicCheckboxOffRoad")),
      uiOutput(ns("dynamicCheckboxInformal")),
      uiOutput(ns("parking"))
    ),

    mapboxer::mapboxerOutput(ns("map"), height = "80vh")
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
      ) %>%add_navigation_control() %>%
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

    output$dynamicCheckboxCorridor <- renderUI({})
    output$dynamicCheckboxOnRoad <- renderUI({})
    output$dynamicCheckboxOffRoad <- renderUI({})
    output$dynamicCheckboxInformal <- renderUI({})
    output$dynamicCheckboxToilet <- renderUI({})
    output$dynamicCheckboxCafe <- renderUI({})
    output$dynamicCheckboxConvenience <- renderUI({})
    output$dynamicCheckboxTraffic <- renderUI({})
    output$dynamicCheckboxCity <- renderUI({})
    output$parking <- renderUI({})
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
        center = c(144.9631, -37.8136),
        zoom = 15,
      ) %>% add_navigation_control() %>%
        # Corridor Bicycle Route
        mapboxer::add_line_layer(
          source = as_mapbox_source(corridor_bicy),
          line_color = "#00cc00",
          line_width = 2,
          line_opacity = 0.8,
          id = "corridor_rt",
          visibility = FALSE,
          popup = paste0(
            "<strong>Route Type:</strong> {{type}}<br>",
            "<strong>Direction:</strong> {{direction}}<br>"
          )
        ) %>%
        # on_road route
        mapboxer::add_line_layer(
          source = as_mapbox_source(on_road_bicy),
          line_color = "#ff9933",
          line_width = 2,
          line_opacity = 0.8,
          id = "onRoad_rt",
          visibility = FALSE,
          popup = paste0(
            "<strong>Route Type:</strong> {{type}}<br>",
            "<strong>Direction:</strong> {{direction}}<br>"
          )
        ) %>%
        # off_road route
        mapboxer::add_line_layer(
          source = as_mapbox_source(off_road_bicy),
          line_color = "#00e6e6",
          line_opacity = 0.8,
          id = "offRoad_rt",
          visibility = FALSE,
          popup = paste0(
            "<strong>Route Type:</strong> {{type}}<br>",
            "<strong>Direction:</strong> {{direction}}<br>"
          )
        ) %>%
        # informal route
        mapboxer::add_line_layer(
          source = as_mapbox_source(informal_bicy),
          line_color = "#9933ff",
          line_width = 2,
          line_opacity = 0.8,
          id = "informal_rt",
          visibility = FALSE,
          popup = paste0(
            "<strong>Route Type:</strong> {{type}}<br>",
            "<strong>Direction:</strong> {{direction}}<br>"
          )
        ) %>%
        # toilet
        mapboxer::add_circle_layer(
          source = toilet %>% as_mapbox_source(lng = "lon", lat = "lat"),
          circle_color = "#4dff4d",
          circle_radius = 6,
          circle_stroke_color = "white",
          circle_stroke_width = 1.5,
          id = "toilet",
          visibility = FALSE,
          popup = paste0("<strong>Toilet:</strong> {{name}}")
        ) %>%
        # cafe
        mapboxer::add_circle_layer(
          source = cafe %>% as_mapbox_source(lng = "lon", lat = "lat"),
          circle_color = "#ff99cc",
          circle_radius = 6,
          circle_stroke_color = "white",
          circle_stroke_width = 1.5,
          id = "cafe",
          visibility = FALSE,
          popup = paste0("<strong>Cafe and Resturuant:</strong> {{name}}")
        ) %>%
        # convenience store
        mapboxer::add_circle_layer(
          source = convenience %>% as_mapbox_source(lng = "lon", lat = "lat"),
          circle_color = "#668cff",
          circle_radius = 6,
          circle_stroke_color = "white",
          circle_stroke_width = 1.5,
          id = "convenience",
          visibility = FALSE,
          popup = paste0("<strong>Convenience Store:</strong> {{name}}")
        )
    })

    output$dynamicCheckboxCorridor <- renderUI({
      checkboxInput(session$ns("show_corridor"), "Corridor", value = FALSE)
    })

    output$dynamicCheckboxOnRoad <- renderUI({
      checkboxInput(session$ns("show_onRoad"), "On-Road", value = FALSE)
    })

    output$dynamicCheckboxOffRoad <- renderUI({
      checkboxInput(session$ns("show_offRoad"), "Off-Road", value = FALSE)
    })

    output$dynamicCheckboxInformal <- renderUI({
      checkboxInput(session$ns("show_informal"), "Informal", value = FALSE)
    })

    output$dynamicCheckboxToilet <- renderUI({
      checkboxInput(session$ns("show_toilet"), "Public Toilet", value = FALSE)
    })

    output$dynamicCheckboxCafe <- renderUI({
      checkboxInput(session$ns("show_cafe"), "Cafe and Resturuant", value = FALSE)
    })

    output$dynamicCheckboxConvenience <- renderUI({
      checkboxInput(session$ns("show_convenience"), "Convenience Store", value = FALSE)
    })
    output$dynamicCheckboxBus <- renderUI({})
    output$dynamicCheckboxTram <- renderUI({})
    output$dynamicCheckboxTrain <- renderUI({})
    output$dynamicCheckboxTraffic <- renderUI({})
    output$dynamicCheckboxCity <- renderUI({})
    output$parking <- renderUI({})
  })

  observe({
    proxy <- mapboxer_proxy(session$ns("map"))
    if (is.null(input$show_corridor)) {
      return()
    }
    if (input$show_corridor) {
      proxy <- set_layout_property(proxy, "corridor_rt", "visibility", TRUE)
      update_mapboxer(proxy, session$ns("map"))
    } else {
      proxy <- set_layout_property(proxy, "corridor_rt", "visibility", FALSE)
      update_mapboxer(proxy, session$ns("map"))
    }
    if (input$show_onRoad) {
      proxy <- set_layout_property(proxy, "onRoad_rt", "visibility", TRUE)
      update_mapboxer(proxy, session$ns("map"))
    } else {
      proxy <- set_layout_property(proxy, "onRoad_rt", "visibility", FALSE)
      update_mapboxer(proxy, session$ns("map"))
    }
    if (input$show_offRoad) {
      proxy <- set_layout_property(proxy, "offRoad_rt", "visibility", TRUE)
      update_mapboxer(proxy, session$ns("map"))
    } else {
      proxy <- set_layout_property(proxy, "offRoad_rt", "visibility", FALSE)
      update_mapboxer(proxy, session$ns("map"))
    }
    if (input$show_informal) {
      proxy <- set_layout_property(proxy, "informal_rt", "visibility", TRUE)
      update_mapboxer(proxy, session$ns("map"))
    } else {
      proxy <- set_layout_property(proxy, "informal_rt", "visibility", FALSE)
      update_mapboxer(proxy, session$ns("map"))
    }
    if (input$show_toilet) {
      proxy <- set_layout_property(proxy, "toilet", "visibility", TRUE)
      update_mapboxer(proxy, session$ns("map"))
    } else {
      proxy <- set_layout_property(proxy, "toilet", "visibility", FALSE)
      update_mapboxer(proxy, session$ns("map"))
    }
    if (input$show_cafe) {
      proxy <- set_layout_property(proxy, "cafe", "visibility", TRUE)
      update_mapboxer(proxy, session$ns("map"))
    } else {
      proxy <- set_layout_property(proxy, "cafe", "visibility", FALSE)
      update_mapboxer(proxy, session$ns("map"))
    }
    if (input$show_convenience) {
      proxy <- set_layout_property(proxy, "convenience", "visibility", TRUE)
      update_mapboxer(proxy, session$ns("map"))
    } else {
      proxy <- set_layout_property(proxy, "convenience", "visibility", FALSE)
      update_mapboxer(proxy, session$ns("map"))
    }
  })



  observeEvent(input$btn_map3, {
    output$map <- mapboxer::renderMapboxer({
      mapboxer::mapboxer(
        center = c(144.9631, -37.8136),
        zoom = 15,
      ) %>%
        add_navigation_control() %>%
        mapboxer::add_fill_layer(
          source = as_mapbox_source(city_work),
          fill_color = "pink",
          fill_opacity = 0.5,
          visibility = FALSE,
          id = "city_work",
          popup = "Location: {{location}}<br>Notes: {{notes}}"
        ) %>%
        mapboxer::add_circle_layer(
          source = as_mapbox_source(parking_sensors),
          circle_color = "red",
          circle_radius = 3,
          id = "parking",
          visibility = FALSE,
          popup = "{{status_description}}"
        ) %>%
        # 根据用户选择，添加车流量图层
        mapboxer::add_line_layer(
          source = as_mapbox_source(traffic_geo),
          visibility = FALSE,
          id = "traffic",
          line_color = list(
            "interpolate",
            list("linear"),
            list("get", "ALLVEHS_AADT"),
            0, "grey",
            5000, "green",
            10000, "yellow",
            20000, "orange",
            50000, "red"
          ),
          line_width = 2,
          popup = "{{ALLVEHS_AADT}}"
        )
    })

    output$dynamicCheckboxTraffic <- renderUI({
      checkboxInput(session$ns("show_traffic"), "Show Traffic Volume", value = FALSE)
    })

    output$dynamicCheckboxCity <- renderUI({
      checkboxInput(session$ns("show_city_work"), "Show City Work", value = FALSE)
    })

    output$parking <- renderUI({
      radioButtons(session$ns("parking_status"), "Parking Status",
        choices = c("All", "Unoccupied", "Occupied", "None"),
        selected = "None"
      )
    })

    output$dynamicCheckboxCorridor <- renderUI({})
    output$dynamicCheckboxOnRoad <- renderUI({})
    output$dynamicCheckboxOffRoad <- renderUI({})
    output$dynamicCheckboxInformal <- renderUI({})
    output$dynamicCheckboxToilet <- renderUI({})
    output$dynamicCheckboxCafe <- renderUI({})
    output$dynamicCheckboxConvenience <- renderUI({})
    output$dynamicCheckboxBus <- renderUI({})
    output$dynamicCheckboxTram <- renderUI({})
    output$dynamicCheckboxTrain <- renderUI({})
  })

  observe({
    if (is.null(input$show_city_work)) {
      return()
    }
    proxy <- mapboxer_proxy(session$ns("map"))
    if (input$show_city_work) {
      proxy <- set_layout_property(proxy, "city_work", "visibility", TRUE)
      update_mapboxer(proxy, session$ns("map"))
    } else {
      proxy <- set_layout_property(proxy, "city_work", "visibility", FALSE)
      update_mapboxer(proxy, session$ns("map"))
    }
    if (input$show_traffic) {
      proxy <- set_layout_property(proxy, "traffic", "visibility", TRUE)
      update_mapboxer(proxy, session$ns("map"))
    } else {
      proxy <- set_layout_property(proxy, "traffic", "visibility", FALSE)
      update_mapboxer(proxy, session$ns("map"))
    }
  })

  observe({
    if (is.null(input$parking_status)) {
      return()
    }
    proxy <- mapboxer_proxy(session$ns("map"))

    if (input$parking_status == "None") {
      proxy <- set_layout_property(proxy, "parking", "visibility", FALSE)
      update_mapboxer(proxy, session$ns("map"))
      return()
    } else {
      proxy <- set_layout_property(proxy, "parking", "visibility", TRUE)
    }
    parking_color <- switch(input$parking_status,
      "Unoccupied" = list("case", list("==", c("get", "status_description"), "Unoccupied"), "green", "transparent"),
      "Occupied" = list("case", list("==", c("get", "status_description"), "Present"), "red", "transparent"),
      "All" = list(
        "case",
        list("==", c("get", "status_description"), "Unoccupied"), "green",
        list("==", c("get", "status_description"), "Present"), "red",
        "transparent" 
      )
    )

    proxy <- set_paint_property(proxy, "parking", "circle-color", parking_color)
    update_mapboxer(proxy, session$ns("map"))
  })
}




## To be copied in the UI
# mod_map_ui("map")

## To be copied in the server
# callModule(mod_map_server, "map")
