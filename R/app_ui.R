
source("R/tableau-in-shiny-v1.0.R")
app_ui <- function() {
  options <- list(
    normalScrollElements = "body"
  )


  tagList(
    # External resources
    setUpTableauInShiny(),
    golem_add_external_resources(),
    # The actual UI
    pagePiling(
      sections.color = c("#2f2f2f", "#2f2f2f", "#f9f7f1", "#2f2f2f", "#f9f7f1", "#8a0f0f"),
      opts = options,
      # Add a custom class to the menu
      menu = c(
        "Home" = "home",
        "Map" = "map",
        "Data Visualization" = "data",
        "About" = "about"
      ),
      pageSectionImage(
        center = TRUE,
        img = "www/img/mel4.jpg",
        menu = "home",
        div( 
          style = "position: absolute; top: 0; left: 0; width: 45%; height: 100%; background-color: rgba(0, 0, 0, 0.5); z-index: 1;"
        ),
        div( 
          style = "position: absolute; top: 10%; left: -2%; z-index: 2;",
          h1("Melbourne", class = "header shadow-dark", style = "font-size: 5em; margin-bottom: 200px; max-width: 40%;"), 
          p("This app is designed to assist commuters on their daily journeys. Whether you're traveling by public transport, car, cycling or walking, our aim is to streamline your experience, provide timely information, and ensure that commuting is smooth and stress-free.", 
            style = "color: white; max-width: 38%; text-align: left; font-size: 2em; margin-left: 100px;")
        )
      ),


      pageSection(
        center = TRUE,
        menu = "map",
        mod_map_ui("map")
      ),
      pageSection(
        center = TRUE,
        menu = "data",
        mod_data_ui("data")
      ),
      pageSection(
        center = TRUE,
        menu = "about",
        div(
          style = "padding: 2%; width: 100%; display: flex; flex-direction: column; align-items: center; overflow-y: auto; max-height: 90vh;",
          div( # First (left aligned)
            style = "width: 45%; padding: 1%; margin-bottom: 0.5em; align-self: flex-start;",
            h3("Introduction:", style = "color: white; text-align: left; font-size: 2.4em;"),
            p("Welcome to our interactive visualization platform, tailor-made for local citizens of Melbourne who often commute to the CBD. With the surge of big and open data, it's crucial to harness this information effectively. Our goal is to help commuters discover practical information about the City of Melbourne and assist in making daily travel decisions.",
              style = "color: white; text-align: left; max-width: 30em; font-size: 1.7em;"
            )
          ),
          div( # Second (right aligned)
            style = "width: 45%; padding: 1%; margin-bottom: 0.5em; align-self: flex-end;",
            h3("Aim:", style = "color: white; text-align: left; font-size: 2.4em;"),
            p("Our primary objectives are to assist local commuters in identifying efficient routes, understand daily commuting patterns, provide insights into the most frequented places, and offer an intuitive interface for a seamless user experience.",
              style = "color: white; text-align: left; max-width: 30em; font-size: 1.7em;"
            )
          ),
          div( # Third (left aligned)
            style = "flex: 1; padding: 2%; align-self: flex-start;",
            h3("Data Used:", style = "color: white; text-align: left; font-size: 2.5em;"),
            p("Here is the Data Source used in this project:",
              style = "color: white; text-align: left; max-width: 30em; font-size: 1.5em;"
            ),
            tags$ul(
              style = "list-style-type: disc; color: white; padding-left: 1em; text-align: left;",
              tags$li(tags$a("PTV Train Corridor Centreline", href = "https://discover.data.vic.gov.au/dataset/ptv-train-corridor-centreline", style = "color: white; text-decoration: underline; font-size: 1.5em;")),
              tags$li(tags$a("PTV Metro Tram Stops", href = "https://discover.data.vic.gov.au/dataset/ptv-metro-tram-stops", style = "color: white; text-decoration: underline; font-size: 1.5em;")),
              tags$li(tags$a("PTV Metro Train Stations", href = "https://discover.data.vic.gov.au/dataset/ptv-metro-train-stations", style = "color: white; text-decoration: underline; font-size: 1.5em;")),
              tags$li(tags$a("PTV Metro Tram Routes", href = "https://discover.data.vic.gov.au/dataset/ptv-metro-tram-routes", style = "color: white; text-decoration: underline; font-size: 1.5em;")),
              tags$li(tags$a("PTV Metro Bus Routes", href = "https://discover.data.vic.gov.au/dataset/ptv-metro-bus-routes", style = "color: white; text-decoration: underline; font-size: 1.5em;")),
              tags$li(tags$a("PTV Metro Bus Stops", href = "https://discover.data.vic.gov.au/dataset/ptv-metro-bus-stops", style = "color: white; text-decoration: underline; font-size: 1.5em;")),
              tags$li(tags$a("Café, restaurant, bistro seats", href = "https://data.melbourne.vic.gov.au/explore/dataset/cafes-and-restaurants-with-seating-capacity/information/", style = "color: white; text-decoration: underline; font-size: 1.5em;")),
              tags$li(tags$a("Public toilets", href = "https://discover.data.vic.gov.au/dataset/public-toilets", style = "color: white; text-decoration: underline; font-size: 1.5em;")),
              tags$li(tags$a("Traffic Volume", href = "https://discover.data.vic.gov.au/dataset/traffic-volume", style = "color: white; text-decoration: underline; font-size: 1.5em;")),
              tags$li(tags$a("On-street Parking Bay Sensors", href = "https://data.melbourne.vic.gov.au/explore/dataset/on-street-parking-bay-sensors/information/", style = "color: white; text-decoration: underline; font-size: 1.5em;")),
              tags$li(tags$a("City Activities and Planned Works", href = "https://discover.data.vic.gov.au/dataset/city-activities-and-planned-works", style = "color: white; text-decoration: underline; font-size: 1.5em;")),
              tags$li(tags$a("Tree Canopies 2021 (Urban Forest)", href = "https://data.melbourne.vic.gov.au/explore/dataset/tree-canopies-2021-urban-forest/information/", style = "color: white; text-decoration: underline; font-size: 1.5em;")),
              tags$li(tags$a("Feature Lighting (including light type, wattage and location)", href = "https://discover.data.vic.gov.au/dataset/feature-lighting-including-light-type-wattage-and-location", style = "color: white; text-decoration: underline; font-size: 1.5em;")),
            )
          ),
          div( # Fourth (right aligned)
            style = "width: 45%; padding: 1%; align-self: flex-end;",
            h3("Contact Us:", style = "color: white; text-align: left; font-size: 2.4em;"),
            p("For any questions, feedback and queries, reach out at:",
              tags$a("yikunt@student.unimelb.edu.au", style = "color: white; text-decoration: underline; font-size: 1.5em;"),
              style = "color: white; text-align: left; max-width: 30em; font-size: 1.7em;"
            )
          )
        )
      )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function() {
  addResourcePath(
    "www", system.file("app/www", package = "app")
  )

  tags$head(
    golem::activate_js(),
    golem::favicon(),
    tags$link(
      rel = "stylesheet", href = shinythemes::shinytheme("sandstone")
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/css/style.css"),
    tags$script(src = "www/script.js"),
    tags$script(async = NA, src = "https://www.googletagmanager.com/gtag/js?id=UA-74544116-1"),
    tags$script(
      "window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());
  gtag('config', 'UA-74544116-1');"
    )
  )
}
