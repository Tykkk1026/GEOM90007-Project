# Module UI


source("R/tableau-in-shiny-v1.0.R")
mod_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        h2("Data Visualization"),
        actionButton(ns("btn_show_graph1"), "Public Transport"),
        actionButton(ns("btn_show_graph2"), "Rainfall"),
        actionButton(ns("btn_show_graph3"), "Crash"),
        actionButton(ns("btn_show_graph4"), "Traffic Volumn")
      )
    ),
    fluidRow(
      column(
        12,
        uiOutput(ns("tableauOutput")),
        uiOutput(ns("tableauOutputRain")),
        uiOutput(ns("tableauOutputCrash")),
        uiOutput(ns("tableauOutputVolumn")),
      )
    )
  )
}


load_data_content <- function(output, session) {
  ns <- session$ns
  output$tableauOutput <- renderUI({
    div(style = "display: block; margin-left: auto; margin-right: auto; width: 1000px; height: 600px;",
      tableauPublicViz(
        id = ns("tableauViz"),
        url = "https://public.tableau.com/views/Book1_16978547183900/Sheet5?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link",
      )
    )
  })
}

load_rain_content <- function(output, session) {
  ns <- session$ns
  output$tableauOutputRain <- renderUI({
    div(style = "display: block; margin-left: auto; margin-right: auto; width: 800px; height: 600px;",
      tableauPublicViz(
        id = ns("tableauViz"),
        url = "https://public.tableau.com/views/MonthlyRainfall_16978543189640/Sheet1?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link",
      )
    )
  })

}


load_crash_content <- function(output, session) {
  ns <- session$ns
  output$tableauOutputCrash <- renderUI({
    div(style = "display: block; margin-left: auto; margin-right: auto; width: 1000px; height: 700px;",
      tableauPublicViz(
        id = ns("tableauViz"),
        url = "https://public.tableau.com/views/numberofcrashesofeachhour/Sheet1?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link",
      )
    )
  })

}

load_volumn_content <- function(output, session) {
  ns <- session$ns
  output$tableauOutputVolumn <- renderUI({
    div(style = "display: block; margin-left: auto; margin-right: auto; width: 1500px; height: 600px;",
      tableauPublicViz(
        id = ns("tableauViz"),
        url = "https://public.tableau.com/views/HourlyVehicleCount/Sheet1?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link",
      )
    )
  })

}

# Module Server

#' @rdname mod_data
#' @export
#' @keywords internal

mod_data_server <- function(input, output, session) {
  ns <- session$ns
  load_data_content(output, session)
  observeEvent(input$btn_show_graph1, {
    load_data_content(output, session)
    output$tableauOutputRain <- renderUI({})
    output$tableauOutputCrash <- renderUI({})
    output$tableauOutputVolumn <- renderUI({})
  })

    observeEvent(input$btn_show_graph2, {
    load_rain_content(output, session)
    output$tableauOutput <- renderUI({})
    output$tableauOutputCrash <- renderUI({})
    output$tableauOutputVolumn <- renderUI({})
  })

  observeEvent(input$btn_show_graph3, {
    load_crash_content(output, session)
    output$tableauOutput <- renderUI({})
    output$tableauOutputRain <- renderUI({})
    output$tableauOutputVolumn <- renderUI({})
  })

  observeEvent(input$btn_show_graph4, {
    load_volumn_content(output, session)
    output$tableauOutput <- renderUI({})
    output$tableauOutputRain <- renderUI({})
    output$tableauOutputCrash <- renderUI({})
  })

}
