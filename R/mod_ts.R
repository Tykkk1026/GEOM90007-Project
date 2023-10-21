# Module UI


source("R/tableau-in-shiny-v1.0.R")
mod_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        h2("Data Visualization"),
        actionButton(ns("btn_show_graph1"), "Graph 1"),
        actionButton(ns("btn_show_graph2"), "Graph 2"),
        actionButton(ns("btn_show_graph3"), "Graph 3")
      )
    ),
    fluidRow(
      column(
        12,
        uiOutput(ns("tableauOutput")),
        uiOutput(ns("tableauOutputRain")),
        uiOutput(ns("tableauOutputCrash"))
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
        url = "https://public.tableau.com/views/BicycleCount/Sheet1?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link",
      )
    )
  })
}

load_rain_content <- function(output, session) {
  ns <- session$ns
  output$tableauOutputRain <- renderUI({
    div(style = "display: block; margin-left: auto; margin-right: auto; width: 1000px; height: 600px;",
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
    div(style = "display: block; margin-left: auto; margin-right: auto; width: 1500px; height: 600px;",
      tableauPublicViz(
        id = ns("tableauViz"),
        url = "https://public.tableau.com/views/numberofcrashesofeachhour/Sheet1?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link",
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
  })

    observeEvent(input$btn_show_graph2, {
    load_rain_content(output, session)
    output$tableauOutput <- renderUI({})
    output$tableauOutputCrash <- renderUI({})
  })

    observeEvent(input$btn_show_graph3, {
    load_crash_content(output, session)
    output$tableauOutput <- renderUI({})
    output$tableauOutputRain <- renderUI({})
  })

}
