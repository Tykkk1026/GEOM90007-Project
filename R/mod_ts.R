# Module UI

#' @title   mod_ts_ui and mod_ts_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_ts
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList

source("R/tableau-in-shiny-v1.0.R")
mod_ts_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        h2("Data Visualization"),
        actionButton(ns("btn_show_graph1"), "Graph 1"),
        actionButton(ns("btn_show_graph2"), "Graph 2")
      )
    ),
    fluidRow(
      column(
        12,
        uiOutput(ns("tableauOutput")),
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


# Module Server

#' @rdname mod_ts
#' @export
#' @keywords internal

mod_ts_server <- function(input, output, session) {
  ns <- session$ns
  load_data_content(output, session)
  observeEvent(input$btn_show_graph1, {
    load_data_content(output, session)
  })

  # 如果需要，你可以为btn_show_graph2添加另一个observeEvent来渲染另一个图表。
}
