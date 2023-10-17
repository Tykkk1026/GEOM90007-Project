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
mod_ts_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("Data"),
    br(),
    fluidRow(
      column(
        6,
        uiOutput(ns("country_select_generated"))
      ),
      column(
        6,
        shinyWidgets::radioGroupButtons(
          inputId = ns("value"),
          label = "Metric",
          choices = c("rank", "score"),
          checkIcon = list(yes = icon("ok", lib = "glyphicon"))
        )
      )
    ),
    br(),
    actionButton(ns("btn_show_graph1"), "Graph 1"),
    actionButton(ns("btn_show_graph2"), "Graph 2"),
    br(),
    br(),

  )
}

    
# Module Server
    
#' @rdname mod_ts
#' @export
#' @keywords internal
    
mod_ts_server <- function(input, output, session){

}
