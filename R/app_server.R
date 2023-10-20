#' @import shiny
app_server <- function(input, output, session) {


  callModule(mod_ts_server, "ts")
  callModule(mod_map_server, "map")

}
