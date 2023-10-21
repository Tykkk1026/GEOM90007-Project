
app_server <- function(input, output, session) {


  callModule(mod_data_server, "data")
  callModule(mod_map_server, "map")

}
