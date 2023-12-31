#' Launch
#' @export
#' @import fullPage
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function() {
  with_golem_options(
    app = shinyApp(ui = app_ui, server = app_server, options=list(launch.browser=TRUE)), 
    golem_opts = list()
  )
}
