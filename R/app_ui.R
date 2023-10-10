#' @import shiny
app_ui <- function() {
   options <- list()

  tagList(
    # External resources
    golem_add_external_resources(),
    
    # The actual UI
    pagePiling(
      sections.color = c("#2f2f2f", "#2f2f2f", "#f9f7f1", "#2f2f2f", "#f9f7f1", "#8a0f0f"),
      opts = options,
      
      # Add a custom class to the menu
      menu = c(
  "Home" = "home",
  "Map" = "map",
  "Series" = "ts",
  "About" = "about",
  "Unknown" = "unknown", 
  "Extra" = "extra"
),
      pageSectionImage(
        center = TRUE,
        img = "www/img/mel.jpg",
        menu = "home",
        h1("Melbourne", class = "header shadow-dark"),
        h3(
          class = "light footer",
          "by Team XXXX GEOM90007"
        )
      ),
      pageSection(
        center = TRUE,
        menu = "map",
        mod_map_ui("map")
      ),
      pageSection(
        center = TRUE,
        menu = "ts",
        mod_ts_ui("ts")
      ),
      pageSection(
        center = TRUE,
        menu = "about",
        h1("About", class = "header shadow-dark"),
        h2(
          class = "shadow-light",
          "This is a shiny app for the  project of GEOM90007"
        ),
        h3(
          class = "light footer",
          "by", tags$a("news-r", href = "https://news-r.org", class = "link"), "with", emo::ji("coffee")
        )
      ),
      # Existing "Unknown" section content
      pageSection(
        center = TRUE,
        menu = "unknown",
        h1("Unknown Content", class = "header shadow-dark"),
        h2("This section is under development.")
      ),
      # New "Extra" section content
      pageSection(
        center = TRUE,
        menu = "extra",
        h1("Extra Content", class = "header shadow-dark"),
        h2("This section is for additional content.")
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
    tags$script(async = NA, src = "https://www.googletagmanager.com/gtag/js?id=UA-74544116-1"),
    tags$script(
      "window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', 'UA-74544116-1');"
    )
  )
}
