library(shiny)
library(golem)
library(waveleT)  # ton package golem

run_app <- function(...) {
  with_golem_options(
    app = shinyApp(ui = waveleT::app_ui, server = waveleT::app_server),
    golem_opts = list(...)
  )
}

options("shiny.port" = 3840, "shiny.host" = "0.0.0.0", "golem.app.prod" = TRUE)
run_app()
