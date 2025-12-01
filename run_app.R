library(shiny)
library(golem)
library(waveleT)  # ton package golem

run_app <- function(...) {
  with_golem_options(
    app = shinyApp(ui = waveleT::app_ui, server = waveleT::app_server),
    golem_opts = list(...)
  )
}


run_app()
