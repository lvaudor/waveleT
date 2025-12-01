golem_add_external_resources <- function(){
  shiny::addResourcePath("www", app_sys("app/www"))
  tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "My Wavelet Toolkat"
    )
  )
}
