#' Application server
#' @noRd
#' @export
app_server <- function(input, output, session) {


  app_options=waveleT::mod_options_server("options")
  app_data=waveleT::mod_data_server("data",app_options)

  output$show_y2_panel <- reactive({req(app_data());app_data()$show_y2})
  outputOptions(output, "show_y2_panel", suspendWhenHidden = FALSE)

  waveleT::mod_fourier_server("fourier",app_options,app_data)
  waveleT::mod_dwt_server("dwt",app_options,app_data)
  waveleT::mod_cwt_server("cwt",app_options,app_data)
  waveleT::mod_xwt_server("xwt",app_options,app_data)

}

#   par(mar = c(4, 4, 3, 2))
#   options("max.contour.segments" = 100000)
#   options(shiny.maxRequestSize = 30 * 1024^2)
#


