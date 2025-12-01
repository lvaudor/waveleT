#' Application UI
#' @noRd
#' @export
app_ui <- function(request) {
  tagList(
  golem_add_external_resources(),
  navbarPage("My Wavelet Toolkit",
             tabPanel(title="INFO",
                      icon=icon("info",lib="font-awesome"),
                      fluidRow(
                        column(width=4,offset=1,
                               br(),br(),br(),
                               tags$img(src="www/logo_toolkat_small.png",height="100%", width="100%")
                        ),
                        column(width=7,
                               h2("The Wavelet ToolKat 1.1"),
                               h4("L. Vaudor, ISIG, UMR 5600"),
                               h5("A set of tools for the analysis of series through wavelet transforms"),
                               wellPanel(includeHTML(app_sys("app/www/info_presentation_1.html"))),
                               p("Information panels are marked by: this icon:"),
                               tags$img(src = "www/catpaw.png", width = "30px", height = "30px")
                        )
                      ),#fluidRow
                      wellPanel(includeHTML(app_sys("app/www/info_presentation_2.html")))
             ),#tabPanel
             tabPanel(title = "DATA",
                      icon = icon("folder", lib = "font-awesome"),
                      waveleT::mod_data_ui("data")
                      ),
             tabPanel(title = "ANALYSES",
                      icon = icon("cogs", lib = "font-awesome"),
                      tabsetPanel(
                        tabPanel(title = "Fourier",
                                 icon = icon("align-left", lib = "glyphicon"),
                                 mod_fourier_ui("fourier")
                        ),
                        tabPanel(title = "DWT",
                                 icon = icon("line-chart", lib = "font-awesome"),
                                 mod_dwt_ui("dwt")
                        ),
                        tabPanel(title = "CWT",
                                 icon = icon("table", lib = "font-awesome"),
                                 mod_cwt_ui("cwt",app_data)
                        ),
                        tabPanel(title = "XWT",
                                 icon = icon("table", lib = "font-awesome"),
                                 conditionalPanel(
                                   condition = "output.show_y2_panel==false",
                                   HTML("<h5>This analysis compares two signals. It is not available here because you have no signal y<sub>2</sub>.</h5>")
                                 ),
                                 conditionalPanel(
                                   condition = "output.show_y2_panel",
                                   mod_xwt_ui("xwt")
                                 )
                        )
                      )#tabsetPanel
              ),#tabPanel ANALYSES
             tabPanel("OPTIONS",
                      icon = icon("file-image-o", lib = "font-awesome"),
                      mod_options_ui("options")
             )
  )#navbarPage
  )#tagList
}

