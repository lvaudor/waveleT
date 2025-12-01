#' Generate a Fourier series panel
#'
#' This function creates a fluidRow containing a panel for a Fourier series
#' (either y1 or y2) with hover outputs, download buttons, and the plot.
#' All input/output IDs are adjusted according to the specified series number.
#'
#' @param ns A namespace function (from NS(id)) for module namespacing.
#' @param series_id Integer, either 1 or 2, specifying which series panel to create.
#' @return A shiny.tag.list containing the fluidRow for the specified Fourier series.
#' @examples
#' ns <- NS("fourier")
#' fourier_series_panel_ui(ns, 1)
#' fourier_series_panel_ui(ns, 2)
#' @export
fourier_series_panel_ui <- function(ns, series_id = 1) {
  if (!series_id %in% c(1,2)) stop("series_id must be 1 or 2")

  hover_x_id <- paste0("hover_fourier", series_id, "_x")
  hover_y_id <- paste0("hover_fourier", series_id, "_y")
  download_table_id <- paste0("dltabFourier", series_id)
  download_fig_id <- paste0("dlfigFourier", series_id)
  plot_id <- paste0("plotFourier", series_id)
  hover_id <- paste0("hover_fourier", series_id)

  fluidRow(
    column(width = 4,
           h3(paste0("Series y", series_id)),
           textOutput(ns(hover_x_id)),
           textOutput(ns(hover_y_id)),
           downloadButton(ns(download_table_id), "Table"),
           downloadButton(ns(download_fig_id), "Figure")
    ),
    column(width = 8,
           plotOutput(ns(plot_id),
                      hover = ns(hover_id),
                      width = "100%", height = "100%")
    )
  )
}


#' Module data
#' UI
#' @param id identifiant du module
#' @export
#'
mod_fourier_ui <- function(id) {
  ns <- NS(id)
  wellPanel(
    actionButton(inputId=ns("i7"),label=tags$img(src = "www/catpaw.png", width = "30px", height = "30px")),
    uiOutput("info7"),
    fourier_series_panel_ui(ns, 1),
    conditionalPanel(condition="output.show_y2_panel",
                     fourier_series_panel_ui(ns, 2))
  )
}

#' Module serveur pour les analyses Fourier
#' @noRd
#' @import shiny
#' @export
mod_fourier_server <- function(id,app_options,app_data) {
  shiny::moduleServer(id, function(input, output, session) {
  ns <- session$ns
  # --- Plot functions ---
  f_plotFourier <- function(y) {#y=1 ou y=2
    plotFourier(
      data = app_data()$xy1y2[, c(1, y+1)],
      mystep = numeric_step(app_data()$step),
      x_is_date = app_data()$x_is_date
    )
  }

  # --- Render plots ---
  output$plotFourier1 <- renderPlot({
    f_plotFourier(1)
  },
  height = reactive({app_options()$height}),
  width =  reactive({app_options()$width }))

  output$plotFourier2 <- renderPlot({
    f_plotFourier(2)
  },
  height = reactive({app_options()$height}),
  width =  reactive({app_options()$width }))

  # --- Download Fourier tables ---
  makeDownloadFourierTab <- function(n, app_data) {
    downloadHandler(
      filename = function() { paste0("tabFourier", n, ".csv") },
      content = function(file) {
        cols <- if (n == 1) 1:2 else 1:3
        fourier_results <- calcFourier(app_data()$xy1y2[, cols], app_data()$step)
        write.table(fourier_results, file, sep = ";", row.names = FALSE)
      }
    )
  }

  # puis tu crées les outputs
  output$dltabFourier1 <- makeDownloadFourierTab(1, app_data)
  output$dltabFourier2 <- makeDownloadFourierTab(2, app_data)


  # --- Download Fourier figures ---
  makeDownloadFourier <- function(n, f_plot_fun, app_options) {
    downloadHandler(
      filename = function() {
        paste0("Fig_Fourier", n, ".", app_options()$graph_format)
      },
      content = function(file) {
        fgraph <- get(app_options()$graph_format)
        fgraph(file, width = app_options()$width, height = app_options()$height)
        f_plot_fun(n)
        dev.off()
      }
    )
  }

  # puis tu crées les outputs
  output$dlfigFourier1 <- makeDownloadFourier(1, f_plotFourier, app_options)
  output$dlfigFourier2 <- makeDownloadFourier(2, f_plotFourier, app_options)


  output$info7=renderUI({
    if(input$i7%%2!=0){wellPanel(includeHTML(app_sys("app/www/info_data_real_1.html")))}
  })

  ############

  makeHoverOutputs("fourier",1, input, output,app_data)
  makeHoverOutputs("fourier",2, input, output,app_data)
  })
}

