#' Generate a CWT series panel
#'
#' This function creates a fluidRow containing a wellPanel for a CWT series
#' (either y1 or y2) with selectable plot type, sliders, conditional inputs,
#' hover outputs, and download buttons. The function adjusts all input/output
#' IDs according to the specified series number.
#'
#' @param ns A namespace function (from `NS(id)`) for module namespacing.
#' @param series_id Integer, either 1 or 2, specifying which series panel to create.
#' @return A `shiny.tag.list` containing the fluidRow for the specified CWT series.
#' @examples
#' ns <- NS("cwt")
#' cwt_series_panel_ui(ns, 1)
#' cwt_series_panel_ui(ns, 2)
#' @export
cwt_series_panel_ui <- function(ns, series_id = 1) {
  if (!series_id %in% c(1,2)) stop("series_id must be 1 or 2")
  plot_type_id <- paste0("CWT_plot_type", series_id)
  slider_xlim_id <- paste0("slider_xlim_cwt", series_id)
  slider_ylim_id <- paste0("slider_ylim_cwt", series_id)
  period_id <- paste0("period", series_id)
  maxima_id <- paste0("plot.maxima", series_id)
  minima_id <- paste0("plot.minima", series_id)
  hover_x_id <- paste0("hover_cwt", series_id, "_x")
  hover_y_id <- paste0("hover_cwt", series_id, "_y")
  download_info_id <- paste0("downloadInfoCWT", series_id)
  download_fig_id <- paste0("downloadFigCWT", series_id)
  plot_id <- paste0("plotCWT", series_id)

  fluidRow(
    column(width = 4,
           wellPanel(
             h3(paste0("Series y", series_id)),
             selectInput(inputId = ns(plot_type_id),
                         label = "Plot type",
                         choices = c("1) Info=f(x,T)",
                                     "2) Info=f(x)",
                                     "3) Power=f(T)"),
                         selected = "1) Info=f(x,T)"),
             uiOutput(ns(slider_xlim_id)),
             uiOutput(ns(slider_ylim_id)),
             conditionalPanel(
               condition = paste0("input.", plot_type_id, "=='2) Info=f(x)'"),
               textInput(inputId = ns(period_id),
                         label = "Period T",
                         value = "0"),
               checkboxInput(inputId = ns(maxima_id),
                             label = "Show local maxima",
                             value = FALSE),
               checkboxInput(inputId = ns(minima_id),
                             label = "Show local minima",
                             value = FALSE)
             ),
             textOutput(ns(hover_x_id)),
             textOutput(ns(hover_y_id)),
             downloadButton(ns(download_info_id), "Table"),
             downloadButton(ns(download_fig_id), "Figure")
           )
    ),
    column(width = 8,
           plotOutput(ns(plot_id),
                      hover = ns(paste0("hover_cwt", series_id)),
                      width = "100%", height = "100%")
    )
  )
}


# This file is part of the WaveleT package
# Copyright 2015 2016 Lise Vaudor
# This program is distributed under the GPLv3 license

#' Module CWT (Continuous Wavelet Transform)
#' UI
#' @param id identifiant du module
#' @param app_data app_data
#' @import shiny
#' @export
mod_cwt_ui <- function(id,app_data) {
  ns <- NS(id)
  tabPanel("CWT spectrum",
           actionButton(inputId=ns("i9"),label=tags$img(src = "www/catpaw.png", width = "30px", height = "30px")),
           uiOutput("info9"),br(),
           wellPanel(
             fluidRow(
               column(width=3,
                      selectInput(inputId=ns("CWT_filter"),
                                  label="Filter",
                                  choices=c("morlet", "paul", "dog"),
                                  selected="morlet")
               ),
               column(width=3,
                      selectInput(inputId=ns("info"),
                                  label="Info",
                                  choices=c("wavelet",
                                            "power",
                                            "power.corr.norm"),
                                  selected="wavelet")
               ),
               column(width=3,
                      checkboxInput(inputId=ns("plot.sig"),
                                    label="Show significance",
                                    value=FALSE),
                      checkboxInput(inputId=ns("plot.cb"),
                                    label="Show colorbar",
                                    value=FALSE)
               ),
               column(width=3,
                      sliderInput(inputId=ns("alpha"),
                                  label="Alpha",
                                  min=0,max=1,step=0.01,value=0.05,ticks=FALSE)
               )
             )#fluidRow
           ),#wellPanel
           cwt_series_panel_ui(ns, 1),
           conditionalPanel(condition="output.show_y2_panel",
                            cwt_series_panel_ui(ns, 2))
  )
}

#' Module serveur pour les analyses CWT
#' @noRd
#' @import shiny
#' @export
mod_cwt_server <- function(id, app_options, app_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # ---- Réactifs CWT ----
    f_CWT1=function(){
      req(app_data())
      biwavelet::wt(app_data()$xy1y2[, c(1,2)],
                    mother = input$CWT_filter,
                    sig.level = 1 - input$alpha)
    }

    f_CWT2=reactive({
      req(app_data())
      biwavelet::wt(app_data()$xy1y2[, c(1,3)],
                    mother = input$CWT_filter,
                    sig.level = 1 - input$alpha)
    })


    # =======================================================
    # ---- Sliders (doivent exister avant les plots) ----
    # =======================================================
    output$slider_xlim_cwt1=renderUI({
      req(app_data())
      fxslider(ns("xlimcwt1"), data = app_data()$xy1y2)
    })

    output$slider_ylim_cwt1=renderUI({
      req(app_data())
      flogyslider(ns("ylimcwt1"), data = app_data()$xy1y2, step=numeric_step(app_data()$step))
    })

    output$slider_xlim_cwt2 <- renderUI({
      req(app_data())
      fxslider(ns("xlimcwt2"), data = app_data()$xy1y2)
    })

    output$slider_ylim_cwt2 <- renderUI({
      req(app_data())
      flogyslider(ns("ylimcwt2"), data = app_data()$xy1y2, step=numeric_step(app_data()$step))
    })


    # =======================================================
    # ---- Fonctions de plot protégées par req() ----
    # =======================================================
    f_plotCWT1 <- function(){
      req(input$xlimcwt1, input$ylimcwt1)
      mywt <- f_CWT1()
      plotWT(mywt,
             data = app_data()$xy1y2[, c(1,2)],
             plot.type   = input$CWT_plot_type1,
             plot.sig    = input$plot.sig,
             alpha       = input$alpha,
             info        = input$info,
             mother      = input$CWT_filter,
             myperiod    = as.numeric(input$period1),
             plot.maxima = input$plot.maxima1,
             plot.minima = input$plot.minima1,
             step        = numeric_step(app_data()$step),
             x_is_date   = app_data()$x_is_date,
             plot.cb     = input$plot.cb,
             xlim        = input$xlimcwt1,
             ylim        = input$ylimcwt1,
             plot.phase  = FALSE)
    }

    f_plotCWT2 <- function(){
      req(input$xlimcwt2, input$ylimcwt2)
      mywt <- f_CWT2()
      res=plotWT(mywt,
             data = app_data()$xy1y2[, c(1,3)],
             plot.type   = input$CWT_plot_type2,
             plot.sig    = input$plot.sig,
             alpha       = input$alpha,
             info        = input$info,
             mother      = input$CWT_filter,
             myperiod    = as.numeric(input$period2),
             plot.maxima = input$plot.maxima2,
             plot.minima = input$plot.minima2,
             step        = numeric_step(app_data()$step),
             x_is_date   = app_data()$x_is_date,
             plot.cb     = input$plot.cb,
             xlim        = input$xlimcwt2,
             ylim        = input$ylimcwt2,
             plot.phase  = FALSE)
      res
    }


    # =======================================================
    # ---- Plots ----
    # =======================================================
    output$plotCWT1 <- renderPlot({
      req(input$xlimcwt1, input$ylimcwt1)
      f_plotCWT1()
    },
    height = reactive({ app_options()$height }),
    width  = reactive({ app_options()$width }))

    output$plotCWT2 <- renderPlot({
      req(input$xlimcwt2, input$ylimcwt2)
      f_plotCWT2()
    },
    height = reactive({ app_options()$height }),
    width  = reactive({ app_options()$width }))

    # Downloads
    output$downloadFigCWT1 <- makeDownloadHandler(id=1,
                                                 fig_or_info="Fig",
                                                 options=app_options(),
                                                 input=input,
                                                 content_fun=function() f_plotCWT1(),
                                                 analysis="CWT",
                                                 variable=app_data()$y1)
    output$downloadFigCWT2 <- makeDownloadHandler(id=2,
                                                  fig_or_info="Fig",
                                                  options=app_options(),
                                                  input=input,
                                                  content_fun=function() f_plotCWT2(),
                                                  analysis="CWT",
                                                  variable=app_data()$y2)

    output$downloadInfoCWT1 <- makeDownloadHandler(id=1,
                                                  fig_or_info="Info",
                                                  input=input,
                                                  content_fun=function() f_plotCWT1(),
                                                  analysis="CWT",
                                                  variable=app_data()$y1)
    output$downloadInfoCWT2 <- makeDownloadHandler(id=2,
                                                  fig_or_info="Info",
                                                  input=input,
                                                  content_fun=function() f_CWT2(),
                                                  analysis="CWT",
                                                  variable=app_data()$y2)


    # =======================================================
    # ---- Info + hover ----
    # =======================================================
    output$info9 <- renderUI({if (input$i9 %% 2 != 0) {wellPanel(includeHTML(app_sys("app/www/info_CWT.html")))}})



    makeHoverOutputs("cwt",1, input, output,app_data)
    makeHoverOutputs("cwt",2, input, output,app_data)

  })
}
