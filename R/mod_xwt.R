# This file is part of the WaveleT package
# Copyright 2015 2016 Lise Vaudor
# This program is distributed under the GPLv3 license

# This file is part of the WaveleT package
# Copyright 2015 2016 Lise Vaudor
# This program is distributed under the GPLv3 license
#' Module DWT (Discrete Wavelet Transform)
#' UI
#' @param id identifiant du module
#' @export
mod_xwt_ui <- function(id) {

  ns <- NS(id)
  tabPanel("XWT spectrum",
           actionButton(inputId=ns("i10"),label=tags$img(src = "www/catpaw.png", width = "30px", height = "30px")),
           uiOutput(ns("info10")),
           wellPanel(
             fluidRow(
               column(width=3,
                      selectInput(inputId=ns("XWT_filter"),
                                  label="Filter",
                                  choices=c("morlet", "paul", "dog"),
                                  selected="morlet")
               ),
               column(width=3,
                      selectInput(inputId=ns("XWT_info"),
                                  label="Info",
                                  choices=c("wavelet",
                                            "power",
                                            "power.corr.norm",
                                            "phase",
                                            "rsq"),
                                  selected="wavelet")
               ),
               column(width=2,
                      checkboxInput(inputId=ns("XWT_plot_sig"),
                                    label="Show significance",
                                    value=FALSE),
                      checkboxInput(inputId=ns("XWT_plot.cb"),
                                    label="Show colorbar",
                                    value=FALSE),
                      checkboxInput(inputId=ns("plot.phase"),
                                    label="Plot phase arrows",
                                    value=FALSE)
               ),
               column(width=2,
                      sliderInput(inputId=ns("nrands"),
                                  label="nrands",
                                  min=10,max=300,step=10,value=10,ticks=FALSE)
               ),
               column(width=2,
                      sliderInput(inputId=ns("XWT_alpha"),
                                  label="Alpha",
                                  min=0,max=1,step=0.01,value=0.05,ticks=FALSE)
               )
             )#fluidRow
           ),#wellPanel
           fluidRow(column(width=4,
                           wellPanel(
                             selectInput(inputId=ns("XWT_plot_type"),
                                         label="Plot",
                                         choices=c("1) Info=f(x,T)",
                                                   "2) Info=f(x)",
                                                   "3) Power=f(T)"),
                                         selected="1) Info=f(x,T)"),
                             conditionalPanel(
                               condition ="input.XWT_plot_type=='2) Info=f(x)'",
                               textInput(inputId=ns("XWT_period"),
                                         label="Period T",
                                         value="0"),
                               checkboxInput(inputId=ns("XWT_plot_maxima"),
                                             label="show local maxima",
                                             value=FALSE),
                               checkboxInput(inputId=ns("XWT_plot_minima"),
                                             label="show local minima",
                                             value=FALSE)
                             ),#conditionalPanel
                             uiOutput(ns("slider_xlim_xwt")),
                             uiOutput(ns("slider_ylim_xwt")),
                             textOutput(ns("hover_xwt_x")),
                             textOutput(ns("hover_xwt_y")),
                             downloadButton(ns("downloadInfoXWT"),"Table"),
                             downloadButton(ns("downloadFigXWT"),"Figure")
                           )#wellPanel
           ), #column
           column(width=8,
                  plotOutput(ns("plotXWT"),hover=ns("hover_xwt"),width="100%",height="100%")
           )#column
           )#fluidRow
  )#tabPanel
}

#' Module serveur pour les analyses XWT
#' @noRd
#' @export
#' @import shiny
mod_xwt_server <- function(id, app_options,app_data) {
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    XWT=reactive({
      biwavelet::xwt(app_data()$xy1y2[,c(1,2)],app_data()$xy1y2[,c(1,3)],
          mother=input$XWT_filter,
          sig.level=1-input$XWT_alpha)
    })
    WTC=reactive({
      biwavelet::wtc(app_data()$xy1y2[,c(1,2)],app_data()$xy1y2[,c(1,3)],
          mother=input$XWT_filter,
          sig.level=1-input$XWT_alpha,
          nrands=input$nrands, s0=max(1,app_data()$step))
    })
    f_plotXWT=function(){
      if(input$XWT_info!="rsq" | input$XWT_plot_type=="3) Power=f(T)"){mywt=XWT()}
      if(input$XWT_info=="rsq" & input$XWT_plot_type!="3) Power=f(T)"){mywt=WTC()}
      plotWT(mywt,
             data=app_data()$xy1y2,
             plot.type=input$XWT_plot_type,
             plot.sig=input$XWT_plot_sig,
             alpha=input$XWT_alpha,
             info=input$XWT_info,
             mother=input$XWT_filter,
             myperiod=as.numeric(input$XWT_period),
             plot.maxima=input$XWT_plot_maxima,
             plot.minima=input$XWT_plot_minima,
             step=numeric_step(app_data()$step),
             x_is_date=app_data()$x_is_date,
             plot.cb=input$XWT_plot.cb,
             xlim=input$xlimxwt,
             ylim=input$ylimxwt,
             plot.phase=input$plot.phase
      )
    }


    output$plotXWT=renderPlot({
      f_plotXWT()
    },
    height=reactive({app_options()$height}),
    width=reactive({app_options()$width})
    )

    output$slider_xlim_xwt <- renderUI({
      req(app_data())
      fxslider(ns("xlimxwt"), data = app_data()$xy1y2)
    })

    output$slider_ylim_xwt <- renderUI({
      req(app_data())
      flogyslider(ns("ylimxwt"), data = app_data()$xy1y2, step=numeric_step(app_data()$step))
    })
    ####################################################
    output$downloadFigXWT <- makeDownloadHandler(id=1,
                                                  fig_or_info="Fig",
                                                  options=app_options(),
                                                  input=input,
                                                  content_fun=function() f_plotXWT(),
                                                  analysis="XWT",
                                                  variable=paste0(app_data()$y1,"_",app_data()$y2))
    ####################################################
    output$downloadInfoXWT <- makeDownloadHandler(id=2,
                                                  fig_or_info="Info",
                                                  input=input,
                                                  content_fun=function() f_plotXWT(),
                                                  analysis="XWT",
                                                  variable=paste0(app_data()$y1,"_",app_data()$y2))

    output$info10=renderUI({
      if(input$i10%%2!=0){wellPanel(withMathJax(includeHTML(app_sys("app/www/info_XWT.html"))))}
    })


    makeHoverOutputs("xwt",NULL, input, output,app_data)

})
}
