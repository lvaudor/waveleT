# This file is part of the WaveleT package
# Copyright 2015 2016 Lise Vaudor
# This program is distributed under the GPLv3 license
#' Module data
#' UI
#' @param id identifiant du module
#' @export
mod_options_ui=function(id){

ns <- NS(id)
wellPanel(h4("Graphs' characteristics"),
          fluidRow(
          column(width=4,
          sliderInput(inputId=ns("height"),
                      label="height",
                      min=200,step=100,
                      max=1200,value=400),
          sliderInput(inputId=ns("width"),
                      label="width",
                      min=200,step=100,
                      max=1600,value=700)
          ),#column
          column(width=4,
                 selectInput(inputId=ns("graph_format"),
                             label="format",
                             choices=c("png","jpeg","bmp","tiff"),
                             selected="png")
          )#column
          ),#fluidRow
          plotOutput(ns("plotblank"))
)
}

#' Module serveur pour les analyses XWT
#' @noRd
#' @export
#' @import shiny
mod_options_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
  ns <- session$ns

  output$plotblank=renderPlot({
    plot(1:100,1:100, col="white",xlab="x",ylab="y", xaxt="n", yaxt="n")
  },
  height=function(x){input$height},
  width=function(x){input$width}
  )

  app_options=reactive({
    list(
      height=input$height,
      width=input$width,
      graph_format=input$graph_format)
    })

  return(app_options)
})
}
