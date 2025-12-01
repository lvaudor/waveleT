#' Generate a DWT series panel
#'
#' This function creates a fluidRow containing a wellPanel for a DWT series
#' (either y1 or y2) with selectable plot type, wavelet filter, conditional inputs,
#' slider, hover outputs, download buttons, and the plot. All input/output IDs
#' are adjusted according to the specified series number.
#'
#' @param ns A namespace function (from `NS(id)`) for module namespacing.
#' @param series_id Integer, either 1 or 2, specifying which series panel to create.
#' @return A `shiny.tag.list` containing the fluidRow for the specified DWT series.
#' @examples
#' ns <- NS("dwt")
#' dwt_series_panel_ui(ns, 1)
#' dwt_series_panel_ui(ns, 2)
#' @export
dwt_series_panel_ui <- function(ns, series_id = 1) {
  if (!series_id %in% c(1,2)) stop("series_id must be 1 or 2")

  dwt_type_id <- paste0("dwt_type_", series_id)
  wt_filter_id <- paste0("wt_filter_", series_id)
  yscale_id <- paste0("DWT_signal_yscale", series_id)
  plot_signal_id <- paste0("DWT_signal_plot", series_id)
  slider_xlim_id <- paste0("slider_xlim_dwt", series_id)
  hover_x_id <- paste0("hover_dwt", series_id, "_x")
  hover_y_id <- paste0("hover_dwt", series_id, "_y")
  download_table_id <- paste0("downloadDWT", series_id)
  download_fig_id <- paste0("downloadFigDWT", series_id)
  plot_id <- paste0("plotDWT", series_id)
  levels_id <- paste0("levels", series_id)

  fluidRow(
    column(width = 3,
           wellPanel(
             h3(paste0("Series y", series_id)),
             selectInput(inputId = ns(dwt_type_id),
                         label = "plot",
                         choices = c("signal decomposition","variance decomposition"),
                         selected = "signal decomposition"),
             selectInput(inputId = ns(wt_filter_id),
                         label = "wavelet filter",
                         choices = c("d2","d4","d6","d8","d10","d12","d14","d16","d18",
                                     "la8","la10","la12","la14","la16","la18","la20",
                                     "bl14","bl18","bl20",
                                     "c6","c12","c18","c24","c30"),
                         selected = "la8"),
             conditionalPanel(
               condition = sprintf("input['%s']=='signal decomposition'", ns(dwt_type_id)),
               checkboxInput(inputId = ns(yscale_id),
                             label = "y-scale fixed on raw signal",
                             value = TRUE),
               checkboxInput(inputId = ns(plot_signal_id),
                             label = "plot signal",
                             value = TRUE)
             ),
             uiOutput(ns(slider_xlim_id)),
             textOutput(ns(hover_x_id)),
             textOutput(ns(hover_y_id)),
             downloadButton(ns(download_table_id),"Table"),
             downloadButton(ns(download_fig_id),"Figure")
           )
    ),
    column(width = 1,
           conditionalPanel(
             condition = sprintf("input['%s']=='signal decomposition'", ns(dwt_type_id)),
             hr(), h6("Levels:"),
             uiOutput(ns(levels_id))
           )
    ),
    column(width = 8,
           plotOutput(ns(plot_id),
                      hover = ns(paste0("hover_dwt", series_id)),
                      width = "100%", height = "100%")
    )
  )
}
# This file is part of the WaveleT package
# Copyright 2015 2016 Lise Vaudor
# This program is distributed under the GPLv3 license
#' Module DWT (Discrete Wavelet Transform)
#' UI
#' @param id identifiant du module
#' @export
mod_dwt_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    actionButton(inputId=ns("i8"),label=tags$img(src = "www/catpaw.png", width = "30px", height = "30px")),
    uiOutput(ns("info8")),uiOutput(ns("iDWT")),br(),

    dwt_series_panel_ui(ns, 1),
    conditionalPanel(condition="output.show_y2_panel",
                     dwt_series_panel_ui(ns, 2),
                     fluidRow(
                        column(width=4,
                               wellPanel(
                                 h3("Series y1 & y2"),
                                 uiOutput(ns("slider_xlim_dwt12")),
                                 textOutput(ns("hover_dwt_x")),
                                 textOutput(ns("hover_dwt_y")),
                                 downloadButton(ns("downloadDWT_superpose"),"Table"),
                                 downloadButton(ns("downloadFigDWT_superpose"),"Figure")
                               )
                        ),
                        column(width=8,
                               plotOutput(ns("plotDWT"),hover=ns("hover_dwt"))
                        )
                      )
    )#conditionalPanel
  )#fluidRow
}


#' Module serveur pour les analyses DWT
#' @noRd
#' @import shiny
#' @export
mod_dwt_server <- function(id, app_options,app_data) {
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Compute displayed levels in UI
    output$levels1 <- renderUI({
      j <- wav_jmax(app_data()$nr)
      values=rep(FALSE,j);values[j]=TRUE
      w <- lapply(1:j, function(i) checkboxInput(ns(paste0("y1_D",i)), label=HTML(paste0("D<sub>",i,"</sub>")), value=values[i]))
      w <- tagList(w, checkboxInput(ns(paste0("y1_S",j)), label=HTML(paste0("S<sub>",j,"</sub>")), value=TRUE))
      return(tagList(w))
      })

    output$levels2 <- renderUI({
      j <- wav_jmax(app_data()$nr)
      values=rep(FALSE,j);values[j]=TRUE
      w <- lapply(1:j, function(i) checkboxInput(ns(paste0("y2_D",i)), label=HTML(paste0("D<sub>",i,"</sub>")),value=values[i]))
      w <- tagList(w, checkboxInput(ns(paste0("y2_S",j)), label=HTML(paste0("S<sub>",j,"</sub>")), value=TRUE))
      return(tagList(w))
    })

    f_specific_levels=function(signal, input){
      jmax=wav_jmax(app_data()$nr)
      subsignals=c(paste0("D",1:jmax),paste0("S",jmax))
      specific_subsignals=paste0(signal,"_",subsignals)
      selected_levels=c()
      for (j in 1:length(subsignals)){
        if(specific_subsignals[j] %in% names(input)){
          if(input[[specific_subsignals[j]]]){selected_levels=c(selected_levels,subsignals[j])}
        }
      }
      return(selected_levels)
    }
    r_specific_levels1=reactive({f_specific_levels("y1", input)})
    r_specific_levels2=reactive({f_specific_levels("y2", input)})

    # Data with D/S components
    f_mra_data1 <- function(){
      mra_obj=wav_mra_obj(app_data()$xy1y2,app_data()$x,app_data()$y1,wt_filter="la8")
      wav_mra_data(mra_obj)
    }
    f_mra_data2 <- function(){
      mra_obj=wav_mra_obj(app_data()$xy1y2,app_data()$x,app_data()$y2,wt_filter="la8")
      wav_mra_data(mra_obj)
    }

    r_mra_sum1 <- reactive({wav_mra_sum(f_mra_data1(),r_specific_levels1(), input$wt_filter_1)})
    r_mra_sum2 <- reactive({wav_mra_sum(f_mra_data2(),r_specific_levels2(), input$wt_filter_2)})

    # Levels UI


    # Plot functions
    f_plotDWT1 <- function(){
      if(input$dwt_type_1 == "variance decomposition"){
        plotDWT_var(y=app_data()$xy1y2[[app_data()$y1]],
                    jmax=wav_jmax(app_data()$nr),
                    wt_filter=input$wt_filter_1,
                    step=numeric_step(app_data()$step))
      } else {
        levels=names(input)[which(stringr::str_detect(names(input),"^y1_"))]
        if(length(levels)==0){levels=c("D1")}
        plotDWT_sig(mra_sum=r_mra_sum1(),
                    yscale=input$DWT_signal_yscale1,
                    plot=input$DWT_signal_plot1,
                    col="blue",
                    xlim=input$xlimdwt1)
      }
    }

    f_plotDWT2 <- function(){
      if(input$dwt_type_2 == "variance decomposition"){
        plotDWT_var(y=app_data()$xy1y2[[app_data()$y2]],
                    jmax=wav_jmax(app_data()$nr),
                    wt_filter=input$wt_filter_2,
                    step=numeric_step(app_data()$step))
      } else {
        plotDWT_sig(mra_sum=r_mra_sum2(),
                    yscale=input$DWT_signal_yscale2,
                    plot=input$DWT_signal_plot2,
                    col="red",
                    xlim=input$xlimdwt2)
      }
    }

    f_plotDWT_superpose <- function(){
      plotDWT_superpose(mra_sum1=r_mra_sum1(),
                        mra_sum2=r_mra_sum2(),
                        yscale1=input$DWT_signal_yscale1,
                        yscale2=input$DWT_signal_yscale2,
                        plot1=input$DWT_signal_plot1,
                        plot2=input$DWT_signal_plot2,
                        input$xlimdwt12)
    }

    # Render plots
    output$plotDWT1 <- renderPlot({f_plotDWT1()},
                                  height = reactive({ app_options()$height }),
                                  width  = reactive({ app_options()$width }))
    output$plotDWT2 <- renderPlot({f_plotDWT2()},
                                  height = reactive({ app_options()$height }),
                                  width  = reactive({ app_options()$width }))
    output$plotDWT_superpose <- renderPlot({f_plotDWT_superpose()},
                                           height = reactive({ app_options()$height }),
                                           width  = reactive({ app_options()$width }))
    # Downloads - figures
    output$downloadFigDWT1 <- makeDownloadHandler(id=1,
                                                  fig_or_info="Fig",
                                                  options=app_options(),
                                                  input=input,
                                                  content_fun=function() f_plotDWT1(),
                                                  analysis="modwt_mra",
                                                  variable=app_data()$y1)
    output$downloadFigDWT2 <- makeDownloadHandler(id=2,
                                                  fig_or_info="Fig",
                                                  options=app_options(),
                                                  input=input,
                                                  content_fun=function() f_plotDWT2(),
                                                  analysis="modwt_mra",
                                                  variable=app_data()$y2)
    output$downloadFigDWT_superpose <- makeDownloadHandler(id=2,
                                                  fig_or_info="Fig",
                                                  options=app_options(),
                                                  input=input,
                                                  content_fun=function() f_plotDWT_superpose(),
                                                  analysis="modwt_mra",
                                                  variable=paste0(app_data()$y1,"_",app_data()$y2))
    #Downloads-tables
    output$downloadDWT1 <- makeDownloadHandler(id=1,
                                                  fig_or_info="Info",
                                                  input=input,
                                                  content_fun=function() f_mra_data1(),
                                                  analysis="modwt_mra",
                                                  variable=app_data()$y1)
    output$downloadDWT2 <- makeDownloadHandler(id=2,
                                                  fig_or_info="Fig",
                                                  input=input,
                                                  content_fun=function() f_mra_data2(),
                                                  analysis="modwt_mra",
                                                  variable=app_data()$y2)
    output$downloadDWT_superpose <-makeDownloadHandler(id=3,
                                                           fig_or_info="Info",
                                                           input=input,
                                                           content_fun=function(){
                                                             function(file){
                                                                      mydata1 <- f_mra_data1()
                                                                      colnames(mydata1) <- paste0(app_data()$y1, "_", colnames(mydata1))
                                                                      mydata2 <- f_mra_data2()
                                                                      colnames(mydata2) <- paste0(app_data()$y2, "_", colnames(mydata2))
                                                                      cbind(mydata1, mydata2)
                                                             }
                                                           },
                                                           analysis="modwt_mra",
                                                           variable=paste0(app_data()$y1,"_",app_data()$y2))

    output$plotMRA=renderPlot({
      step=numeric_step(app_data()$step)
      n=app_data()$nr
      data=app_data()$xy1y2
      units=""
      if(app_data()$x_is_date){
        Tprop=Tproperties(data[,1],origin=data[1,1])
        units=Tprop$units
        units=paste0(" (",units,")")
      }
      jmax=wav_jmax(n)
      par(mar=c(0,0,0,0))
      plot(c(-2,jmax+1.5),c(1,-jmax-1),col="white")
      text(1,0,"Y")
      text(rep(0.5,jmax),(-1):(-jmax),"Y=")
      text(rep(-1.9,jmax),(-1):(-jmax),paste("lev",1:jmax, sep=""))
      text(rep(-0.8,jmax),(-1):(-jmax), paste0(2^(1:jmax)*step,units))
      mygrid=expand.grid(x0=1:(jmax+1),y0=(-1):(-jmax));colnames(mygrid)=c("x0","y0")
      for (j in 1:jmax){
        text(rep(j,jmax),(-j):(-jmax),substitute(D[truc],list(truc=j)))
        for (k in 1:j){
          arrows(x0=k,x1=k,y0=-j+0.8,y1=-j+0.2,length=0.1)
          text(k+0.5,-j,"+")
        }
        arrows(x0=j+0.2,x1=j+0.8,y0=-j+0.8,y1=-j+0.2,length=0.1)
        text(j+1,-j,substitute(S[truc],list(truc=j)))
      }
    })


    output$info8=renderUI({
      if(input$i8%%2!=0){wellPanel(withMathJax(includeHTML(app_sys("app/www/info_DWT.html"))))}
    })

    output$iDWT=renderUI({
      if(input$i8%%2!=0){
        wellPanel(plotOutput(ns("plotMRA"),width="1000px"))
      }
    })



    makeHoverOutputs("dwt",1, input, output,app_data)
    makeHoverOutputs("dwt",2, input, output,app_data)
    makeHoverOutputs("dwt",NULL, input, output,app_data)

})
}
