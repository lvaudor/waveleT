#' Generate a data (simulation) series panel
#'
#' This function creates a fluidRow containing a wellPanel for a data series
#' (either y1 or y2) with selectable plot type, sliders, conditional inputs,
#' hover outputs, and download buttons. The function adjusts all input/output
#' IDs according to the specified series number.
#'
#' @param ns A namespace function (from `NS(id)`) for module namespacing.
#' @param series_id Integer, either 1 or 2, specifying which series panel to create.
#' @return A `shiny.tag.list` containing the fluidRow for the specified data series.
#' @examples
#' ns <- NS("data")
#' data_series_panel_ui(ns, 1)
#' data_series_panel_ui(ns, 2)
#' @export
data_series_panel_ui <-  function(ns, series_id = 1) {
  if (!series_id %in% c(1,2)) stop("series_id must be 1 or 2")
  pref <- as.character(series_id)

  wellPanel(
    HTML(paste0("<h2>Signal y<sub>", pref, "</sub></h2>")),

    # --- Noise block E₁ or E₂ ---
    fluidRow(
      column(width = 1, HTML(paste0("<h3>E<sub>", pref, "</sub></h3>"))),
      column(
        width = 2,
        sliderInput(
          inputId = ns(paste0("sigma", pref)),
          label = HTML("Noise (&#963)"),
          ticks = FALSE, value = 0, min = 0, max = 10, step = 1
        )
      ),
      column(
        width = 2,
        radioButtons(
          inputId = ns(paste0("noisetype", pref)),
          label = "Noise type",
          choices = c("white", "red"),
          selected = "white"
        )
      ),
      column(
        width = 3, offset = 4,
        plotOutput(
          ns(paste0("plotNoise", pref)),
          height = "100px", width = "200px"
        )
      )
    ),

    # --- Sub-signal y_{a} ---
    fluidRow(
      column(width = 1, HTML(paste0("<h3> y<sub>", pref, ",a</sub></h3>"))),
      column(width = 2, sliderInput(ns(paste0("A", pref, "a")),  "A", ticks=FALSE, value=2, min=0, max=10, step=1)),
      column(width = 2, sliderInput(ns(paste0("T", pref, "a")),  "T", ticks=FALSE, value=100, min=1, max=1000, step=1)),
      column(width = 2, sliderInput(ns(paste0("phi", pref, "a")), HTML("&#934"), ticks=FALSE, value=0, min=0, max=1, step=0.05)),
      column(width = 2, sliderInput(ns(paste0("t",  pref, "a")),  "t", ticks=FALSE, value=c(1, 1000), min=1, max=1000, step=1)),
      column(width = 3, plotOutput(ns(paste0("plotSimsignal", pref, "a")), height="100px", width="200px"))
    ),

    # --- Sub-signal y_{b} ---
    fluidRow(
      column(width = 1, HTML(paste0("<h3> y<sub>", pref, ",b</sub></h3>"))),
      column(width = 2, sliderInput(ns(paste0("A", pref, "b")),  "A", ticks=FALSE, value=1, min=0, max=10, step=1)),
      column(width = 2, sliderInput(ns(paste0("T", pref, "b")),  "T", ticks=FALSE, value=15, min=1, max=1000, step=1)),
      column(width = 2, sliderInput(ns(paste0("phi", pref, "b")), HTML("&#934"), ticks=FALSE, value=0, min=0, max=1, step=0.05)),
      column(width = 2, sliderInput(ns(paste0("t",  pref, "b")),  "t", ticks=FALSE, value=c(500, 1000), min=1, max=1000, step=1)),
      column(width = 3, plotOutput(ns(paste0("plotSimsignal", pref, "b")), height="100px", width="200px"))
    ),

    # --- Sub-signal y_{c} ---
    fluidRow(
      column(width = 1, HTML(paste0("<h3> y<sub>", pref, ",c</sub></h3>"))),
      column(width = 2, sliderInput(ns(paste0("A", pref, "c")),  "A", ticks=FALSE, value=1, min=0, max=10, step=1)),
      column(width = 2, sliderInput(ns(paste0("T", pref, "c")),  "T", ticks=FALSE, value=40, min=1, max=1000, step=1)),
      column(width = 2, sliderInput(ns(paste0("phi", pref, "c")), HTML("&#934"), ticks=FALSE, value=0, min=0, max=1, step=0.05)),
      column(width = 2, sliderInput(ns(paste0("t",  pref, "c")),  "t", ticks=FALSE, value=c(1, 250), min=1, max=1000, step=1)),
      column(width = 3, plotOutput(ns(paste0("plotSimsignal", pref, "c")), height="100px", width="200px"))
    )
  )
}


#' Module data
#' UI
#' @param id identifiant du module
#' @export
mod_data_ui <- function(id) {
  ns <- NS(id)
   tagList(
     wellPanel(
       fluidRow(
         column(width=3,
                h3("Type of data")
         ),
         column(width=3,
                radioButtons(ns("real_or_sim"),
                             "",
                             choices=c("real","sim"),
                             selected="real")
         ),
         column(width=6,
                p("You can analyse your own data but also
          generate data. Specifying the characteristics
          of the data to simulate will help you to understand
          how you should interpret the results of wavelet-based
          tools.")
         )
       )#fluidRow
     ),#wellPanel real or sim
############ conditionalPanel real data
     conditionalPanel(
       condition = sprintf("input['%s'] == 'real'", "data-real_or_sim"),
       wellPanel(
         h1("Load real data:"),
         ###########################################################################################
         fluidRow(
           column(width=1,
                  br(),
                  actionButton(inputId=ns("i3"),label=tags$img(src = "www/catpaw.png", width = "30px", height = "30px"))
           ),
           column(width=2,
                  h3("Load data")
           ),
           column(width=2,
                  fileInput(ns('file'), 'in CSV File',
                            accept=c('text/csv',
                                     'text/comma-separated-values,text/plain',
                                     '.csv'))
           ),
           column(width=1,
                  checkboxInput(inputId=ns("header"),
                                label = "Header",
                                value=TRUE)
           ),
           column(width=1,
                  textInput(inputId=ns("na.strings"),
                            label = "Missing data",
                            value="NA")
           ),
           column(width=2,
                  selectInput(inputId=ns("sep"),
                              label = "Column separator",
                              choices =c(";",".","tab",","),
                              selected = ";")
           ),
           column(width=2,
                  selectInput(inputId=ns("dec"),
                              label = "Decimal separator",
                              choices =c(".",","),
                              selected = ".")
           )
         ),#fluidRow
         uiOutput(ns("info3")),
         #######################################################################
         uiOutput(ns("ui_variables")),
         #######################################################################
         uiOutput(ns("info4")),
         #######################################################################
         fluidRow(
           hr(),
           column(width=1,
                  br(),
                  actionButton(inputId=ns("i5"),label=tags$img(src = "www/catpaw.png", width = "30px", height = "30px"))
           ),
           column(width=2,
                  h3("Choose step")
           ),
           column(width=3,
                  uiOutput(ns("step_ui")),
                  textOutput(ns("n_output"))
           )
         ),#fluidRow
         uiOutput(ns("info5"))
       )#wellPanel
     ),#,conditionalPanel "real"
############ conditionalPanel sim data
     conditionalPanel(
       condition = sprintf("input['%s'] == 'sim'", "data-real_or_sim"),
       wellPanel(
         h1("Simulate Data:"),
         #######################################################
         fluidRow(
           column(width=1,br(),
                  actionButton(inputId=ns("i6"),label=tags$img(src = "www/catpaw.png", width = "30px", height = "30px"))
           ),
           column(width=2,
                  numericInput(inputId=ns("n"),
                               label="series length",
                               value=1000,
                               min=100,max=1000000,step=1)
           ),
           column(width=2,
                  checkboxInput(inputId=ns("y2sim_exists"),
                                label=HTML("Add variable y<sub>2</sub>"),
                                value=FALSE)
           )
         ),#fluidRow
         uiOutput(ns("info6"))
         ),#wellPanel
         ##wellPanel signal y1 ###########################################
         data_series_panel_ui(ns,1),
         ##conditional wellPanel y2  #####################################
         conditionalPanel(
           condition = sprintf("input['%s'] == true", "data-y2sim_exists"),
           data_series_panel_ui(ns,2)
        )#conditionalPanel y2
        ),#conditionalPanel simdata
#  Info y1 ###################################
fluidRow(column(width=2,
                HTML("<h3>Signal y<sub>1</sub></h3>"),
                textOutput(ns("hover_raw1_x")),
                textOutput(ns("hover_raw1_y")),
                downloadButton(ns("dltabRaw1"),"Table"),
                downloadButton(ns("dlfigRaw1"),"Figure")
          ),
          column(width=3,
                 p("First lines:"),
                 tableOutput(ns("tabRaw1"))
          ),
          column(width=7,
                 plotOutput(ns("plotRaw1"),
                            hover = ns("hover_raw1"), width="100%",height="100%")
          )
         ),#fluidRow Info y1
#  Info y2 ###################################
conditionalPanel(
  condition = sprintf("(input['%s'] == true && input['%s'] == 'real') || (input['%s'] == true && input['%s'] == 'sim')",
                      "data-y2_exists","data-real_or_sim", "data-y2sim_exists","data-real_or_sim"),
  fluidRow(
    column(width=2,
           HTML("<h3>Signal y<sub>2</sub></h3>"),
           textOutput(ns("hover_raw2_x")),
           textOutput(ns("hover_raw2_y")),
           downloadButton(ns("dltabRaw2"),"Table"),
           downloadButton(ns("dlfigRaw2"),"Figure")
    ),
    column(width=3,
           p("First lines:"),
           tableOutput(ns("tabRaw2"))
    ),
    column(width=7,
           plotOutput(ns("plotRaw2"),
                      hover = ns("hover_raw2"),width="100%",height="100%")
    )
  )#end fluidRow Info y2
  )# end conditionalPanel info y2
)#tagList
}


#' Module serveur pour les analyses data
#' @noRd
#' @export
#' @import shiny
mod_data_server <- function(id,app_options, app_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ##### Reactive: load data ######
      fload_data <- reactive({
        #print("in fload_data")
        sep <- input$sep
        input$header
        input$dec
        input$na.strings

        if(!is.null(input$file) && nrow(input$file) > 0){
          datapath = input$file$datapath[1]}else{
          datapath=app_sys("app/data/data_example.csv")
          }
        if(sep=="tab"){sep="\t"}
        mydata <- read.csv(datapath,
                           header=input$header,
                           sep=sep,
                           dec=input$dec,
                           na.strings=input$na.strings)
        classes <- sapply(mydata, class)
        if(all(classes != "numeric")){
          mydata <- read.csv(datapath,
                             header=input$header,
                             sep=sep,
                             dec=",",
                             na.strings=input$na.strings)
        }
        mydata
      })
      v <- reactiveValues(
        ui_vars_ready = FALSE,  # inputs x, y1, etc existent
      )
      output$ui_variables=renderUI({
        #print("in ui_variables")
        mydata <- fload_data()
        coln <- colnames(mydata)
        if(length(coln)==1) coln <- c(coln, coln, coln)

        fluidRow(
          hr(),
          column(width=1,
                 br(),
                 actionButton(inputId=ns("i4"),label=tags$img(src = "www/catpaw.png", width = "30px", height = "30px"))
          ),
          column(width=2,
                 h3("Choose variables")
          ),
          column(width=3,
                 selectInput(inputId=ns("x"),
                             label = "Variable x",
                             choices =coln[1:3],
                             selected =coln[1]),
                 checkboxInput(inputId=ns("x_is_date"),
                               label="x is a date",
                               value=FALSE),
                 textInput(inputId=ns("date_format"),
                           label="date format",
                           value="%Y-%m-%d %H:%M:%S")
          ),
          column(width=2,
                 selectInput(inputId=ns("y1"),
                             label = HTML("Variable y<sub>1</sub>"),
                             choices=coln[1:3],
                             selected=coln[2]),
                 checkboxInput(inputId=ns("y2_exists"),
                               label = HTML("Add Variable y<sub>2</sub>"),
                               value=FALSE)
          ),
          column(width=2,
                 conditionalPanel(condition = sprintf("input['%s'] == true", ns("y2_exists")),
                                  selectInput(inputId=ns("y2"), label = "Variable y2",
                                              choices =coln[1:3],
                                              selected =coln[3])
                 )
          ),
          column(width=1, offset=1,
                 actionButton(ns("ok"), "OK",
                              style = "background-color: #e60000; color: white; border-radius:5px; font-weight:bold; border:none; padding:8px 16px;"))
        )
      })
      observeEvent(input$ok, {
          v$ui_vars_ready <- TRUE   # already true, but re-declared
          #print("User validated inputs -> go")
      })

      ###### Reactive: step for interpolation ######
      r_step = reactive({
        req(v$ui_vars_ready)
        #print("in r_step")
        real_or_sim=input$real_or_sim
        x_is_date=input$x_is_date
        x=fload_data()[, input$x]
        time_format=input$date_format
          if(real_or_sim=="sim"){step=1}
          if(real_or_sim=="real" & x_is_date){
            step=format_time(x=x,time_format=time_format)$step
          }
          if(real_or_sim=="real" & !x_is_date){step=median(diff(x),na.rm=TRUE)}
        if(is.null(step)){step="1"}
        #print(step)
        step
      })

      output$step_ui= renderUI({
          mystep=r_step()

          textInput(inputId=ns("step"),
                    label=HTML("Step between x-values (&#916x)"),
                    value=mystep)
      })


      ###### Reactive: format data into x,y1,y2 ######
      format_data <- reactive({
        #print("in format_data")
        req(v$ui_vars_ready)
        data=fload_data()
        x <- as.vector(data[, input$x])
        y1 <- as.numeric(as.vector(data[, input$y1]))
        ## If x is not a date
        if(!input$x_is_date){
          x=as.numeric(x)
          if(all(is.na(x))) x <- 1:nrow(data)
        }
        ## If x is a date
        if(input$x_is_date){
          x=format_time(x,time_format=input$date_format)$x
        }

        if(all(is.na(y1))) y1 <- x
        mydata <- data.frame(x, y1)

        if(input$y2_exists){
          y2 <- as.numeric(as.vector(data[, input$y2]))
          if(all(is.na(y2))) y2 <- mydata$x
          mydata <- data.frame(mydata, y2)
        }

        mydata <- na.omit(mydata)
        mydata <- mydata[order(mydata$x),]
        mydata
      })
      ###### Reactive: interpolated x values ######
      fxout <- reactive({
        req(v$ui_vars_ready)
        #print("in fxout")
        req(format_data(),input$step)
        if(input$real_or_sim=="sim") return(1:input$n)
        x = format_data()[[input$x]]
        if(input$x_is_date){step=input$step}
        if(!input$x_is_date){step=numeric_step(input$step)}
        xout=seq(from= min(x),to=max(x),by=step)
        if(length(xout) > 50000) xout <- seq(min(xout), max(xout), length.out=50000)
        xout
      })

      ###### Reactive: interpolated y1/y2 ######
      f_xy1y2 <- reactive({
        req(v$ui_vars_ready)
        #print("in f_xy1y2")
        #req(fxout(),format_data())
        xout <- fxout()
        mydata <- format_data()

        if (input$real_or_sim == "sim") {
          req(simSignal1)
          y1 <- simSignal1()
          y2 <- if (isTruthy(input$y2sim_exists)) simSignal2() else y1
        } else {
          # Cas réel
          validate(need("y1" %in% names(mydata), "y1 manquant dans format_data()"))
          y1 <- approx(mydata$x, mydata$y1, xout = xout)$y
          y2 <- if (isTruthy(input$y2_exists) && "y2" %in% names(mydata)) {
            approx(mydata$x, mydata$y2, xout = xout)$y
          } else {
            y1
          }
        }

        dat <- data.frame(x = xout, y1 = y1, y2 = y2)

        # Renommer les colonnes selon input$x, input$y1, input$y2
        colnames(dat) <- c(input$x, input$y1, input$y2)
        na.omit(dat)
        print(head(dat))
        dat
      })

      # ###### Observers: update xlim sliders ######
      # observe({
      #   data <- f_xy1y2()
      #   for(slider in c("xlimcwt1","xlimcwt2","xlimxwt","xlimdwt12","xlimdwt1","xlimdwt2")){
      #     updateSliderInput(session, slider, value=c(min(data[,1]), max(data[,1])), min=min(data[,1]), max=max(data[,1]))
      #   }
      # })

      ###### Outputs: raw tables ######
      output$tabRaw1 <- renderTable({
        mydata <- f_xy1y2()
        mydata=mydata[,c(1,2)]
        if(input$x_is_date) mydata[,1] <- as.character(mydata[,1])
        mydata[1:5,]
      })

      output$tabRaw2 <- renderTable({
        mydata <- f_xy1y2()[,c(1,3)]
        if(input$x_is_date) mydata[,1] <- as.character(mydata[,1])
        mydata[1:5,]
      })

      ###### Outputs: raw plots ######
      f_plotRaw1 <- function(){
        dat <- f_xy1y2()
        par(mar=c(4,4,3,2), las=1, xaxs="i")
        plot(dat[,1], dat[,2], type="l", xlab=colnames(dat)[1], ylab=colnames(dat)[2])
      }

      f_plotRaw2 <- function(){
        dat <- f_xy1y2()
        par(mar=c(4,4,3,2), las=1, xaxs="i")
        plot(dat[,1], dat[,3], type="l", xlab=colnames(dat)[1], ylab=colnames(dat)[3])
      }

      output$plotRaw1 <- renderPlot({f_plotRaw1()},
                                    height = reactive({app_options()$height}),
                                    width = reactive({app_options()$width})
                                    )
      output$plotRaw2 <- renderPlot({f_plotRaw2()},
                                    height = reactive({app_options()$height}),
                                    width = reactive({app_options()$width})
                                    )

      ###### Output: number of points ######
      output$n_output <- renderText({ paste("n=",length(fxout())) })


      ##########################################################
      makeSimSignal <- function(id, fn, input) {
        reactive({
          n <- input$n
          x <- 1:n
          sim_signal(
            x,
            T = input[[paste0("T", id)]],
            A = input[[paste0("A", id)]],
            phi = input[[paste0("phi", id)]],
            tmin = input[[paste0("t", id)]][1],
            tmax = input[[paste0("t", id)]][2]
          )
        })
      }
      simSignal1a <- makeSimSignal("1a", fn, input)
      simSignal1b <- makeSimSignal("1b", fn, input)
      simSignal1c <- makeSimSignal("1c", fn, input)
      simSignal2a <- makeSimSignal("2a", fn, input)
      simSignal2b <- makeSimSignal("2b", fn, input)
      simSignal2c <- makeSimSignal("2c", fn, input)


      makeSimSignalCombined <- function(ids, fn, noise_fun) {
        reactive({
          input$n
          y_list <- lapply(ids, function(id) get(paste0("simSignal", id))())
          y <- Reduce(`+`, y_list)
          y + noise_fun()
        })
      }

      simSignal1 <- makeSimSignalCombined(c("1a", "1b", "1c"), fn, noise1)
      simSignal2 <- makeSimSignalCombined(c("2a", "2b", "2c"), fn, noise2)

      ######################################################
      rangesim=reactive({
        Amax=max(input$A1a,
                 input$A1b,
                 input$A1c,
                 input$A2a,
                 input$A2b,
                 input$A2c)
        noisemax=max(input$sigma1,input$sigma2)
        mymax=max(Amax,3*noisemax)
        myrange=c(-mymax,mymax)
        return(myrange)})
      #####################################################
      makeNoise <- function(id,input) {
        reactive({
          n=input$n
          type <- input[[paste0("noisetype", id)]]
          sigma <- input[[paste0("sigma", id)]]

          if (type == "white") {
            y <- rnorm(n, 0, sigma)
          } else if (type == "red") {
            y <- arima.sim(n, model = list(order = c(1, 0, 0), ar = sigma / 11))
          } else {
            y <- rep(0, n)
          }

          y
        })
      }
      noise1 <- makeNoise("1", input)
      noise2 <- makeNoise("2", input)
      #####################################################

      makePlotNoise <- function(id, input, noise_fun, rangesim) {
        renderPlot({
          n <- input$n
          x <- 1:n
          y <- noise_fun()
          par(mar = c(2, 2, 1, 1))
          plot(x, y, type = "l", cex = 0.6, ylim = rangesim())
        })
      }
      output$plotNoise1 <- makePlotNoise("1", input, noise1, rangesim)
      output$plotNoise2 <- makePlotNoise("2", input, noise2, rangesim)

      #####################################################
      makePlotSimSignal <- function(id, input, simSignal_fun, rangesim) {
        renderPlot({
          n <- input$n
          x <- 1:n
          y <- simSignal_fun()
          par(mar = c(2, 2, 1, 1))
          plot(x, y, type = "l", cex = 0.6, ylim = rangesim())
        })
      }

      output$plotSimsignal1a <- makePlotSimSignal("1a", input, simSignal1a, rangesim)
      output$plotSimsignal1b <- makePlotSimSignal("1b", input, simSignal1b, rangesim)
      output$plotSimsignal1c <- makePlotSimSignal("1c", input, simSignal1c, rangesim)
      output$plotSimsignal2a <- makePlotSimSignal("2a", input, simSignal2a, rangesim)
      output$plotSimsignal2b <- makePlotSimSignal("2b", input, simSignal2b, rangesim)
      output$plotSimsignal2c <- makePlotSimSignal("2c", input, simSignal2c, rangesim)
      ########################################################
      observe({
        n=length(fxout())
        updateSliderInput(session, "t1a",value=c(1,n),max=n)
        updateSliderInput(session, "t1b",value=c(1,n),max=n)
        updateSliderInput(session, "t1c",value=c(1,n),max=n)
        updateSliderInput(session, "t2a",value=c(1,n),max=n)
        updateSliderInput(session, "t2b",value=c(1,n),max=n)
        updateSliderInput(session, "t2c",value=c(1,n),max=n)
        updateSliderInput(session, "T1a",step=1,max=round(n/2))
        updateSliderInput(session, "T1b",step=1,max=round(n/2))
        updateSliderInput(session, "T1c",step=1,max=round(n/2))
        updateSliderInput(session, "T2a",step=1,max=round(n/2))
        updateSliderInput(session, "T2b",step=1,max=round(n/2))
        updateSliderInput(session, "T2c",step=1,max=round(n/2))
      })

      output$dltabRaw1 <- makeDownloadHandler(id=1,
                                              fig_or_info="Info",
                                              input=input,
                                              content_fun=function() f_xy1y2()[,c(input$x,input$y1)],
                                              analysis="Raw",
                                              variable=input$y1)
      output$dltabRaw2 <- makeDownloadHandler(id=2,
                                              fig_or_info="Info",
                                              input=input,
                                              content_fun=function() f_xy1y2()[,c(input$x,input$y2)],
                                              analysis="Raw",
                                              variable=input$y1)
      output$dlfigRaw1 <- makeDownloadHandler(id=1,
                                              fig_or_info="Fig",
                                              options=app_options(),
                                              input=input,
                                              content_fun=function() f_plotRaw1(),
                                              analysis="Raw",
                                              variable=input$y1)
      output$dlfigRaw2 <- makeDownloadHandler(id=2,
                                              fig_or_info="Fig",
                                              options=app_options(),
                                              input=input,
                                              content_fun=function() f_plotRaw2(),
                                              analysis="Raw",
                                              variable=input$y2)


      ##########################################################################
    output$info3=renderUI({
      if(input$i3%%2!=0){wellPanel(includeHTML(app_sys("app/www/info_data_real_1.html")))}
    })
    output$info4=renderUI({
        if(input$i4%%2!=0){wellPanel(includeHTML(app_sys("app/www/info_data_real_2.html")))}
    })
    output$info5=renderUI({
        if(input$i5%%2!=0){wellPanel(includeHTML(app_sys("app/www/info_data_real_3.html")))}
    })
    output$info6=renderUI({
        if(input$i6%%2!=0){wellPanel(withMathJax(includeHTML(app_sys("app/www/info_data_sim_1.html"))))}
    })

    r_show_y2 = reactive({
      y2    = input$y2_exists
      y2sim = input$y2sim_exists
      real  = input$real_or_sim=="real"
      sim   = input$real_or_sim=="sim"
      panel2=(y2 && real) || (y2sim && sim)
      isTRUE((y2 && real) || (y2sim && sim))
    })

    makeHoverOutput <- function(suffix, input) {
    list(
      x = renderText({
        type <- "raw"
        if (input$x_is_date) type <- "date"
        fhoverx(input[[paste0("hover_", suffix)]], type, input$date_format)
      }),
      y = renderText({
        fhovery(input[[paste0("hover_", suffix)]], type = "raw", input$date_format)
      })
    )
  }
  hover1 <- makeHoverOutput("raw1", input)
  output$hover_raw1_x <- hover1$x
  output$hover_raw1_y <- hover1$y

  hover2 <- makeHoverOutput("raw2", input)
  output$hover_raw2_x <- hover2$x
  output$hover_raw2_y <- hover2$y


  app_data=reactive({
    list(
        xy1y2 = f_xy1y2(),
        x = input$x,
        y1 = input$y1,
        y2 = input$y2,
        show_y2=r_show_y2(),
        n = max(1, input$n, na.rm=TRUE),
        nr = length(fxout()),
        step = input$step,
        real_or_sim=input$real_or_sim,
        x_is_date=input$x_is_date,
        date_format=input$date_format
      )
  })

  return(app_data)
  })
}
