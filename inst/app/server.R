# This file is part of the WaveleT package
# Copyright 2015 2016 Lise Vaudor
# This program is distributed under the GPLv3 license

shinyServer(function(input, output, session) {
  par(mar=c(4,4,3,2))
  options("max.contour.segments"=100000)
  options(shiny.maxRequestSize=30*1024^2)
  datapath=findmypath("app/data","data_example.csv")
  mydata=read.csv(datapath,
                  header=TRUE,
                  sep=";",
                  dec=".")
  buildname=function(fig_or_info,
                     analysis,
                     variable=NA,
                     type=NA,
                     info=NA,
                     period=NA){
    name=paste0(fig_or_info,
                "_",analysis)
    if(!is.na(variable)){name=paste0(name,"_",variable)}
    if(!is.na(type)){name=paste0(name,"_type",type)}
    if(!is.na(info) & type!=3){name=paste0(name,"_",info)}
    if(!is.na(period)){name=paste0(name,"_T",period)}
    if(fig_or_info=="Fig"){name=paste0(name,".",input$graph_format)}
    if(fig_or_info=="Info"){name=paste0(name,".csv")}
    return(name)
  }
  Tproperties=function(x,origin=x[1]){
    t_as_date=as.POSIXct(x,origin=origin)
    xb=difftime(t_as_date[2:length(t_as_date)],
                t_as_date[1:(length(t_as_date)-1)])
    return(list(mean=mean(xb),units=units(xb)))
  }
  flogyslider=function(inputId){
      data=xy1y2()
      x=data[,1]
      mystep=fstep()
      maxy=(length(x) * 0.17) * 2 * mystep
      ticks=c(mystep*2^(1:floor(log2(maxy))),maxy)
      ticks=ticks[ticks>=1]
      args=list(inputId=inputId, label="T range :",
                ticks=ticks, value=c(0,length(ticks)-1))
      args$min=ticks[1]
      args$max=ticks[length(ticks)]
      if (sessionInfo()$otherPkgs$shiny$Version>="0.11") {
        ticks <- paste0(args$ticks, collapse=',')
        args$ticks <- T
        html  <- do.call('sliderInput', args)
        html$children[[2]]$attribs[['data-values']] <- ticks;
      } else {
        html<- do.call('sliderInput', args)
      }
      html
  }
  fxslider=function(inputId){
    data=xy1y2()
    x=data[,1]
    html=sliderInput(inputId=inputId,
                     label="x range",
                     min=min(x),max=max(x),
                     value=c(min(x),max(x)))
    html
  }
  output$slider_xlim_cwt1=renderUI({fxslider("xlimcwt1")})
  output$slider_xlim_cwt2=renderUI({fxslider("xlimcwt2")})
  output$slider_xlim_xwt=renderUI({fxslider("xlimxwt")})
  output$slider_xlim_dwt1=renderUI({fxslider("xlimdwt1")})
  output$slider_xlim_dwt2=renderUI({fxslider("xlimdwt2")})
  output$slider_xlim_dwt12=renderUI({fxslider("xlimdwt12")})
  output$slider_ylim_cwt1=renderUI({flogyslider("ylimcwt1")})
  output$slider_ylim_cwt2=renderUI({flogyslider("ylimcwt2")})
  output$slider_ylim_xwt=renderUI({flogyslider("ylimxwt")})

  source(findmypath("app/scripts","server_info.R"), local=TRUE)
  source(findmypath("app/scripts","server_data.R"), local=TRUE)
  source(findmypath("app/scripts","server_data_dl.R"), local=TRUE)
  source(findmypath("app/scripts","server_data_sim.R"), local=TRUE)
  source(findmypath("app/scripts","server_side_info.R"),local=TRUE)
  source(findmypath("app/scripts","server_fourier.R"),local=TRUE)
  source(findmypath("app/scripts","server_fourier_dl.R"),local=TRUE)
  source(findmypath("app/scripts","server_DWT.R"), local=TRUE)
  source(findmypath("app/scripts","server_DWT_dl.R"), local=TRUE)
  source(findmypath("app/scripts","server_CWT.R"), local=TRUE)
  source(findmypath("app/scripts","server_CWT_dl.R"), local=TRUE)
  source(findmypath("app/scripts","server_XWT.R"), local=TRUE)
  source(findmypath("app/scripts","server_XWT_dl.R"), local=TRUE)
  source(findmypath("app/scripts","server_options.R"),local=TRUE)
  source(findmypath("app/scripts","server_images.R"),local=TRUE)
})
