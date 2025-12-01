# This file is part of the WaveleT package
# Copyright 2015 2016 Lise Vaudor
# This program is distributed under the GPLv3 license

x_l_transfo=function(x){return(exp(x*log(2))-1)}
y_ml_transfo=function(y){return(exp(-y*log(2)))}
y_l_transfo=function(y){return(exp(y*log(2)))}

fhoverx=function(hover,type, date_format=NULL){
  x=""
  if(!is.null(hover)){
    xog=hover[1]$x
    if(type=="raw"){
      x=round(xog,2)
    }
    if(type=="date"){
      x=as.POSIXct(xog,origin="1970-01-01")
      x=format(x,format=date_format)
      x=as.character(x)
      return(x)
    }
    if(type=="x_l_transfo"){
      x=round(x_l_transfo(xog),2)
    }
  }
  x=paste("x=",as.character(x))
  return(x)
}

fhovery=function(hover,type="r",step=1){
  y=""
  if(!is.null(hover)){
      yog=hover[2]$y
      if(type=="raw"){
        y=signif(yog,digits=3)
      }
      if(type=="y_ml_transfo"){
        y=signif(y_ml_transfo(yog)*step,digits=3)
      }
      if(type=="y_ml_transfo_nostep"){
        y=signif(y_ml_transfo(yog),digits=3)
      }
      if(type=="y_l_transfo"){
        y=signif(y_l_transfo(yog),digits=3)
      }
  }
  y=paste("y=",y)
  return(y)
}


#' Module to create hover outputs
#' @param module Module name ("cwt" or "xwt")
#' @param n input number
#' @param input Shiny input object
#' @param output Shiny output object
#' @param app_data the app_data reactive
#' @noRd
#' @export
makeHoverOutputs <- function(module, n=NULL, input, output, app_data) {
  # Identify plot type and create hover outputs accordingly
  if(is.null(n)){n=""}
  name=paste0("hover_",module, n)
  r_plot_type=reactive({
    if(module=="fourier"){
      plot_type="fourier"
    }
    if(module=="dwt"){
      plot_type=input[[paste0("dwt_type_", n)]]
      if(n==""){plot_type="signal decomposition"}
    }
    if(module=="cwt"){
      plot_type=input[[paste0("CWT_plot_type", n)]]
    }
    if(module=="xwt"){
      plot_type=input$XWT_plot_type
    }
    plot_type
  })
  # hover_x
  output[[paste0("hover_", module, n, "_x")]] <- renderText({
    plot_type=r_plot_type()
    type="raw"
    if (app_data()$x_is_date) type="date"
    ##Fourier
    if(plot_type=="fourier") type="raw"
    ##DWT
    if(plot_type=="variance decomposition") type <- "raw"
    ##CWT
    if(plot_type == "3) Power=f(T)") type <- "x_l_transfo"

    fhoverx(input[[paste0("hover_",module, n)]], type, app_data()$date_format)
  })

  # hover_y
  output[[paste0("hover_",module, n, "_y")]] <- renderText({
    plot_type=r_plot_type()
    type="raw"
    ##Fourier
    if(plot_type=="fourier"){type="y_ml_transfo_nostep"}
    ##DWT
    if(plot_type=="signal decomposition") type="raw"
    if(plot_type=="variance decomposition") type="y_ml_transfo"
    ##CWT/XWT
    if (plot_type == "1) Info=f(x,T)") type="y_l_transfo"
    if (plot_type == "2) Info=f(x)") type="raw"
    if (plot_type == "3) Power=f(T)") type="y_ml_transfo_nostep"

    fhovery(input[[paste0("hover_",module, n)]], type,step=numeric_step(app_data()$step))
  })
}

