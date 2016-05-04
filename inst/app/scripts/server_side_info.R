# This file is part of the WaveleT package
# Copyright 2015 2016 Lise Vaudor
# This program is distributed under the GPLv3 license

x_l_transfo=function(x){return(exp(x*log(2))-1)}
y_ml_transfo=function(y){return(exp(-y*log(2)))}
y_l_transfo=function(y){return(exp(y*log(2)))}

fhoverx=function(hover,type){
  x=""
  if(!is.null(hover)){
    xog=hover[1]$x
    if(type=="raw"){
      x=round(xog,2)
    }
    if(type=="date"){
      x=as.POSIXct(xog,origin="1970-01-01")
      x=format(x,input$date_format)
      x=as.character(x)
      return(x)
    }
    if(type=="x_l_transfo"){
      x=round(x_l_transfo(xog),2)
    }
  }
  x=paste("x=",x)
  return(x)  
}

fhovery=function(hover,type){
  y=""
  if(!is.null(hover)){
      yog=hover[2]$y
      if(type=="raw"){
        y=signif(yog,digits=3)
      }
      if(type=="y_ml_transfo"){
        y=signif(y_ml_transfo(yog),digits=3)
      }
      if(type=="y_l_transfo"){
        y=signif(y_l_transfo(yog),digits=3)
      }
  }
  y=paste("y=",y)
  return(y)  
}

###########
output$hover_raw1_x=renderText({
  type="raw"
  if(input$x_is_date){type="date"}
  x=fhoverx(input$hover_raw1,type)
  return(x)
})
output$hover_raw1_y=renderText({
  y=fhovery(input$hover_raw1,type="raw")
  return(y)
})
output$hover_raw2_x=renderText({
  type="raw"
  if(input$x_is_date){type="date"}
  x=fhoverx(input$hover_raw2,type)
  return(x)
})
output$hover_raw2_y=renderText({
  y=fhovery(input$hover_raw2,type="raw")
  return(y)
})

############
output$hover_fourier1_x=renderText({
  type="raw"
  x=fhoverx(input$hover_fourier1,type)
  return(x)
})
output$hover_fourier1_y=renderText({
  y=fhovery(input$hover_fourier1,type="y_ml_transfo")
  return(y)
})
output$hover_fourier2_x=renderText({
  type="raw"
  x=fhoverx(input$hover_fourier2,type)
  return(x)
})
output$hover_fourier2_y=renderText({
  y=fhovery(input$hover_fourier2,type="y_ml_transfo")
  return(y)
})
#############
output$hover_dwt1_x=renderText({
  type="raw"
  if(input$x_is_date){type="date"}
  x=fhoverx(input$hover_dwt1,type)
  return(x)
})
output$hover_dwt1_y=renderText({
  type="raw"
  if(input$dwt_type_1=="variance decomposition"){type="y_ml_transfo"}
  y=fhovery(input$hover_dwt1,type)
  return(y)
})
output$hover_dwt2_x=renderText({
  type="raw"
  if(input$x_is_date){type="date"}
  x=fhoverx(input$hover_dwt2,type)
  return(x)
})
output$hover_dwt2_y=renderText({
type="raw"
  if(input$dwt_type_2=="variance decomposition"){type="y_ml_transfo"}
  y=fhovery(input$hover_dwt2,type)
  return(y)
})

output$hover_dwt_superpose_x=renderText({
  type="raw"
  if(input$x_is_date){type="date"}
  x=fhoverx(input$hover_dwt_superpose,type)
  return(x)
})
output$hover_dwt_superpose_y=renderText({
  type="raw"
  y=fhovery(input$hover_dwt_superpose,type)
  return(y)
})
#############
output$hover_cwt1_x=renderText({
  type="raw"
  if(input$x_is_date){type="date"}
  if(input$CWT_plot_type1=="3) Power=f(T)"){type="x_l_transfo"}
  x=fhoverx(input$hover_cwt1,type)
  return(x)
})
output$hover_cwt1_y=renderText({
  type="y_l_transfo"
  if(input$CWT_plot_type1=="2) Info=f(x)"){type="raw"}
  if(input$CWT_plot_type1=="3) Power=f(T)"){type="y_ml_transfo"}
  y=fhovery(input$hover_cwt1,type)
  return(y)
})
output$hover_cwt2_x=renderText({
  type="raw"
  if(input$x_is_date){type="date"}
  if(input$CWT_plot_type2=="3) Power=f(T)"){type="x_l_transfo"}
  x=fhoverx(input$hover_cwt2,type)
  return(x)
})
output$hover_cwt2_y=renderText({
  type="y_l_transfo"
  if(input$CWT_plot_type2=="2) Info=f(x)"){type="raw"}
  if(input$CWT_plot_type2=="3) Power=f(T)"){type="y_ml_transfo"}
  y=fhovery(input$hover_cwt2,type)
  return(y)
})

#############
output$hover_xwt_x=renderText({
  type="raw"
  if(input$x_is_date){type="date"}
  if(input$XWT_plot_type=="3) Power=f(T)"){type="x_l_transfo"}
  x=fhoverx(input$hover_xwt,type)
  return(x)
})
output$hover_xwt_y=renderText({
  type="y_l_transfo"
  if(input$XWT_plot_type=="2) Info=f(x)"){type="raw"}
  if(input$XWT_plot_type=="3) Power=f(T)"){type="y_ml_transfo"}
  y=fhovery(input$hover_xwt,type)
  return(y)
})
#############
output$hover_wtc_x=renderText({
  type="raw"
  if(input$x_is_date){type="date"}
  if(input$WTC_plot_type=="3) Power=f(T)"){type="x_l_transfo"}
  x=fhoverx(input$hover_wtc,type)
  return(x)
})
output$hover_wtc_y=renderText({
  type="y_l_transfo"
  if(input$WTC_plot_type=="2) Info=f(x)"){type="raw"}
  if(input$WTC_plot_type=="3) Power=f(T)"){type="y_ml_transfo"}
  y=fhovery(input$hover_wtc,type)
  return(y)
})