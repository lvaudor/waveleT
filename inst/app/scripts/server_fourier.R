calcFourier=function(data,mystep){
  x=data[,1]
  y=data[,2]
  n=nrow(data)
  #
  scalemin=2*mystep
  J1 = round(log2(n* 0.17)/(1/12))
  scalemax = 2*mystep * 2^(J1 * (1/12))
  
  jmax=floor(log(n)/log(2))
  ####
  freq=1:n
  T=(n*mystep)/freq
  y=Mod(fft(y))
  ind=which(T>scalemin & T<scalemax)
  fourier_results=data.frame(freq=freq[ind],
                             T=T[ind],
                             y=y[ind])
  o=order(fourier_results$T,decreasing=FALSE)
  fourier_results=fourier_results[o,]
  return(fourier_results)
}
  
plotFourier=function(data,mystep,real_or_sim,x_is_date){
  units=""
  if(real_or_sim=="real" & x_is_date){
    Tprop=Tproperties(data[,1],origin=data[1,1])
    units=Tprop$units
    units=paste0(" (",units,")")
  }
  fourier_results=calcFourier(data,mystep)
  n=nrow(fourier_results)
  jmax=floor(log(n)/log(2))
  par(las=1)
  plot(fourier_results$y,
       -log2(fourier_results$T),
       type="b",
       pch=20,cex=2,
       lwd=2,
       yaxt="n",
       ylab=paste0("T",units),
       xlab="Fourier coef.", yaxs="i")
  axis(side=2,at=-log2(2^(1:jmax)*mystep),
       lab=2^(1:jmax)*mystep)
  abline(h=-log2(2^(1:jmax)*mystep),lty=2)
}


###################################################################
f_plotFourier1=function(){
  plotFourier(data=xy1y2()[,c(1,2)],
              mystep=fstep(),
              real_or_sim=input$real_or_sim,
              x_is_date=input$x_is_date)
}
f_plotFourier2=function(){
  plotFourier(data=xy1y2()[,c(1,3)],
              mystep=fstep(),
              real_or_sim=input$real_or_sim,
              x_is_date=input$x_is_date)
}
output$plotFourier1=renderPlot({
 f_plotFourier1()
},
height=function(x){input$height},
width=function(x){input$width}
)

output$plotFourier2=renderPlot({
  f_plotFourier2()
},
height=function(x){input$height},
width=function(x){input$width}
)
###################################################################
