output$i1=renderUI({
    wellPanel(includeHTML("scripts/info_presentation_1.html"))
})

output$i2=renderUI({
    wellPanel(includeHTML("scripts/info_presentation_2.html"))
})

output$i3=renderUI({
  if(input$i3%%2!=0){
    wellPanel(includeHTML("scripts/info_data_real_1.html"))
  }
})


output$i4=renderUI({
  if(input$i4%%2!=0){
    wellPanel(includeHTML("scripts/info_data_real_2.html"))
  }
})


output$i5=renderUI({
  if(input$i5%%2!=0){
    wellPanel(includeHTML("scripts/info_data_real_3.html"))
  }
})

output$i6=renderUI({
  if(input$i6%%2!=0){
    wellPanel(withMathJax(includeHTML("scripts/info_data_sim_1.html")))
  }
})


output$i7=renderUI({
  if(input$i7%%2!=0){
    wellPanel(withMathJax(includeHTML("scripts/info_fourier.html")))
  }
})


output$plotMRA=renderPlot({
  step=fstep()
  n=fnr()
  data=xy1y2()
  units=""
  if(input$real_or_sim=="real" & input$x_is_date){
    Tprop=Tproperties(data[,1],origin=data[1,1])
    units=Tprop$units
    units=paste0(" (",units,")")
  }
  jmax=floor(log(n)/log(2))
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


output$i8=renderUI({
  if(input$i8%%2!=0){
    wellPanel(withMathJax(includeHTML("scripts/info_DWT.html")))
  }
})
output$iDWT=renderUI({
  if(input$i8%%2!=0){
    wellPanel(plotOutput("plotMRA",width="1000px"))
  }
})

output$i9=renderUI({
  if(input$i9%%2!=0){
    wellPanel(withMathJax(includeHTML("scripts/info_CWT.html")))
  }
})


output$i10=renderUI({
  if(input$i10%%2!=0){
    wellPanel(withMathJax(includeHTML("scripts/info_XWT.html")))
  }
})

output$i11=renderUI({
  if(input$i11%%2!=0){
    wellPanel(withMathJax(includeHTML("scripts/info_WTC.html")))
  }
})

output$i12=renderUI({
  if(input$i12%%2!=0){
    wellPanel(
      
    )
  }
})