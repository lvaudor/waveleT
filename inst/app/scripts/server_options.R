output$plotblank=renderPlot({
  plot(1:100,1:100, col="white",xlab="x",ylab="y", xaxt="n", yaxt="n")
},
height=function(x){input$height},
width=function(x){input$width}
)
