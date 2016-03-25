output$dltabFourier1 <- downloadHandler(
  filename=function(){"tabFourier1.csv"},
  content=function(file){
    fourier_results=calcFourier(xy1y2()[,1:2],input$mystep)
    write.table(fourier_results,
                file,
                sep=";",
                row.names=FALSE)}
)
output$dltabFourier2 <- downloadHandler(
  filename=function(){"tabFourier2.csv"},
  content=function(file){
    fourier_results=calcFourier(xy1y2()[,1:3], input$mystep)
    write.table(fourier_results,
                file,
                sep=";",
                row.names=FALSE)}
)


output$dlfigFourier1 <- downloadHandler(
  filename=function(){
    name=paste0("Fig_Fourier1.",input$graph_format)
    return(name)
  },
  content=function(file){
    fgraph=get(input$graph_format)
    fgraph(file,width=input$width,height=input$height)
    f_plotFourier1()
    dev.off()
  }
)
output$dlfigFourier2 <- downloadHandler(
  filename=function(){
    name=paste0("Fig_Fourier2.",input$graph_format)
    return(name)
  },
  content=function(file){
    fgraph=get(input$graph_format)
    fgraph(file,width=input$width,height=input$height)
    f_plotFourier2()
    dev.off()
  }
)