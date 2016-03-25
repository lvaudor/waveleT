output$downloadDWT1 <- downloadHandler(
  filename=function(){paste0("data_DWT_results_",fy1(),".csv")},
  content=function(file){
    mydata=mradata1()
    write.table(mydata,
                file,
                sep=";",
                row.names=FALSE)}
)
output$downloadDWT2 <- downloadHandler(
  filename=function(){paste0("data_DWT_results_",fy2(),".csv")},
  content=function(file){
    mydata=mradata2()
    write.table(mydata,
                file,
                sep=";",
                row.names=FALSE)}
)
output$downloadDWT_superpose <- downloadHandler(
  filename=function(){paste0("data_DWT_results_",fy1(),"_", fy2(),".csv")},
  content=function(file){
    mydata1=mradata1()
    colnames(mydata1)=paste0(fy1(),"_",colnames(mydata1))
    mydata2=mradata2()
    colnames(mydata2)=paste0(fy2(),"_",colnames(mydata2))
    mydata=data.frame(mydata1,mydata2)
    write.table(mydata,
                file,
                sep=";",
                row.names=FALSE)}
)

output$downloadFigDWT1 <- downloadHandler(
  filename=function(){
    name=paste0("modwt_mra_",fy1(),".",input$graph_format)
  },
  content=function(file){
    fgraph=get(input$graph_format)
    fgraph(file,width=input$width,height=input$height)
    f_plotDWT1()
    dev.off()
  }
)

output$downloadFigDWT2 <- downloadHandler(
  filename=function(){
    name=paste0("modwt_mra_",fy2(),".",input$graph_format)
  },
  content=function(file){
    fgraph=get(input$graph_format)
    fgraph(file,width=input$width,height=input$height)
    f_plotDWT2()
    dev.off()
  }
)

# ###############################################
output$downloadFigDWT_superpose <- downloadHandler(
  filename=function(){
    name=paste0("modwt_mra_",fy1(),"_",fy2(),".",input$graph_format)
  },
  content=function(file){
    fgraph=get(input$graph_format)
    fgraph(file,width=input$width,height=input$height)
    f_plotDWT_superpose()
    dev.off()
  }
)