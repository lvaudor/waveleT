# This file is part of the WaveleT package
# Copyright 2015 2016 Lise Vaudor
# This program is distributed under the GPLv3 license

output$plotblank=renderPlot({
  plot(1:100,1:100, col="white",xlab="x",ylab="y", xaxt="n", yaxt="n")
},
height=function(x){input$height},
width=function(x){input$width}
)
