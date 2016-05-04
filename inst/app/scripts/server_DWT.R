# This file is part of the WaveleT package
# Copyright 2015 2016 Lise Vaudor
# This program is distributed under the GPLv3 license


jmax1=reactive({
  n=fnr()
  L=wt.filter(input$wt_filter_1)@L
  jmax=floor(log((n-1)/(L-1))/log(2))
  return(jmax)
})
jmax2=reactive({
  n=fnr()
  L=wt.filter(input$wt_filter_2)@L
  jmax=floor(log((n-1)/(L-1))/log(2))
  return(jmax)
})

mra1=reactive({
  mydata=xy1y2()
  mymra=mra(mydata[,2],
            n.levels=jmax1(),
            method="modwt",
            boundary="reflection",
            filter=input$wt_filter_1
  )
  return(mymra)
})

mra2=reactive({
  mydata=xy1y2()
  L=wt.filter(input$wt_filter_2)@L
  mymra=mra(mydata[,3],
            n.levels=jmax2(),
            method="modwt",
            boundary="reflection",
            filter=input$wt_filter_2
  )
  return(mymra)
})
######################################################################
mradata1=reactive({
  mydata=xy1y2()[,c(1,2)]
  n=nrow(mydata)
  mymra=mra1()
  jmax=jmax1()
  dataDWT=mydata
  for(j in 1:jmax){
    datatmp=data.frame(
      mymra@S[[j]][1:n],
      mymra@D[[j]][1:n]
    )
    colnames(datatmp)=c(paste("S",j,sep=""),
                        paste("D",j,sep="")
                        )
    dataDWT=data.frame(dataDWT,
                       datatmp)
  }
  dataDWT
})
mradata2=reactive({
  mydata=xy1y2()[,c(1,3)]
  n=nrow(mydata)
  mymra=mra2()
  jmax=jmax2()
  dataDWT=mydata
  for(j in 1:jmax){
    datatmp=data.frame(
      mymra@S[[j]][1:n],
      mymra@D[[j]][1:n]
    )
    colnames(datatmp)=c(paste("S",j,sep=""),
                        paste("D",j,sep="")
    )
    dataDWT=data.frame(dataDWT,
                       datatmp)
  }
  dataDWT
})


###############
MRAsum=function(mymradata,signalname,lev,my_wt_filter){
  n=nrow(mymradata)
  y=rep(0,n)
  lab=""
  n.boundary=0
  condition=paste0(signalname,"_D1") %in% names(input)
  jmax=1
  if(condition){
    for (j in 1:lev){
      if(input[[paste0(signalname,"_D",j)]]){
        var=paste0("D",j)
        y=y+mymradata[[var]]
        lab=paste0(lab,"+D[",j,"]")
        jmax=j
      }
    }
    if(input[[paste0(signalname,"_S",j)]]){
      var=paste0("S",j)
      y=y+mymradata[[var]]
      lab=paste0(lab,"+S[",j,"]")
      jmax=j
    }
  }
  labsplit=unlist(strsplit(lab,""))
  lab=paste(labsplit[2:length(labsplit)], collapse="")
  L=wt.filter(my_wt_filter)@L
  Lj= (2^jmax - 1) * (L - 1) + 1
  n.boundary=min(Lj,n)
  return(list(y=y,lab=lab,n.boundary=n.boundary))
}

MRAsum1=reactive({
  mymrasum=MRAsum(mymra=mradata1(),
                  signalname="y1",
                  lev=jmax1(),
                  my_wt_filter=input$wt_filter_1)
  return(list(y=mymrasum$y,lab=mymrasum$lab,n.boundary=mymrasum$n.boundary))
})
MRAsum2=reactive({
  mymrasum=MRAsum(mymra=mradata2(),
                  signalname="y2",
                  lev=jmax2(),
                  my_wt_filter=input$wt_filter_2)
  return(list(y=mymrasum$y,lab=mymrasum$lab,n.boundary=mymrasum$n.boundary))
})

###############
plotDWT_sig=function(data,
                     mymrasum,
                     DWT_signal_yscale,
                     DWT_signal_plot,
                     col="blue",
                     xlim){
  y=mymrasum$y
  lab=mymrasum$lab
  nlim=mymrasum$n.boundary/2
  ### compute y-scale limits ###
  myrange=range(y)
  if(DWT_signal_yscale){myrange=range(data[,2])}
  ### plot frame ##### 
  plot(data[,1],data[,2],
       ylim=myrange, 
       col="white",
       xlab=colnames(data)[1],
       ylab=colnames(data)[2],
       main=parse(text=lab),
       xaxs="i", yaxs="i",
       xlim=xlim)
  ### plot boundary regions
  abline(v=c(data[nlim,1],data[nrow(data)-nlim,1]),col="light grey",lty=3)
  ### plot raw signal
  if(DWT_signal_plot){points(data[,1],data[,2], type="l", col="dark grey")}
  ### plot MRA sum
  points(data[,1],y,col=col,type="l", lwd=2)
}
##########################################

plotDWT_superpose=function(data,
                           mymrasum1,
                           mymrasum2,
                           DWT_signal_yscale1,
                           DWT_signal_yscale2,
                           DWT_signal_plot1,
                           DWT_signal_plot2,
                           xlim){
  y1=mymrasum1$y
  lab1=mymrasum1$lab
  y2=mymrasum2$y
  lab2=mymrasum2$lab
  nlim=mymrasum1$n.boundary/2
  ### compute y-scale limits ###
  y1tmp=y1
  y2tmp=y2
  if(DWT_signal_yscale1){y1tmp=data[,2]}
  if(DWT_signal_yscale2){y2tmp=data[,3]}
  ### plot frame ##### 
  plot(data[,1],y1tmp,
       ylim=range(y1tmp), 
       type="n",
       xlab=colnames(data)[1],
       ylab=colnames(data)[2],
       xaxs="i", yaxs="i",
       xlim=xlim)
  if(DWT_signal_plot1){points(data[,1],data[,2], type="l", col="light blue")}
  points(data[,1],y1,col="blue",type="l", lwd=2)
  ################################# 
  par(new=TRUE)
  plot(data[,1],y2tmp,
       ylim=range(y2tmp),
       type="n",
       xlab=colnames(data)[1],
       ylab="",
       yaxt="n",
       xaxs="i",yaxs="i",
       xlim=xlim)
  if(DWT_signal_plot2){points(data[,1],data[,3], type="l", col="pink")}
  points(data[,1],y2,col="red",type="l",lwd=2)
  axis(4)
  ### plot boundary regions
  abline(v=c(data[nlim,1],data[nrow(data)-nlim,1]),col="light grey",lty=3)
  ### legend
  legend("topright",
         col=c("blue","red","light blue","pink"),
         lty=1,
         lwd=c(2,2,1,1),
         legend=c(parse(text=lab1),parse(text=lab2),colnames(data)[2],colnames(data)[3])
  )
}
###########################################
plotDWT_var=function(y,jmax,filter,step){
  mydwt=modwt(y,filter=filter)
  f=function(x){mean(x^2)} 
  V=rep(NA,jmax)
  for (j in 1:jmax){
    V[j]=f(mydwt@W[[j]])
  }
  scales=2^((1:jmax))*step
  Vn=V/sum(V)
  Ve=sqrt(scales)
  Ve=Ve/sum(Ve)
  par(mar=c(3,6,1,1),las=1)
  plot(Vn/Ve,
       -(1:jmax),
       yaxt="n",
       ylab="",
       main=colnames(data)[2],
       type="b",
       lwd=2,pch=20,cex=3)
  abline(v=1,lty=2)
  axis(side=2,at=-(1:jmax), paste("lev",1:jmax,"=",scales))
}
##########################################################
f_plotDWT1=function(){
  if(input$dwt_type_1=="variance decomposition"){
    plotDWT_var(xy1y2()[,2],
                jmax=jmax1(),
                filter=input$wt_filter_1,
                step=fstep())
  }
  if(input$dwt_type_1=="signal decomposition"){
    plotDWT_sig(data=xy1y2()[,c(1,2)],
                mymrasum=MRAsum1(),
                DWT_signal_yscale=input$DWT_signal_yscale1,
                DWT_signal_plot=input$DWT_signal_plot1,
                col="blue",
                xlim=input$xlimdwt1
    )
  }
}

f_plotDWT2=function(){
  if(input$dwt_type_2=="variance decomposition"){
    plotDWT_var(xy1y2()[,3],
                jmax=jmax2(),
                filter=input$wt_filter_2,
                step=fstep())
  }
  if(input$dwt_type_2=="signal decomposition"){
    plotDWT_sig(data=xy1y2()[,c(1,3)],
                mymrasum=MRAsum2(),
                DWT_signal_yscale=input$DWT_signal_yscale2,
                DWT_signal_plot=input$DWT_signal_plot2,
                col="red",
                xlim=input$xlimdwt2
    )
  }
}

f_plotDWT_superpose=function(){
  plotDWT_superpose(data=xy1y2()[,1:3],
                    mymrasum1=MRAsum1(),
                    mymrasum2=MRAsum2(),
                    DWT_signal_yscale1=input$DWT_signal_yscale1,
                    DWT_signal_yscale2=input$DWT_signal_yscale2,
                    DWT_signal_plot1 =input$DWT_signal_plot1,
                    DWT_signal_plot2=input$DWT_signal_plot2,
                    xlim=input$xlimdwt12
  )
}

output$plotDWT1=renderPlot({
   f_plotDWT1() 
},
height=function(x){input$height},
width=function(x){input$width}
)

output$plotDWT2=renderPlot({
    f_plotDWT2()
},
height=function(x){input$height},
width=function(x){input$width}
)

output$plotDWT_superpose=renderPlot({
   f_plotDWT_superpose()
},
height=function(x){input$height},
width=function(x){input$width}
)

###########################################
output$levels1=renderUI({
  w=""
  j=jmax1()
  for (i in 1:j){
  w=paste0(w,
           checkboxInput(paste0("y1_D",i),label=HTML(paste0("D<sub>",i,"</sub>")),value=FALSE))
  }
  w=paste(paste0(w,
                 checkboxInput(paste0("y1_S",j),label=HTML(paste0("S<sub>",j,"</sub>")),value=TRUE)))
  HTML(w)
  })
###########################################
output$levels2=renderUI({
  w=""
  j=jmax2()
  for (i in 1:j){
    w=paste0(w,
             checkboxInput(paste0("y2_D",i),label=HTML(paste0("D<sub>",i,"</sub>")),value=FALSE))
  }
  w=paste(paste0(w,
                 checkboxInput(paste0("y2_S",j),label=HTML(paste0("S<sub>",j,"</sub>")),value=TRUE)))
  HTML(w)
})
