
#this function loads the data from data file
fload_data=reactive({
  inFile <- input$file
  datapath=inFile$datapath
  sep=input$sep
  if(sep=="tab"){sep="\t"}
  #datapath defaults to data_example
  if (is.null(inFile)) datapath=findmypath("app/data","data_example.csv")
  mydata=read.csv(datapath,
                  header=input$header,
                  sep=sep,
                  dec=input$dec,
                  na.strings=input$na.strings)
  #if no variable in mydata is numeric it is probably due to
  #decimal separator being a coma
  classes=apply(mydata,2,"class")
  if(length(which(classes=="numeric"))==0){
    mydata=read.csv(datapath,
                    header=input$header,
                    sep=sep,
                    dec=",",
                    na.strings=input$na.strings)

  }
  mydata
})

##############
fx= reactive({input$x})
fy1=reactive({input$y1})
fy2=reactive({input$y2})
#number of individuals in real data
fn=reactive({max(1,input$n,na.rm=T)})
#number of individuals in simulated data
fnr=reactive({length(fxout())})
#############

#this function calculates the time or space between two consecutive values
fmystep=function(step,real_or_sim, x_is_date){
  mystep=step
  if(real_or_sim=="sim"){mystep=1}
  if(mystep=="?"){mystep=1}
  if(real_or_sim=="real" & !x_is_date){mystep=as.numeric(mystep)}
  return(mystep)
}
#this function is useful for displaying step in Fourier analysis
fstep=reactive({
if(input$real_or_sim=="sim"){
     mystep=1
   }
   if(input$real_or_sim=="real"){
     mystep=input$step
     if(input$x_is_date){
       stepinfo=as.character(mystep)
       elems=unlist(strsplit(stepinfo,split=" "))
       mystep=elems[1]
     }
     mystep=as.numeric(mystep)
   }
   return(mystep)
 })
#############
#this function formats loaded data into a 3-column dataframe x,y1,y2
format_data=reactive({
  data=isolate(fload_data())
  ######## x values #########
  x=as.vector(data[,fx()])
  y1=as.numeric(as.vector(data[,fy1()]))
  if(input$x_is_date==FALSE){
    x=as.numeric(x)
    if(length(which(is.na(x)))==length(x)){
      x=1:nrow(data)
    }
  }
  if(input$x_is_date==TRUE){
    x=strptime(x, format=input$date_format)
    if(length(which(is.na(x)))==length(x)){
      x=seq(as.POSIXct("1970-01-01",origin="1970-01-01"),
            as.POSIXct("1975-01-01",origin="1970-01-01"),
            length.out=nrow(data))
      }
  }
  ######### y1 values ######
  if(length(which(!is.na(y1)))==0){y1=x}
  mydata=data.frame(x,y1)
  mydata=mydata[order(mydata$x),]
  ######### y2 values ######
  if(input$y2_exists==TRUE){
    y2=as.numeric(as.vector(data[,fy2()]))
    if(length(which(!is.na(y2)))==0){y2=mydata$x}
    mydata=data.frame(mydata,y2)
  }
  mydata=na.omit(mydata)
  return(mydata)
})

##############
#This function calculates the values xout at which values
#of y1 and y2 should be interpolated
fxout=reactive({
  if(input$real_or_sim=="sim"){
    xout=1:fn()
  }
  if(input$real_or_sim=="real"){
    mydata=format_data()
    if(input$x_is_date){mydata$x=as.POSIXct(as.vector(mydata$x),origin="1970-01-01")}
    mystep=fmystep(input$step,real_or_sim="real", x_is_date=input$x_is_date)
    mymin=min(mydata$x)
    mymax=max(mydata$x)
    if(input$x_is_date & !is.na(as.numeric(mystep))){
      xout=seq(from=mymin,
               to=mymax,
               length.out=1000)
    }
    if(is.na(as.numeric(mystep))){
      xout=seq(from=mymin,to=mymax,length.out=1000)
    }
    if(!(input$x_is_date & !is.na(as.numeric(mystep)))){
      xout=seq(from=mymin,
                 to=mymax,
                 by=mystep)
    }

    if(length(xout)>50000){xout=seq(min(xout),to=max(xout),length.out=50000)}
  }
  return(xout)
})
##############
##this function interpolates y1 and y2 for values xout
xy1y2=reactive({
  mydata=isolate(format_data())
  xout=fxout()
  if(input$real_or_sim=="sim"){
    mydata=data.frame(x=xout,
                      y1=simSignal1())
    if(input$y2sim_exists==TRUE){
      mydata=data.frame(mydata,
                        y2=simSignal2())
    }
  }
  if(input$real_or_sim=="real"){
        y1_appro=approx(mydata$x,mydata$y1,xout=xout)$y
        mydatanew=data.frame(x=xout,y1=y1_appro)
        colnames(mydatanew)=c(fx(),fy1())
        if(input$y2_exists==TRUE){
          y2_appro=approx(mydata$x,mydata$y2,xout=xout)$y
          mydatanew=data.frame(mydatanew,
                            y2=y2_appro)
          colnames(mydatanew)=c(fx(),fy1(),fy2())
        }
        ###########################
        mydata=na.omit(mydatanew)
  }
  return(mydata)
})

########################################################
#### OBSERVERS #########################################

observe({
  mydata=fload_data()
  coln=colnames(mydata)
  if(length(coln)==1){coln=c(coln,coln,coln)}
  updateSelectInput(session, "x",
                    choices  = coln,
                    selected = coln[1])
  updateSelectInput(session, "y1",
                    choices  = coln,
                    selected = coln[2])
  updateSelectInput(session, "y2",
                    choices  = coln,
                    selected = coln[2])
})

observe({
  fx() #trigger when x variable is changed
  x=as.vector(format_data()[,1])
  if(input$x_is_date==FALSE){
    x=as.numeric(x)
    mystep=as.character(round(mean(diff(sort(x))),2))
    if(mystep<=0){mystep=1}
  }
  if(input$x_is_date==TRUE){
    mystep="?"
    x=as.POSIXct(x, origin="1970-01-01")
    if(class(x)[1]!="integer"){
        Tprop=Tproperties(x,origin=x[1])
        xnew=seq(from=min(x),to=max(x),by=Tprop$mean)
        Tprop=Tproperties(xnew,origin=x[1])
        mystep=paste(meandiff=round(Tprop$mean,2),
                       Tprop$units)
    }
  }
  updateTextInput(session,"step",value=mystep)
})

## suggest right values for xlim and ylim
observe({
  data=xy1y2()
  x=data[,1]
  updateSliderInput(session,
                    "xlimcwt1",
                    value=c(min(x),max(x)),
                    min=min(x),max=max(x))
  updateSliderInput(session,
                    "xlimcwt2",
                    value=c(min(x),max(x)),
                    min=min(x),max=max(x))
  updateSliderInput(session,
                    "xlimxwt",
                    value=c(min(x),max(x)),
                    min=min(x),max=max(x))
  updateSliderInput(session,
                    "xlimdwt12",
                    value=c(min(x),max(x)),
                    min=min(x),max=max(x))
  updateSliderInput(session,
                    "xlimdwt1",
                    value=c(min(x),max(x)),
                    min=min(x),max=max(x))
  updateSliderInput(session,
                    "xlimdwt2",
                    value=c(min(x),max(x)),
                    min=min(x),max=max(x))
  updateSliderInput(session,
                    "xlimdwt12",
                    value=c(min(x),max(x)),
                    min=min(x),max=max(x))
})
#####################################################################
########## OUTPUTS ##################################################

output$tabRaw1=renderTable({
  mydata=xy1y2()[,c(1,2)]
  if(input$x_is_date){mydata[,1]=as.character(mydata[,1])}
  mydata[1:5,]
})

output$tabRaw2=renderTable({
  mydata=xy1y2()[,c(1,3)]
  if(input$x_is_date){mydata[,1]=as.character(mydata[,1])}
  mydata[1:5,]
})

#######################################################################
f_plotRaw1=function(){
  dat=xy1y2()
  par(mar=c(4,4,3,2),las=1,xaxs="i")
  plot(dat[,1],dat[,2], type="l", xlab=colnames(dat)[1], ylab=colnames(dat)[2])
}

f_plotRaw2=function(){
  fy2()
  dat=xy1y2()
  par(mar=c(4,4,3,2),las=1, xaxs="i")
  plot(dat[,1],dat[,3],type="l", xlab=colnames(dat)[1], ylab=colnames(dat)[3])
}

########################################################################
output$plotRaw1=renderPlot({
  f_plotRaw1()
},
height=function(x){input$height},
width=function(x){input$width}
)

output$plotRaw2=renderPlot({
  f_plotRaw2()
},
height=function(x){input$height},
width=function(x){input$width}
)

output$n_output=renderText({
  paste("n=",fnr())
})
