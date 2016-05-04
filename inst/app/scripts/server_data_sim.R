# This file is part of the WaveleT package
# Copyright 2015 2016 Lise Vaudor
# This program is distributed under the GPLv3 license

sim_signal=function(x,
                    T,
                    A=1,
                    phi=0,
                    tmin=1,
                    tmax=length(x)){
  n=length(x)
  freq=n/T
  delta_x=phi*T 
  y=A*cos((2*pi*freq*1/n)*(x+delta_x))
  if(tmin>1){y[1:tmin]=0}
  if(tmax<n){y[tmax:n]=0}
  return(y)
}

sim_signalsum=function(x,
                       T,
                       A=rep(1,length(freq)),
                       phi=rep(0,length(freq)),
                       tmin=rep(1,length(freq)),
                       tmax=rep(length(x),length(freq))){
  y=rep(0,length(t))
  for(i in 1:length(T)){
    y_i=sim_signal(x,T[i],A[i],phi[i])
    y=y+y_i
  }
  return(y)
}
##########################################################
##########################################################
simSignal1a=reactive({
  n=fn()
  x=1:n
  y=sim_signal(x,
               T=input$T1a,
               A=input$A1a,
               phi=input$phi1a,
               tmin=input$t1a[1],
               tmax=input$t1a[2])
  return(y)
})
simSignal1b=reactive({
  n=fn()
  x=1:n
  y=sim_signal(x,
               T=input$T1b,
               A=input$A1b,
               phi=input$phi1b,
               tmin=input$t1b[1],
               tmax=input$t1b[2])
  return(y)
})
simSignal1c=reactive({
  n=fn()
  x=1:n
  y=sim_signal(x,
               T=input$T1c,
               A=input$A1c,
               phi=input$phi1c,
               tmin=input$t1c[1],
               tmax=input$t1c[2])
  return(y)
})
simSignal2a=reactive({
  n=fn()
  x=1:n
  y=sim_signal(x,
               T=input$T2a,
               A=input$A2a,
               phi=input$phi2a,
               tmin=input$t2a[1],
               tmax=input$t2a[2])
  return(y)
})
simSignal2b=reactive({
  n=fn()
  x=1:n
  y=sim_signal(x,
               T=input$T2b,
               A=input$A2b,
               phi=input$phi2b,
               tmin=input$t2b[1],
               tmax=input$t2b[2])
  return(y)
})
simSignal2c=reactive({
  n=fn()
  x=1:n
  y=sim_signal(x,
               T=input$T2c,
               A=input$A2c,
               phi=input$phi2c,
               tmin=input$t2c[1],
               tmax=input$t2c[2])
  return(y)
})
##################################################################
simSignal1=reactive({
  fn()
  y1=simSignal1a()
  y2=simSignal1b()
  y3=simSignal1c()
  y=y1+y2+y3
  y=y+noise1()
  return(y)
})
simSignal2=reactive({
  fn()
  y1=simSignal2a()
  y2=simSignal2b()
  y3=simSignal2c()
  y=y1+y2+y3
  y=y+noise2()
  return(y)
})
######################################################
rangesim=reactive({
  Amax=max(input$A1a,
           input$A1b,
           input$A1c,
           input$A2a,
           input$A2b,
           input$A2c)
  noisemax=max(input$sigma1,input$sigma2)
  mymax=max(Amax,3*noisemax)
  myrange=c(-mymax,mymax)
  return(myrange)})
#####################################################
noise1=reactive({
  n=fn()
  if(input$noisetype1=="white"){
    y=rnorm(n,0,input$sigma1)
  }
  if(input$noisetype1=="red"){
    y=arima.sim(n,model=list(order=c(1,0,0),ar=input$sigma1/11))
  }
  return(y)
})
noise2=reactive({
  n=fn()
  if(input$noisetype2=="white"){
    y=rnorm(n,0,input$sigma2)
  }
  if(input$noisetype2=="red"){
    y=arima.sim(n,model=list(order=c(1,0,0),ar=input$sigma2/11))
  }
  return(y)
  })
#####################################################

output$plotNoise1=renderPlot({
  n=fn()
  x=1:n
  y=noise1()
  par(mar=c(2,2,1,1))
  plot(x,y,type="l", cex=0.6,
       ylim=rangesim())
})

output$plotNoise2=renderPlot({
  n=fn()
  x=1:n
  y=noise2()
  par(mar=c(2,2,1,1))
  plot(x,y,type="l", cex=0.6,
       ylim=rangesim())
})

#####################################################

output$plotSimsignal1a=renderPlot({
  n=fn()
  x=1:n
  y=simSignal1a()
par(mar=c(2,2,1,1))
  plot(x,y,type="l", cex=0.6,
       ylim=rangesim())
})

output$plotSimsignal1b=renderPlot({
  n=fn()
  x=1:n
  y=simSignal1b()
par(mar=c(2,2,1,1))
  plot(x,y,type="l", cex=0.6,
       ylim=rangesim())
})

output$plotSimsignal1c=renderPlot({
  n=fn()
  x=1:n
  y=simSignal1c()
  par(mar=c(2,2,1,1))
  plot(x,y,type="l", cex=0.6,
       ylim=rangesim())
})
output$plotSimsignal2a=renderPlot({
  n=fn()
  x=1:n
  y=simSignal2a()
  par(mar=c(2,2,1,1))
  plot(x,y,type="l", cex=0.6,
       ylim=rangesim())
})

output$plotSimsignal2b=renderPlot({
  n=fn()
  x=1:n
  y=simSignal2b()
  par(mar=c(2,2,1,1))
  plot(x,y,type="l", cex=0.6,
       ylim=rangesim())
})

output$plotSimsignal2c=renderPlot({
  n=fn()
  x=1:n
  y=simSignal2c()
  par(mar=c(2,2,1,1))
  plot(x,y,type="l", cex=0.6,
       ylim=rangesim())
})
########################################################
observe({
  n=fn()
  updateSliderInput(session, "t1a",
                    value=c(1,n),
                    max=n)
  updateSliderInput(session, "t1b",
                    value=c(1,n),
                    max=n)
  updateSliderInput(session, "t1c",
                    value=c(1,n),
                    max=n)
  updateSliderInput(session, "t2a",
                    value=c(1,n),
                    max=n)
  updateSliderInput(session, "t2b",
                    value=c(1,n),
                    max=n)
  updateSliderInput(session, "t2c",
                   value=c(1,n),
                   max=n)
  updateSliderInput(session, "T1a",
                    step=1,
                    max=round(n/2))
  updateSliderInput(session, "T1b",
                    step=1,
                    max=round(n/2))
  updateSliderInput(session, "T1c",
                    step=1,
                    max=round(n/2))
  updateSliderInput(session, "T2a",
                    step=1,
                    max=round(n/2))
  updateSliderInput(session, "T2b",
                    step=1,
                    max=round(n/2))
  updateSliderInput(session, "T2c",
                    step=1,
                    max=round(n/2))
})