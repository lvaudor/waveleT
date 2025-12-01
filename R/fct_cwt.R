#' Extract information matrix from a wavelet object
#'
#' Returns a matrix of values from a wavelet object based on the `info` argument.
#'
#' @param mywt A wavelet object, typically returned by `wt()`.
#' @param info Character. Type of information to extract: `"rsq"`, `"power"`, `"power.norm"`, `"power.corr"`, `"power.corr.norm"`, `"wavelet"`, `"phase"`.
#' @return A matrix corresponding to the requested information.
#' @examples
#' \dontrun{
#' wt_obj <- wt(matrix(rnorm(100), ncol=2))
#' M <- wt_infomatrix(wt_obj, "power")
#' }
wt_infomatrix=function(mywt,info){
  if(info=="rsq"){M=mywt$rsq}
  # corr
  if (info == "power.corr" | info == "power.corr.norm") {
    if (mywt$type == "wtc" | mywt$type == "xwt") {
      mywt$power= M= mywt$power.corr
      mywt$wave= M= mywt$wave.corr
    }
    else mywt$power = M= mywt$power.corr
  }
  # norm
  if (info == "power.norm" | info == "power.corr.norm") {
    if (mywt$type == "xwt") {
      M = mywt$power/(mywt$d1.sigma * mywt$d2.sigma)
    }
    else if (mywt$type == "wtc" | mywt$type == "pwtc") {
      M=mywt$rsq
    }
    else {
      M = abs(mywt$power/mywt$sigma2)
    }
  }
  else if (info == "power" | info == "power.corr") {
    M = mywt$power
  }
  else if (info == "wavelet") {
    M = Re(mywt$wave)
  }
  else if (info == "phase") {
    M = mywt$phase
  }
  return(M)
}

#' Extract local extrema from a numeric vector
#'
#' Returns the indices of local maxima or minima.
#'
#' @param y Numeric vector.
#' @param minima_only Logical, default FALSE. If TRUE, only minima are returned.
#' @param maxima_only Logical, default FALSE. If TRUE, only maxima are returned.
#' @return Integer vector of indices of local extrema.
#' @examples
#' x <- c(1,3,2,4,3)
#' extract_local_extrema(x)          # indices of all extrema
#' extract_local_extrema(x, maxima_only=TRUE)
extract_local_extrema=function(y, minima_only=F, maxima_only=F){
  sdy=sign(diff(y))
  spdy=c(sign(sdy[1:(length(sdy)-1)]*sdy[2:length(sdy)]),0)
  ind=which(spdy==-1)+1
  if(minima_only){ind=which(spdy==-1 & sdy==-1)+1}
  if(maxima_only){ind=which(spdy==-1 & sdy==+1)+1}
  return(ind)
}

#' Compute wavelet significance
#'
#' Computes significance levels for a wavelet object.
#'
#' @param data Numeric vector or matrix of input data.
#' @param mywt Wavelet object.
#' @param alpha Numeric, significance level.
#' @return List with `sig1` and `sig2` numeric vectors.
#' @examples
#' \dontrun{
#' sig <- WT_sig(matrix(rnorm(100), ncol=2), wt_obj, 0.05)
#' }
WT_sig=function(data,mywt,alpha){
  sig1=rep(NA,length(mywt$scale))
  sig2=rep(NA,length(mywt$scale))
  if(mywt$type=="wt"){
    mywtsig=biwavelet::wt.sig(d=data,
                   dt=mywt$dt,
                   scale=mywt$scale,
                   sig.level=1-alpha,
                   mother=mywt$mother)
    sig1=mywtsig$fft.theor
    sig2=mywtsig$signif
  }
  return(list(sig1,sig2))
}


#' Plot wavelet analysis type 1 (Info=f(x,T))
#'
#' @param data Data matrix used for the wavelet analysis.
#' @param mywt Wavelet object.
#' @param plot.sig Logical, whether to plot significance.
#' @param info Character, type of info to plot.
#' @param step Numeric, step for scales.
#' @param units Character, units for time axis.
#' @param plot.cb Logical, whether to show colorbar.
#' @param xlim Numeric vector for x-axis limits.
#' @param ylim Numeric vector for y-axis limits.
#' @param plot.phase Logical, whether to plot phase arrows.
#' @return Data frame of extracted info for the plotted wavelet.
#' @examples
#' \dontrun{
#' plotWT_type1(data_matrix, wt_obj, TRUE, "power", 1, "s", TRUE, NULL, NULL, FALSE)
#' }
plotWT_type1=function(data,
                      mywt,
                      plot.sig,
                      info,
                      step,
                      units,
                      plot.cb,
                      xlim=NULL,
                      ylim=NULL,
                      plot.phase){
  if(plot.cb){par(mar=c(4,4,3,8),las=1)}
  if(mywt$type!="wtc" & mywt$type!="xwt"){
    mymain=paste(colnames(data)[2],
                 ": ",info, sep="")
  }
  if(mywt$type=="wtc"|mywt$type=="xwt"){
    mymain=paste(colnames(data)[2],
                 " and ",
                 colnames(data)[3],
                 ": ",info, sep="")
  }
  if(mywt$type=="wtc" & info=="rsq"){
    plot(mywt,
         plot.sig=plot.sig,
         plot.cb=plot.cb,
         plot.phase=plot.phase,
         arrow.cutoff=0.5,
         main=mymain,
         xlab=colnames(data)[1],
         ylab=paste0("T",units),
         xaxt="n",
         xlim=xlim,
         ylim=ylim
    )
  }
  if(mywt$type!="wtc" | info!="rsq"){
    plot(mywt,
         plot.sig=plot.sig,
         type=info,
         plot.cb=plot.cb,
         main=mymain,
         plot.phase=plot.phase,
         arrow.cutoff=0.5,
         xlab=colnames(data)[1],
         ylab=paste0("T",units),
         xaxt="n",
         xlim=xlim,
         ylim=ylim
    )
  }
  par(mar=c(4,4,3,2))
  xlocs=pretty(mywt$t)
  axis(side=1,at=xlocs,lab=xlocs)
  dl_info=as.data.frame(wt_infomatrix(mywt,info))
  dl_info=data.frame(period=mywt$period,dl_info)
  return(data.frame(dl_info))
}


#' #' Plot wavelet analysis type 2 (Info=f(x) at specific period)
#'
#' @param data Data matrix.
#' @param mywt Wavelet object.
#' @param info Character, info type.
#' @param myperiod Numeric, selected period.
#' @param plot.sig Logical, whether to plot significance.
#' @param plot.maxima Logical, whether to mark local maxima.
#' @param plot.minima Logical, whether to mark local minima.
#' @param step Numeric step.
#' @param units Character units.
#' @param xlim Numeric vector for x-axis limits.
#' @return Data frame of extracted info at the specified period.
plotWT_type2=function(data,
                      mywt,
                      info,
                      myperiod,
                      plot.sig,
                      plot.maxima,
                      plot.minima,
                      step,
                      units,
                      xlim){
  M=wt_infomatrix(mywt,info)
  Msignif=mywt$signif
  row=which.min(abs(mywt$period-myperiod))
  ####
  x=data[,1]
  y=M[row,]
  ysignif=Msignif[row,]
  indnotsignif=which(ysignif<1)

  plot(x,y,
       type="l", ylab="", xaxs="i",
       xlab=colnames(data)[1],
       xlim=xlim,
       main=paste(colnames(data)[2],
                  ": ",
                  info,
                  " at period T=",
                  myperiod, units,
                  sep="")
  )
  mydata=ts(data.frame(x,y))
  mydatabis=mydata
  mydatabis[indnotsignif,]=NA
  if(plot.sig){
    points(mydatabis[,1],mydatabis[,2],col="red",type="l")
  }
  #########
  mymaxima=extract_local_extrema(y, maxima_only=T)
  myminima=extract_local_extrema(y, minima_only=T)
  if(plot.maxima){abline(v=x[mymaxima], col="red")}
  if(plot.minima){abline(v=x[myminima], col="blue")}
  ##################################################
  signif=rep("no",length(x))
  signif[-indnotsignif]="yes"
  localmax=rep("no",length(x))
  localmax[mymaxima]="yes"
  localmin=rep("no",length(x))
  localmin[myminima]="yes"
  dl_info=data.frame(x,
                     y,
                     signif,
                     localmax,
                     localmin)
  colnames(dl_info)[3]=paste("signif_alpha",1-mywt$sig.level,sep="")
  return(dl_info)
}

#' Plot wavelet analysis type 3 (Power=f(T))
#'
#' @param data Data matrix.
#' @param mywt Wavelet object.
#' @param plot.sig Logical.
#' @param alpha Numeric significance level.
#' @param step Numeric step.
#' @param units Character units.
#' @param ylim Numeric vector for y-axis limits.
#' @return Data frame of global power spectrum.
plotWT_type3=function(data,
                      mywt,
                      plot.sig,
                      alpha,
                      step,
                      units,
                      ylim
){
  f=function(x){log2(x+1)}
  ### main plot #####################################
  if(mywt$type=="wt"){M=mywt$power/mywt$sigma2}
  if(mywt$type=="xwt"|mywt$type=="cwt"){M=mywt$power/(mywt$d1.sigma*mywt$d2.sigma)}
  T=mywt$period
  GlobalSpectrum=rowMeans(M)
  plot(f(GlobalSpectrum),-log2(T),
       xlab="power",
       ylab=paste0("T",units),type="l",
       xaxt="n",yaxt="n",
       xaxs="r", yaxs="i",
       xlim=f(range(M)),
       main=paste(colnames(data)[2],": Power spectrum",sep=""),
       ylim=-log2(ylim[2:1]))
  for (k in 1:nrow(M)){
    points(x=f(range(M[k,])),
           y=rep(-log2(T[k]),2),
           type="l", col="lightgrey", lwd=3)
  }
  for (k in 1:nrow(M)){
    points(x=f(quantile(M[k,],c(0.25,0.75))),
           y=rep(-log2(T[k]),2),
           type="l", col="dark grey", lwd=3)
  }
  points(f(GlobalSpectrum),-log2(T),col="red", type="l", lwd=2)

  sig=WT_sig(data,mywt,alpha)
  if(plot.sig){
    for(k in 1:2){
      sigk=sig[[k]]
      points(f(sigk),-log2(mywt$period),
             col="forestgreen", type="l", lty=k, lwd=2)
    }
  }
  axis(side=1,at=f(c(0,2^(0:30))),lab=c(0,2^(0:30)))
  abline(h=-log2(max(T)))
  axis(side=2,
       at=-log2((2^(0:100))*step),
       lab=2^(0:100)*step)
  abline(h=-log2((2^(0:100))*step), lty=2)

  dl_info=data.frame(period=T,
                     meanpower=GlobalSpectrum,
                     fft_theo=sig[[1]],
                     fft_sig=sig[[2]])
  colnames(dl_info)[4]=paste("fft_sig_alpha",1-mywt$sig.level)
  return(dl_info)
}

#' Plot WT transforms results
#'
#' @param mywt Wavelet object.
#' @param data Data matrix.
#' @param plot.sig Logical.
#' @param alpha Numeric.
#' @param info Character info type.
#' @param mother Character, mother wavelet.
#' @param plot.type Character, one of "1) Info=f(x,T)", "2) Info=f(x)", "3) Power=f(T)".
#' @param myperiod Numeric, selected period.
#' @param plot.maxima Logical.
#' @param plot.minima Logical.
#' @param step Numeric.
#' @param x_is_date Logical.
#' @param plot.cb Logical.
#' @param xlim Numeric vector.
#' @param ylim Numeric vector.
#' @param plot.phase Logical.
#' @export
#' @return Data frame of info used for plotting.
plotWT=function(mywt,
                data,
                plot.sig,
                alpha,
                info,
                mother,
                plot.type,
                myperiod,
                plot.maxima,
                plot.minima,
                step,
                x_is_date,
                plot.cb,
                xlim,
                ylim,
                plot.phase){
  par(mar=c(4,4,3,2),las=1)
  plot.sig.tmp=plot.sig
  units=""
  if(x_is_date){
    Tprop=Tproperties(data[,1],origin=data[1,1])
    units=Tprop$units
    units=paste0(" (",units,")")
  }
  x=data[,1]
  if(mywt$type!="wt"){plot.sig.tmp=FALSE}
  if(plot.type=="1) Info=f(x,T)"){
    myinfo=plotWT_type1(data,
                        mywt,
                        plot.sig,
                        info,
                        step,
                        units,
                        plot.cb,
                        xlim,
                        ylim,
                        plot.phase
    )
  }
  ####
  if(plot.type=="2) Info=f(x)"){
    myinfo=plotWT_type2(data,
                        mywt,
                        info=info,
                        plot.sig=plot.sig.tmp,
                        myperiod=myperiod,
                        plot.maxima=plot.maxima,
                        plot.minima=plot.minima,
                        step,
                        units,
                        xlim
    )
  }
  ####
  if(plot.type=="3) Power=f(T)"){
    myinfo=plotWT_type3(data=data,
                        mywt=mywt,
                        plot.sig=plot.sig.tmp,
                        alpha=alpha,
                        step,
                        units,
                        ylim
    )
  }
  return(myinfo)
}

