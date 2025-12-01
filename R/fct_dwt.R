#' Maximum level of decomposition in DWT/MRA
#' @param n signal length
#' @param wt_filter Wavelet filter name (from wavelets' package `wt.filter`). Useful only if modwt=FALSE. Defaults to "la8".
#' @param modwt whether to consider MODWT (TRUE, default) or DWT (FALSE)
#' @return An integer indicating the maximum level of decomposition
#' @export
#' @examples
#' wav_jmax(1000)
#' wav_jmax(1000,wt_filter="la8",modwt=FALSE)
#' wav_jmax(1000,wt_filter="d4",modwt=FALSE)
wav_jmax=function(n,wt_filter="la8", modwt=TRUE){
  jmax = floor(log2(n))
  if(!modwt){
    L=wavelets::wt.filter(wt_filter)@L
    jmax= floor(log2(n/(L-1)+1))
  }
  return(as.integer(jmax))
}


#' Multi-resolution analysis (MRA) object
#' @param data Data frame with raw data
#' @param xname The name of the x (spatial or temporal position) column in data. Defaults to x.
#' @param signalname The name of the signal column in data
#' @param wt_filter Wavelet filter name (from wavelets' package `wt.filter`)
#' @export
#' @examples
#' data(data_waveleT)
#' mra_obj=wav_mra_obj(data_waveleT,"y1",wt_filter="la8")
wav_mra_obj=function(data,xname="x", signalname,wt_filter="la8"){
  x=as.matrix(data[[xname]])
  y=as.matrix(data[[signalname]])
  mra_obj=wavelets::mra(y,
                n.levels=wav_jmax(length(y),
                                  wt_filter=wt_filter),
                filter=wt_filter,
                method="modwt")
  mra_obj=list(data=data, xname=xname ,signalname=signalname,mra=mra_obj)
  return(mra_obj)
}

#' Multi-resolution analysis (MRA) data.frame
#' @param mra_obj mra analysis as returned by wav_mra_obj()
#' @return A data.frame with all detail and smooth sub-signals
#' @export
#' @examples
#' data(data_waveleT)
#' mra_obj=wav_mra_obj(data_waveleT,"x","y1",wt_filter="la8")
#' mra_data=wav_mra_data(mra_obj)
wav_mra_data <- function(mra_obj){
  data_obj=mra_obj$data[,c(mra_obj$xname, mra_obj$signalname)]
  n <- nrow(data_obj)
  jmax=mra_obj$mra@level
  resulting_data=data_obj
  for(j in 1:jmax){
    datatmp <- data.frame(mra_obj$mra@S[[j]][1:n], mra_obj$mra@D[[j]][1:n])
    colnames(datatmp) <- c(paste0("S",j), paste0("D",j))
    resulting_data <- cbind(resulting_data, datatmp)
  }
  return(resulting_data)
}


#' Multi-resolution analysis (MRA) sum
#'
#' Computes the sum of selected decomposition levels from a multi-resolution analysis.
#'Z
#' @param mra_data Data frame or list containing the MRA components (e.g., D1, D2, ..., S).
#' @param levels Character vector specifying which levels to include (e.g., c("D1","S3")).
#' @param wt_filter Wavelet filter name (from wavelets' package `wt.filter`)
#' @return A list containing:
#' \item{y}{Numeric vector, the reconstructed signal sum of selected levels.}
#' \item{lab}{Character, a label representing the included levels.}
#' \item{n.boundary}{Integer, number of boundary points.}
#' @export
#' @examples
#' data(data_waveleT)
#' mra_obj=wav_mra_obj(data_waveleT,"x","y1",wt_filter="la8")
#' mra_data=wav_mra_data(mra_obj)
#' mra_sum <- wav_mra_sum(mra_data,levels=c("D8","D9","S9"),wt_filter="la8")
wav_mra_sum=function(mra_data,levels, wt_filter){
  n=nrow(mra_data)
  y=rep(0,n)
  lab=""
  n.boundary=0
  jmax=wav_jmax(n)
  if(length(levels)>1){
    jmax=max(as.numeric(gsub("^[D,S]",levels,replacement="")))
    for (j in 1:length(levels)){
        if(!(levels[j] %in% colnames(mra_data))){
          stop(paste0("Level ",levels[j]," not found in mra_data"))
        }
        y=y+mra_data[[levels[j]]]
        lab=paste0(lab,"+", levels[j])
    }
  }
  labsplit=unlist(strsplit(lab,""))
  lab=paste(labsplit[2:length(labsplit)], collapse="")
  L=wavelets::wt.filter(wt_filter)@L
  Ljmax= (L-1)*2^(jmax-1)
  n.boundary=min(Ljmax,n)
  resulting_data=cbind(mra_data,mrasum=y)
  return(list(data=resulting_data, lab=lab,n.boundary=n.boundary))
}


#' Plot a single DWT/MRA signal with significance boundaries
#' @param mra_sum List output from `wav_mra_sum`, containing $y, $lab, $n.boundary.
#' @param yscale Logical, whether to use the original signal's y-scale.
#' @param plot Logical, whether to overlay the original signal.
#' @param col Color for the MRA sum line (default `"blue"`).
#' @param xlim Numeric vector, x-axis limits.
#' @export
#' @return Invisibly returns NULL. The function produces a plot.
#' @examples
#' data(data_waveleT)
#' mra_obj=wav_mra_obj(data_waveleT,"x","y1",wt_filter="la8")
#' mra_data=wav_mra_data(mra_obj)
#' mra_sum <- wav_mra_sum(mra_data, c("D2","D3","S3"),wt_filter="la8")
#' plotDWT_sig(mra_sum, col="blue")
plotDWT_sig=function(mra_sum,
                     yscale=TRUE,
                     plot=TRUE,
                     col="blue",
                     xlim=NULL){
  y=mra_sum$data$mrasum
  x=mra_sum$data[,1]
  yraw=mra_sum$data[,2]
  signalname=colnames(mra_sum$data)[2]
  lab=mra_sum$lab
  nlim=mra_sum$n.boundary
  if(is.null(xlim)){xlim=range(x)}
  ### compute y-scale limits ###
  myrange=range(y)
  if(yscale){myrange=range(yraw)}
  ### plot frame #####
  plot(mra_sum$data[,1],yraw,
       ylim=myrange,
       col="white",
       xlab=colnames(mra_sum$data)[1],
       ylab=signalname,
       main=parse(text=lab),
       xaxs="i", yaxs="i",
       xlim=xlim)
  ### plot boundary regions
  abline(v=c(x[nlim],x[length(x)-nlim]),col="light grey",lty=3)
  ### plot raw signal
  if(plot){points(x,yraw, type="l", col="dark grey")}
  ### plot MRA sum
  points(x,y,col=col,type="l", lwd=2)
}
##########################################

#' Superpose two DWT/MRA signals
#' Plots two MRA signals together, optionally overlaying the original signals.
#'
#' @param xname name of x variable
#' @param y1name name of y1 variable
#' @param y2name name of y2 variable
#' @param mra_sum1 List output from `wav_mra_sum()` for the first signal.
#' @param mra_sum2 List output from `wav_mra_sum()` for the second signal.
#' @param yscale1 Logical, whether to use original signal y-scale for signal 1.
#' @param yscale2 Logical, whether to use original signal y-scale for signal 2.
#' @param plot1 Logical, whether to overlay original signal 1.
#' @param plot2 Logical, whether to overlay original signal 2.
#' @param xlim Numeric vector, x-axis limits.
#' @export
#' @return Invisibly returns NULL. The function produces a plot.
#' @examples
#' data(data_waveleT)
#' mra_obj1=wav_mra_obj(data_waveleT,"x","y1",wt_filter="la8")
#' mra_obj2=wav_mra_obj(data_waveleT,"x","y2",wt_filter="la8")
#' mra_data1=wav_mra_data(mra_obj1)
#' mra_data2=wav_mra_data(mra_obj2)
#' mra_sum1 <- wav_mra_sum(mra_data1, c("D6","D7","S7"),wt_filter="la8")
#' mra_sum2 <- wav_mra_sum(mra_data2, c("D6","S6"),wt_filter="la8")
#' plotDWT_superpose(mra_sum1, mra_sum2)
plotDWT_superpose=function(mra_sum1,
                           mra_sum2,
                           yscale1=TRUE,
                           yscale2=TRUE,
                           plot1=TRUE,
                           plot2=TRUE,
                           xlim=NULL){
  x=mra_sum1$data[,1]
  y1raw=mra_sum1$data[,2]
  y2raw=mra_sum2$data[,2]
  y1=mra_sum1$data$mrasum
  y2=mra_sum2$data$mrasum
  xname=colnames(mra_sum1$data)[1]
  yname1=colnames(mra_sum1$data)[2]
  yname2=colnames(mra_sum2$data)[2]
  lab1=paste0(yname1,":",mra_sum1$lab)
  lab2=paste0(yname2,":",mra_sum2$lab)
  nlim1=mra_sum1$n.boundary
  nlim2=mra_sum2$n.boundary
  if(is.null(xlim)){xlim=range(x)}
  ### compute y-scale limits ###
  y1tmp=y1
  y2tmp=y2
  if(yscale1){y1tmp=y1raw}
  if(yscale2){y2tmp=y2raw}
  ### plot frame #####
  plot(x,y1tmp,
       ylim=range(y1tmp),
       type="n",
       xlab=xname,
       xaxs="i", yaxs="i",
       xlim=xlim)
  if(plot1){points(x,y1,type="l", lwd=2, col="blue")}
  if(plot2){points(x,y2, type="l", col="red")}
  #################################
  points(x,y2,col="red",type="l",lwd=2)
  ### plot boundary regions
  abline(v=c(x[nlim1],x[length(x)-nlim1]),col="blue",lty=3)
  abline(v=c(x[nlim2],x[length(x)-nlim2]),col="red",lty=3)
  ### legend
  legend("topright",
         col=c("blue","red"),
         lty=1,
         lwd=c(2,2,1,1),
         legend=c(parse(text=lab1),parse(text=lab2))
  )
}
###########################################
#' Plot variance by decomposition level from a DWT
#'
#' Computes and plots the normalized variance of wavelet coefficients at each decomposition level.
#'
#' @param y Numeric vector, the signal.
#' @param jmax Integer, maximum decomposition level.
#' @param wt_filter Character, wavelet filter name (e.g., `"la8"`).
#' @param step Numeric, sampling step of the signal.
#' @export
#' @return Invisibly returns NULL. The function produces a plot of normalized variance vs decomposition levels.
#' @examples
#' data(data_waveleT)
#' mra_obj=wav_mra_obj(data_waveleT,"y1",wt_filter="la8")
#' mra_data=wav_mra_data(mra_obj)
#' mra_sum <- wav_mra_sum(mra_data,c("D8","D9","S9"), "la8")
#' plotDWT_var(data_waveleT[[2]], jmax=3, wt_filter="la8")
#' plotDWT_var(data_waveleT[[2]], jmax=7, wt_filter="la8",step=10)
plotDWT_var=function(y,jmax,wt_filter,step=1){
  jmax=wav_jmax(length(y))
  mydwt=wavelets::modwt(y,filter=wt_filter, n.levels=jmax)
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
       type="b",
       lwd=2,pch=20,cex=3)
  abline(v=1,lty=2)
  axis(side=2,at=-(1:jmax), paste("lev",1:jmax,"=",scales))
}

