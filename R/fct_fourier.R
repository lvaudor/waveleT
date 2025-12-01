#' Calculate Fourier transform for a 2-column dataset
#'
#' This function computes the Fourier transform of the second column of a dataset
#' and returns the amplitudes for frequencies corresponding to periods within a
#' specified range.
#'
#' @param data A numeric data frame or matrix with at least two columns.
#'             The first column is treated as the x-axis (time or space),
#'             the second column as the signal to transform.
#' @param mystep Numeric. The step size between consecutive x values.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{freq}{Frequency index.}
#'     \item{T}{Period corresponding to each frequency.}
#'     \item{y}{Amplitude (modulus) of the Fourier transform.}
#'   }
#' @export
calcFourier=function(data,mystep){
  x=data[,1]
  y=data[,2]
  n=nrow(data)
  #
  scalemin=2*mystep
  J1 = round(log2(n* 0.17)/(1/12))
  scalemax = 2*mystep * 2^(J1 * (1/12))

  jmax=floor(log(n)/log(2))
  ####
  freq=1:n
  T=(n*mystep)/freq
  y=Mod(fft(y))
  ind=which(T>scalemin & T<scalemax)
  fourier_results=data.frame(freq=freq[ind],
                             T=T[ind],
                             y=y[ind])
  o=order(fourier_results$T,decreasing=FALSE)
  fourier_results=fourier_results[o,]
  return(fourier_results)
}


#' Plot Fourier transform results
#'
#' This function plots the modulus of the Fourier transform against
#' the logarithm of the period. It uses `calcFourier` internally to
#' compute the transform.
#'
#' @param data A numeric data frame or matrix with at least two columns.
#' @param mystep Numeric. Step size between consecutive x values.
#' @param x_is_date Logical. If TRUE, interprets the first column as date/time.
#'
#' @return A plot of Fourier coefficients versus period. No return value.
#' @export
plotFourier=function(data,mystep,x_is_date){
  units=""
  if(x_is_date){
    Tprop=Tproperties(data[,1],origin=data[1,1])
    units=Tprop$units
    units=paste0(" (",units,")")
  }
  fourier_results=calcFourier(data,mystep)
  n=nrow(fourier_results)
  jmax=floor(log(n)/log(2))
  par(las=1)
  plot(fourier_results$y,
       -log2(fourier_results$T),
       type="b",
       pch=20,cex=2,
       lwd=2,
       yaxt="n",
       ylab=paste0("T",units),
       xlab="Fourier coef.", yaxs="i")
  axis(side=2,at=-log2(2^(1:jmax)*mystep),
       lab=2^(1:jmax)*mystep)
  abline(h=-log2(2^(1:jmax)*mystep),lty=2)
}

