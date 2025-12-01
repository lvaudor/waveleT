# This file is part of the WaveleT package
# Copyright 2015 2016 Lise Vaudor
# This program is distributed under the GPLv3 license

#' Generate a Single Cosine Signal
#'
#' This function generates a cosine signal of length \code{x} with specified period,
#' amplitude, phase shift, and optional truncation at the beginning and end.
#'
#' @param x Numeric vector. The time or space points at which to evaluate the signal.
#' @param T Numeric. Period of the cosine signal.
#' @param A Numeric. Amplitude of the signal. Default is 1.
#' @param phi Numeric. Phase shift as a fraction of the period. Default is 0.
#' @param tmin Integer. Index from which the signal starts (values before are set to 0). Default is 1.
#' @param tmax Integer. Index at which the signal ends (values after are set to 0). Default is \code{length(x)}.
#'
#' @return Numeric vector of the same length as \code{x} containing the generated cosine signal.
#'
#' @examples
#' x <- 1:100
#' y <- sim_signal(x, T = 20, A = 2, phi = 0.25)
#' plot(x, y, type = "l")
#'
#' @export
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


#' Generate a Sum of Cosine Signals
#'
#' This function generates a signal by summing multiple cosine signals with
#' specified periods, amplitudes, phase shifts, and optional truncations.
#'
#' @param x Numeric vector. The time or space points at which to evaluate the signals.
#' @param T Numeric vector. Periods of the cosine signals.
#' @param A Numeric vector. Amplitudes of each cosine signal. Default is 1 for each period.
#' @param phi Numeric vector. Phase shifts for each cosine signal. Default is 0 for each period.
#' @param tmin Integer vector. Starting indices for each signal (values before are set to 0). Default is 1 for each signal.
#' @param tmax Integer vector. Ending indices for each signal (values after are set to 0). Default is \code{length(x)} for each signal.
#'
#' @return Numeric vector of the same length as \code{x} containing the summed signal.
#'
#' @examples
#' x <- 1:100
#' y <- sim_signalsum(x, T = c(20, 50), A = c(1, 0.5))
#' plot(x, y, type = "l")
#'
#' @export
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
