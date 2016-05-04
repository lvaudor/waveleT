# This file is part of the WaveleT package
# Copyright 2015 2016 Lise Vaudor
# This program is distributed under the GPLv3 license

#' Find path to waveleT package
#'
#' This function is used internally to find package's path.
#' @keywords waveleT, wavelet, toolkat
#' @export

findmypath=function(dir,file){
  path=system.file(dir,file,package="waveleT")
  return(path)
}
