# This file is part of the WaveleT package
# Copyright 2015 2016 Lise Vaudor
# This program is distributed under the GPLv3 license


require(wavelets)
require(biwavelet)


findmypath=function(dir="",file=""){
  path=paste0("../",dir,"/",file)
  if(dir==""){path=file}
  if(file==""){path=dir}
  return(path)
}