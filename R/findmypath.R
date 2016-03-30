#' Find path to waveleT package
#'
#' This function is used internally to find package's path.
#' @keywords waveleT, wavelet, toolkat
#' @export

findmypath=function(dir,file){
  path=system.file(dir,file,package="waveleT")
  return(path)
}
