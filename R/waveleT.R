#' Launch the Wavelet ToolKat
#'
#' This function allows you to launch the Wavelet ToolKat interface. This is a Shiny App designed to help users analyze series through wavelet transforms.
#' For more information see package's vignette.
#' @keywords waveleT, wavelet, toolkat
#' @export
#' @examples
#' waveleT()

waveleT=function(){
  shiny::runApp(findmypath("app",""))
}

