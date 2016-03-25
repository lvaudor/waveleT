#' Launch the Wavelet ToolKat
#'
#' This function allows you to launch the Wavelet ToolKat
#' @keywords bwegek
#' @export
#' @examples
#' graphiT()

waveleT=function(){
  shiny::runApp(findmypath("app",""))
}

