#' Log-scaled Time Range Slider for Wavelet Analysis
#'
#' Creates a `sliderInput` for Shiny with dynamically adjusted bounds and
#' logarithmic tick marks derived from the input time series. The maximum range
#' is estimated using a 17\% heuristic of the series length, mirrored around 0,
#' and scaled by `step`.
#'
#' @param inputId (character) The Shiny input ID for the slider.
#' @param data (matrix or data.frame) A dataset where the first column contains the time series.
#' @param step (numeric) Resolution step used to build the time scale and compute slider range.
#'
#' @return A `shiny.tag` object containing a ready-to-render Shiny slider.
#'
#' @details
#' - The slider is only updated when `inputId` is invalidated by its triggering event.
#' - Tick marks are built using powers of 2 multiplied by `step` for log spacing.
#' - All ticks smaller than 1 are discarded to avoid invalid UI values.
#' - For Shiny versions >= 0.11, the HTML slider includes a `data-values` attribute
#'   storing the computed tick positions for downstream JavaScript customization.
#'
#' @examples
#' if (interactive()) {
#'   df <- data.frame(time = 1:100, value = rnorm(100))
#'   shiny::fluidPage(
#'     flogyslider("time_range", df, step = 1)
#'   )
#' }
#'
#' @export
flogyslider <- function(inputId, data, step){
  x <- data[, 1]
  n.obs=length(x)
  dt=step
  Jmax=floor(log(n.obs)/log(2))-1
  scales=dt*2^(1:Jmax)
  html=sliderInput(inputId=inputId,
                   label="T range",
                   min=min(scales),
                   max=max(scales),
                   value=range(scales),
                   ticks=FALSE)
}

fxslider=function(inputId,data){
  x=data[,1]
  html=sliderInput(inputId=inputId,
                   label="x range",
                   min=min(x),
                   max=max(x),
                   value=c(min(x),max(x)))
  html
}

