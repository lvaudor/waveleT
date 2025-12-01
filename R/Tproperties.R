#' Get info regarding x if x is time or date values
#'
#'' @param x Numeric vector of time or date values.
Tproperties <- function(x, origin = x[1]) {
  t_as_date <- as.POSIXct(x, origin = origin)
  xb <- difftime(t_as_date[2:length(t_as_date)],
                 t_as_date[1:(length(t_as_date) - 1)])
  list(mean = mean(xb), units = units(xb))
}

#' Build file name for saving figures or info
#' @param fig_or_info type of file: "Fig" or "Info"
#' @param analysis type of analysis ("CWT" or "DWT" for instance)
#' @param variable name of variable ("y1" for instance)
#' @param type type of representation (1, 2 or 3)
#' @param info the name of info (coefficients)
#' @param period (the period if type=2)
buildname <- function(fig_or_info,
                      analysis,
                      variable = NA,
                      type = NA,
                      info = NA,
                      period = NA) {
  name <- paste0(fig_or_info, "_", analysis)
  if (!is.na(variable)) name <- paste0(name, "_", variable)
  if (!is.na(type)) name <- paste0(name, "_type", type)
  if (!is.na(info) & type != 3) name <- paste0(name, "_", info)
  if (!is.na(period)) name <- paste0(name, "_T", period)
  if (fig_or_info == "Fig") name <- paste0(name, ".", input$graph_format)
  if (fig_or_info == "Info") name <- paste0(name, ".csv")
  return(name)
}
