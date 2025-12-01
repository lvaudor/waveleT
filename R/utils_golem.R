#' golem
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
app_sys <- function(...){
  system.file(..., package = "waveleT")
}
