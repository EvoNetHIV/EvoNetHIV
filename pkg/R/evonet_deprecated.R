#' Deprecated function(s) in the evonet package
#'
#'
#' @export

plots_popsumm <- function(...) {
  .Deprecated("evoplot",package="evonet")
  evoplot(...)
}

input_parameters_cd4_data <- function(...) {
  .Deprecated(package="evonet")
  NULL
}

input_parameters_primary<- function(...) {
  .Deprecated("evonet_setup",package="evonet")
  evonet_setup(...)
}