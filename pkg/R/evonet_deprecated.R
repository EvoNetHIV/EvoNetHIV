#' Deprecated function(s) in the evonet package
#'
#'
#' @export

plots_popsumm <- function(...) {
  #.Deprecated("evoplot",package="evonet")
  evoplot(...)
}

#' @export

input_parameters_cd4_data <- function(...) {
 # .Deprecated(package="evonet")
  NULL
}

#' @export

input_parameters_primary<- function(...) {
#  .Deprecated("evonet_setup",package="evonet")
  evonet_setup(...)
}

#' @export

vital_aging_module<- function(...) {
  #  .Deprecated("aging","package="evonet")
  aging(...)
}

#' @export

vital_births_module <- function(...) {
  #  .Deprecated("births","package="evonet")
  births(...)
}

#' @export

social_testing_diagnosis_module <- function(...) {
  #  .Deprecated("testing","package="evonet")
  testing(...)
}

#' @export

social_treatment_module <- function(...) {
  #  .Deprecated("treatment","package="evonet")
  treatment(...)
}

#' @export

social_coital_acts_module <- function(...) {
  #  .Deprecated("coital_acts","package="evonet")
  coital_acts(...)
}

#' @export

vital_deaths_module<- function(...) {
  #  .Deprecated("deaths","package="evonet")
  deaths(...)
}

#' @export

vital_births_module <- function(...) {
  #  .Deprecated("births","package="evonet")
  births(...)
}
