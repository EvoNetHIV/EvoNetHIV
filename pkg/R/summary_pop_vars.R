#' @export

pop_vars <- function(model) {
  
  pop_vars <- c("Status", "Time_Inf", "age_infection", "CD4", "CD4_time_death", "virus_sens_vacc", "vaccinated", "vacc_init_time", "age", "arrival_time", "id", "eligible_care", "treated", "tx_init_time", "circum", "sti_status", "sex", "vacc_rr", "Time_Death")
  
  rm_pop_ix <- which(!names(model$pop[[1]]) %in% pop_vars)
  
  for(i in 1:length(model$pop)) {
    model$pop[[i]][rm_pop_ix] <- NULL
  }
  
  return(model)
}


