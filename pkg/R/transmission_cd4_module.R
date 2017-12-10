#' @title Title
#'
#' @description Description
#'
#' @param x A number.
#' @param y A number.
#' @return return value here.
#' @details
#' Additional details here
#' @examples
#' example function call here
#' @export
transmission_cd4_module <- function(dat,at)
{  
  #Descritpion:
  # add cd4 info to newly infected, exit if none
  # if infections occur in timestep, once SPVL determined in “transmission_bookkeeping_module”, 
  # then initial CD4 values and waiting times per CD4 categories can be calculated. 
  # main input: dat$pop$LogSetPoint
  # outputs:CD4,
           #CD4_initial_value,
           #CD4_initial_value
           #CD4_time
           #CD4_treatment_delay_index
           #CD4_time_cat1
           #CD4_time_cat2
           #CD4_time_cat3
           #CD4_time_cat4

  
  #####################################
  #if no new infections occur, stop fxn
  if(is.null(dat$discord_coital_df)){return(dat)}
  index <- which(dat$discord_coital_df$infection==1)
  if(length(index)==0){return(dat)}
  ######################################

  index1 <- dat$discord_coital_df$sus_id[index]
  
  dat$pop$spvl_cat[index1] <- viral_spvl_cat_fxn(dat$pop$LogSetPoint[index1])
  dat$pop$CD4[index1]      <-  viral_initialCD4(dat$pop$spvl_cat[index1],dat$param )
  dat$pop$CD4_initial_value[index1] <- dat$pop$CD4[index1]
  dat$pop$CD4_nadir[index1] <- dat$pop$CD4[index1]
  dat$pop$CD4_time[index1] <- 0
  dat$pop$CD4_treatment_delay_index[index1] <- 0
  
  #should be deleted, test first 3/2/16
  #temp_vec <- c("CD4_time_cat1","CD4_time_cat2",
  #              "CD4_time_cat3","CD4_time_cat4")
  
  #for(ii in 1:length(temp_vec))
  #{
   # part1 <- log(runif(length(index1)))
    #part2 <- -1.0/dat$param$CD4_lookup[cbind(dat$pop$spvl_cat[index1],ii)]
    
    #dat$pop[[temp_vec[ii]]][index1]  <- part1 / part2
  #}
 

  return(dat)
}
