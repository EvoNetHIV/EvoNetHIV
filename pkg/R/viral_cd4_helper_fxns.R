#################################
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
#' 
#' @export
viral_spvl_cat_fxn <- function(spvl_vec)
{ #Description:
  # converts SPVL into 1 of 9 CD4 categories (based on Pickles et al. data), 
  # this is used to index appropriate row in CD4 data tables
  # Note: hardwired for 9 SPVL categories

  spvl_seq<-c(0,seq(3,6.5,by=0.5),100)
  spvl_seq_labels <- 1:9
  return(as.numeric(as.character(cut(spvl_vec,
                                     breaks = spvl_seq,
                                     labels = spvl_seq_labels) )) )
}
#####################################
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
#' 
#' @export
viral_initialCD4<-function(spvl_values,plist){
  # Description:
  # determines initial CD4 category (1, 2, or 3) for given SPVL category from viral_spvl_cat_fxn()
  
  probs <- runif(length(spvl_values))
  cd4_vec <- rep(1, length(spvl_values))
  
  cat3 <- which(probs < plist$cd4_init_probs[cbind(spvl_values,3)])
  
  cat2 <- which(probs < plist$cd4_init_probs[cbind(spvl_values,2)]  &
                  probs >  plist$cd4_init_probs[cbind(spvl_values,3)] )
  
  if(length(cat3)>0){cd4_vec[cat3] <- 3}
  if(length(cat2)>0){cd4_vec[cat2] <- 2}
  
  return(cd4_vec)
}


#######################################################################
