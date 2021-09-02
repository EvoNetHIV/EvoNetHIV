#' @title Create initial network
#'
#' @description Create initial network for EvoNet simulation
#'
#' @param params A list of EvoNet parameters
#' @return Network object
#' @details
#' Creates initial network and sets network attributes “age”,”sqrt_age”, “sex”, “role”, and “att1” for each node (agent) on the network,
#' which is required for estimation of networks using these attributes. Wrapper function for "network.initialize" and 
#' "set.vertex.attribute" functions from "network" package.
#' If additional attributes are desired, they can be added following the template within the function.
#' @examples
#' nw <- setup_initialize_network(evoparams)
#' 
#' @export
setup_initialize_network <- function(params){
#create initial network, set nodal attributes "sex","att1","role"
#input: initial parameter list (evoparams)
#output: initial network ("nw")
  
# initialize network
nw <- network::network.initialize(params$initial_pop,
                                  directed = FALSE)


if(params$model_sex!="msm"){
  sex_vec <- sample(c(0,1),params$initial_pop,prob=c(0.5,0.5),replace=T)
  network::set.vertex.attribute(x = nw, attr = "sex", value =sex_vec)
} else {
  network::set.vertex.attribute(x=nw, attr="sex", value=1)
}
#---------------------------------


#age
age_vec=rep(NA_real_,params$initial_pop)
if(params$model_sex=="msm"){
index_male <- 1:params$initial_pop
age_vec[index_male] <- vital_initial_age_dist( 
  age.range = params$min_age : (params$max_age-1),
  popsize   = length(index_male),
  age_dist = params$male_age_dist)
}else{
  index_male <- which(sex_vec==0)
  index_female <- which(sex_vec==1)
  age_vec[index_male] <- vital_initial_age_dist( 
    age.range = params$min_age : (params$max_age-1),
    popsize   = length(index_male),
    age_dist = params$male_age_dist)
  age_vec[index_female] <- vital_initial_age_dist( 
    age.range = params$min_age : (params$max_age-1),
    popsize   = length(index_female),
    age_dist = params$female_age_dist)
}

#temp qaqc
if(any(is.na(age_vec))){browser()}

network::set.vertex.attribute(x=nw, attr="age", value=age_vec)
network::set.vertex.attribute(x=nw, attr="sqrt_age", value=sqrt(age_vec))


#if (params$age_homo_terms) {
if (0==1) {  # Disable for now
    # Assign age groups (loosely based on agent ages)
  age_group_vec=rep(NA_real_,params$initial_pop)
  
  age_for_age_group_calcs <- round( age_vec + 2.0 - 4.0*runif(length(age_vec)) ,5 )  # Allow for some spread
  age_for_age_group_calcs[index_male] <- age_for_age_group_calcs[index_male] - 3
  
  under_age_15 <- which(age_for_age_group_calcs  < 15)
  ages_15_20   <- which(age_for_age_group_calcs  >= 15 & age_for_age_group_calcs < 20)
  ages_20_27   <- which(age_for_age_group_calcs  >= 20 & age_for_age_group_calcs < 27)
  ages_27_36   <- which(age_for_age_group_calcs  >= 27 & age_for_age_group_calcs < 36)
  ages_36_47   <- which(age_for_age_group_calcs  >= 36 & age_for_age_group_calcs < 47)
  ages_47_60   <- which(age_for_age_group_calcs  >= 47 & age_for_age_group_calcs < 60)
  over_age_60  <- which(age_for_age_group_calcs  >= 60)
  
  age_group_vec[under_age_15] = 0 # Call this 0 b/c this group is assumed never to have sex
  age_group_vec[ages_15_20]   = 1 # These will increase as the agents age.
  age_group_vec[ages_20_27]   = 2
  age_group_vec[ages_27_36]   = 3
  age_group_vec[ages_36_47]   = 4
  age_group_vec[ages_47_60]   = 5
  age_group_vec[over_age_60]  = 6
  if(any(is.na(age_group_vec))){browser()}
  
  network::set.vertex.attribute(x=nw, attr="age_group", value=sqrt(age_group_vec))
}  
  
  
#--------------------------------
if(params$vaccine_trial){
  index <- sample(1:params$initial_pop,params$initial_trial_participants,replace=F)
  trial_vector <- numeric(params$initial_pop)
  trial_vector[index] <- 1
  network::set.vertex.attribute(x=nw, attr="trial_status", value=trial_vector)
}

#--------------------------------
#set generic atts on nw if in use
if(!is.logical(params$generic_nodal_att_values)) 
  nw<- social_set_generic_attribute_on_nw(params, nw)

#set msm roles on nw if in use (default: all versatile)
if(!is.logical(params$role_props) && params$model_sex=="msm")
  nw<-social_set_msm_role_on_nw(params,nw)

if(length(params$age_nw_groups)>1){
  att1 <- rep(NA,length(age_vec))
  age_cats <- 1:length(params$age_nw_groups)
  for(ii in 1:length(age_cats)){
     age1 <- params$age_nw_groups[[ii]][1]
     age2 <- params$age_nw_groups[[ii]][2]
     ix <- which(age_vec > age1 & age_vec < age2+1)
     att1[ix] <- ii
  }
  if(any(is.na(att1))){stop("ages in age_nw_groups don't match with population age structure")}
  network::set.vertex.attribute(x=nw, attr="att1", value=att1)
}


return(nw)
}
