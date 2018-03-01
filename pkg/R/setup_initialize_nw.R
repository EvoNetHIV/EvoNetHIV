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

#"id" mainly for qaqc testing
id_vec <- 1:params$initial_pop
network::set.vertex.attribute(x = nw, attr = "id",
                              value = id_vec)


if(params$model_sex!="msm"){
  sex_vec <- sample(c("m","f"),params$initial_pop,prob=c(0.5,0.5),replace=T)
  network::set.vertex.attribute(x = nw, attr = "sex", value =sex_vec)
} else {
  network::set.vertex.attribute(x=nw, attr="sex", value="m")
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
  index_male <- which(sex_vec=="m")
  index_female <- which(sex_vec=="f")
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
network::set.vertex.attribute(x=nw, attr="age_cat", value=c(1, 2)[findInterval(x = age_vec, vec = c(14, 25))])
network::set.vertex.attribute(x=nw, attr="sqrt_age", value=sqrt(age_vec))

## Set vaccination/risk compensation status on network if risk compensation in the form of increased degree is in effect. Initially, all agents are unvaccinated, so nodal attribute is equal to 0 for all agents. During simulation, vaccination/risk compensation status are set in social_treatment_vaccination
if(params$risk_comp_degree) {
  network::set.vertex.attribute(x = nw, attr = "vacc_rc", value = rep(0, params$initial_pop))  
}

#--------------------------------
#set generic atts on nw if in use
if(!is.logical(params$generic_nodal_att_values)) 
  nw<- social_set_generic_attribute_on_nw(params, nw)

#set msm roles on nw if in use (default: all versatile)
if(!is.logical(params$role_props) && params$model_sex=="msm")
  nw<-social_set_msm_role_on_nw(params,nw)

return(nw)
}