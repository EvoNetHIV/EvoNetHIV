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
plot_table_parameters <- function(params,outpath,open=FALSE )
{
  
  index <- ((params$min_age : params$max_age)-
             params$min_age)
  
  agedf <-  data.frame(
                AGE = params$min_age:(params$max_age-1),
                ANNUAL_ASMR = params$ASMR_18_to_70[index],
                TIMESTEP_ASMR = format(params$mort_per_timestep ,digits=4),
                INITIAL_AGE_DIST = format(params$male_age_dist,4) )
  
  agent_atts_df <- data.frame(
                     index = 1:length(params$popVariables),
                     name = params$popVariables)
  
  #get rid of matrices (for now, too hard to display)
  params$male_age_dist_18_to_70 <- NULL
  params$ASMR_18_to_70 <- NULL
  params$mort_per_timestep <- NULL
  params$male_age_dist <- NULL
  params$cd4_init_probs <- NULL
  params$CD4_lookup <- NULL
  params$cd4_time_aids_matrix <- NULL
  params$popVariables <- NULL
  params$ASMR <- NULL
  
  if(is.null(params$nw_coef.form)){
    params$nw_coef.form <- NA
  }
  
  paramdf <- data.frame(
                  PARAMETERS = names(params),
                  VALUES = unlist(unname(params)),
                  stringsAsFactors=F)
  
  paramdf$VALUES[is.na(paramdf$VALUES)]="NA"
  paramdf$CATEGORY <- rep("                      ",nrow(paramdf))
  paramdf$COMMENTS <- rep("                      ",nrow(paramdf))
  
  ix2 <- 1:nrow(paramdf)
  tempdf2 <- paramdf[ix2,]
  
  summary_stats<- c("aids_deaths", "natural_deaths", "natural_deaths_infecteds", 
    "natural_deaths_susceptibles", "births", "new_infections", "total_infections", 
    "susceptibles", "alive", "mean_spvl_pop", "median_spvl_pop", 
    "variance_spvl_pop", "mean_spvl_incident", "median_spvl_incident", 
    "variance_spvl_incident", "mean_vl_pop", "median_vl_pop", "variance_vl_pop", 
    "mean_age_incident", "mean_age_susceptibles", "mean_age_infecteds", 
    "mean_age_died_AIDS", "mean_age_infected_died_natural", "mean_age_susceptibles_died_natural", 
    "diagnosed", "no_edges", "mean_degree", "no_nodes_degree_0", 
    "no_nodes_degree_1", "no_nodes_concurrent")
  
  summary_stats_df <- data.frame(index= 1:length(summary_stats),
                                 statistic = summary_stats,
                                 stringsAsFactors=F)
  
  temp_path <- file.path(outpath,"initial_parameters.pdf")
  pdf(temp_path,6,18)
  plot(ix2,ix2,type='n',xlab=NA,ylab=NA,axes=F)
 # title("initial parameter values")
  plotrix::addtable2plot(1,-4,tempdf2,cex=.6,yjust=1,ypad=.8,hlines=T,vlines=T,xpad=.1)
  plot(ix2,ix2,type='n',xlab=NA,ylab=NA,axes=F)
  plotrix::addtable2plot(1,80,agedf,cex=.6,yjust=1,ypad=.8,hlines=T,vlines=T,xpad=.1)
  title("initial mortality / aged dist. values")
  plot(ix2,ix2,type='n',xlab=NA,ylab=NA,axes=F)
  plotrix::addtable2plot(1,55,agent_atts_df,cex=.6,yjust=1,ypad=.8,hlines=T,vlines=T,xpad=.1)
  title("agent attributes")
  plot(ix2,ix2,type='n',xlab=NA,ylab=NA,axes=F)
  plotrix::addtable2plot(1,95,summary_stats_df,cex=.6,yjust=1,ypad=.8,hlines=T,vlines=T,xpad=.1)
  title("summary statistics per timestep")
    
  dev.off()
  if(open){shell.exec(temp_path)}
  
}