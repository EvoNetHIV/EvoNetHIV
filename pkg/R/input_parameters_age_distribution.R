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

input_parameters_age_distribution <- function(initial_param,mort_per_timestep_female,
                                              pop_growth_rate_timestep, baseline_input_exp_growth,
                                              data_name="usa_men_18_to_100",min_age,max_age)
{
  if(is.null(data_name)){
    data_name <- "usa_men_18_to_100"
  }
  #description: returns initial age distribution for population
  #called in "input_parameters_derived"
  
  if(data_name=="linear_decrease"){
     if(min_age==18 && max_age==55){
      final_age_dist <- seq(50, 10, -10/9)/1110
      return(final_age_dist)
     }else{stop("can't use 'linear_decrease' age dist when min age != 18 or max age != 55")}
  }
  
  if (data_name=="stable_age_no_hiv_dist") {
    if (initial_param$birth_model != "exponential_growth") stop("Johns_Stable_Age_Dist requires exponential_growth model \n")
    # Code that worked in stand-alone script 
    #   b = (a + r + d[1])*N[1]
    #   for (age in 2: 83) {
    #     N[age] <- N[age-1]*a/(a+r+d[age])
    #   }
    #  where N1 = lowest age class (=18 in most of our models)
    #  a = aging rate
    #  di = death rate of persons of age i
    #  r = rate of growth in the birth rate
    #  b = initial birth rate (rate of entry of persons of "age 1") 
    #  Equations
    #   dN1/dt = b*exp(r*t) - a*N1 - d1*N1
    #   dN2/dt = a*N1 - a*N2 - d2*N2
    #   dNi/dt = a*Ni-1 - a*Ni - di*Ni
    # Equation only works when population is at steady state, meaning that b = (a+r+d[1])*N[1]
    # In other words "baseline_input_exp_growth" (=b) needs to be tuned so that rate of initial input into the youngest
    # age class equals the initial death and ageing rates of the youngest age class
    
    b <- baseline_input_exp_growth  # Initial birth rate
    a <- 1/365  # Per day aging rate
    r <- pop_growth_rate_timestep
    d_f <- mort_per_timestep_female # Per day death rate (assume females and males the same)
    age_vec <- c(min_age:(max_age-1))  # Set up the vector (arbitrary numbers for now)
    age_vec[1] <- b/(a + r + d_f[min_age])
    low_limit <- min_age +1
    upper_limit <- max_age -1
    for (age in low_limit:upper_limit) {
      age_index <- age - min_age +1 # Youngest age class (e.g., 16) is represented as 1 in our model
      age_vec[age_index] <- age_vec[age_index-1]*a/(a +r + d_f[age_index])
    }
    age_vec[1] <- age_vec[1]/2  # Reduce youngest class by 50% to reflect fact that the average age of newly entering agents is min_age + 0.5.  
    final_age_dist <- age_vec/sum(age_vec)
    return(final_age_dist)
  }
  
  age_dist_data_list <- list()
  ########## Alternative AMSR tables that include elderly persons. #########################
  # Data obtained from the CDC "WONDER" webpage (wonder.cdc.gov) for USA men from 1999-2003
  # downloaded on 8/25/15.  The CDC data only apply to people 85 and under.  To fill in
  # the rest, I (John) made various extrapolations and approximations, as follows... 
  #  (1) In the absence of data, I assumed zero people over 85 years at the beginning of the
  #      simulation.  The model does, however, allow people to age-in to ages 86-100 over time.
  #  (2) Death rate data for those over 86 were obtained from Society of Actuaries' "Social
  #      Security" data set (pretty closely matches the CDC estimate for those 85!).  Obtained from
  #      https://www.soa.org/research/software-tools/research-simple-life-calculator.aspx 
  #      downloaded on 8/25/15.
  #  (3) To keep this from getting totally out of hand, I assumed a 0% chance of living past 100. 
  #
  # While these approximations and extrapolations are imperfect, I figure that the advanced
  # elderly are rare enough (and so inactive sexually) that any imperfections will have
  # little-to-no effect on our conclusions.
  #
  #  Note: Eldery persons can easily be excluded from the simulations by setting max_age to 
  #        something less than 100 (current default, as of 8/25/15, is max_age = 55 years)
  #  
  age_dist_data_list$"usa_men_18_to_100"<- list(
    age_dist = 
      c(0.0205, 0.0206, 0.0204, 0.0202, 0.0200, 0.0196, 0.0194, 0.0192,
        0.0189, 0.0189, 0.0188, 0.0188, 0.0191, 0.0187, 0.0189, 0.0188,
        0.0189, 0.0192, 0.0193, 0.0194, 0.0196, 0.0199, 0.0201, 0.0203,
        0.0204, 0.0204, 0.0203, 0.0202, 0.0202, 0.0201, 0.0199, 0.0197,
        0.0194, 0.0191, 0.0188, 0.0181, 0.0177, 0.0170, 0.0165, 0.0158,
        0.0152, 0.0145, 0.0138, 0.0132, 0.0126, 0.0120, 0.0114, 0.0109,
        0.0102, 0.0096, 0.0092, 0.0088, 0.0084, 0.0080, 0.0076, 0.0073,
        0.0069, 0.0066, 0.0062, 0.0059, 0.0056, 0.0052, 0.0048, 0.0044,
        0.0040, 0.0036, 0.0032, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001,
        0.0001, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001, 0.0001,
        0.0001),age_range=c(18,100))
    age_dist_data_list$"south_africa_male_16_to_100_2014" <- list(
      age_dist= c(0.0360,0.0360,0.0360,0.0360,
                  0.0293,0.0293,0.0293,0.0293,0.0293,
                  0.0276,0.0276,0.0276,0.0276,0.0276,
                  0.0224,0.0224,0.0224,0.0224,0.0224,
                  0.0191,0.0191,0.0191,0.0191,0.0191,
                  0.0163,0.0163,0.0163,0.0163,0.0163,
                  0.0140,0.0140,0.0140,0.0140,0.0140,
                  0.0120,0.0120,0.0120,0.0120,0.0120,
                  0.0100,0.0100,0.0100,0.0100,0.0100,
                  0.0077,0.0077,0.0077,0.0077,0.0077,
                  0.0054,0.0054,0.0054,0.0054,0.0054,
                  0.0036,0.0036,0.0036,0.0036,0.0036,
                  0.0022,0.0022,0.0022,0.0022,0.0022,
                  0.0004,0.0004,0.0004,0.0004,0.0004,
                  0.0004,0.0004,0.0004,0.0004,0.0004,
                  0.0004,0.0004,0.0004,0.0004,0.0004,
                  0.0004,0.0004,0.0004,0.0004,0.0004,
                  0.0004),age_range=c(16,100))
    age_dist_data_list$"south_africa_female_16_to_100_2014" <- list(
      age_dist= c(0.0330,0.0330,0.0330,0.0330,
                  0.0265,0.0265,0.0265,0.0265,0.0265,
                  0.0249,0.0249,0.0249,0.0249,0.0249,
                  0.0210,0.0210,0.0210,0.0210,0.0210,
                  0.0180,0.0180,0.0180,0.0180,0.0180,
                  0.0167,0.0167,0.0167,0.0167,0.0167,
                  0.0151,0.0151,0.0151,0.0151,0.0151,
                  0.0131,0.0131,0.0131,0.0131,0.0131,
                  0.0111,0.0111,0.0111,0.0111,0.0111,
                  0.0088,0.0088,0.0088,0.0088,0.0088,
                  0.0070,0.0070,0.0070,0.0070,0.0070,
                  0.0051,0.0051,0.0051,0.0051,0.0051,
                  0.0033,0.0033,0.0033,0.0033,0.0033,
                  0.0007,0.0007,0.0007,0.0007,0.0007,
                  0.0007,0.0007,0.0007,0.0007,0.0007,
                  0.0007,0.0007,0.0007,0.0007,0.0007,
                  0.0007,0.0007,0.0007,0.0007,0.0007,
                  0.0007),age_range=c(16,100))
    age_dist_data_list$"south_africa_male_16_to_100_1990" <- list(
      age_dist = c(rep(0.0452, 4),
                   rep(0.0314, 5),
                   rep(0.0268, 5),
                   rep(0.0234, 5),
                   rep(0.0201, 5),
                   rep(0.0161, 5),
                   rep(0.0134, 5),
                   rep(0.0100, 5),
                   rep(0.0080, 5),
                   rep(0.0060, 5),
                   rep(0.0040, 5),
                   rep(0.0027, 5),
                   rep(0.0013, 5),
                   rep(0.0002, 21)), age_range = c(16, 100))
    age_dist_data_list$"south_africa_female_16_to_100_1990" <- list(
      age_dist = c(rep(0.0434, 4),
                   rep(0.0302, 5),
                   rep(0.0257, 5),
                   rep(0.0225, 5),
                   rep(0.0193, 5),
                   rep(0.0154, 5),
                   rep(0.0129, 5),
                   rep(0.0109, 5),
                   rep(0.0090, 5),
                   rep(0.0064, 5),
                   rep(0.0051, 5),
                   rep(0.0039, 5),
                   rep(0.0019, 5),
                   rep(0.0005, 21)), age_range = c(16, 100))
      
  data_range <- (age_dist_data_list[[data_name]]$age_range[1]:
                   age_dist_data_list[[data_name]]$age_range[2])
  user_age_range        <- min_age : (max_age-1)
  #age_dist_index   <- user_age_index - min_age + 1
  data_ix <- match(user_age_range,data_range)
  initial_age_dist <- age_dist_data_list[[data_name]]$age_dist[data_ix]
  
  final_age_dist   <- initial_age_dist/sum(initial_age_dist)
  return(final_age_dist)
}

