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
input_parameters_asmr <- function(data_name="usa_men_18_to_100",min_age,max_age)
{
  #description:  
  #stores different ASMR data sets and converts annual values to 
  #daily rates and subsets out values for requested age range
  #daily rates are used in "vital_deaths_non_aids" fxn to 
  asmr_data_list <- list()
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
  asmr_data_list$"usa_men_18_to_100"<- list(
    asmr = 
      c(0.0011, 0.0012, 0.0013, 0.0014, 0.0014, 0.0014, 0.0014, 0.0014,
        0.0014, 0.0014, 0.0014, 0.0014, 0.0014, 0.0015, 0.0015, 0.0016,
        0.0016, 0.0017, 0.0018, 0.0019, 0.0021, 0.0022, 0.0024, 0.0026,
        0.0028, 0.0030, 0.0033, 0.0036, 0.0039, 0.0043, 0.0046, 0.0050,
        0.0055, 0.0059, 0.0064, 0.0069, 0.0074, 0.0080, 0.0086, 0.0093,
        0.0100, 0.0108, 0.0117, 0.0125, 0.0136, 0.0147, 0.0160, 0.0172,
        0.0186, 0.0203, 0.0220, 0.0240, 0.0263, 0.0286, 0.0314, 0.0344,
        0.0374, 0.0413, 0.0450, 0.0495, 0.0543, 0.0599, 0.0660, 0.0727,
        0.0805, 0.0886, 0.0986, 0.1100, 0.1300, 0.1440, 0.1600, 0.1760,
        0.1930, 0.2110, 0.2310, 0.2510, 0.2730, 0.2940, 0.3160, 0.3400,
        1.0000),
        age_range=c(18,100))
  
  asmr_data_list$"south_africa_female"<- list(
    asmr =c(0.0013, 0.0013, 0.0013, 0.0013, 
    0.0035, 0.0035, 0.0035, 0.0035, 0.0035, 
    0.0072, 0.0072, 0.0072, 0.0072, 0.0072, 
    0.0113, 0.0113, 0.0113, 0.0113, 0.0113, 
    0.0130, 0.0130, 0.0130, 0.0130, 0.0130, 
    0.0129, 0.0129, 0.0129, 0.0129, 0.0129, 
    0.0127, 0.0127, 0.0127, 0.0127, 0.0127, 
    0.0125, 0.0125, 0.0125, 0.0125, 0.0125, 
    0.0127, 0.0127, 0.0127, 0.0127, 0.0127, 
    0.0194, 0.0194, 0.0194, 0.0194, 0.0194, 
    0.0269, 0.0269, 0.0269, 0.0269, 0.0269, 
    0.0379, 0.0379, 0.0379, 0.0379, 0.0379, 
    0.0563, 0.0563, 0.0563, 0.0563, 0.0563, 
    0.1403, 0.1403, 0.1403, 0.1403, 0.1403, 
    0.1403, 0.1403, 0.1403, 0.1403, 0.1403, 
    0.1403, 0.1403, 0.1403, 0.1403, 0.1403, 
    0.1403, 0.1403, 0.1403, 0.1403, 0.1403, 
    0.1403), age_range=c(16,100))
  
  # Variant in which death rate is independent of age (average of log10's of "south_africa_female" above)
  asmr_data_list$"south_africa_female_average"<- list(
     asmr =rep(0.02296779,85), 
             age_range=c(16,100))

  asmr_data_list$"south_africa_male"<- list(
    asmr =c(0.0018, 0.0018, 0.0018, 0.0018,
    0.0039, 0.0039, 0.0039, 0.0039, 0.0039,
    0.0071, 0.0071, 0.0071, 0.0071, 0.0071, 
    0.0117, 0.0117, 0.0117, 0.0117, 0.0117, 
    0.0157, 0.0157, 0.0157, 0.0157, 0.0157, 
    0.0185, 0.0185, 0.0185, 0.0185, 0.0185, 
    0.0197, 0.0197, 0.0197, 0.0197, 0.0197, 
    0.0197, 0.0197, 0.0197, 0.0197, 0.0197, 
    0.0219, 0.0219, 0.0219, 0.0219, 0.0219, 
    0.0325, 0.0325, 0.0325, 0.0325, 0.0325, 
    0.0441, 0.0441, 0.0441, 0.0441, 0.0441, 
    0.0582, 0.0582, 0.0582, 0.0582, 0.0582, 
    0.0815, 0.0815, 0.0815, 0.0815, 0.0815, 
    0.1629, 0.1629, 0.1629, 0.1629, 0.1629, 
    0.1629, 0.1629, 0.1629, 0.1629, 0.1629, 
    0.1629, 0.1629, 0.1629, 0.1629, 0.1629, 
    0.1629, 0.1629, 0.1629, 0.1629, 0.1629, 
    0.1629),age_range=c(16,100))
  
  # Variant in which death rate is independent of age (average of log10's of "south_africa_male" above)
  asmr_data_list$"south_africa_male_average"<- list(
    asmr = rep(0.03048575,85),
            age_range=c(16,100))
  
  
  asmr_data_list$"south_africa_male_1990" <- list(
    asmr = c(rep(0.0019, 4),
             rep(0.0039, 5),
             rep(0.0049, 5),
             rep(0.0055, 5),
             rep(0.0064, 5),
             rep(0.0080, 5),
             rep(0.0107, 5),
             rep(0.0146, 5),
             rep(0.0203, 5),
             rep(0.0296, 5),
             rep(0.0416, 5),
             rep(0.0574, 5),
             rep(0.0817, 5),
             rep(0.1622, 21)),
    age_range = c(16, 100))
  
  asmr_data_list$"south_africa_female_1990" <- list(
    asmr = c(rep(0.0010, 4),
             rep(0.0015, 5),
             rep(0.0019, 5),
             rep(0.0023, 5),
             rep(0.0029, 5),
             rep(0.0039, 5),
             rep(0.0052, 5),
             rep(0.0076, 5),
             rep(0.0107, 5),
             rep(0.0171, 5),
             rep(0.0243, 5),
             rep(0.0351, 5),
             rep(0.0541, 5),
             rep(0.1361, 21)),
    age_range = c(16, 100))

  data_range <- asmr_data_list[[data_name]]$age_range[1]:asmr_data_list[[data_name]]$age_range[2]
  user_range <- min_age:(max_age-1)
  data_ix <- match(user_range,data_range)
  final_asmr       <- asmr_data_list[[data_name]]$asmr[data_ix]
  mort_per_timestep<-  utilities_annual_mortality_conversion(final_asmr, 
                                                             user_range, 365)
  
return(mort_per_timestep)
    
}

