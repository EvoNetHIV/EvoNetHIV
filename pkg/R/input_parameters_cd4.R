#' @title CD4 progression data
#'
#' @description Returns Two tables that describe CD4+ T cell decline over time in untreated infectionns, as based on Cori, Pickles, et al (AIDS, 2015):  i) cd4_init_probs is a 9x3 table that gives the probability of a newly infected agent starting with one of the first three cd4 categories base on agents SPVL; ii) CD4_lookup is a 9x4 table that contains the mean expected passage time in each of the 4 CD4 categories based on agent's SPVL.
#'
#' @return List of 2 elements, one table per element: 'cd4_init_probs' table and 
#' 'CD4_lookup' table 
#'
#'
#' @details
#' No function arguments.
#' @examples
#' cd4_data  <- input_parameters_cd4_data()


#' @export
input_parameters_cd4_data <- function()
{
  #Description:
  # List of CD4 data tables from Pickles, Cori, et al.
  # Change CD4 SPVL tables here

#see cori and pickles report for details
#data tables for cori and pickles:
  #"cd4_init_probs"      : probability of initial cd4 category
  #"CD4_lookup"          : time in each cd4 category

list(

#--------------------------------------------------------------

cd4_init_probs =
  structure(list(cd4_500     = c(0.88, 0.87, 0.85, 0.78, 0.73, 0.71, 0.64, 0, 0),
                 cd4_500_350 = c(.12,  0.12, 0.12, 0.19, 0.21, 0.25, 0.27, 0, 0),
                 cd4_350_200 = c(0.0,  0.01, 0.03, 0.03, 0.05, 0.04, 0.09, 1, 1)),
            .Names = c("cd4_500+", "cd4_500_350", "cd4_350_200"),
            class = "data.frame",
            row.names = c("spvl<3",       "spvl_3.0_3.5", "spvl_3.5_4.0",
                          "spvl_4.0_4.5", "spvl_4.5_5.0", "spvl_5.0_5.5",
                          "spvl_5.5_6.0", "spvl_6.0_6.5", "spvl>6.5")),

#                cd4_500+ cd4_500_350 cd4_350_200
#spvl<3           0.88        0.12        0.00
#spvl_3.0_3.5     0.87        0.12        0.01
#spvl_3.5_4.0     0.85        0.12        0.03
#spvl_4.0_4.5     0.78        0.19        0.03
#spvl_4.5_5.0     0.73        0.21        0.05
#spvl_5.0_5.5     0.71        0.25        0.04
#spvl_5.5_6.0     0.64        0.27        0.09
#spvl_6.0_6.5     0.00        0.00        1.00
#spvl>6.5         0.00        0.00        1.00

#--------------------------------------------------------------

CD4_lookup =
structure(list(cd4_500  = c(6.08, 4.69, 3.94, 2.96, 2.25, 1.47, 0.95, 0.32, 0.30),
               cd4_500_350  = c(5.01, 2.52, 4.07, 3.09, 2.32, 1.55, 1.19, 0.59, 0.46),
               cd4_350_200  = c(3.60,  3.68, 2.38, 3.81, 3.21, 2.27, 1.00,  0.68, 0.37),
               cd4_200  = c(4.67, 4.11, 3.54, 2.98, 2.42, 1.86, 1.29, 0.73, 0.17)),
          .Names = c("cd4_500+", "cd4_500_350", "cd4_350_200", "cd4_200-"),
            class = "data.frame",
            row.names = c("spvl<3", "spvl_3.0_3.5", "spvl_3.5_4.0", "spvl_4.0_4.5",
                          "spvl_4.5_5.0",
                          "spvl_5.0_5.5", "spvl_5.5_6.0", "spvl_6.0_6.5", "spvl>6.5"))


#CD4_lookup
#              cd4_500+  cd4_500_350 cd4_350_200 cd4_200-
#spvl<3           6.08        5.01        3.60     4.67
#spvl_3.0_3.5     4.69        2.52        3.68     4.11
#spvl_3.5_4.0     3.94        4.07        2.38     3.54
#spvl_4.0_4.5     2.96        3.09        3.81     2.98
#spvl_4.5_5.0     2.25        2.32        3.21     2.42
#spvl_5.0_5.5     1.47        1.55        2.27     1.86
#spvl_5.5_6.0     0.95        1.19        1.00     1.29
#spvl_6.0_6.5     0.32        0.59        0.68     0.73
#spvl>6.5         0.30        0.46        0.37     0.17

#--------------------------------------------------------------

)#end of list
}#end of fxn


