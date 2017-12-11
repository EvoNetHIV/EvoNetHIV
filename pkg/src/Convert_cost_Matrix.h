#include <Rcpp.h>
void Convert_cost_Matrix(Rcpp::DataFrame cost_DT) {
  Rcpp::NumericVector  cost1_vec = cost_DT["cost1"];
  Rcpp::NumericVector  cost2_vec = cost_DT["cost2"];
  Rcpp::NumericVector  cost3_vec = cost_DT["cost3"];
  Rcpp::NumericVector  cost4_vec = cost_DT["cost4"];
  Rcpp::NumericVector  cost5_vec = cost_DT["cost5"];
  Rcpp::NumericVector  cost_reduct5on1_vec = cost_DT["cost_reduct5on1"];
  Rcpp::NumericVector  cost_reduct4on2_vec = cost_DT["cost_reduct4on2"];
  
  // Not clear this is the most efficient way to convert R variables into C variables
  // While this does the trick (works for now), would be worth revisiting later.
  cost1 = cost1_vec[0];
  cost2 = cost2_vec[0];
  cost3 = cost3_vec[0];
  cost4 = cost4_vec[0];
  cost5 = cost5_vec[0];
  cost_reduct5on1 = cost_reduct5on1_vec[0];
  cost_reduct4on2 = cost_reduct4on2_vec[0];
}