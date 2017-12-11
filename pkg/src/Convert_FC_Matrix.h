#include <Rcpp.h>
void Convert_FC_Matrix(Rcpp::DataFrame FC_DT) {
  Rcpp::NumericVector  FC_D1_Mut1_vec = FC_DT["FC_D1_Mut1"];
  Rcpp::NumericVector  FC_D1_Mut2_vec = FC_DT["FC_D1_Mut2"];
  Rcpp::NumericVector  FC_D1_Mut3_vec = FC_DT["FC_D1_Mut3"];
  Rcpp::NumericVector  FC_D1_Mut4_vec = FC_DT["FC_D1_Mut4"];
  Rcpp::NumericVector  FC_D1_Mut5_vec = FC_DT["FC_D1_Mut5"];
  
  Rcpp::NumericVector  FC_D2_Mut1_vec = FC_DT["FC_D2_Mut1"];
  Rcpp::NumericVector  FC_D2_Mut2_vec = FC_DT["FC_D2_Mut2"];
  Rcpp::NumericVector  FC_D2_Mut3_vec = FC_DT["FC_D2_Mut3"];
  Rcpp::NumericVector  FC_D2_Mut4_vec = FC_DT["FC_D2_Mut4"];
  Rcpp::NumericVector  FC_D2_Mut5_vec = FC_DT["FC_D2_Mut5"];
  
  Rcpp::NumericVector  FC_D3_Mut1_vec = FC_DT["FC_D3_Mut1"];
  Rcpp::NumericVector  FC_D3_Mut2_vec = FC_DT["FC_D3_Mut2"];
  Rcpp::NumericVector  FC_D3_Mut3_vec = FC_DT["FC_D3_Mut3"];
  Rcpp::NumericVector  FC_D3_Mut4_vec = FC_DT["FC_D3_Mut4"];
  Rcpp::NumericVector  FC_D3_Mut5_vec = FC_DT["FC_D3_Mut5"];
  
  Rcpp::NumericVector  FC_D4_Mut1_vec = FC_DT["FC_D4_Mut1"];
  Rcpp::NumericVector  FC_D4_Mut2_vec = FC_DT["FC_D4_Mut2"];
  Rcpp::NumericVector  FC_D4_Mut3_vec = FC_DT["FC_D4_Mut3"];
  Rcpp::NumericVector  FC_D4_Mut4_vec = FC_DT["FC_D4_Mut4"];
  Rcpp::NumericVector  FC_D4_Mut5_vec = FC_DT["FC_D4_Mut5"];
  
  FC_D1_Mut1 = FC_D1_Mut1_vec[0];
  FC_D1_Mut2 = FC_D1_Mut2_vec[0];
  FC_D1_Mut3 = FC_D1_Mut3_vec[0];
  FC_D1_Mut4 = FC_D1_Mut4_vec[0];
  FC_D1_Mut5 = FC_D1_Mut5_vec[0];
  
  FC_D2_Mut1 = FC_D2_Mut1_vec[0];
  FC_D2_Mut2 = FC_D2_Mut2_vec[0];
  FC_D2_Mut3 = FC_D2_Mut3_vec[0];
  FC_D2_Mut4 = FC_D2_Mut4_vec[0];
  FC_D2_Mut5 = FC_D2_Mut5_vec[0];
  
  FC_D3_Mut1 = FC_D3_Mut1_vec[0];
  FC_D3_Mut2 = FC_D3_Mut2_vec[0];
  FC_D3_Mut3 = FC_D3_Mut3_vec[0];
  FC_D3_Mut4 = FC_D3_Mut4_vec[0];
  FC_D3_Mut5 = FC_D3_Mut5_vec[0];
  
  FC_D4_Mut1 = FC_D4_Mut1_vec[0];
  FC_D4_Mut2 = FC_D4_Mut2_vec[0];
  FC_D4_Mut3 = FC_D4_Mut3_vec[0];
  FC_D4_Mut4 = FC_D4_Mut4_vec[0];
  FC_D4_Mut5 = FC_D4_Mut5_vec[0];
}