#include <Rcpp.h>
void Convert_stop_restart_Matrix(Rcpp::DataFrame stop_restart_DT) {
  Rcpp::NumericVector  StopDrug1_vec = stop_restart_DT["StopDrug1"];
  Rcpp::NumericVector  RestartDrug1_vec = stop_restart_DT["RestartDrug1"];
  Rcpp::NumericVector  StopDrug2_vec = stop_restart_DT["StopDrug2"];
  Rcpp::NumericVector  RestartDrug2_vec = stop_restart_DT["RestartDrug2"];
  Rcpp::NumericVector  StopDrug3_vec = stop_restart_DT["StopDrug3"];
  Rcpp::NumericVector  RestartDrug3_vec = stop_restart_DT["RestartDrug3"];
  Rcpp::NumericVector  StopDrug4_vec = stop_restart_DT["StopDrug4"];
  Rcpp::NumericVector  RestartDrug4_vec = stop_restart_DT["RestartDrug4"];
    
  StopDrug1 = StopDrug1_vec[0];
  RestartDrug1 = RestartDrug1_vec[0];
  StopDrug2 = StopDrug2_vec[0];
  RestartDrug2 = RestartDrug2_vec[0];
  StopDrug3 = StopDrug3_vec[0];
  RestartDrug3 = RestartDrug3_vec[0];  
  StopDrug4 = StopDrug4_vec[0];
  RestartDrug4 = RestartDrug4_vec[0];  
}