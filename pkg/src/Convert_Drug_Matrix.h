#include <Rcpp.h>
void Convert_Drug_Matrix(Rcpp::DataFrame Drug_DT) {
  Rcpp::NumericVector  DrugDose1_vec = Drug_DT["DrugDose1"];
  Rcpp::NumericVector  DrugDose2_vec = Drug_DT["DrugDose2"];
  Rcpp::NumericVector  DrugDose3_vec = Drug_DT["DrugDose3"];
  Rcpp::NumericVector  DrugDose4_vec = Drug_DT["DrugDose4"];

  Rcpp::NumericVector  BaseIC50Drug1_vec = Drug_DT["BaseIC50Drug1"];
  Rcpp::NumericVector  BaseIC50Drug2_vec = Drug_DT["BaseIC50Drug2"];
  Rcpp::NumericVector  BaseIC50Drug3_vec = Drug_DT["BaseIC50Drug3"];
  Rcpp::NumericVector  BaseIC50Drug4_vec = Drug_DT["BaseIC50Drug4"];

  Rcpp::NumericVector  drug_decay1_vec = Drug_DT["drug_decay1"];
  Rcpp::NumericVector  drug_decay2_vec = Drug_DT["drug_decay2"];
  Rcpp::NumericVector  drug_decay3_vec = Drug_DT["drug_decay3"];
  Rcpp::NumericVector  drug_decay4_vec = Drug_DT["drug_decay4"];
  
  Rcpp::NumericVector  drug_2nd_decay1_vec = Drug_DT["drug_2nd_decay1"];
  Rcpp::NumericVector  drug_2nd_decay2_vec = Drug_DT["drug_2nd_decay2"];
  Rcpp::NumericVector  drug_2nd_decay3_vec = Drug_DT["drug_2nd_decay3"];
  Rcpp::NumericVector  drug_2nd_decay4_vec = Drug_DT["drug_2nd_decay4"];
  
  Rcpp::NumericVector conc_2nd_phase1_vec = Drug_DT["conc_2nd_phase1"];
  Rcpp::NumericVector conc_2nd_phase2_vec = Drug_DT["conc_2nd_phase2"];
  Rcpp::NumericVector conc_2nd_phase3_vec = Drug_DT["conc_2nd_phase3"];
  Rcpp::NumericVector conc_2nd_phase4_vec = Drug_DT["conc_2nd_phase4"];
  
  // Not clear this is the most efficient way to convert R variables into C variables
  // While this does the trick (works for now), would be worth revisiting later.
  DrugDose1 = DrugDose1_vec[0];
  DrugDose2 = DrugDose2_vec[0];
  DrugDose3 = DrugDose3_vec[0];
  DrugDose4 = DrugDose4_vec[0];
 
  BaseIC50Drug1 = BaseIC50Drug1_vec[0];
  BaseIC50Drug2 = BaseIC50Drug2_vec[0];
  BaseIC50Drug3 = BaseIC50Drug3_vec[0];
  BaseIC50Drug4 = BaseIC50Drug4_vec[0];
 
  drug_decay1 = drug_decay1_vec[0];
  drug_decay2 = drug_decay2_vec[0];
  drug_decay3 = drug_decay3_vec[0];
  drug_decay4 = drug_decay4_vec[0];
  
  drug_2nd_decay1 = drug_2nd_decay1_vec[0];
  drug_2nd_decay2 = drug_2nd_decay2_vec[0];
  drug_2nd_decay3 = drug_2nd_decay3_vec[0];
  drug_2nd_decay4 = drug_2nd_decay4_vec[0];
  
  conc_2nd_phase1 = conc_2nd_phase1_vec[0];
  conc_2nd_phase2 = conc_2nd_phase2_vec[0];
  conc_2nd_phase3 = conc_2nd_phase3_vec[0];
  conc_2nd_phase4 = conc_2nd_phase4_vec[0];
}