  long  Immune_Resp;
  long Chronic_Phase;
 
  double deltaDrug[5], deltaGIcnc[5];
  double closeness_to_cutoff;
  double ran_val;
  double base_h;
  double Time;
  double new_val, deltaX;
   double float_part;
  double plusterm, minusterm, probability;
  long count, problems;
  double V_vec[31],I_vec[31],M_vec[31],L_vec[31];  
  double r_base_orig; // r_base set by parent program.  Allows for diff r prior to peak.
  double  K_adj; // Adjusted carrying capacity that accounts for costs of drug resistance
  double  K_term; // Multiplication term that forces V to rapidly converge on K_adj
  double  I[2][2][2][2][2], deltaI[2][2][2][2][2];
  double  M[2][2][2][2][2], deltaM[2][2][2][2][2];
  double  L[2][2][2][2][2], deltaL[2][2][2][2][2];
  double  V[2][2][2][2][2], deltaV[2][2][2][2][2];
  double  r[2][2][2][2][2];
  double V_total;
  double I_total;
  double  deltaCD4;
  
  double intrinsic_fitness[2][2][2][2][2];
  double Drug_effect;
  double GIcnc[5];
  double Drug[5];
  double DrugDose[5];
  double BaseIC50[5];
  double  IC50[5][2][2][2][2][2];
  int i, i1,i2,i3,i4, i5, drug;
  double I_one_step = 0.0; // Sum of types that are one mutational step away
  double M_one_step = 0.0; // Sum of types that are one mutational step away
  double L_one_step = 0.0; // Sum of types that are one mutational step away
  int j;
  int alleles[17];
  double Effect[6][5]; // Effect of mutation i on drug j (note array size one more than number to avoid subscripts of 0)
   double jrand;
    
  double FC_D1_Mut1;
  double FC_D1_Mut2;
  double FC_D1_Mut3;
  double FC_D1_Mut4;
  double FC_D1_Mut5;
  
  double FC_D2_Mut1;
  double FC_D2_Mut2;
  double FC_D2_Mut3;
  double FC_D2_Mut4;
  double FC_D2_Mut5;
  
  double FC_D3_Mut1;
  double FC_D3_Mut2;
  double FC_D3_Mut3;
  double FC_D3_Mut4;
  double FC_D3_Mut5;
  
  double FC_D4_Mut1;
  double FC_D4_Mut2;
  double FC_D4_Mut3;
  double FC_D4_Mut4;
  double FC_D4_Mut5;
  double cost1, cost2, cost3, cost4, cost5;
  double cost_reduct5on1, cost_reduct4on2;
  double StopDrug1, RestartDrug1;
  double StopDrug2, RestartDrug2;
  double StopDrug3, RestartDrug3;
  double StopDrug4, RestartDrug4;
  double M_act;
  double L_act; 
  
  double DrugDose1;
  double DrugDose2;
  double DrugDose3;
  double DrugDose4;
  double BaseIC50Drug1;
  double BaseIC50Drug2;
  double BaseIC50Drug3;
  double BaseIC50Drug4;
  double drug_decay1;
  double drug_decay2;
  double drug_decay3;
  double drug_decay4;
  double drug_2nd_decay1;
  double drug_2nd_decay2;
  double drug_2nd_decay3;
  double drug_2nd_decay4;
  double conc_2nd_phase1;
  double conc_2nd_phase2;
  double conc_2nd_phase3;
  double conc_2nd_phase4;
  
  double growth_and_mut_terms_I; 
  int do_calcs; // Only do calculations when there is a possibility for growth or death
  
  #include "SetVtotal_ZeroOutDrugsAndDelta.h"
  #include "StochasticRoutine.h"
  #include "Initialize_values.h"
  #include "Init_alleles.h"
 // #include "Translate_array.h"
  #include "Init_effect_array.h"
  #include "Drug_effects.h"
  #include "Update_carrying_capacity.h"
 // #include "Update_drug_concentrations.h"
  #include "Update_drug_concs_pill_combos_two_phase.h"
//  #include "Update_drug_concs_pill_combos_stockout.h"
  #include "Get_I_one_step.h"
  #include "Get_M_one_step.h"
  #include "Get_L_one_step.h"
  #include "Define_growth_rate.h"
//  #include "Problem_count_qaqc.h"
  #include "Rounding_algo.h"
  #include "GetAverageFitness.h"
  #include "Convert_FC_Matrix.h"
  #include "Convert_cost_Matrix.h"
  #include "Convert_Drug_Matrix.h"
  #include "Convert_stop_restart_Matrix.h"
  #include "GetCarryingCapacity.h"
 //  #include "GetViralMatrices.h"
//  #include "ConvertVLMatricesToDT.h"
