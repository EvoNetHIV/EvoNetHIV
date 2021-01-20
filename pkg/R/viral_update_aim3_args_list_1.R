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
viral_update_aim3_args_list_1 <-  function(dat,evonet_params,ind)
{
  #description:
  #organizes input parameters into list to feed into
  #john's/aim3 viral_update_modified_logistic fxn; differs
  #from 'viral_update_modified_logistic_args_list_2' as it processes only
  #raw input parameters, not derived model quantities
  
  list(
    Num_Loci = evonet_params$no_loci,
    h        = evonet_params$step_size_C,
    prog_rate = evonet_params$prog_rate,
    m        = evonet_params$m_CD4,
    k        = evonet_params$k,
    r_base   = evonet_params$r_inf_cells,
    d        = evonet_params$d_inf_cells,
    c        = evonet_params$c,
    f_M      = evonet_params$f_M,
    f_L      = evonet_params$f_L,
    d_M      = evonet_params$d_M,
    d_L      = evonet_params$d_L,
    p        = evonet_params$p_inf_cells,
    p_M      = evonet_params$p_M,
    p_L      = evonet_params$p_L,
    M_act    = evonet_params$M_act,
    L_act    = evonet_params$L_act,
    mu       = evonet_params$mu,
    V0       = evonet_params$V0,
    V_peak   = evonet_params$vl_peak_acute,
    t_peak   = evonet_params$t_peak,
    t_acute  = evonet_params$t_acute,
    V_AIDS   = evonet_params$vl_max_aids,
    vl_increase_AIDS = evonet_params$vl_increase_AIDS,
    cost_DT = data.table::data.table(
      cost1    = evonet_params$cost1,
      cost2    = evonet_params$cost2,
      cost3    = evonet_params$cost3,
      cost4    = evonet_params$cost4,
      cost5    = evonet_params$cost5,
      cost_reduct5on1 = evonet_params$cost_reduct5on1,
      cost_reduct4on2 = evonet_params$cost_reduct4on2
    ),
    additive_fitness = evonet_params$additive_fitness,
    Drug_DT = data.table::data.table(
       DrugDose1    = evonet_params$DrugDose1,
       DrugDose2    = evonet_params$DrugDose2,
       DrugDose3    = evonet_params$DrugDose3,
       DrugDose4    = evonet_params$DrugDose4,
       BaseIC50Drug1    = evonet_params$BaseIC50Drug1,
       BaseIC50Drug2    = evonet_params$BaseIC50Drug2,
       BaseIC50Drug3    = evonet_params$BaseIC50Drug3,
       BaseIC50Drug4    = evonet_params$BaseIC50Drug4,
       drug_decay1 =  evonet_params$drug_decay1,
       drug_decay2 =  evonet_params$drug_decay2,
       drug_decay3 =  evonet_params$drug_decay3,
       drug_decay4 =  evonet_params$drug_decay4,
       drug_2nd_decay1 =  evonet_params$drug_2nd_decay1,
       drug_2nd_decay2 =  evonet_params$drug_2nd_decay2,
       #drug_2nd_decay3 =  evonet_params$drug_2nd_decay3,
       #changing decay rate if CYP_allele is present #is this really drug1_2nd_decay #0.54 from Haas et al. 2004 #516t/t genotype #0.26 is an arbitrary number
       drug_2nd_decay3 = if(dat$attr$CYP_6_slow[ind] == 1){ evonet_params$drug_2nd_decay3*evonet_params$aim3_decay_change} else { evonet_params$drug_2nd_decay3},
       
       drug_2nd_decay4 =  evonet_params$drug_2nd_decay4,
       conc_2nd_phase1 =  evonet_params$conc_2nd_phase1,
       conc_2nd_phase2 =  evonet_params$conc_2nd_phase2,
       conc_2nd_phase3 =  evonet_params$conc_2nd_phase3,
       conc_2nd_phase4 =  evonet_params$conc_2nd_phase4
    ),
    Interaction_Model_Drugs12 = evonet_params$Interaction_Model_Drugs12,
    FC_DT   =  data.table::data.table(
      FC_D1_Mut1 =  evonet_params$FC_D1_Mut1, 
      FC_D1_Mut2 =  evonet_params$FC_D1_Mut2, 
      FC_D1_Mut3 =  evonet_params$FC_D1_Mut3, 
      FC_D1_Mut4 =  evonet_params$FC_D1_Mut4, 
      FC_D1_Mut5 =  evonet_params$FC_D1_Mut5, 
      
      FC_D2_Mut1 =  evonet_params$FC_D2_Mut1, 
      FC_D2_Mut2 =  evonet_params$FC_D2_Mut2, 
      FC_D2_Mut3 =  evonet_params$FC_D2_Mut3, 
      FC_D2_Mut4 =  evonet_params$FC_D2_Mut4, 
      FC_D2_Mut5 =  evonet_params$FC_D2_Mut5,
      
      FC_D3_Mut1 =  evonet_params$FC_D3_Mut1, 
      FC_D3_Mut2 =  evonet_params$FC_D3_Mut2, 
      FC_D3_Mut3 =  evonet_params$FC_D3_Mut3, 
      FC_D3_Mut4 =  evonet_params$FC_D3_Mut4,
      FC_D3_Mut5 =  evonet_params$FC_D3_Mut5,
      
      FC_D4_Mut1 =  evonet_params$FC_D4_Mut1, 
      FC_D4_Mut2 =  evonet_params$FC_D4_Mut2, 
      FC_D4_Mut3 =  evonet_params$FC_D4_Mut3, 
      FC_D4_Mut4 =  evonet_params$FC_D4_Mut4,
      FC_D4_Mut5 =  evonet_params$FC_D4_Mut5
    ),
    StochasticCut = evonet_params$StochasticCut,
    cut = evonet_params$AbsoluteCut,
    Dosing_Interval = evonet_params$Dosing_Interval,
    Therapy_Type = evonet_params$Therapy_Type,
    stop_restart_DT   =  data.table::data.table(
      StopDrug1 = evonet_params$StopDrug1,
      RestartDrug1 = evonet_params$RestartDrug1,
      StopDrug2 = evonet_params$StopDrug2,
      RestartDrug2 = evonet_params$RestartDrug2,
      StopDrug3 = evonet_params$StopDrug3,
      RestartDrug3 = evonet_params$RestartDrug3,
      StopDrug4 = evonet_params$StopDrug4,
      RestartDrug4 = evonet_params$RestartDrug4)
  )
}
