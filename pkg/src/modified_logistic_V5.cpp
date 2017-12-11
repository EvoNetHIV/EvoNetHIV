#include <Rcpp.h>
using namespace Rcpp;// [[Rcpp::export]]

#include <math.h>
#include "DeclareGlobalVariables.h"



// [[Rcpp::export]]

List viral_dynamics_aim3(long Agent,
                     double time_0,
                     double time_final,
                     double h, 
                     long Num_Loci,
                     double m, 
                     double k, 
                     double r_base,
                     double d ,
                     double f_M, 
                     double f_L ,
                     double d_M ,
                     double d_L ,
                     double p ,
                     double p_M, 
                     double p_L ,
                     double M_act,
                     double L_act,
                     double V0,
                     double V_peak, 
                     double t_peak,
                     double t_acute,
                     double V_AIDS,
                     double mu ,
                     DataFrame cost_DT,
                     int additive_fitness,
                     DataFrame Drug_DT,
                     double SPVL,
                     double prog_rate,
                     double vl_increase_AIDS,
                     double Time_Inf,
                     long Immune_Response_Triggered, 
                     long ChronicPhaseBegins,
                     long TherapyStarted,           
                     long SecondLineTherapyStarted,
                     double Adherence1,
                     double Adherence2,
                     double Adherence3,
                     double Adherence4,
                     DataFrame Virus_DT,
                     int Interaction_Model_Drugs12,
                     DataFrame FC_DT,
                     double StochasticCut,
                     double cut,
                     double K,
                     double c,
                     double CD4,
                     double Drug1,
                     double Drug2,
                     double Drug3,
                     double Drug4,
                     int Dosing_Interval,
                     int Therapy_Type,
                     DataFrame stop_restart_DT,
                     long Aim3RoundingErrors
                     )
{
// create output list of 14 elements
   List out(14);  
   
   if (time_0 == 1200000000) {
     printf("\n%lf: Agent %ld (therapy=%ld) entering with: D1: %lf, D2: %lf, D3: %lf, D4: %lf\n",
             time_0,Agent,TherapyStarted,Drug1,Drug2,Drug3,Drug4);
   }
 
// Translate selected input parameters into global C variables 
  Immune_Resp = Immune_Response_Triggered;
  Chronic_Phase = ChronicPhaseBegins;
  r_base_orig = r_base;
  r_base = (d + log(V_peak/V0)/t_peak)/(1-f_M-f_L); // Allow different growth in acute to match V_peak


 // Translate R data.tables into Rcpp objects
  Convert_FC_Matrix(FC_DT); 
  Convert_Drug_Matrix(Drug_DT); 
  Convert_cost_Matrix(cost_DT); 
  Convert_stop_restart_Matrix(stop_restart_DT); 
  
  // Creates an "alleles" array of 1's and 0's corresponding to the number of loci.
  init_alleles(Num_Loci);

 // Translate R data.table into Rcpp object
  NumericVector   V_vec = Virus_DT["V"] ;
  NumericVector   I_vec = Virus_DT["I"] ;
  NumericVector   M_vec = Virus_DT["M"] ;
  NumericVector   L_vec = Virus_DT["L"] ;
  
  // Translate Rcpp object into four dimensional C array
	for (i1=0;i1<=alleles[1];i1++) {
  for (i2=0;i2<=alleles[2];i2++) {
  for (i3=0;i3<=alleles[3];i3++) {
  for (i4=0;i4<=alleles[4];i4++) {
  for (i5=0;i5<=alleles[5];i5++) {
     V[i1][i2][i3][i4][i5] = V_vec[ 16*i5 + 8*i4 + 4*i3 + 2*i2 + i1];
     I[i1][i2][i3][i4][i5] = I_vec[ 16*i5 + 8*i4 + 4*i3 + 2*i2 + i1];
     M[i1][i2][i3][i4][i5] = M_vec[ 16*i5 + 8*i4 + 4*i3 + 2*i2 + i1];
     L[i1][i2][i3][i4][i5] = L_vec[ 16*i5 + 8*i4 + 4*i3 + 2*i2 + i1];
     //printf("V[%1d%1d%1d%1d%1d] = %lf, I[%1d%1d%1d%1d%1d] = %lf\n",
     //      i1,i2,i3,i4,i5, V[i1][i2][i3][i4][i5],i1,i2,i3,i4,i5, I[i1][i2][i3][i4][i5]);
  }}}}} 
  
 if (time_0 > 12000000) {
  printf("%lf: Agent %ld (2nd=%ld) befr int vals: D1: %lf, D2: %lf, D3: %lf, D4: %lf\n",
              Time,Agent,SecondLineTherapyStarted,Drug1,Drug2,Drug3,Drug4);
 }
 
  initialize_values(h,Drug1,Drug2,Drug3,Drug4,
                    BaseIC50Drug1,BaseIC50Drug2,BaseIC50Drug3,BaseIC50Drug4,
                    DrugDose1,DrugDose2,DrugDose3,DrugDose4,
                    time_0,Aim3RoundingErrors,r_base,additive_fitness);
      
  
   if (time_0 > 120000000) {
     printf("%lf: Agent %ld (2nd=%ld) starting with: D1: %lf, D2: %lf, D3: %lf, D4: %lf\n",
             Time,Agent,SecondLineTherapyStarted,Drug[1],Drug[2],Drug[3],Drug[4]);
   }
   
  SetVtotal_ZeroOutDrugsAndDelta(); 
  
  init_effect_array();  // Converts input values for the effect of each mutation on each drug into C variables
      
  drug_effects();   // Combined effects of all drugs on IC50 values
  // Note: In later versions we might be able to cut a couple of microseconds off the simulation time
  // by moving these calculations to input_parameters_derived (since they are the same for every run).
  // I am laving this for later (much later) because:
  //    (i) The IC50[drug][i1][i2][i3][i4][i5] data structure requires a more complex C function parameter
  //   (ii) The overhead is comparativelly small (~48 multiplications compared to ~16000 below )


  /******************************/
  /*** Start of big time loop ***/
  /******************************/

  //printf("At time %lf, V_total = %lf, I_total = %lf (about to start big loop)\n",Time,V_total,I_total);
  Time = time_0;
  closeness_to_cutoff = 1.0;
  do {  
 
    // Get baseline carrying capacity (designed to match aim 2 parameters)
    K = GetCarryingCapacity(V_total, K, V_peak, CD4, V_AIDS,
                            vl_increase_AIDS, h, prog_rate,
                            SPVL, Time_Inf,  t_peak, t_acute); 
     
    // Multiply carrying capacity by average viral fitness cost imposed by resistance mutations
    // to arrive at an "fitness-adjusted" carrying capacity
    K_adj = K*GetAverageFitness(additive_fitness); // 1 = additive fitness, 2 = multiplicative fitness
   
    // K_term is a multiplier [ = K_adj^3/(Vtotal^3 + K_adj^3) ] used to adjust growth terms so 
    // that viral load rapidly converge on K_adj
    K_term = update_carrying_capacity(K_adj,h);
 
    // Adaptive Time steps.  Use smaller step sizes when stochastic routine gives probs > 1
    h = 0.9*base_h/closeness_to_cutoff;  // Set step size based on closeness to cutoff in previous timestep
    if (closeness_to_cutoff > 1.0) {
      if (problems <= 3) {
        printf("Agent %ld: Rounding errors at %lf: h = %lf, prob(change) = %lf, base_h = %lf (probs=%ld)\n",Agent,Time,h,closeness_to_cutoff,base_h,problems);
        if (problems == 3) {
          printf("  Problem count for agent %ld now at 3.  Silencing further warning messages\n",Agent);
        }
      }
    }
    closeness_to_cutoff = 1.0;
   
     // Update drug concentrations
    update_drug_concs_pill_combos_two_phase(h,Time,time_0,
                              drug_decay1,drug_decay2, drug_decay3, drug_decay4,
                              TherapyStarted, SecondLineTherapyStarted,
                              Adherence1, Adherence2, Adherence3, Adherence4,
                              Therapy_Type, Dosing_Interval);
     
    // Update Time
    Time = Time + h;
 
     
    // Update CD4 T-cell counts
    // CD4 code removed: We now update CD4 in a separate routine
   
    // Update I, M, L, and V for each genotype
    for (i1=0;i1<=alleles[1];i1++) {
    for (i2=0;i2<=alleles[2];i2++) {
    for (i3=0;i3<=alleles[3];i3++) {
    for (i4=0;i4<=alleles[4];i4++) { 
    for (i5=0;i5<=alleles[5];i5++) { 
      // Sum up infected cells that are one mutational step away
      I_one_step = Get_I_one_step(i1,i2,i3,i4,i5);
      do_calcs = 1; // Default is to do all of calculations
      
      // Identify conditions under which it is not necessary to do update V[i1][i2][i3][i4][i5]
      // Not needed if no virus or infected cells in this class and no infected cells that are one step away
      if (I_one_step == 0.0) {
        if ( ((V[i1][i2][i3][i4][i5] == 0.0) && (I[i1][i2][i3][i4][i5] == 0.0)) &&
             ((M[i1][i2][i3][i4][i5] == 0.0) && (L[i1][i2][i3][i4][i5] == 0.0)) ) {
          M_one_step = Get_M_one_step(i1,i2,i3,i4,i5);
          if (M_one_step == 0.0) {
             L_one_step = Get_L_one_step(i1,i2,i3,i4,i5);
             if (L_one_step == 0.0) {
               do_calcs = 0; // If there is no virus or cells with this genotype, nor any cells that are one 
                             // mutational step away, we can skip the calculations below
               //if ( (Time > time_0 + 0.2) && (Time < time_0 + 0.2 + h)) {
               //   printf("Agent : %ld: Skipping calcs for %1d%1d%1d%1d%1d\n",Agent,i1,i2,i3,i4,i5);
               //}
            }
          }
        }
      }
      if (do_calcs == 1) {
      
        // Define growth rate, r, for each genotype based on intrinsic fitness & drug concentrations
        
        r[i1][i2][i3][i4][i5] = define_growth_rate(intrinsic_fitness[i1][i2][i3][i4][i5],Interaction_Model_Drugs12);
       
        growth_and_mut_terms_I = r[i1][i2][i3][i4][i5]*K_term*((1-Num_Loci*mu)*I[i1][i2][i3][i4][i5] + mu*I_one_step);
      
        // if ( (Time > time_0 + 0.2) && (Time < time_0 + 0.2 + h)) {
        //     printf("Agent %ld (%lf):r[%1d%1d%1d%1d%1d] = %lf, intrinsic_fitness[...] = %lf, Interaction_Model_Drugs12 = %d\n",
        //     Agent,Time,i1,i2,i3,i4,i5, r[i1][i2][i3][i4][i5], intrinsic_fitness[i1][i2][i3][i4][i5], Interaction_Model_Drugs12);
        //  }
      
        // First work on infected cells
        plusterm = growth_and_mut_terms_I*(1-f_M-f_L) + M_act*M[i1][i2][i3][i4][i5] + L_act*L[i1][i2][i3][i4][i5];  // Assume that M and L cells can be activated
        minusterm = d*I[i1][i2][i3][i4][i5];
        probability = plusterm + minusterm;
        deltaI[i1][i2][i3][i4][i5] = StochasticRoutine(I[i1][i2][i3][i4][i5],
                                                 plusterm, minusterm,
                                                 StochasticCut, cut,
                                                 time_0, h);
      
        
        //  Moderately long-lived infected cells
        plusterm = growth_and_mut_terms_I*f_M;
        minusterm =  d_M*M[i1][i2][i3][i4][i5] + M_act*M[i1][i2][i3][i4][i5];
        probability = plusterm + minusterm;
        deltaM[i1][i2][i3][i4][i5] = StochasticRoutine(M[i1][i2][i3][i4][i5],
                                                plusterm,minusterm,
                                                StochasticCut,cut,
                                                time_0, h);
   
        // Latently infected cells
        plusterm = growth_and_mut_terms_I*f_L;
        minusterm =  d_L*L[i1][i2][i3][i4][i5] + L_act*L[i1][i2][i3][i4][i5];
        probability = plusterm + minusterm;
        deltaL[i1][i2][i3][i4][i5] = StochasticRoutine(L[i1][i2][i3][i4][i5],
                                                plusterm,minusterm,
                                                StochasticCut,cut,
                                                time_0, h);
     
        // Free virus
        plusterm = p*I[i1][i2][i3][i4][i5] + p_M*M[i1][i2][i3][i4][i5] + p_L*L[i1][i2][i3][i4][i5];
        minusterm = c*V[i1][i2][i3][i4][i5];
        probability = plusterm + minusterm;
        deltaV[i1][i2][i3][i4][i5] = StochasticRoutine(V[i1][i2][i3][i4][i5],
                                                plusterm,minusterm,
                                                StochasticCut,cut,
                                                time_0, h);
      } // do calcs
    }}}}} // five loci
  
    /************************************************************************************/
    /*** Update values for CD4, drugs, V, I, M, and L using deltas calculated above *****/ 
    /************************************************************************************/
    
    for (i=1;i<=4;i++) {
      GIcnc[i] = GIcnc[i] + deltaGIcnc[i];
      Drug[i] = Drug[i] + deltaDrug[i];
      if (Drug[i] < 0) Drug[i] = 1e-90;
      if (GIcnc[i] < 0) GIcnc[i] = 1e-90;
    } 
    
    /* Use a rounding algorithm to integerize values near the extinction threshold */
    V_total = 0.0;
    for (i1=0;i1<=alleles[1];i1++) {
    for (i2=0;i2<=alleles[2];i2++) {
    for (i3=0;i3<=alleles[3];i3++) {
    for (i4=0;i4<=alleles[4];i4++) { 
    for (i5=0;i5<=alleles[5];i5++) { 
       V[i1][i2][i3][i4][i5] = round_algo(V[i1][i2][i3][i4][i5],
                                     deltaV[i1][i2][i3][i4][i5],cut,
                                      StochasticCut);
        I[i1][i2][i3][i4][i5] = round_algo(I[i1][i2][i3][i4][i5],
                                     deltaI[i1][i2][i3][i4][i5],cut,
                                     StochasticCut);
        M[i1][i2][i3][i4][i5] = round_algo(M[i1][i2][i3][i4][i5],
                                     deltaM[i1][i2][i3][i4][i5],cut,
                                     StochasticCut);
       L[i1][i2][i3][i4][i5] = round_algo(L[i1][i2][i3][i4][i5],
                                     deltaL[i1][i2][i3][i4][i5],cut,
                                     StochasticCut);
       V_total = V_total + V[i1][i2][i3][i4][i5];

   }}}}}
  } while (Time < time_final);
  
  /* Translate back from multidimensional array to vector format recognized in R */
  V_total = 0.0;
  for (i1=0;i1<=alleles[1];i1++) {
  for (i2=0;i2<=alleles[2];i2++) {
  for (i3=0;i3<=alleles[3];i3++) {
  for (i4=0;i4<=alleles[4];i4++) { 
  for (i5=0;i5<=alleles[5];i5++) { 
     V_total = V_total + V[i1][i2][i3][i4][i5];
     V_vec[ 16*i5 + 8*i4 + 4*i3 + 2*i2 + i1] = V[i1][i2][i3][i4][i5]; 
     I_vec[ 16*i5 + 8*i4 + 4*i3 + 2*i2 + i1] = I[i1][i2][i3][i4][i5];
     M_vec[ 16*i5 + 8*i4 + 4*i3 + 2*i2 + i1] = M[i1][i2][i3][i4][i5];
     L_vec[ 16*i5 + 8*i4 + 4*i3 + 2*i2 + i1] = L[i1][i2][i3][i4][i5];
     
  }}}}}
  
  out = List::create(Time, V_total, K, Immune_Resp,
      Chronic_Phase, V_vec, I_vec, M_vec, L_vec,
      Drug[1],Drug[2],Drug[3],Drug[4],problems);   
      if (time_0 > 120000000) {
   printf("%lf: Agent %ld (2nd=%ld)  exiting with: D1: %lf, D2: %lf, D3: %lf, D4: %lf\n",
             Time,Agent,SecondLineTherapyStarted,Drug[1],Drug[2],Drug[3],Drug[4]);
   }
// out = List::create(time_final, 1e4, K, Immune_Response_Triggered,
//   ChronicPhaseBegins, TherapyStarted, Drug1,Drug2,Drug3);  
   
//    out.names() = CharacterVector::create("time","V", "K","Imm_Trig",
//    "ChronPhase","OnDrug",  "Drug1", "Drug2", "Drug3");

  out.names() = CharacterVector::create("time","V", "K","Imm_Trig",
  "ChronPhase","V_vec", "I_vec", "M_vec", "L_vec",
  "Drug1", "Drug2", "Drug3","Drug4","Aim3RoundingErrors");
  return out;
  
}
