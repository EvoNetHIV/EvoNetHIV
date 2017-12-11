double GetCarryingCapacity(double V_total, double K, double V_peak, double CD4, double V_AIDS, 
                           double vl_increase_AIDS, double h, double prog_rate,
                           double SPVL, double Time_Inf, double t_peak, double t_acute){
    double t_peak_corr;
    
    t_peak_corr = t_peak;
    
    if (Time_Inf == 0) {  
      t_peak_corr = t_peak + 1; // Special correction for persons infected at time 0
                                // Epimodel counts them as being infected at time 1 ==> Add one day to t_peak to get proper V_peak
    }
    
    if (Time - Time_Inf <= t_peak_corr) {
       K =  V_peak;
    } 
    
    if ((Time - Time_Inf > t_peak_corr) && (Immune_Resp == 0)) {
       K =  SPVL;
       Immune_Resp = 1; // Assume immune response causes carrying capacity to drop to the SPVL
    } 
    
    if ((Time - Time_Inf > t_peak_corr) && (Immune_Resp == 1)) {
      if (CD4 < 4) { 
         if (V_total > 50.0) { // Only allow carrying capacity to increase in non-suppressed patients
           K = K * exp(h * prog_rate/365); //  // Slow increase in carrying capacity during the chronic phase
          }
      } else {
         if (V_total > 50.0) { // Only allow carrying capacity to increase in non-suppressed patients
            K = K * exp(h * log(vl_increase_AIDS));  // Rapid increase in carrying capacity in AIDS. 
         }
      }
      if (K > V_AIDS) { 
         K = V_AIDS;  // Don't allow virus to go above V_AIDS
      }
    }
    return(K);
 }
 // Notes on how to handle exponentation using exp function.
 // y = x^a
 // ln y = a * ln x
 // y = exp(a * ln x)
