void update_drug_concs_pill_combos_two_phase(double h, double Time,double time_0,
                                   double drug_decay1, double drug_decay2, double drug_decay3, double drug_decay4,
                                   long TherapyStarted, long SecondLineTherapyStarted,
                                   double Adherence1,double Adherence2, double Adherence3, double Adherence4,
                                   int Therapy_Type, int Dosing_Interval){
  int i;
  int Individual_pills = 1;
  int Drug12Combo = 2;
  int TripleDrugCombo = 3;
 
  /* Decay of drug with time *** would be nice to replace these commands with vectors */ 
  if (Drug[1] > conc_2nd_phase1) {
    deltaDrug[1] = - h*drug_decay1*Drug[1];    
  } else {
    deltaDrug[1] = - h*drug_2nd_decay1*Drug[1];
  }
  if (Drug[2] > conc_2nd_phase2) {
    deltaDrug[2] = - h*drug_decay2*Drug[2];   
  } else {
    deltaDrug[2] = - h*drug_2nd_decay2*Drug[2];
  }
  if (Drug[3] > conc_2nd_phase3) {
    deltaDrug[3] = - h*drug_decay3*Drug[3];   
  } else {
    deltaDrug[3] = - h*drug_2nd_decay3*Drug[3];
  }
  if (Drug[4] > conc_2nd_phase4) {
    deltaDrug[4] = - h*drug_decay4*Drug[4];  
  } else {
    deltaDrug[4] = - h*drug_2nd_decay4*Drug[4];
  }
  
  if (Therapy_Type == Individual_pills) { // Each drug is a separate pill and patient has independent probability of skipping any of them.
    if (TherapyStarted == 1) {
       if ((Time <= time_0 + h/2) || ((Dosing_Interval == 2) && ((Time > time_0 + 0.5 - h/2) && (Time <= time_0 + 0.5 + h/2)))) { 
     
         if ( (Time < StopDrug1) || (Time > RestartDrug1)) {  // Drug 1 not out of stock
           ran_val = R::runif(0,1);
           if (ran_val < Adherence1) {
             deltaDrug[1] = deltaDrug[1] + DrugDose[1]; 
           }
         } 
         if ( (Time < StopDrug2) || (Time > RestartDrug2)) {  // Drug 2 not out of stock
           ran_val = R::runif(0,1);
           if (ran_val < Adherence2) {
             deltaDrug[2] = deltaDrug[2] + DrugDose[2]; 
           }
         } 
         
         if ( (Time < StopDrug3) || (Time > RestartDrug3)) {  // Drug 3 not out of stock
           ran_val = R::runif(0,1);
           if (ran_val < Adherence3) {
             deltaDrug[3] = deltaDrug[3] + DrugDose[3]; 
           }
         }
         
       } // Time for pills
    } // therapy started
  } // individual pills

  if (Therapy_Type == Drug12Combo) {  // Drugs 1 and 2 are taken together.  Drug 3 is separate
    if (TherapyStarted == 1) {
      if ((Time <= time_0 + h/2) || ((Dosing_Interval == 2) && ((Time > time_0 + 0.5 - h/2) && (Time <= time_0 + 0.5 + h/2)))) { 
        if ( (Time < StopDrug1) || (Time > RestartDrug1)) {  // "Drug 1" (has both 1 & 2 if combo) not out of stock
          ran_val = R::runif(0,1);
          if (ran_val < Adherence1) { // Adherence 1 refers to both drugs 1 and 2 if combo
            deltaDrug[1] = deltaDrug[1] + DrugDose[1]; 
            deltaDrug[2] = deltaDrug[2] + DrugDose[2];
          }
        } // Drugs 1 and 2 not out of stock
        if ( (Time < StopDrug3) || (Time > RestartDrug3)) {  // Drug 3 not out of stock
          ran_val = R::runif(0,1);
          if (ran_val < Adherence3) { // 
            deltaDrug[3] = deltaDrug[3] + DrugDose[3]; 
          }
        } // Drug  3 not out of stock
      } // Time for pills
    } // Therapy Started
  } // drug1and2combo


  if (Therapy_Type == TripleDrugCombo) {  // Drugs 1, 2 and 3 are taken togetehr
     if (TherapyStarted == 1) {
       if ((Time <= time_0 + h/2) || ((Dosing_Interval == 2) && ((Time > time_0 + 0.5 - h/2) && (Time <= time_0 + 0.5 + h/2)))) { 
         if ( (Time < StopDrug1) || (Time > RestartDrug1)) {
           ran_val = R::runif(0,1);
           if (ran_val < Adherence1) { // Assume adherence 1 refers to drugs 1, 2 and 3 (i.e., adherence2 and adherence3 ought to equal adherence1)
             for (i=1;i<=3;i++) { // All three drugs get update if the patient adheres
              deltaDrug[i] = deltaDrug[i] + DrugDose[i]; 
             }
           } 
         } // Time
       } // Drug 1 (proxy for all three for triple combo) isn't out of stock   
     } // Therapy Started
   } // triple_drug_combo
    
   if (SecondLineTherapyStarted == 1) {
      if ((Time <= time_0 + h/2) || ((Dosing_Interval == 2) && ((Time > time_0 + 0.5 - h/2) && (Time <= time_0 + 0.5 + h/2)))) { 
        if ( (Time < StopDrug4) || (Time > RestartDrug4)) {
          ran_val = R::runif(0,1);
          if (ran_val < Adherence4) { 
              deltaDrug[4] = deltaDrug[4] + DrugDose[4]; 
          }
        } // Drug 4 isn't out of stock
      } // Time
    } // Therapy Started
   
  
}