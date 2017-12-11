void update_drug_concs_pill_combos(double h, double Time,double time_0,
                                   double drug_decay1, double drug_decay2, double drug_decay3, double drug_decay4,
                                   long TherapyStarted, long SecondLineTherapyStarted,
                                   double Adherence1,double Adherence2, double Adherence3, double Adherence4,
                                   int Therapy_Type, int Dosing_Interval){
  int i;
  int Individual_pills = 1;
  int Drug12Combo = 2;
  int TripleDrugCombo = 3;
  double Adherence;
  
  /* Decay of drug with time */
  deltaDrug[1] = - h*drug_decay1*Drug[1]; // Need to replace drug_decay with a vector 
  deltaDrug[2] = - h*drug_decay2*Drug[2]; // Need to replace drug_decay with a vector 
  deltaDrug[3] = - h*drug_decay3*Drug[3]; // Need to replace drug_decay with a vector 
  deltaDrug[4] = - h*drug_decay4*Drug[4]; // Need to replace drug_decay with a vector 
  
  /* Effect of taking pills.   Note: I will override all of these cals during stock out periods (see code below) */ 
  /*                           This coding strategy, while inefficient, is less likely to cause errors.      */
  
  if (Therapy_Type == Individual_pills) { // Each drug is a separate pill and patient has independent probability of skipping any of them.
    if (TherapyStarted == 1) {
       if ((Time <= time_0 + h/2) || ((Dosing_Interval == 2) && ((Time > time_0 + 0.5 - h/2) && (Time <= time_0 + 0.5 + h/2)))) { 
         //printf("Updating drug concs at Time = %lf, time_0 = %lf, h = %lf, Dosing_Interval = %d, Therapy_Type = %d\n",Time,time_0, h, Dosing_Interval,Therapy_Type);
         for (i=1;i<=3;i++) {
          ran_val = R::runif(0,1);
          if(i==1) {Adherence=Adherence1;}
          else
            if(i==2){Adherence=Adherence2;}
            else
              Adherence=Adherence3;
            if (ran_val < Adherence) {
              deltaDrug[i] = deltaDrug[i] + DrugDose[i]; 
            }
        } // Time
      } // i = 1 to 3
    } // therapy started
  } // individual pills

  if (Therapy_Type == Drug12Combo) {  // Drugs 1 and 2 are taken together.  Drug 3 is separate
    for (i=1;i<=3;i=i+2) { // This means i = 1 or 3
      if (TherapyStarted == 1) {
        if ((Time <= time_0 + h/2) || ((Dosing_Interval == 2) && ((Time > time_0 + 0.5 - h/2) && (Time <= time_0 + 0.5 + h/2)))) { 
          ran_val = R::runif(0,1);
          if (i==1) {
            if (ran_val < Adherence1) { // Assume adherence 1 refers to drugs 1 and 2 (i.e., adherence2 ought to equal adherence1)
              deltaDrug[1] = deltaDrug[1] + DrugDose[1]; 
              deltaDrug[2] = deltaDrug[2] + DrugDose[2]; 
            }
          } 
          if (i==3) {
            if (ran_val < Adherence3) { // Assume adherence 1 refers to drugs 1 and 2 (i.e., adherence2 ought to equal adherence1)
              deltaDrug[3] = deltaDrug[3] + DrugDose[3]; 
            }
          } // i==3 
        } // Time
      } // Therapy Started
    } // i = 1 or 3
  } // drug1and2combo


  if (Therapy_Type == TripleDrugCombo) {  // Drugs 1, 2 and 3 are taken togetehr
     if (TherapyStarted == 1) {
       if ((Time <= time_0 + h/2) || ((Dosing_Interval == 2) && ((Time > time_0 + 0.5 - h/2) && (Time <= time_0 + 0.5 + h/2)))) { 
         ran_val = R::runif(0,1);
         if (ran_val < Adherence1) { // Assume adherence 1 refers to drugs 1, 2 and 3 (i.e., adherence2 and adherence3 ought to equal adherence1)
           for (i=1;i<=3;i++) { // All three drugs get update if the patient adheres
              deltaDrug[i] = deltaDrug[i] + DrugDose[i]; 
            }
         } 
         } // Time
      } // Therapy Started
   } // triple_drug_combo
    
   if (SecondLineTherapyStarted == 1) {
      if ((Time <= time_0 + h/2) || ((Dosing_Interval == 2) && ((Time > time_0 + 0.5 - h/2) && (Time <= time_0 + 0.5 + h/2)))) { 
        ran_val = R::runif(0,1);
         if (ran_val < Adherence4) { 
              deltaDrug[4] = deltaDrug[4] + DrugDose[4]; 
         } 
      } // Time
    } // Therapy Started
   
   
   
   /* Revert to simple drug decay model during stock out periods */
   if ( (Time >= StopDrug1) && (Time < RestartDrug1)) {
      deltaDrug[1] = - h*drug_decay1*Drug[1];  
   }
   if ( (Time >= StopDrug2) && (Time < RestartDrug2)) {
      deltaDrug[2] = - h*drug_decay2*Drug[2];  
   }
   if ( (Time >= StopDrug3) && (Time < RestartDrug3)) {
      deltaDrug[3] = - h*drug_decay3*Drug[3];  
   }
   if ( (Time >= StopDrug4) && (Time < RestartDrug4)) {
      deltaDrug[4] = - h*drug_decay4*Drug[4];  
   }
   
}