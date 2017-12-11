double define_growth_rate(double intrinsic_fitness, int Interaction_Model_Drugs12){
  double sum_ratios;
  double Drug_effect = 1.0;
//  printf("Inside define_growth_rate\n");
//  printf("\tIC50[1][%d][%d][%d][%d][%d] = %lf\n",i1,i2,i2,i4,i5,IC50[1][i1][i2][i3][i4][i5]);
  if (Interaction_Model_Drugs12 == 1) {  // Bliss independence (effectiveness is the product of the effect of each drug)
    Drug_effect = Drug_effect * IC50[1][i1][i2][i3][i4][i5] / (Drug[1] + IC50[1][i1][i2][i3][i4][i5]);
    Drug_effect = Drug_effect * IC50[2][i1][i2][i3][i4][i5] / (Drug[2] + IC50[2][i1][i2][i3][i4][i5]);
  } else { // Huang et al's simple saturation model (assuming drugs 1 and 2 compete for the same target)
    //HuangsHybrid = 1 - (C1/IC1 + C2/IC2) / ( 1 + C1/IC1 + C2/IC2)
    sum_ratios = Drug[1]/IC50[1][i1][i2][i3][i4][i5] + Drug[2]/IC50[2][i1][i2][i3][i4][i5];
    Drug_effect = 1.0 - ( sum_ratios / (1.0 + sum_ratios));
  }
  Drug_effect = Drug_effect * IC50[3][i1][i2][i3][i4][i5] / (Drug[3] + IC50[3][i1][i2][i3][i4][i5]); 
  Drug_effect = Drug_effect * IC50[4][i1][i2][i3][i4][i5] / (Drug[4] + IC50[4][i1][i2][i3][i4][i5]); 
  // Time permiting I will add a third model for drug interactions (Lowe additivity)
  return(intrinsic_fitness * Drug_effect);
}