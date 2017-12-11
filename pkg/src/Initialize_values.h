void initialize_values(double h, double Drug1, double Drug2, double Drug3, double Drug4,
                       double BaseIC50Drug1,double BaseIC50Drug2, double BaseIC50Drug3, double BaseIC50Drug4,
                       double DrugDose1, double DrugDose2, double DrugDose3, double DrugDose4,
                       double time_0, long Aim3RoundingErrors, double r_base, int additive_fitness){
ran_val = 0.0;
base_h = h;
Time = time_0;
problems = Aim3RoundingErrors;
int i1,i2,i3,i4,i5;

count = 0;
V_total = 0.0;
I_total = 0.0;
I_one_step = 0.0; // Sum of types that are one mutational step away

Drug[1] = Drug1;
Drug[2] = Drug2;
Drug[3] = Drug3;
Drug[4] = Drug4;

DrugDose[1] =  DrugDose1; DrugDose[2] = DrugDose2; DrugDose[3] = DrugDose3; DrugDose[4] = DrugDose4;

BaseIC50[1] = BaseIC50Drug1; BaseIC50[2] = BaseIC50Drug2; BaseIC50[3] = BaseIC50Drug3; BaseIC50[4] = BaseIC50Drug4;

// Maximum growth rate in the absence of drugs and cell death for each genotype 
// Note two models (additive and multiplicative) for the effects of mutations on maximum growth
for (i1=0;i1<=1;i1++) {
for (i2=0;i2<=1;i2++) {
for (i3=0;i3<=1;i3++) {
for (i4=0;i4<=1;i4++) {
for (i5=0;i5<=1;i5++) {
  if (additive_fitness == 1) {
     intrinsic_fitness[i1][i2][i3][i4][i5] = r_base* (1 - i1*cost1 - i2*cost2*(1-i5) - i3*cost3 - i4*cost4 - i5*cost5);  // Additive model 
  } else { 
     intrinsic_fitness[i1][i2][i3][i4][i5] = r_base * (1 - cost1*i1*(1-i5*cost_reduct5on1)) * (1 - cost2*i2*(1-i4*cost_reduct4on2))* (1 - cost3*i3)* (1 - cost4*i4)* (1 - cost5*i5);  // Multiplicative model
  }
}}}}}

}