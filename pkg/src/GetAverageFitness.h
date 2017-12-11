double GetAverageFitness(int additive_fitness) {
   int i1,i2,i3,i4;
   double Fitness, freq;
   Fitness = 0.0;
   V_total = 0.0;
   for (i1=0;i1<=alleles[1];i1++) {
    for (i2=0;i2<=alleles[2];i2++) {
      for (i3=0;i3<=alleles[3];i3++) {
        for (i4=0;i4<=alleles[4];i4++) { 
          for (i5=0;i5<=alleles[5];i5++) { 
          V_total = V_total + V[i1][i2][i3][i4][i5];
   }}}}}
   if (V_total > 0.0) {
    for (i1=0;i1<=alleles[1];i1++) {
    for (i2=0;i2<=alleles[2];i2++) {
    for (i3=0;i3<=alleles[3];i3++) {
    for (i4=0;i4<=alleles[4];i4++) { 
    for (i5=0;i5<=alleles[5];i5++) { 
      freq = V[i1][i2][i3][i4][i5]/ V_total;
      if (additive_fitness == 1) {
         Fitness = Fitness + freq * (1 - i1*cost1 - i2*cost2*(1-i5) - i3*cost3 - i4*cost4 - i5*cost5);  // Additive model 
      } else { 
         Fitness = Fitness + freq * (1 - cost1*i1*(1-i5*cost_reduct5on1)) * (1 - cost2*i2*(1-i4*cost_reduct4on2))* (1 - cost3*i3)* (1 - cost4*i4)* (1 - cost5*i5);  // Multiplicative model
      }   
      if (Fitness < 0.0) {
        //printf("Warning: Fitness[1%d1%d1%d1%d] = %lf\n",i1,i2,i3,i4,Fitness);
        Fitness = 0.01; // Put a floor on fitness (don't let it go negative)
      }
    }}}}}
 
   } // V_total is zero (assume average fitness of 0.0 for consistency)
   return(Fitness);
}
