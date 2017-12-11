double update_carrying_capacity(double K_in, double h){
  int i1,i2,i3,i4,i5;
  double K_term;
  V_total = 0.0;
  for (i1=0;i1<=alleles[1];i1++) {
    for (i2=0;i2<=alleles[2];i2++) {
      for (i3=0;i3<=alleles[3];i3++) {
        for (i4=0;i4<=alleles[4];i4++) { 
          for (i5=0;i5<=alleles[5];i5++) { 
            V_total = V_total + V[i1][i2][i3][i4][i5];
  }}}}}
        
  K_term = K_in*K_in*K_in/(K_in*K_in*K_in + V_total*V_total*V_total); // ranges from 0 (V_total big) to 1 (V_total small)
  return(K_term);
}