void integerize_values(double cut, double StochasticCut,
                       int i1, int i2, int i3, int i4){
/* Use a rounding algorithm to integerize values near the extinction threshold */
for (i1=0;i1<=alleles[1];i1++) {
  for (i2=0;i2<=alleles[2];i2++) {
    for (i3=0;i3<=alleles[3];i3++) {
      for (i4=0;i4<=alleles[4];i4++) { 
        V[i1][i2][i3][i4] = round_algo(V[i1][i2][i3][i4],
                                       deltaV[i1][i2][i3][i4],cut,
                                       StochasticCut);
        I[i1][i2][i3][i4] = round_algo(I[i1][i2][i3][i4],
                                       deltaI[i1][i2][i3][i4],cut,
                                       StochasticCut);
        M[i1][i2][i3][i4] = round_algo(M[i1][i2][i3][i4],
                                       deltaM[i1][i2][i3][i4],cut,
                                       StochasticCut);
        L[i1][i2][i3][i4] = round_algo(L[i1][i2][i3][i4],
                                       deltaL[i1][i2][i3][i4],cut,
                                       StochasticCut);
        
      }}}}
}