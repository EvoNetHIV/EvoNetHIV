void SetVtotal_ZeroOutDrugsAndDelta(void) {
   int i,i1,i2,i3,i4,i5;
   for (i=1 ; i<=4; i++) {
    deltaGIcnc[i] = 0.0;
    deltaDrug[i] = 0.0;
  }
  V_total = 0.0; I_total = 0.0;
  for (i1=0;i1<=alleles[1];i1++) {
   for (i2=0;i2<=alleles[2];i2++) {
    for (i3=0;i3<=alleles[3];i3++) {
      for (i4=0;i4<=alleles[4];i4++) {
        for (i5=0;i5<=alleles[5];i5++) {
          deltaV[i1][i2][i3][i4][i5] = 0.0;
          deltaI[i1][i2][i3][i4][i5] = 0.0;
          deltaM[i1][i2][i3][i4][i5] = 0.0;
          deltaL[i1][i2][i3][i4][i5] = 0.0;
          r[i1][i2][i3][i4][i5] = 0.0;
          V_total = V_total + V[i1][i2][i3][i4][i5];
          I_total = I_total + I[i1][i2][i3][i4][i5] + M[i1][i2][i3][i4][i5] + L[i1][i2][i3][i4][i5];
  }}}}} 
}
