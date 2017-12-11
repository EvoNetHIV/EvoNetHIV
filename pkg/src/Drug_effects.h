void drug_effects(void){
  double Affect1;
  double Affect2;
  double Affect3;
  double Affect4;
  double Affect5;
 int drug, i1,i2,i3,i4,i5;
 for (drug = 1; drug<=4; drug++) { // Number of drugs
  for (i1=0;i1<=alleles[1];i1++) {
    for (i2=0;i2<=alleles[2];i2++) {
      for (i3=0;i3<=alleles[3];i3++) {
        for (i4=0;i4<=alleles[4];i4++) { 
          for (i5=0;i5<=alleles[5];i5++) { 
            if (i1 == 1) Affect1 = Effect[1][drug] ;
            else Affect1 = 1.0;
            if (i2 == 1) Affect2 = Effect[2][drug] ;
            else Affect2 = 1.0;
            if (i3 == 1) Affect3 = Effect[3][drug] ;
            else Affect3 = 1.0;
            if (i4 == 1) Affect4 = Effect[4][drug] ;
            else Affect4 = 1.0;           
            if (i5 == 1) Affect5 = Effect[5][drug] ;
            else Affect5 = 1.0;           
          IC50[drug][i1][i2][i3][i4][i5] = BaseIC50[drug] * Affect1 * Affect2 * Affect3 * Affect4 * Affect5; // Need to verify this formula
          
          // printf("IC50[%d][%d][%d][%d][%d] = %lf\n",drug,i1,i2,i3,i4,IC50[drug][i1][i2][i3][i4]);
          
        }}}}}
 }
}
