void init_alleles(long Num_Loci){
  int j;
  for (j = 0; j <= 16; j++) {
    alleles[j] = 0;
  }
  for (j = 1; j <= Num_Loci;j++) {
    alleles[j] = 1;
  }
}