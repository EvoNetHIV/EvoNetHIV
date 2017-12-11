double Get_M_one_step(int i1, int i2, int i3, int i4, int i5){
double temp_sum;
temp_sum = 0.0; // Sum up infected cells that are one mutational step away
if (i1 == 1)  temp_sum =  temp_sum  + I[ 0][i2][i3][i4][i5]; // Mutations from sensitivity to resistance
if (i2 == 1)  temp_sum =  temp_sum  + I[i1][ 0][i3][i4][i5];
if (i3 == 1)  temp_sum =  temp_sum  + I[i1][i2][ 0][i4][i5];
if (i4 == 1)  temp_sum =  temp_sum  + I[i1][i2][i3][ 0][i5];
if (i5 == 1)  temp_sum =  temp_sum  + I[i1][i2][i3][i4][ 0];
if (i1 == 0)  temp_sum =  temp_sum  + I[ 1][i2][i3][i4][i5]; // Back mutations from resistance to sensitivity
if (i2 == 0)  temp_sum =  temp_sum  + I[i1][ 1][i3][i4][i5];
if (i3 == 0)  temp_sum =  temp_sum  + I[i1][i2][ 1][i4][i5];
if (i4 == 0)  temp_sum =  temp_sum  + I[i1][i2][i3][ 1][i5];
if (i5 == 0)  temp_sum =  temp_sum  + I[i1][i2][i3][i4][ 1];
return(temp_sum);
}