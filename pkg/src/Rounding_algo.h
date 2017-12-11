double round_algo(double X,double dX, double cut, double StochasticCut)
{
  double deltaX;
  double new_val;
  new_val = X + dX;
  deltaX = 0.0; 
  if ( new_val < StochasticCut - cut) {
    float_part = (new_val/cut - (long) (new_val/cut));
    jrand = Rcpp::runif(1)[0];
    if (float_part <jrand) deltaX = (-1)* float_part*cut;
                      else deltaX = cut - float_part*cut;
  }
  if (new_val < cut/2.0) new_val = 0; 
  else new_val = new_val + deltaX;
  X = new_val;
  if (X  < 0.0) X = 0.0;
  return(X);
}
