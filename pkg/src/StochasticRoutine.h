double StochasticRoutine(double X, double plusterm, double minusterm, double StochasticCut, double cut, double time_0, double h) {
  double finalvalue,deltaX;
  if (X > StochasticCut) {
    finalvalue = h*(plusterm - minusterm);
  } else {
    if (  probability*h/cut > 1.0)  {
      closeness_to_cutoff = probability*h/cut;
      problems++;
    }
    jrand = Rcpp::runif(1)[0];
    if (cut*jrand < h*probability) {
      jrand = Rcpp::runif(1)[0];
      if (jrand < minusterm/probability) deltaX = -cut;
                                    else deltaX =  cut;
      } else {
         deltaX = 0.0;
      }
      finalvalue = deltaX;
   }
   return (finalvalue);
}
