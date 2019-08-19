#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix countit(CharacterVector x, int cutoff, CharacterVector lvls) {
  IntegerMatrix out(lvls.size(), 2);
  for(int j=0; j < lvls.size(); j++)
  {
    int before = 0;
    int after = 0;
    for(int i=0; i < x.size(); i++)
    {
      if(i < cutoff)
      {
        before += (x[i] == lvls[j]);
      } else
      {
        after += (x[i] == lvls[j]);
      }
    }
    out(j, 0) = before;
    out(j, 1) = after;
  }

  return out;
}

