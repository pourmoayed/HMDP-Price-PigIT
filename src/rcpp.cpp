#include "hmdp2.h"

using namespace Rcpp;
using namespace std;


//' Build the HMDP (2 levels with shared linking) using the C++ binary writer. 
//' 
//' @param filePrefix Prefix used by the binary files storing the MDP model.
//' @param param Model parameters of the HMDP.
//' @param paramDLMPi A list including the parameters for the DLMW (DLM for weigh information). 
//' @param paramDLMP A list including the parameters for the DLMP (DLM for pork price information). 
//' @param paramDLMF A list including the parameters for the DLMF (DLM for feed price information) 
//' @param paramPolicy A given policy for the hmdp. In finding the optimal policy of the HMDP, this policy will not be used. 
//' 
//' @return Build log (character).
//' @author Lars Relund \email{lars@@relund.dk}, Reza Pourmoayed \email{rpourmoayed@@econ.au.dk}
//' @export
// [[Rcpp::export]]
SEXP BuildHMDP2(const CharacterVector filePrefix, const List param, const List paramDLMP, const List paramDLMPi, const List paramDLMF
,const List paramPolicy) {
   string prefix = as<string>(filePrefix);
   HMDP2 Model(prefix, param, paramDLMP, paramDLMPi, paramDLMF, paramPolicy);
   //Rcout << "Total number of states: " << Model.countStatesHMDP() << endl;
   return( Model.BuildHMDP() );
   //return(wrap(0));
}















