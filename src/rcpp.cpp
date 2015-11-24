#include "hmdp2.h"

using namespace Rcpp;
using namespace std;


//' Build the HMDP (2 levels with shared linking) using the C++ binary writer. 
//' 
//' @param filePrefix Prefix used by the binary files storing the MDP model.
//' @param param Model parameters a list created using \code{\link{setParameters}}.
//' @param paramDLMPi A list including the parameters for the DLMW (DLM for weigh information) created using \file{\link{setParam.R}} 
//' @param paramDLMP A list including the parameters for the DLMP (DLM for pig price information) created using \file{\link{setParam.R}}
//' @param paramDLMF A list including the parameters for the DLMF (DLM for feed price information) created using \file{\link{setParam.R}}
//'   
//' @return Build log (character).
//' @author Lars Relund \email{lars@@relund.dk}, Reza Pourmoayed \email{rpourmoayed@econ.au.dk}
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















