#ifndef DLM_H
#define DLM_H

#include "RcppArmadillo.h"    // we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "cumNorm.h"
using namespace Rcpp;
//using namespace std;

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do
//
// [[Rcpp::depends(RcppArmadillo)]]

// ===================================================

/**
* Class with variables and functions related to a DLM. Only store the variables needed (rest in R).
* \author Reza pourmoayed
*/
class DLM
{
public:  // methods

    /** Constructor. Store the parameters.
     * @param dlmParam A list created using \code{iniDLM} and \code{buildDLM} in R.
     */
   DLM(const List paramDLMP, const List paramDLMPi, const List paramDLMF) {
      List rParamDLMP(paramDLMP);
      List rParamDLMPi(paramDLMPi);
      List rParamDLMF(paramDLMF);            
      int n, k, dim;
      //List tmpGG,tmpFF,tmpQ,tmpL,tmpC;      
                
      
      //dlm for pig price information
      dim = as<int>(rParamDLMP["tMax"]);
      List tmpP(dim);
            
      dlmPL1 = as<arma::mat>(rParamDLMP["L1"]);       
      dlmPGk1 = as<arma::mat>(rParamDLMP["Gk1"]); 
      
      tmpP = as<List>(rParamDLMP["GG"]); 
      for (int i=0;i<dim;i++) {
         NumericMatrix tmp = as<NumericMatrix>(tmpP[i]);
         n = tmp.nrow(), k = tmp.ncol();
         dlmPG.push_back( arma::mat(tmp.begin(), n, k, true) );
      }
            
      tmpP = as<List>(rParamDLMP["L"]); 
      for (int i=0;i<dim;i++) {
         NumericMatrix tmp = as<NumericMatrix>(tmpP[i]);
         n = tmp.nrow(), k = tmp.ncol();
         dlmPL.push_back( arma::mat(tmp.begin(), n, k, true) );
      }
      
      tmpP = as<List>(rParamDLMP["L3"]); 
      for (int i=0;i<dim;i++) {
         NumericMatrix tmp = as<NumericMatrix>(tmpP[i]);
         n = tmp.nrow(), k = tmp.ncol();
         dlmPL3.push_back( arma::mat(tmp.begin(), n, k, true) );
      }
            
      tmpP = as<List>(rParamDLMP["Gk3"]); 
      for (int i=0;i<dim;i++) {
         NumericMatrix tmp = as<NumericMatrix>(tmpP[i]);
         n = tmp.nrow(), k = tmp.ncol();
         dlmPGk3.push_back( arma::mat(tmp.begin(), n, k, true) );
      }


      //dlm for piglet price information
      dim = as<int>(rParamDLMPi["tMax"]);
      List tmpPi(dim);
      
      dlmPiL1 = as<arma::mat>(rParamDLMPi["L1"]);       
      dlmPiGk1 = as<arma::mat>(rParamDLMPi["Gk1"]);

      tmpPi = as<List>(rParamDLMPi["GG"]); 
      for (int i=0;i<dim;i++) {
         NumericMatrix tmp = as<NumericMatrix>(tmpPi[i]);
         n = tmp.nrow(), k = tmp.ncol();
         dlmPiG.push_back( arma::mat(tmp.begin(), n, k, true) );
      }
            
      tmpPi = as<List>(rParamDLMPi["L"]); 
      for (int i=0;i<dim;i++) {
         NumericMatrix tmp = as<NumericMatrix>(tmpPi[i]);
         n = tmp.nrow(), k = tmp.ncol();
         dlmPiL.push_back( arma::mat(tmp.begin(), n, k, true) );
      }
      
      tmpPi = as<List>(rParamDLMPi["L3"]);  //covariance matrix forecast distribution of observable variables when action=term.
      for (int i=0;i<dim;i++) {
         NumericMatrix tmp = as<NumericMatrix>(tmpPi[i]);
         n = tmp.nrow(), k = tmp.ncol();
         dlmPiL3.push_back( arma::mat(tmp.begin(), n, k, true) );
      }
            
      tmpPi = as<List>(rParamDLMPi["Gk3"]);  //coeficant of mean parameter fot the forecast distribution of observable variables when action=term.
      for (int i=0;i<dim;i++) {
         NumericMatrix tmp = as<NumericMatrix>(tmpPi[i]);
         n = tmp.nrow(), k = tmp.ncol();
         dlmPiGk3.push_back( arma::mat(tmp.begin(), n, k, true) );
      }
                            
      //dlm for feed price information
      dim = as<int>(rParamDLMF["tMax"]);
      List tmpF(dim);
      
      dlmFL1 = as<arma::mat>(rParamDLMF["L1"]);       
      dlmFGk1 = as<arma::mat>(rParamDLMF["Gk1"]);

      tmpF = as<List>(rParamDLMF["GG"]); 
      for (int i=0;i<dim;i++) {
         NumericMatrix tmp = as<NumericMatrix>(tmpF[i]);
         n = tmp.nrow(), k = tmp.ncol();
         dlmFG.push_back( arma::mat(tmp.begin(), n, k, true) );
      }
            
      tmpF = as<List>(rParamDLMF["L"]); 
      for (int i=0;i<dim;i++) {
         NumericMatrix tmp = as<NumericMatrix>(tmpF[i]);
         n = tmp.nrow(), k = tmp.ncol();
         dlmFL.push_back( arma::mat(tmp.begin(), n, k, true) );
      }
            
      tmpF = as<List>(rParamDLMF["L3"]); 
      for (int i=0;i<dim;i++) {
         NumericMatrix tmp = as<NumericMatrix>(tmpF[i]);
         n = tmp.nrow(), k = tmp.ncol();
         dlmFL3.push_back( arma::mat(tmp.begin(), n, k, true) );
      }
       
            
      tmpF = as<List>(rParamDLMF["Gk3"]); 
      for (int i=0;i<dim;i++) {
         NumericMatrix tmp = as<NumericMatrix>(tmpF[i]);
         n = tmp.nrow(), k = tmp.ncol();
         dlmFGk3.push_back( arma::mat(tmp.begin(), n, k, true) );
      }
            
   }
      
     
     /** Transition probability, i.e. bivariate cumulative normal of pig price. */  //Two levels
     double logTransPrTSP(int s, int t, arma::vec lower, arma::vec upper, arma::vec mt){
       arma::vec mean; 
       arma::mat sigma(2,2);
       if(s==1){ // s=1 means t=0
          mean = dlmPGk1 * mt;
          sigma = dlmPL1;
       }else{
          mean = dlmPG[t] * mt; // mean = G_{t+1}*m{t}
          sigma = dlmPL[t];  //sigma = A_{t+1}*Q_{t+1}*A'_{t+1} = L_{t+1}
       }         
       return log( pNorm2D_arma(lower, upper, mean, sigma) );       
     }
     
     
     /** Transition probability, i.e. normal of feed price (slope). */  //Two levels
     double logTransPrSF(int s, int t, double lower, double upper, double mt){
       arma::vec mean; 
       arma::mat sigma(1,1); 
       if(s==1){ // s=1 means t=0 
          mean = dlmFGk1 * mt; 
          sigma = dlmFL1; 
       }else{ 
          mean = dlmFG[t] * mt;  // mean = G_{t+1}*m{t} 
          sigma = dlmFL[t];  //sigma = A_{t+1}*Q_{t+1}*A'_{t+1} = L_{t+1}
       }        
       return log( R::pnorm(upper,mean[0],sqrt( sigma(0,0) ),1,0) - R::pnorm(lower,mean[0],sqrt( sigma(0,0) ),1,0) );
     }
     

     /** Transition probability, i.e. normal of piglet price (slope). */  //Two levels
     double logTransPrSPi(int s, int t, double lower, double upper, double mt){
       arma::vec mean; 
       arma::mat sigma(1,1); 
       if(s==1){ // s=1 means t=0 
          mean = dlmPiGk1 * mt; 
          sigma = dlmPiL1; 
       }else{ 
          mean = dlmPiG[t] * mt;  // mean = G_{t+1}*m{t} 
          sigma = dlmPiL[t];  //sigma = A_{t+1}*Q_{t+1}*A'_{t+1} = L_{t+1}
       }        
       return log( R::pnorm(upper,mean[0],sqrt( sigma(0,0) ),1,0) - R::pnorm(lower,mean[0],sqrt( sigma(0,0) ),1,0) ); 
     }
     
     
     /** Transition probability, i.e. bivariate cumulative normal of pig price when we have term. action. */  //Two levels
     double logTransPrTermTP(int s, int t, double lower, double upper, arma::vec mt){
       arma::vec mean; 
       arma::mat sigma(1,1);
          mean = dlmPGk3[t-1] * mt;  
          sigma = dlmPL3[t-1];  
       return log( R::pnorm(upper,mean[0],sqrt( sigma(0,0) ),1,0) - R::pnorm(lower,mean[0],sqrt( sigma(0,0) ),1,0) );       
     }     

     
     /** Transition probability, i.e. bivariate cumulative normal of feed price when we have term. action. */  //Two levels
     double logTransPrTermTF(int s, int t, double lower, double upper, double mt){
       arma::vec mean; 
       arma::mat sigma(1,1);
          mean = dlmFGk3[t-1] * mt; 
          sigma = dlmFL3[t-1]; 
       return log( R::pnorm(upper,mean[0],sqrt( sigma(0,0) ),1,0) - R::pnorm(lower,mean[0],sqrt( sigma(0,0) ),1,0) );       
     }
     
     
     /** Transition probability, i.e. bivariate cumulative normal of piglet price when we have term. action. */  //Two levels     
     double logTransPrTermPi(int s, int t, double lower, double upper, double mt){
       arma::vec mean; 
       arma::mat sigma(1,1);
          mean = dlmPiGk3[t-1] * mt;  
          sigma = dlmPiL3[t-1];        
       return log( R::pnorm(upper,mean[0],sqrt( sigma(0,0) ),1,0) - R::pnorm(lower,mean[0], sqrt( sigma(0,0) ),1,0) );      
     }
              

private:   

   vector<arma::mat> dlmPG;     //Parametr for one ahead forecast 
   vector<arma::mat> dlmPL;     //Parametr for one ahead forecast
   vector<arma::mat> dlmPL3;    //Parametr for k2 (explained in R file) ahead forecast for term. action
   vector<arma::mat> dlmPGk3;   //Parametr for k2 (explained in R file) ahead forecast for term. action
   arma::mat dlmPL1;            //Parametr for tStartMarketing ahead forecast
   arma::mat dlmPGk1;           //Parametr for tStartMarketing ahead forecast
   
   vector<arma::mat> dlmPiG;     //Parameter for one ahead forecast
   vector<arma::mat> dlmPiL;     //Parameter for one ahead forecast
   vector<arma::mat> dlmPiL3;    //Parameter for k2 (explained in R file) ahead forecast for term. action
   vector<arma::mat> dlmPiGk3;   //Parameter for k2 (explained in R file) ahead forecast for term. action
   arma::mat dlmPiL1;            //Parameter for tStartMarketing ahead forecast
   arma::mat dlmPiGk1;           //Parameter for tStartMarketing ahead forecast
   
   vector<arma::mat> dlmFG;     //Parametr for one ahead forecast
   vector<arma::mat> dlmFL;     //Parametr for one ahead forecast
   vector<arma::mat> dlmFL3;    //Parametr for k2 (explained in R file) ahead forecast for term. action (observable variables)
   vector<arma::mat> dlmFGk3;   //Parametr for k2 (explained in R file) ahead forecast for term. action (observable variables)
   arma::mat dlmFL1;            //Parametr for tStartMarketing ahead forecast
   arma::mat dlmFGk1;           //Parametr for tStartMarketing ahead forecast

};


#endif