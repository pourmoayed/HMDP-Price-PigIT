#ifndef HMDP2_HPP
#define HMDP2_HPP

#include "RcppArmadillo.h"    // we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include "binaryMDPWriter.h"
#include "dlm.h"
#include "time.h"
#include "cumNorm.h"
using namespace Rcpp;
using namespace std;

// via the depends attribute we tell Rcpp to create hooks for
// RcppArmadillo so that the build process will know what to do

// [[Rcpp::depends(RcppArmadillo)]]

// ===================================================

/**
* Class for building a 2 level HMDP modelling a pen with info about weight and price.
* 
* It store all the info needed for writing the HMDP to the binary files.
* 
* @author Lars Relund (lars@relund.dk)
*/
class HMDP2
{
public:  // methods

    /** Constructor. Store the parameters.
     * @param filePrefix Prefix used by the binary files storing the MDP model.
     * @param param Model parameters a list created using \code{setParameters} in R.
     * @param paramDLMPi Model parameter related to DLMPi defind for the piglet price information in R.
     * @param ParamDLMP Model parameter related to DLMP defind for the pig price information in R.
     * @param ParamDLMF Model parameters related to DLMF defind for the feed price information in R.
     * @param ParamPolicy A given policy for the HMDP, If the bool variable modPolicy be "TRUE". 
     */
   HMDP2(const string filePrefix, const List param, const List paramDLMP, const List paramDLMPi, const List paramDLMF
   ,const List paramPolicy);
   
   
   /** Build the HMDP (to binary files). Use "shared linking". 
    *  
    *  Build a 2 level HMDP saved in binary files by using the 
    *  binaryMDPWriter (c++ version). 
    *  
    *  @return Build log (string)
    *  @author Lars Relund \email{lars@@relund.dk}
    *  @export
    */
   SEXP BuildHMDP();
   
    /** Create the process at level 1 for orginal model
    * @param iFeed Index of state variable for trend of feed price at time t.
    */   
    SEXP BuildL1Process(int & iFeed);
   
   
    /** Create the process at level 1 based on a given policy
    * @param iFeed Index of state variable for trend of feed price at time t.
    */   
    SEXP BuildL1ProcessMPolicy(int & iFeed);
   
   
   /** Count the number of states in the HMDP */
   int countStatesHMDP() {
     int iTP, iTF, iSPi, x, y;
     x=0; y=0;
     for(iTP=0; iTP<sizeSTP; iTP++){
       for(iTF=0; iTF<sizeSTF; iTF++){
         for(iSPi=0; iSPi<sizeSSPi; iSPi++ ){
               x++;
               }
            }
         }
      
     for(iTP=0; iTP<sizeSTP; iTP++)   
         y = y + IdCountL2();
         
    return ( x + y );
 }
   
   
private:
   
   /** First stage of process 1
    * @param iTFt Index of state variable for trend of feed price at time t.
    */   
   void extProbFirst(int & iTFt);

   /** Calculate the transition probabilities of action cont. in the second level. 
    * 
    * Set the class vectors scope, pr and index.
    * 
    * @param iTPt Index of state variable for trend of pig price at time t. 
    * @param iTFt Index of state variable for trend of feed price at time t.
    * @param iSPt Index of state variable for slope of pig price at time t.
    * @param iSFt Index of state variable for slope of feed price at time t.
    * @param iSPit Index of state variable for slope of piglet price at time t.
    * @param int Index of state variable for the nember of remining pigs in the pen at time t. 
    * @param s Stage number in the second level of HMDP. 
    * 
    * @return vectors scope, index and pr for action con. . 
    */
    void CalcTransPrCont(int & iTPt, int & iSPt, int & iTFt, int & iSFt, int & iSPit, int & nt, int & s);

    
    /** Calculate the transition probabilities of action cull. in the second level. 
    * 
    * Set the class vectors scope, pr and index.
    * 
    * @param iTPt Index of state variable for trend of pig price at time t. 
    * @param iTFt Index of state variable for trend of feed price at time t.
    * @param iSPt Index of state variable for slope of pig price at time t.
    * @param iSFt Index of state variable for slope of feed price at time t.
    * @param iSPit Index of state variable for slope of piglet price at time t.
    * @param nt Index of state variable for the nember of remining pigs in the pen at time t.
    * @param cull Number of pigs that should be culles at time t.
    * @param s Stage number in the second level of HMDP. 
    * 
    * @return vectors scope, index and pr for action cull. . 
    */
    void CalcTransPrCull(int & iTPt, int & iSPt, int & iTFt, int & iSFt, int & iSPit, int & nt, int & cull, int & s);

    
    /** Calculate the transition probabilities of action term. in the second level. 
    * 
    * Set the class vectors scope, pr and index.
    * 
    * @param iTPt Index of state variable for trend of pig price at time t. 
    * @param iTFt Index of state variable for trend of feed price at time t.
    * @param iSPt Index of state variable for slope of pig price at time t.
    * @param iSFt Index of state variable for slope of feed price at time t.
    * @param iSPit Index of state variable for slope of piglet price at time t.
    * @param s Stage number in the second level of HMDP. 
    * 
    * @return vectors scope, index and pr for action term. . 
    */
    void CalcTransPrTerm(int & iTPt, int & iSPt, int & iTFt, int & iSFt, int & iSPit, int & s);

    /** Calculate the transition probabilities of action dummyJump for external processes. 
    * 
    * Set the class vectors scope, pr and index.
    * 
    * @param iTPt Index of state variable for trend of pig price at time t. 
    * @param iTFt Index of state variable for trend of feed price at time t.
    * @param iSPt Index of state variable for slope of pig price at time t.
    * @param iSFt Index of state variable for slope of feed price at time t.
    * @param iSPit Index of state variable for slope of piglet price at time t.
    * @param s Stage number in the second level of HMDP. 
    * 
    * @return vectors scope, index and pr for action term. . 
    */
    void CalcTransPrJump(int & iTPt, int & iSPt, int & iTFt, int & iSFt, int & iSPit, int & s);


    /** Build a map of the index of states for the given stage in the second level. That is, the map to identify state id in the second level given the current stage.
    * 
    * @param stage Stage number to build the map.
    */
    void BuildMapL2Vector(int stage);
    
    
    /** Build a map of the index of states in the first level of HMDP. That is, the map to identify state id in the fisrt level.
    * 
    * @param stage Stage number to build the map.
    */
    void BuildMapLVector();
    
        
    /** Calculate the  weight values under action Marketing (cull).
    * 
    *  Values are stored in the 7-dim vector \var(weightCull[s][cull][iTP][iTF][iW][iSd][n]).
    */
    void CalcWeightMarket();
    
    /** Calculate the weights of a cull action at level 2. 
    * 
    * @param s Stage number.
    * @param t Week number.
    * @param cull number of cull pigs.
    * @param nt Number of pigs in the pen at time t.
    * @param iTP Index of state variable for trend of pig price. 
    * @param iTF Index of state variable for trend of feed price.
    * 
    * @author Reza Pourmoayed \email{rpourmoayed@econ.au.dk}
    */                 
    double CalcWeightsCull(int & s, int &t, int & cull, int & nt, int & iTP, int & iTF);
                   
    /** Calculate the log transformed probabilities related to the pig price.
    * 
    *  Values are stored in the vector \var(prTSP[s][iTPt][iSPt][iTP][iSP]).
    */
    void CalcTransPrTSP();


    /** Calculate the log transformed probabilities related to the feed price.
    * 
    *  Values are stored in the vector \var(prTSF[s][iSFt][iSF]).
    */
    void CalcTransPrSF();


    /** Calculate the log transformed probabilities related related to the piglet price information.
    * 
    *  Values are stored in the vector \var(prTermPi[s][iSPit][iSPi]).
    */ 
   void CalcTransPrSPi();


    /** Calculate the log transformed probabilities related to the pig price when we have term. action.
    * 
    *  Values are stored in the vector \var(prTermTSP[s][iTPt][iSPt][iTP]).
    */
    void CalcTransPrTermTP();
    

    /** Calculate the log transformed probabilities related to the pig price when we have term. action.
    * 
    *  Values are stored in the vector \var(prTermTSF[s][iTFt][iSFt][iTF]).
    */
    void CalcTransPrTermTF();
    
     
   /** Calculate the log transformed probabilities related related to the piglet price information when we have term. action.
   * 
   *  Values are stored in the vector \var(prSF[s][iSFt][iSF]).
   */ 
   void CalcTransPrTermPi();


   
   /** Convert integers into a string. */
   string getLabel(const int & a, const int & b) {
      std::ostringstream os;
      os << "(" << a << "," << b << ")";
      return os.str();
   }
   
   /** Convert integers into a string. */
   string getLabel(const int & a, const int & b, const int & c) {
      std::ostringstream os;
      os << "(" << a << "," << b << "," << c << ")";
      return os.str();
   }

   /** Convert integers into a string. */
   string getLabel(const int & a, const int & b, const int & c, const int & d) {
      std::ostringstream os;
      os << "(" << a << "," << b << "," << c << "," << d << ")";
      return os.str();
   }
   /** Convert integers into a string. */
   string getLabel(const int & a, const int & b, const int & c, const int & d, const int & e) {
      std::ostringstream os;
      os << "(" << a << "," << b << "," << c << "," << d << "," << e << ")";
      return os.str();
   }

   /** Convert integers into a string. */
   string getLabel(const int & a, const int & b, const int & c, const int & d, const int & e, const int & f) {
      std::ostringstream os;
      os << "(" << a << "," << b << "," << c << "," << d << "," << e << "," << f << ")";
      return os.str();
   }
   
    /** Convert integers into a string. */
   string getLabel(const int & a, const int & b, const int & c, const int & d, const int & e, const int & f, const int & g) {
      std::ostringstream os;
      os << "(" << a << "," << b << "," << c << "," << d << "," << e << "," << f << "," << g << ")";
      return os.str();
   }
   
    /** Convert integers into a string. */
   string getLabel(const int & a, const int & b, const int & c, const int & d, const int & e, const int & f, const int & g, const int & h) {
      std::ostringstream os;
      os << "(" << a << "," << b << "," << c << "," << d << "," << e << "," << f << "," << g << "," << h << ")";
      return os.str();
   }

    /** Convert integers into a string. */
   string getLabel(const int & a, const int & b, const int & c, const int & d, const int & e, const int & f, const int & g, const int & h, const int & k) {
      std::ostringstream os;
      os << "(" << a << "," << b << "," << c << "," << d << "," << e << "," << f << "," << g << "," << h << "," << k << ")";
      return os.str();
   }

   /** Count the number of states in the second level of HMDP
    * 
    */  
    int IdCountL2(){
      int s, n, iTP, iSP, iSF, iSPi, x;
      x=0;
             
      for(s = 1; s<=(tMax-tStartMarketing +2); s++){
         for(iTP=0; iTP<sizeSTP; iTP++ ){
            for(iSP=0; iSP<sizeSSP; iSP++){
              if( (s==1) & (sSP[iSP] != 0) ) continue;
                  for(iSF=0; iSF<sizeSSF; iSF++){
                    if( (s==1) & (sSF[iSF] != 0) ) continue;
                     for(iSPi=0; iSPi<sizeSSPi; iSPi++){
                        for(n=1; n<=pigs; n++){
                           if( ( (s==1) || (s==2) ) & (n!=pigs) ) continue;
                           x++;
                        }
                     }
                  }
               }
            }
         }
      return (x);
    }


 /** Count the number of states in the second level of HMDP
    * 
    */  
    int IdCountL2Last(){
      int  n, iTP, iSP, iSF, iSPi, x;
      x=0;
             
         for(iTP=0; iTP<sizeSTP; iTP++ ){
            for(iSP=0; iSP<sizeSSP; iSP++){
                  for(iSF=0; iSF<sizeSSF; iSF++){
                     for(iSPi=0; iSPi<sizeSSPi; iSPi++){
                        for(n=0; n<=pigs; n++){
                           if( n!=0 ) continue;
                           x++;
                        }
                     }
                  }
               }
            }
      return (x);
    }
    

private:   // variables

   static const double ZERO;  // trans pr below are considered as ZERO

   int pigs;
   int tMax, tStartMarketing;
   int minMarketingSize; 
   double cleaningPeriod;
   double marketingLength;
   double pigletLeadTime;
   double avgGRate;              
   double avgLeanP;               
   double avgInsWeight;
   double avgInsSd;
   double convRateSd;
   double coefPiglet;
   
   bool modPolicy; 

   arma::vec sdWeights;           
   arma::vec meanWeights;
   arma::vec sdGrowth;           
   arma::vec meanGrowth;   
   arma::vec sTP;
   arma::vec sTF;   
   arma::vec sSP;
   arma::vec sSF;
   arma::vec sSPi;
   
   arma::mat dTP;
   arma::mat dTF;
   arma::mat dSP;
   arma::mat dSF;
   arma::mat dSPi;
      
   int sizeSTP;  
   int sizeSTF;  
   int sizeSSP;   
   int sizeSSF;
   int sizeSSPi;
   int iMTP;
   int iMTF;
   int iMSP;
   int iMSF;
   int iMSPi;
   
   vector<arma::mat> mPolicy; // a vector to store the given policy for each child
   
   arma::mat feedWeek;
   arma::mat feedCull;
   vector<arma::mat> rewCull;
         
   int idS;         // the value of sId in the two level hmdp
   int lastStage; 
   
   DLM dlm;     // DLMs models used 
   
   // variables used when build process
   vector<int> scope;
   vector<int> index;
   vector<flt> pr;
   vector<flt> weights;
   map<string,int> mapR;   // map to identify the unique state id of states where tR=0 (used when do shared linking)
   
   vector < vector < vector < vector < vector<double> > > > > prTSP; //prTSP[s][iTPt][iSPt][iTP][iSP]  log probability of pig price information.
   vector < vector < vector<double> > > prSF; //prTSF[s][iSFt][iSF]  log probability of feed price information.
   vector < vector < vector<double> > > prSPi; //prSPi[s][iSPit][iSPi] log probability of the slope of piglet price.

   vector < vector < vector < vector<double> > > > prTermTP; //prTermTP[s][iTPt][iSPt][iTP]  log probability of pig price information when we have term. action.
   vector < vector < vector < vector<double> > > > prTermTF; //prTermTF[s][iTFt][iSFt][iTF]  log probability of feed price information when we have term. action.
   vector < vector < vector<double> > > prTermPi; //prTermPi[s][iSPit][iSPi] log probability of the piglet price when action is term.

   vector< vector < vector <vector <vector <double> > > > > weightCull; // weightCull[s][cull][iTP][iTF][n] reward of hmdp for all actions.  
   vector< vector < vector< vector< vector<int> > > > > mapL2Vector; //mapL2Vector[iTP][iSP][iSF][iSPi][n] map of states in the second level. 
   vector< vector< vector<int> > > mapLVector; //mapLVector[iTP][iTF][iSPi] map of states in the first level.
        
   string label;
   string processLabel;
   BinaryMDPWriter w;
   ostringstream os;         // stream to write labels
   
   TimeMan cpuTime;
};


#endif