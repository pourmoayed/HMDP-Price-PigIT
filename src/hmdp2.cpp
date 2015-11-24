#include "hmdp2.h"

// ===================================================

const double HMDP2::ZERO = 1e-10;

// ===================================================

HMDP2::HMDP2(const string prefix, const List param, const List paramDLMP, const List paramDLMPi, const List paramDLMF
,const List paramPolicy) : 
             dlm(paramDLMP,paramDLMPi,paramDLMF), w(prefix) {   
   List rParam(param);       // Get parameters in params
   pigs = as<int>(rParam["pigs"]);
   tMax = as<int>(rParam["tMax"]);
   tStartMarketing = as<int>(rParam["tStartMarketing"]);
   minMarketingSize = as<int>(rParam["minMarketingSize"]);
   cleaningPeriod = as<double>(rParam["cleaningPeriod"]);
   marketingLength = as<double>(rParam["marketingLength"]);
   pigletLeadTime = as<double>(rParam["pigletLeadTime"]);
   avgGRate = as<double>(rParam["avgGRate"]);            
   avgLeanP = as<double>(rParam["avgLeanP"]);            
   avgInsWeight = as<double>(rParam["avgInsWeight"]);
   avgInsSd = as<double>(rParam["avgInsSd"]);
   convRateSd = as<double>(rParam["convRateSd"]);
   coefPiglet = as<double>(rParam["coefPiglet"]);   
   meanWeights = as<arma::vec>(rParam["meanWeights"]);      
   sdWeights = as<arma::vec>(rParam["sdWeights"]);
   meanGrowth = as<arma::vec>(rParam["meanGrowth"]);      
   sdGrowth = as<arma::vec>(rParam["sdGrowth"]);
   modPolicy = as<bool>(rParam["modPolicy"]);
   iMTP = as<int>(rParam["iMTP"]);
   iMTF = as<int>(rParam["iMTF"]);
   iMSP = as<int>(rParam["iMSP"]);
   iMSF = as<int>(rParam["iMSF"]);
   iMSPi = as<int>(rParam["iMSPi"]);
        
   sTP = as<arma::vec>(rParam["centerPointsTP"]); 
   sTF = as<arma::vec>(rParam["centerPointsTF"]); 
   sSP = as<arma::vec>(rParam["centerPointsSP"]);
   sSF = as<arma::vec>(rParam["centerPointsSF"]);
   sSPi = as<arma::vec>(rParam["centerPointsSPi"]);
   
   dTP = as<arma::mat>(rParam["IntervalsTP"]); //discretization of TP for transition probabilities 
   dTF = as<arma::mat>(rParam["IntervalsTF"]); //discretization of TF for transition probabilities
   dSP = as<arma::mat>(rParam["IntervalsSP"]); //discretization of SP for transition probabilities
   dSF = as<arma::mat>(rParam["IntervalsSF"]); //discretization of TF for transition probabilities
   dSPi = as<arma::mat>(rParam["IntervalsSPi"]); //discretization of SPi for transition probabilities
   
   sizeSTP = sTP.size();  
   sizeSTF = sTF.size();  
   sizeSSP = sSP.size();   
   sizeSSF = sSF.size();
   sizeSSPi = sSPi.size();
   
      
   prTSP = vector < vector < vector < vector < vector<double> > > > > (tMax-tStartMarketing +2, 
      vector < vector < vector < vector<double> > > > (sizeSTP, 
      vector < vector < vector<double> > > (sizeSSP, 
      vector < vector<double> > (sizeSTP,
      vector <double>(sizeSSP) ) ) ) ) ;  // prTSP[s][iTPt][iSPt][iTP][iSP]


   prSF = vector < vector < vector<double> > > (tMax-tStartMarketing +2, 
      vector <vector<double> > (sizeSSF, 
      vector <double>(sizeSSF) ) );  // prSF[s][iSFt][iSF]
      
      
   prSPi = vector < vector < vector<double> > > (tMax-tStartMarketing +2, 
      vector < vector<double> > (sizeSSPi, 
      vector <double>(sizeSSPi) ) ) ;  // prSPi[s][iSPit][iSPi]      
   
   
   prTermTP = vector < vector < vector < vector<double> > > > (tMax-tStartMarketing +2 + 1, 
      vector < vector < vector<double> > > (sizeSTP, 
      vector < vector<double> > (sizeSSP, 
      vector <double>(sizeSTP) ) ) );  // prTermTP[s][iTPt][iSPt][iTP]
 
 
   prTermTF = vector < vector < vector < vector<double> > > > (tMax-tStartMarketing +2 + 1, 
      vector < vector < vector<double> > > (sizeSTF, 
      vector < vector<double> > (sizeSSF, 
      vector <double>(sizeSTF) ) ) ) ;  // prTermTSF[s][iTFt][iSFt][iTF]
 
 
  prTermPi =  vector < vector < vector<double> > > (tMax-tStartMarketing +2 + 1, 
      vector < vector<double> > (sizeSSPi, 
      vector <double>(sizeSSPi) ) ) ;  // prTermPi[s][iSPit][iSPi]

            
   weightCull = vector< vector < vector <vector <vector <double> > > > >(tMax-tStartMarketing +2 + 1, 
   vector < vector <vector <vector <double> > > >(pigs+1,
   vector <vector <vector <double> > >(sizeSTP,
   vector <vector <double> >(sizeSTF, 
   vector <double>(pigs+1) ) ) ) );  //weightCull[s][cull][iTP][iTF][n]
      
   mapL2Vector = vector< vector < vector< vector< vector<int> > > > >(sizeSTP, 
   vector< vector< vector< vector<int> > > >(sizeSSP, 
   vector< vector< vector<int> > >(sizeSSF, 
   vector< vector<int> >(sizeSSPi,
   vector <int>(pigs+1) ) ) ) ) ; //mapL2Vector[iTP][iSP][iSF][iSPi][n]

   mapLVector = vector< vector< vector<int> > >(sizeSTP, 
   vector< vector<int> >(sizeSTF, 
   vector <int>(sizeSSPi) ) ); //mapLVector[iTP][iTF][iSPi]
   
// Read the reward values and feed intake data from R   
  
  feedWeek = as<arma::mat>(rParam["feedWeek"]);
  feedCull = as<arma::mat>(rParam["feedCull"]);
  List revenue = as<List>(rParam["rewCull"]);
  int s,d;
  
  for (int h=0;h<sizeSTP;h++) {
  NumericMatrix tmp = as<NumericMatrix>(revenue[h]);
  s = tmp.nrow(), d = tmp.ncol();
  rewCull.push_back( arma::mat(tmp.begin(), s, d, true) );
   }

// read the mPolicy (modified policy from R):
   if(modPolicy){
     List rParamPolicy(paramPolicy);
     int dim =rParamPolicy.size();
     int rowNum,n,k;
     for (int i=0;i<dim;i++) {
     NumericMatrix tmp = as<NumericMatrix>(rParamPolicy[i]);
     n = tmp.nrow(), k = tmp.ncol();
     mPolicy.push_back( arma::mat(tmp.begin(), n, k, true) );
      }
  //replace the action for the state that the deviations are not zero, should we consider iMTF in replacing (variation in feed price)?   
      cpuTime.Reset(0); cpuTime.StartTime(0);
      for (int i=0;i<dim;i++){
        for(int j=0;j<mPolicy[i].n_rows ;j++){
                 if( (mPolicy[i](j,1)!=iMTP) || (mPolicy[i](j,2)!=iMSP)  || (mPolicy[i](j,3)!=iMTF) ||  (mPolicy[i](j,4)!=iMSF) || (mPolicy[i](j,5)!=iMSPi) ){
                   rowNum = findIndice(mPolicy[iMTF], iMTP, iMSP, iMTF, iMSF, iMSPi, mPolicy[i](j,6) , mPolicy[i](j,7) ); 
                   mPolicy[i](j,0)=mPolicy[iMTF](rowNum,0);                   
           }
        }
                              
      }
          Rcout << "Time for modifying " << cpuTime.StopAndGetTotalTimeDiff(0) << endl;
        
    }
        
}

// ===================================================

SEXP HMDP2::BuildHMDP() { 
   lastStage =tMax-tStartMarketing +2;
   CalcTransPrTSP();
   CalcTransPrTermTP();
   CalcTransPrSF();
   CalcTransPrTermTF();
   CalcTransPrSPi();
   CalcTransPrTermPi();
   CalcWeightMarket();
   
  //DBG4("specified weight value is: "<<weightCull[2][3][1][1][1][1][5]<<endl)  //weightCull[s][cull][iTP][iTF][iW][iSd][n]
      
   int currentT, n, cull, iTP, iTF, iSP, iSF, iSPi, iTPt, iTFt, iSPit;
   
   for(iTF=0; iTF<sizeSTF; iTF++){
   if(modPolicy)
   BuildL1ProcessMPolicy(iTF);
   else
   BuildL1Process(iTF);
   }

   w.SetWeight("Time");
   w.SetWeight("Reward");
    
   w.Process();    // level 0 (founder)
   w.Stage();

 BuildMapLVector();
   idS=0;
   for(iTFt=0; iTFt<sizeSTF; iTFt++){
      for(iTPt=0; iTPt<sizeSTP; iTPt++){
           for(iSPit=0; iSPit<sizeSSPi; iSPit++){
               label = getLabel(iTPt,iTFt,iSPit);
               idS=w.State(label);
               if( (iTPt==0)  & (iSPit==0) ){
                 extProbFirst(iTFt);
                 processLabel = "exPro" + ToString<int>(iTFt) + "_";
                 label = "feed cost" + ToString<int>(iTFt);
                 weights.assign(2,0); 
                 weights[1] = -(exp(sSPi[iSPit])*sTP[iTPt])*pigs;
                 w.IncludeProcess(processLabel, scope, index, pr, weights, label, IdCountL2Last() );
                 w.Stage();
                 currentT=lastStage; // To be able to compute transition probabilities correctly!
                 for(iTP=0; iTP<sizeSTP; iTP++ ){
                   for(iSP=0; iSP<sizeSSP; iSP++){
                     for(iSF=0; iSF<sizeSSF; iSF++){
                       for(iSPi=0; iSPi<sizeSSPi; iSPi++){
                         for(n=0; n<=pigs; n++){
                           if(n!=0) continue;
                           label = getLabel(iTP,iSP,iTFt,iSF,iSPi,n);
                           w.State(label);
                            CalcTransPrJump(iTP,iSP,iTFt,iSF,iSPi,currentT);
                            weights.assign(2,0);
                            weights[0] = 1; // we assume that sum of cleaning period and delivery is 1 week
                            weights[1] =0;
                            w.Action(scope, index, pr, weights, "dummyJump", true);
                            w.EndState();   
                         }
                       }
                     }
                   }
                 }
                 w.EndStage();
                 w.EndIncludeProcess();     
               }else{
                 pr.clear(); index.clear(); scope.clear();
                 index.assign(1,mapR[getLabel(iTPt,iTFt,iSPit)]); scope.assign(1,3); pr.assign(1,1); 
                  weights.assign(2,0);
                  weights[1] = -(exp(sSPi[iSPit])*sTP[iTPt])*pigs;
                  label = "Shared";
                  w.Action(scope, index, pr, weights, label, true);
               }
               w.EndState();
              }
           }
        }
        
   w.EndStage();
   w.EndProcess();  // end level 1 (founder)
   w.CloseWriter();
   return wrap(w.log.str());
}
// ===================================================

void HMDP2::extProbFirst(int & iTFt){
  
  int s, n, iTP, iSP, iSF, iSPi;
  double pr2;
  int idFirst=0;
  int idShared=idS;
  pr.clear(); index.clear(); scope.clear();
  
  s=1;
  for(iTP=0; iTP<sizeSTP; iTP++ ){
    for(iSP=0; iSP<sizeSSP; iSP++){
      if( (s==1) & (sSP[iSP] != 0) ) continue;
      for(iSF=0; iSF<sizeSSF; iSF++){
        if( (s==1) & (sSF[iSF] != 0) ) continue;
        for(iSPi=0; iSPi<sizeSSPi; iSPi++){
          for(n=0; n<=pigs; n++){
            if( ( (s==1) || (s==2) ) & (n!=pigs) ) continue;
            if( (s==2) & (n==0) ) continue;
            
            if( (iTP==0)  & (iSPi==0) ){
              pr2=1;
//              pr.push_back(pr2);
//              index.push_back(idFirst);
              }else{
               pr2=0;
              }
//              if(pr2>0){
//              pr.push_back(pr2);
//              index.push_back(idFirst); 
//                 }
            pr.push_back(pr2);
            index.push_back(idFirst); 
            idFirst++;
            idShared++;
            mapR[getLabel(iTP,iTFt,iSPi)] =idShared;    
          }
        }
      }
    }
  }
  scope.assign(pr.size(),2);  
}


// ===================================================

SEXP HMDP2::BuildL1Process(int & iFeed) {
   
   processLabel = "exPro" + ToString<int>(iFeed) + "_";  
   BinaryMDPWriter w(processLabel);
   w.SetWeight("Time");
   w.SetWeight("Reward");

   int s, n, cull, iTP, iSP, iSF, iSPi;
   w.Process(); // level 2
   DBG4("procese: " <<iFeed+1<<endl)
      for(s = 1; s<=(lastStage); s++) {
         //if(s != (tMax-tStartMarketing +2) ) 
         BuildMapL2Vector(s+1);            
             w.Stage();
             for(iTP=0; iTP<sizeSTP; iTP++ ){
                for(iSP=0; iSP<sizeSSP; iSP++){
                  if( (s==1) & (sSP[iSP] != 0) ) continue; 
                      for(iSF=0; iSF<sizeSSF; iSF++){
                        if( (s==1) & (sSF[iSF] != 0) ) continue;
                         for(iSPi=0; iSPi<sizeSSPi; iSPi++){
                            for(n=0; n<=pigs; n++){
                               if( ( (s==1) || (s==2) ) & (n!=pigs) ) continue;
                               if( (s==2) & (n==0) ) continue;
                               label = getLabel(iTP,iSP,iFeed,iSF,iSPi,n,s);
                               w.State(label);
                                                             
                               // continue action
                               if ( (s!=lastStage) & (n!=0) ){
                                  CalcTransPrCont(iTP,iSP,iFeed,iSF,iSPi,n,s);
                                  weights.assign(2,0);
                                  cull=0;
                                  if(s==1){
                                    weights[0] = tStartMarketing - 1;
                                  }
                                  if(s!=1){
                                    weights[0] = 1;
                                  }                                  
                                  weights[1]=weightCull[s][cull][iTP][iFeed][n];
                                  w.Action(scope, index, pr, weights, "cont.", true);
                               }
                               
                               //Individual marketing actions
                               if ( (s>=2) & (s!=lastStage) & (n>minMarketingSize) & (n!=0)  ){
                                  for (cull=1; cull<n; cull++) {    
                                       CalcTransPrCull(iTP,iSP,iFeed,iSF,iSPi,n,cull,s);
                                       weights.assign(2,0);
                                       weights[0]=1;
                                       weights[1]=weightCull[s][cull][iTP][iFeed][n];
                                       label = ToString<double>(cull);
                                       w.Action(scope, index, pr, weights, label, true);
                                        }                                     
                                  }
                                  
                                // terminate action
                                if( s>=2 ){
                                  CalcTransPrTerm(iTP,iSP,iFeed,iSF,iSPi,s);                                  
                                   weights.assign(2,0);
                                   weights[0] = 0;
                                   if(n==0){
                                     weights[1] =0;
                                     w.Action(scope, index, pr, weights, "dummyTerm", true);
                                   }else{
                                     cull=n;
                                     weights[1] = weightCull[s][cull][iTP][iFeed][n];
                                     w.Action(scope, index, pr, weights, "term.", true);
                                   }                                   
                                }
                                w.EndState();
                            }
                         }
                      }
                   }
                }
             w.EndStage();
        }
        w.Stage(); // Dummy stage for externalprocesses
        for(iTP=0; iTP<sizeSTP; iTP++ ){
          for(iSP=0; iSP<sizeSSP; iSP++){
            for(iSF=0; iSF<sizeSSF; iSF++){
              for(iSPi=0; iSPi<sizeSSPi; iSPi++){
                for(n=0; n<=pigs; n++){
                  if(n!=0) continue;
                  label=getLabel(iTP,iSP,iFeed,iSF,iSPi,n);
                  w.State(label);
                  w.EndState();                  
                }
              }
            }
          }
        }
        w.EndStage();
     w.EndProcess(); // end level
     w.CloseWriter();
     return wrap(w.log.str());        
 }

// ===================================================

SEXP HMDP2::BuildL1ProcessMPolicy(int & iFeed) {

// The structure of the given policy dlm.mPolicy[iFeed]: a matrix with columns: actionLabelM, TP, SP, TF, SF, SPi, n  s
// The values in actionLabelM mean: 
// -1:cont. 
// -2:term. 
// -3:dummyTerm 
// 1,2,...(q^max-1): culling actions
   processLabel = "exPro" + ToString<int>(iFeed) + "_";  
   BinaryMDPWriter w(processLabel);
   w.SetWeight("Time");
   w.SetWeight("Reward");

   int s, n, cull, iTP, iSP, iSF, iSPi, rowNum;
   w.Process(); // level 2
   DBG4("procese: " <<iFeed+1<<endl)
      for(s = 1; s<=(lastStage); s++) {
         //if(s != (tMax-tStartMarketing +2) ) 
         BuildMapL2Vector(s+1);            
             w.Stage();
             for(iTP=0; iTP<sizeSTP; iTP++ ){
                for(iSP=0; iSP<sizeSSP; iSP++){
                  if( (s==1) & (sSP[iSP] != 0) ) continue; 
                      for(iSF=0; iSF<sizeSSF; iSF++){
                        if( (s==1) & (sSF[iSF] != 0) ) continue;
                         for(iSPi=0; iSPi<sizeSSPi; iSPi++){
                            for(n=0; n<=pigs; n++){
                               if( ( (s==1) || (s==2) ) & (n!=pigs) ) continue;
                               if( (s==2) & (n==0) ) continue;
                               label = getLabel(iTP,iSP,iFeed,iSF,iSPi,n,s);
                               w.State(label);
                               
                               rowNum = findIndice(mPolicy[iFeed],iTP,iSP,iFeed, iSF, iSPi, n, s);
                               //if(rowNum == -1) DBG4("error find action"<<endl);
                               
                               // Continue action
                               if(mPolicy[iFeed](rowNum,0)==-1 ){
                                  CalcTransPrCont(iTP,iSP,iFeed,iSF,iSPi,n,s);
                                  weights.assign(2,0);
                                  cull=0;
                                  if(s==1){
                                    weights[0] = tStartMarketing - 1;
                                  }
                                  if(s!=1){
                                    weights[0] = 1;
                                  }                                                                    
                                  weights[1]=weightCull[s][cull][iTP][iFeed][n];  //  Should we change reward function?
                                  w.Action(scope, index, pr, weights, "cont.", true);
                               }
                               
                               //Individual marketing actions
                               if(mPolicy[iFeed](rowNum,0)>0){
                                 cull=mPolicy[iFeed](rowNum,0);
                                 CalcTransPrCull(iTP,iSP,iFeed,iSF,iSPi,n,cull,s);
                                 weights.assign(2,0);
                                 weights[0]=1;
                                 weights[1]=weightCull[s][cull][iTP][iFeed][n]; //  Should we change reward function?
                                 label = ToString<double>(cull);
                                 w.Action(scope, index, pr, weights, label, true); 
                               }
                               
                               // Terminate action
                               if(mPolicy[iFeed](rowNum,0)==-2){
                                 CalcTransPrTerm(iTP,iSP,iFeed,iSF,iSPi,s);                                  
                                 weights.assign(2,0);
                                 weights[0] = 0;
                                 cull=n;
                                 weights[1] = weightCull[s][cull][iTP][iFeed][n];  //  Should we change reward function?
                                 w.Action(scope, index, pr, weights, "term.", true);
                               }
                               
                               // Dummy terminate action
                               if(mPolicy[iFeed](rowNum,0)==-3){
                                 CalcTransPrTerm(iTP,iSP,iFeed,iSF,iSPi,s);                                  
                                 weights.assign(2,0);
                                 weights[0] = 0;
                                 weights[1] =0;
                                 w.Action(scope, index, pr, weights, "dummyTerm", true);
                               }
                                                             
                                w.EndState();
                            }
                         }
                      }
                   }
                }
             w.EndStage();
        }
        w.Stage(); // Dummy stage for externalprocesses
        for(iTP=0; iTP<sizeSTP; iTP++ ){
          for(iSP=0; iSP<sizeSSP; iSP++){
            for(iSF=0; iSF<sizeSSF; iSF++){
              for(iSPi=0; iSPi<sizeSSPi; iSPi++){
                for(n=0; n<=pigs; n++){
                  if(n!=0) continue;
                  label=getLabel(iTP,iSP,iFeed,iSF,iSPi,n);
                  w.State(label);
                  w.EndState();                  
                }
              }
            }
          }
        }
        w.EndStage();
     w.EndProcess(); // end level
     w.CloseWriter();
     return wrap(w.log.str());        
 }

// ===================================================
void HMDP2::CalcTransPrCont(int & iTPt, int & iSPt, int & iTFt, int & iSFt, int & iSPit, int & nt, int & s){
   double pr4;
   int iTP,iSP,iSF,iSPi,n,idCon,s1;
   n=nt;
   s1=s+1;
   pr.clear(); index.clear(); scope.clear();
   
   for(iTP=0; iTP<sizeSTP; iTP++ ){
      for(iSP=0; iSP<sizeSSP; iSP++){
        if( (s1==1) & (sSP[iSP] != 0) ) continue;
           for(iSF=0; iSF<sizeSSF; iSF++){
             if( (s1==1) & (sSF[iSF] != 0) ) continue;
              for(iSPi=0; iSPi<sizeSSPi; iSPi++){
                 idCon = mapL2Vector[iTP][iSP][iSF][iSPi][n];
                 pr4 = exp( prTSP[s][iTPt][iSPt][iTP][iSP] + prSF[s][iSFt][iSF] + prSPi[s][iSPit][iSPi]);
                  if (pr4>ZERO){
                  pr.push_back(pr4);
                  index.push_back(idCon);
                 }
              }
           }
         } 
      }  
   scope.assign(pr.size(),1);
}

// ===================================================

void HMDP2::CalcTransPrCull(int & iTPt, int & iSPt, int & iTFt, int & iSFt, int & iSPit, int & nt, int & cull, int & s) {
   double pr4, prCull;
   int iTP,iSP,iSF,iSPi,n,idCull,s1;
   s1=s+1; //next stage
   pr.clear(); index.clear(); scope.clear();
   
   for(iTP=0; iTP<sizeSTP; iTP++ ){
      for(iSP=0; iSP<sizeSSP; iSP++){
        if( (s1==1) & (sSP[iSP] != 0) ) continue;
           for(iSF=0; iSF<sizeSSF; iSF++){
             if( (s1==1) & (sSF[iSF] != 0) ) continue;
              for(iSPi=0; iSPi<sizeSSPi; iSPi++){
                 for(n=0;n<=nt;n++){
                    if( ( (s1==1) || (s1==2) ) & (n!=pigs) ) continue;
                    if( (s1==2) & (n==0) ) continue;
                    idCull=mapL2Vector[iTP][iSP][iSF][iSPi][n];
                    
                    if( (nt-n)==cull ){
                       prCull=1;
                    }else{
                        prCull=0;
                    }
                      
                    pr4 = prCull * exp(prTSP[s][iTPt][iSPt][iTP][iSP] + prSF[s][iSFt][iSF] + prSPi[s][iSPit][iSPi] );
                    if (pr4>ZERO){
                      pr.push_back(pr4);
                      index.push_back(idCull);
                    }
                 }
              }
           }
        } 
      }
   scope.assign(pr.size(),1); 
}  

// ===================================================

void HMDP2::CalcTransPrTerm(int & iTPt, int & iSPt, int & iTFt, int & iSFt, int & iSPit, int & s){
   double pr4;
   int n1,idTerm;
   pr.clear(); index.clear(); scope.clear();   
     n1=0;
     idTerm=mapL2Vector[iTPt][iSPt][iSFt][iSPit][n1];
     pr4=1;
     pr.push_back(pr4);
     index.push_back(idTerm);
     scope.assign(pr.size(),1);         
}

// ===================================================


void HMDP2::CalcTransPrJump(int & iTPt, int & iSPt, int & iTFt, int & iSFt, int & iSPit, int & s){
   double pr4;
   int iTP,iTF,iSPi,idFirst;
   pr.clear(); index.clear(); scope.clear();
   
     for(iTP=0; iTP<sizeSTP; iTP++ ){
      for(iTF=0; iTF<sizeSTF; iTF++ ){
          for(iSPi=0; iSPi<sizeSSPi; iSPi++){
               idFirst = mapLVector[iTP][iTF][iSPi];
               pr4 = exp( prTermTP[s][iTPt][iSPt][iTP] + prTermTF[s][iTFt][iSFt][iTF] + prTermPi[s][iSPit][iSPi]);  // At the moment I am not sure about prF[s][iSPt][iPFeedt][iTF].
               if (pr4>=ZERO){
                      pr.push_back(pr4);
                      index.push_back(idFirst);
                    }
                }
            }
         }
   scope.assign(pr.size(),0);         
}

// ===================================================

void HMDP2::BuildMapL2Vector(int stage) {   //  mapL2Vector[iTP][iSP][iSF][iSPi][n]
   int iTP,iSP,iSF,iSPi,n,idL2;
   // level 2
   idL2=0;
   
   for(iTP=0; iTP<sizeSTP; iTP++ ){
      for(iSP=0; iSP<sizeSSP; iSP++){
        if( (stage==1) & (sSP[iSP] != 0) ) continue;
            for(iSF=0; iSF<sizeSSF; iSF++){
              if( (stage==1) & (sSF[iSF] != 0) ) continue;
               for(iSPi=0; iSPi<sizeSSPi; iSPi++){
                  for(n=0; n<=pigs; n++){
                     if( ( (stage==1) || (stage==2) ) & (n!=pigs) ) continue;
                     if( (stage==2) & (n==0) ) continue;
                     if( (stage==(lastStage+1) ) & (n!=0) ) continue;
                     mapL2Vector[iTP][iSP][iSF][iSPi][n] = -1;
                     mapL2Vector[iTP][iSP][iSF][iSPi][n] = idL2;
                     idL2++;
                  }
               }
            }
         }
      }
   }

// ===================================================

void HMDP2::BuildMapLVector() {   //  mapLVector[iTP][iTF][iSPi]
   int iTP,iTF,iSPi,idL1;
   // level 2
   idL1=0;
   
   for(iTF=0; iTF<sizeSTF; iTF++){
      for(iTP=0; iTP<sizeSTP; iTP++){
           for(iSPi=0; iSPi<sizeSSPi; iSPi++){
           mapLVector[iTP][iTF][iSPi] = -1;
           mapLVector[iTP][iTF][iSPi] = idL1;
           idL1++;
               }
            }
         }
      }

// ===================================================
void HMDP2::CalcWeightMarket(){   
   cpuTime.Reset(0); cpuTime.StartTime(0);
   int iTP,iTF,n,s,t,cull;
   
   for(s = 1; s<=lastStage; s++){
            if(s==1){
               t=1;
            }else{
              t = s + tStartMarketing - 2;   
            }
            for(n=1; n<=pigs; n++){
              if( ( (s==1) || (s==2) ) & (n!=pigs) ) continue;
              for (cull=0; cull<=pigs; cull++){
                if( (s==1) & (cull!=0) ) continue;
                if( (s==lastStage) & (cull!=n) ) continue;
                for(iTP=0; iTP<sizeSTP; iTP++ ){
                  for(iTF=0; iTF<sizeSTF; iTF++){                     
                        weightCull[s][cull][iTP][iTF][n] = CalcWeightsCull(s,t,cull,n,iTP,iTF);
                     }   
                  }
               }
           }
       }
   Rcout << "Time for calculating weightTH: " << cpuTime.StopAndGetTotalTimeDiff(0) << endl; 
}


double HMDP2::CalcWeightsCull(int & s, int & t, int & cull, int & nt, int & iTP, int & iTF) {
      double sumReveneue, sumCost;
      sumReveneue=0; sumCost=0;      
      int limit=nt-cull;
      if(s==1){
        int weekBeforMark = tStartMarketing - 1;  
        sumCost = -1*sTF[iTF]*accu(feedWeek.rows(0,weekBeforMark-1)); // we should have "weekBeforMark-1" for the Cpp array!
        return(sumCost);        
      }else{
          for(int c=0; c<nt; c++) {
          if(c>=limit)
            sumReveneue = sumReveneue + rewCull[iTP](t-1,c) - sTF[iTF]*feedCull(t-1,c);
          else sumCost = sumCost + sTF[iTF]*feedWeek(t-1,c);
         }
      return( sumReveneue - sumCost );        
      }      
   }  
// ==========================================================================================

void HMDP2::CalcTransPrTSP() {   // calc values prTSP[s][iTPt][iSPt][iTP][iSP]
   cpuTime.Reset(0); cpuTime.StartTime(0);
   int s, t, iTPt, iSPt, iTP, iSP;
   arma::vec lower(2), upper(2), mt(2);
    
   for(s = 1; s<lastStage; s++){
      for(iTPt=0; iTPt<sizeSTP; iTPt++){
         for(iSPt=0; iSPt<sizeSSP; iSPt++){
            mt[0] = dTP(iTPt,0); mt[1] = dSP(iSPt,0);
            for(iTP=0; iTP<sizeSTP; iTP++){
               for(iSP=0; iSP<sizeSSP; iSP++){
                  lower[0] = dTP(iTP,1); lower[1] = dSP(iSP,1);
                  upper[0] = dTP(iTP,2); upper[1] = dSP(iSP,2);
                  t = s + tStartMarketing - 2;
                  prTSP[s][iTPt][iSPt][iTP][iSP] = dlm.logTransPrTSP(s,t,lower,upper,mt);
               }
            }
         }
      }
   }
    Rcout << "Time for calculating prTSP: " << cpuTime.StopAndGetTotalTimeDiff(0) << endl;
}

// ==========================================================================================

void HMDP2::CalcTransPrSF() {   // calc values prTSF[s][iSFt][iSF]
   cpuTime.Reset(0); cpuTime.StartTime(0);
   int s, t, iSFt, iSF;
   double lower, upper, mt;
    
   for(s = 1; s<lastStage; s++){
         for(iSFt=0; iSFt<sizeSSF; iSFt++){
            mt = dSF(iSFt,0);
               for(iSF=0; iSF<sizeSSF; iSF++){
                  lower = dSF(iSF,1);
                  upper = dSF(iSF,2);
                  t = s + tStartMarketing - 2;
                  prSF[s][iSFt][iSF] = dlm.logTransPrSF(s,t,lower,upper,mt);
               }
            }
         }
    Rcout << "Time for calculating prTF: " << cpuTime.StopAndGetTotalTimeDiff(0) << endl;
}

// ==========================================================================================

void HMDP2::CalcTransPrSPi() {   // prSPi[s][iSPit][iSPi]
   cpuTime.Reset(0); cpuTime.StartTime(0);
   int s, t, iSPit, iSPi;
   double lower, upper, mt;
    
   for(s = 1; s<lastStage; s++){
      for(iSPit=0; iSPit<sizeSSPi; iSPit++){
         mt = dSPi(iSPit,0);
         for(iSPi=0; iSPi<sizeSSPi; iSPi++){
            lower = dSPi(iSPi,1);
            upper = dSPi(iSPi,2);
            t = s + tStartMarketing - 2;
            prSPi[s][iSPit][iSPi] = dlm.logTransPrSPi(s,t,lower,upper,mt);
         }
      }
   }
    Rcout << "Time for calculating PrSPi: " << cpuTime.StopAndGetTotalTimeDiff(0) << endl;
}


// ==========================================================================================

void HMDP2::CalcTransPrTermTP() {   // calc values prTermTSP[s][iTPt][iSPt][iTP]
   cpuTime.Reset(0); cpuTime.StartTime(0);
   int s, t, iTPt, iSPt, iTP;
   arma::vec mt(2);
   double lower, upper;
    
   for(s = 2; s<=lastStage; s++){
      for(iTPt=0; iTPt<sizeSTP; iTPt++){
         for(iSPt=0; iSPt<sizeSSP; iSPt++){
            mt[0] = dTP(iTPt,0); mt[1] = dSP(iSPt,0);
            for(iTP=0; iTP<sizeSTP; iTP++){
                  lower = dTP(iTP,1);
                  upper = dTP(iTP,2); 
                  t = s + tStartMarketing - 2;
                  prTermTP[s][iTPt][iSPt][iTP] = dlm.logTransPrTermTP(s,t,lower,upper,mt);
               }
            }
         }
      }
    Rcout << "Time for calculating prTermTP: " << cpuTime.StopAndGetTotalTimeDiff(0) << endl;
}

// ==========================================================================================

void HMDP2::CalcTransPrTermTF() {   // calc values prTermTF[s][iTFt][iSFt][iTF]
   cpuTime.Reset(0); cpuTime.StartTime(0);
   int s, t, iTFt, iSFt, iTF;
   double lower, upper, mt;
    
   for(s = 2; s<=lastStage; s++){
      for(iTFt=0; iTFt<sizeSTF; iTFt++){
         for(iSFt=0; iSFt<sizeSSF; iSFt++){
            mt = dSF(iSFt,0);
            for(iTF=0; iTF<sizeSTF; iTF++){
                  lower = dTF(iTF,1) - dTF(iTFt,0); 
                  upper = dTF(iTF,2) - dTF(iTFt,0); 
                  t = s + tStartMarketing - 2;
                  prTermTF[s][iTFt][iSFt][iTF] = dlm.logTransPrTermTF(s,t,lower,upper,mt);
               }
            }
         }
      }
    Rcout << "Time for calculating prTermTF: " << cpuTime.StopAndGetTotalTimeDiff(0) << endl;
}

// ==========================================================================================

void HMDP2::CalcTransPrTermPi() {   // prTermPi[s][iSPit][iSPi]
   cpuTime.Reset(0); cpuTime.StartTime(0);
   int s, t, iSPit, iSPi;
   double lower, upper, mt;
    
   for(s = 2; s<=lastStage; s++){
      for(iSPit=0; iSPit<sizeSSPi; iSPit++){
         mt = dSPi(iSPit,0);
         for(iSPi=0; iSPi<sizeSSPi; iSPi++){
            lower = dSPi(iSPi,1);
            upper = dSPi(iSPi,2);
            t = s + tStartMarketing - 2;
            prTermPi[s][iSPit][iSPi] = dlm.logTransPrTermPi(s,t,lower,upper,mt);
         }
      }
   }
    Rcout << "Time for calculating PrTermPi: " << cpuTime.StopAndGetTotalTimeDiff(0) << endl;
}
