#include <RcppArmadillo.h>    // we only include RcppArmadillo.h which pulls Rcpp.h in for us
using namespace Rcpp;
using namespace arma;


/**
 * Compute the price of carcass given the carcass weight 
 * @param cW carcass weight.
 * @param p maximum price of 1 kg carcass (weight interval [70-95] )
 * @param s Stage number in the second level of HMDP.
 *  
 * @return carcass price. 
 */
double priceCarcass(const double & cW, const double & p) {
  if(cW<50) return 0;  // interval [0,50) 
  if(50<=cW && cW<60) return 0.1010101010101010082787*(cW-50) + p - 4; // interval [50,60) 
  if(60<=cW && cW<70) return 0.1868686868686868784994*(cW-60) + p - 2;  // interval [60,70) 
  if(70<=cW && cW<95) return p;   // interval [70,95) 
  if(95<=cW && cW<96) return p - 0.2; // interval [95,96) 
  if(96<=cW && cW<97) return p - 0.6; // interval [96,97) 
  if(97<=cW && cW<98) return p - 0.9; // interval [97,98) 
  if(98<=cW && cW<100) return p - 1.2;  // interval [98,100) 
  if(100<=cW) return p - 2.5; // interval [100,Inf) 
  return(0);
}

/**
 * Compute the bonus of leanness persentage in the meat.
 * @param lean Leanness persentage.
 * 
 * @return The bonus of leanness persentage
 */
double priceLeanness(const double & lean) {
  if(lean<50) return -2.2;   // interval [0,50) 
  if(50<=lean && lean<57) return 0.2*(lean - 61);  // interval [50,57) 
  if(57<=lean && lean<65) return 0.1*(lean - 61);  // interval [57,65)   
  if(65<=lean) return 0.4; // interval [65,Inf) 
  return(0);
}

/**
 * Sort a vector based on the order of elements in a given vector. 
 * 
 * @param givenR A vector that should be sorted
 * @param givenI A given sorted vector
 * 
 * @return A sorted vector.
 *   
 */
rowvec sortRow(rowvec  givenR, urowvec givenI){
  rowvec sortR(givenR.size());
  for(unsigned int i=0; i<givenR.size(); i++ ){
      sortR[i] = givenR[ givenI[i] ];
  }
  return(sortR);
}


//' Calculate the reward of seeling and the feed intake of the pigs in an unselected pen using simulation.
//' 
//' @param pigs Number of pigs in the pen.
//' @param samples Number of samples in the simulation.  
//' @param weeks Number of weeks in a production cycle.
//' @param prices Set of possible prices for the carcass.
//' @param V Covariance matrix of the RRM model for the random parameters.
//' @param B Fixed parameters of the RRM model.  
//' @param R Standard deviation of residual error in the RRM model.
//' 
//' @return A list used in R including the weight, growth, leanness, 
//' and the reward of sorted pigs in the pen. 
//' @export
// [[Rcpp::export]]
SEXP SimulatePigs(const int & pigs, const int & samples, const int & weeks, const arma::vec & prices, const arma::mat & V, const arma::mat & B, const arma::mat & R) {
  double tM = 3;    // lead time marketing
  mat X(1,3);   // covariate matrix 
  vec meanW(weeks);
  vec sdW(weeks);
  mat weight(weeks,pigs,fill::zeros);// avg. weight
  mat weightSd(weeks,pigs,fill::zeros);// std.dev. weight
  mat weightTest(weeks,pigs,fill::zeros);// avg. weight
  mat weightTestSd(weeks,pigs,fill::zeros);
  mat lean(weeks,pigs,fill::zeros);  // avg. leanness
  mat leanP(weeks,pigs,fill::zeros);  // avg. leanness price
  mat growth(weeks,pigs,fill::zeros);// avg. growth
  mat growthSd(weeks,pigs,fill::zeros);// avg. growth
  mat feed3(weeks,pigs,fill::zeros); // avg. total feed usage of the first tM days
  mat feed7(weeks,pigs,fill::zeros); // avg. total feed usage of the week
  cube rew(weeks,pigs,prices.size());   // avg. reward of culling the k'th pig
 
  mat randEff(pigs,V.n_cols,fill::zeros);
  cube randEffSample(V.n_cols,pigs,samples);
  
  for(int s=0; s<samples; s++ ){    
    randEff =   randn<mat>(pigs,V.n_cols) * chol(V);
    randEffSample.slice(s) = randEff.t(); 
    }
  
//  double muT, muN; // mu_t and next mean mu_t+1
//  double sdT, sdN;
  mat leanT(samples,pigs);  // leanness  
  mat lWT(samples,pigs);    // live weight
  mat sortLWT(samples,pigs);    // live weight of sorted pigs
  mat lWN(samples,pigs);    // live weight t+1
  mat sortLWN(samples,pigs);    // live weight t+1 of sorted pigs
  mat cWT(samples,pigs);    // carcass weight
  mat sortCWT(samples,pigs);    // carcass weight of sorted pigs
  mat rewT(samples,pigs);   // reward
  mat sumFeed(samples,pigs);  // feed sums
  mat gT(samples,pigs);       // growth t to t+1
  mat sortGT(samples,pigs);       // growth t to t+1 of sorted pigs(based on sortLWT)
  
  umat sortIndexLWT(samples,pigs); // indices of sorted pigs in sortLWT

  // run simulation using only the RRM
  double t = 1;
  X(0,0) = 1; X(0,1) = t; X(0,2) = pow(t,2);
  for(int s=0; s<samples; s++ ){
    lWT.row(s) = as_scalar(X*B) + X*randEffSample.slice(s) + randn<mat>(1,pigs)*sqrt( as_scalar(R) );
  }
  for (int j=1; j<=weeks; ++j) {
    t = (double)j;   
    X(0,1) = t+1; X(0,2) = X(0,2) + 1 + 2*t;  // new values for X_t+1
    for(int s=0; s<samples; s++ ){
      lWN.row(s) = as_scalar(X*B) + X*randEffSample.slice(s) + randn<mat>(1,pigs)*sqrt( as_scalar(R) );
    }
    gT = lWN-lWT;
         
    for(int s=0; s<samples; s++ ){
      sortIndexLWT.row(s) = sort_index( lWT.row(s),"ascend").t();
      sortGT.row(s) = sortRow(gT.row(s),sortIndexLWT.row(s));
    }    
    
    growth.row(t-1) = mean(sortGT);
    growthSd.row(t-1) = stddev(sortGT);
    
    sortLWT = sort(lWT,"ascend",1);
    
    weight.row(t-1) = mean(sortLWT);   
    weightSd.row(t-1) = stddev(sortLWT);
        
    sortGT = sortGT/7;  // daily growth
    sumFeed.fill(0);//zeros();  // initialize
    for (int d=1;d<8;d++) { // days in week
      //uvec q1 = find(lWT < 0);bool neg = any(any(lWT < 0) ); lWT.elem(q1).print("lWT:");
      sumFeed = sumFeed + 1.549*sortGT + 0.044*pow(sortLWT,0.75);
      if (d==tM) {
        // carcass weight
        sortCWT = 0.84*sortLWT-5.89 + randn<mat>(samples,pigs)*1.4; 
        // leanness
        leanT = (-30*(sortGT-0.8571429))/4 + 61;   // assume constant growth during the week
        lean.row(t-1) = mean(leanT);  //leanT.print("leanT:");
        //sumFeed.print("\n3:");
        feed3.row(t-1) = mean(sumFeed);
      }
      sortLWT = sortLWT + sortGT;
      sortLWT.elem( find(sortLWT < 0) ).zeros();  // set negative to zero - hack to not get negative weights for very rare samples
    }
    //sumFeed.print("\n7:");
    feed7.row(t-1) = mean(sumFeed);
    // reward
    for(unsigned int i=0; i<leanT.n_elem; ++i) {
      leanT[i] = priceLeanness(leanT[i]);
    }
    leanP.row(t-1) = mean(leanT);  //leanT.print("leanT:");
    //leanT.print("pLeanT:"); 
    for (unsigned int p=0; p<prices.size();++p) {
      for(unsigned int i=0; i<leanT.n_elem; ++i) {
        rewT[i] = priceCarcass(sortCWT[i],prices[p]);
      }
      //rewT.print("pRewT:");
      rewT = sortCWT % (leanT + rewT);
      rew.slice(p).row(t-1) = mean(rewT);
    }
    lWT=lWN;
  }
  
  
  return List::create( 
    //_["V"]  = V, 
    _["prices"] = prices,
    _["growth"] = growth,
    _["growthSd"] = growthSd,
    _["lean"] = lean,
    _["leanP"] = leanP,
    _["weightSd"] = weightSd,
    _["weight"] = weight,
    _["feed3"] = feed3,
    _["feed7"] = feed7,
    _["rew"] = rew
  ) ;
}

