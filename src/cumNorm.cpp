#include "cumNorm.h"


//' Univariate cumulative normal (distribution function)
//' 
//' Based on the paper "Better approximations to cumulative normal functions" by 
//' G. West \url{http://www.wilmott.com/pdfs/090721_west.pdf}. The source code
//' has been taken from the authors webpage \url{http://finmod.co.za/research.html}.
//' 
//' @param x Value of quantiles.
//' @param mean The mean.
//' @param sd The standard deviation.
//' @return The distribution function.
//' @author Lars Relund \email{lars@@relund.dk}
//' @export
// [[Rcpp::export]]
SEXP pNorm1D(NumericVector x, double mean, double sd) {
   int n = x.size();
   NumericVector out(n);
   for(int i = 0; i < n; ++i) {
      out[i] = cumNorm1D( (x[i]-mean)/sd );
   }
   return out;
}


//' Bivariate cumulative normal (distribution function) using Armadillo
//' 
//' Based on the paper "Better approximations to cumulative normal functions" by 
//' G. West \url{http://www.wilmott.com/pdfs/090721_west.pdf}. The source code
//' has been taken from the authors webpage \url{http://finmod.co.za/research.html}.
//' 
//' Transform into a bivariate with sd = 1 using Y = 1/sqrt(diag(sigma))(X-mean), i.e.
//' Pr(X<=x) = P(Y<=1/sqrt(diag(sigma))(x-mean)).
//' 
//' @param lower The vector of lower limits (2-dim vector).
//' @param upper The vector of upper limits (2-dim vector).
//' @param mean The mean vector (2-dim vector).
//' @param sigma The covariance matrix (2x2). 
//' @return The distribution function.
//' @author Lars Relund \email{lars@@relund.dk}
//' @export
// [[Rcpp::export]]
double pNorm2D_arma(arma::vec lower, arma::vec upper, arma::vec mean, arma::mat sigma) {
   arma::vec lb = sigma.diag();
   arma::vec ub = sigma.diag();
   double rho;
   
   lb = (lower-mean)/sqrt(lb);
   ub = (upper-mean)/sqrt(ub);
   rho = sigma(1,0)/ ( sqrt( sigma(0,0) ) * sqrt( sigma(1,1) ) ); //modified by Reza
   //Rcout << lb << " " << ub << " " << rho << endl;
   
   double p1 = cumNorm2D(ub[0], ub[1], rho);
   double p2 = cumNorm2D(lb[0], lb[1], rho);
   double p3 = cumNorm2D(ub[0], lb[1], rho);
   double p4 = cumNorm2D(lb[0], ub[1], rho);
   //Rcout << p1 << " " << p2 << " " << p3 << " " << p4 << endl;
   return fmax(0,p1-p3-p4+p2);   // use max since sometimes may be negative
}



//' Bivariate cumulative normal (distribution function)
//' 
//' Based on the paper "Better approximations to cumulative normal functions" by 
//' G. West \url{http://www.wilmott.com/pdfs/090721_west.pdf}. The source code
//' has been taken from the authors webpage \url{http://finmod.co.za/research.html}.
//' 
//' Transform into a bivariate with sd = 1 using Y = 1/sqrt(diag(sigma))(X-mean), i.e.
//' Pr(X<=x) = P(Y<=1/sqrt(diag(sigma))(x-mean)).
//' 
//' @param lower The vector of lower limits (2-dim vector).
//' @param upper The vector of upper limits (2-dim vector).
//' @param mean The mean vector (2-dim vector).
//' @param sigma The covariance matrix (2x2). 
//' @return The distribution function.
//' @author Lars Relund \email{lars@@relund.dk}
//' @export
// [[Rcpp::export]]
double pNorm2D(NumericVector lower, NumericVector upper, NumericVector mean, NumericMatrix sigma) {
   NumericVector lb(2);
   NumericVector ub(2);
   double rho;

   for (int i=0;i<2;i++) {
      lb[i] = (lower[i] - mean[i])/sqrt(sigma(i,i));
      ub[i] = (upper[i] - mean[i])/sqrt(sigma(i,i));
   }
   rho = sigma(1,0)/sqrt(sigma(0,0)*sigma(1,1));
   
   double p1 = cumNorm2D(ub[0], ub[1], rho);
   double p2 = cumNorm2D(lb[0], lb[1], rho);
   double p3 = cumNorm2D(ub[0], lb[1], rho);
   double p4 = cumNorm2D(lb[0], ub[1], rho);
   //Rcout << p1 << " " << p2 << " " << p3 << " " << p4 << endl;
   return fmax(0,p1-p3-p4+p2);   // use max since sometimes may be negative
}

