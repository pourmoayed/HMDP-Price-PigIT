#ifndef BASICDT_HPP
#define BASICDT_HPP

//#include <iostream>
//#include <string>
#include <sstream>
#include <vector>
using namespace std;

// Basic datatypes and definitions
//-----------------------------------------------------------------------------

#ifndef NULL
#define NULL 0
#endif

#define MIN(x,y) ( (x) < (y) ? (x) : (y) )  ///< If x smaller than y then return x, otherwise return y
#define MAX(x,y) ( (x) > (y) ? (x) : (y) )  ///< If x larger than y then return x, otherwise return y

typedef double flt;                 ///< A floating number datatype.
typedef double* fltPtr;             ///< A floating number pointer.
typedef unsigned int idx;           ///< A datatype for storing array indexes etc. Note don't make it short since also used as index for states! //unsigned since then have to cast!!
typedef idx* idxPtr;                ///< Pointer to idx.
typedef unsigned short int uSInt;   ///< Unsigned short integer.
typedef unsigned int uInt;          ///< Unsigned long integer
typedef int* intPtr;


const flt INF=18000000000000000.0;      ///< Infinity (or values above).
const uInt INFINT = 1000000000;         ///< Infinity integer.
const flt PRECISION = 1e-10;  ///< used for comparison floats
//const int FALSE = 0;
//const int TRUE = 1;

/** Global function for comparing two floats. Assume equal if their difference
* if less than PRECISION.
* \return True if equal.
*/
inline bool Equal(flt n1,flt n2) {
    return ((n2-PRECISION)<=n1 && n1<=(n2+PRECISION));
};

/** Global function for converting a number to a string */
template <typename T>
std::string inline ToString(T t) {
    std::ostringstream s;
    s << t;
    return s.str();
};

/** Global function for converting a string
 \param t The variable of the result.
 \param s The string.
 \param *f One of std::hex, std::dec or std::oct.
 \return 0 if failed and 1 otherwise.
 Example

 if(from_string<float>(f, std::string("123.456"), std::dec))
  {
    std::cout << f << std::endl;
  }
  else
  {
    std::cout << "from_string failed" << std::endl;
  }
 */
template <typename T>
bool inline from_string(T& t,
                 const std::string& s,
                 std::ios_base& (*f)(std::ios_base&))
{
  std::istringstream iss(s);
  return !(iss >> f >> t).fail();
};


/** convert a vector to a comma separated string. */
template <typename T>
string inline vec2String(const vector<T>& v) {
    std::ostringstream s;
    s << "(";
    for (idx i=0; i<v.size()-1; ++i) s << v[i] << ",";
    s << v[v.size()-1] << ")";
    return s.str();
}



/** Global function for converting a flt to a string */
/*inline string flt2String(const flt i)
{
  ostringstream stream;
  stream << i;
  return stream.str();
};*/
//-----------------------------------------------------------------------------
//#define null 0
//-----------------------------------------------------------------------------

/** Compute the pow function fast based on 
 * the codes in \url{http://martin.ankerl.com/2012/01/25/optimized-approximative-pow-in-c-and-cpp/}.
 *   
 */

inline double fastPow(double a, double b) {
  union {
    double d;
    int x[2];
  } u = { a };
  u.x[1] = (int)(b * (u.x[1] - 1072632447) + 1072632447);
  u.x[0] = 0;
  return u.d;
}


/** Find the indices of a given policy matrix (matPolicy) based on the given values. 
 *   
 */

inline int findIndice(arma::mat  matPolicy, int  tp, int  sp, int  tf, int  sf, int  spi, int  n, int  s){
  
// arma::uvec q=find( matPolicy.col(1)==tp && matPolicy.col(2)==sp 
//  && matPolicy.col(3)==tf  && matPolicy.col(4)==sf && matPolicy.col(5)==spi  
//  && matPolicy.col(6)==n && matPolicy.col(7)==s);
//  return(q[0]);
      
  for (unsigned int i=0; i<matPolicy.n_rows; i++){    
    if( (matPolicy(i,1)==tp) & (matPolicy(i,2)==sp) & (matPolicy(i,3)==tf) & (matPolicy(i,4)==sf)
    & (matPolicy(i,5)==spi) & (matPolicy(i,6)==n) & (matPolicy(i,7)==s) ){
      return (i);      
    }    
  }
  return(-1);
}

#endif
