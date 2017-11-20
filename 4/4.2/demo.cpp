#include <Rcpp.h>
#include <string>  

using namespace std;
using namespace Rcpp;

//[[Rcpp::export]]
string hello(string name) {
  cout << "hello "<<name << endl;  
  return name;
}


/*** R
hello('world')
hello('Conan')
*/


