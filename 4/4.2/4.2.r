library(Rcpp)
sourceCpp(file='demo.cpp')
hello('world')
hello('Conan')
hello('R')

sourceCpp(code='
  #include <Rcpp.h>
  #include <string>  
  
  using namespace std;
  using namespace Rcpp;
  
  //[[Rcpp::export]]
  string hello(string name) {
    cout << "hello "<<name << endl;  
    return name;
  }
')
hello('R2')


a1<-char_type('a')
a1;class(a1)
char_type('bbii')

a2<-int_type(111)
a2;class(a2)
int_type(111.1)

a3<-double_type(111.1)
a3;class(a3)
double_type(111)

a4<-bool_type(TRUE)
a4;class(a4)
bool_type(1)
bool_type(0)

a5<-void_return_type()
a5;class(a5)

a6<-CharacterVector_type(c('abc','12345'))
a6;class(a6)
CharacterVector_type(c('abc',123.5, NA, TRUE))

a7<-StringVector_type(c('abc','12345'))
a7;class(a7)
StringVector_type(c('abc',123.5, NA, TRUE))

a8<-NumericVector_type(rnorm(5))
a8;class(a8)
NumericVector_type(c(rnorm(5),NA,TRUE))

a9<-IntegerVector_type(c(11,9.9,1.2))
a9;class(a9)
IntegerVector_type(c(11,9.9,1.2,NA,TRUE))

a10<-DoubleVector_type(rnorm(5))
a10;class(a10)
DoubleVector_type(c(rnorm(5),NA,TRUE))

a11<-LogicalVector_type(c(TRUE,FALSE))
a11;class(a11)
LogicalVector_type(c(TRUE,FALSE,TRUE,0,-1,NA))

a12<-DateVector_type(c(Sys.Date(),as.Date('2016-10-10')))
a12;class(a12)
DateVector_type(c(Sys.Date(),as.Date('2016-10-10'),NA,TRUE,FALSE))

a13<-DatetimeVector_type(c(Sys.time(),as.POSIXct('2016-10-10')))
a13;class(a13)
DatetimeVector_type(c(Sys.time(),as.POSIXct('2016-10-10'),NA,TRUE,FALSE))

a14<-CharacterMatrix_type(matrix(LETTERS[1:20],ncol=4))
a14;class(a14)

a15<-StringMatrix_type(matrix(LETTERS[1:20],ncol=4))
a15;class(a15)

a16<-NumericMatrix_type(matrix(rnorm(20),ncol=4))
a16;class(a16)

a17<-IntegerMatrix_type(matrix(seq(1,10,length.out = 20),ncol=4))
a17;class(a17)

a18<-LogicalMatrix_type(matrix(c(rep(TRUE,5),rep(FALSE,5),rnorm(10)),ncol=4))
a18;class(a18)

a19<-ListMatrix_type(matrix(rep(list(a=1,b='2',c=NA,d=TRUE),10),ncol=5))
a19;class(a19)

a19<-DataFrame_type(data.frame(a=rnorm(3),b=1:3))
a19;class(a19)

a20<-List_type(list(a=1,b='2',c=NA,d=TRUE))
a20;class(a20)

a21<-Date_type(Sys.Date())
a21;class(a21)
Date_type(Sys.time())

a22<-Datetime_type(Sys.time())
a22;class(a22)
Datetime_type(Sys.Date())

setClass("Person",slots=list(name="character",age="numeric"))
s4<-new("Person",name="F",age=44)
a23<-S4_type(s4)
a23;class(a23)

s3<-structure(2, class = "foo")
a24<-RObject_type(s3)
a24;class(a24)

a25<-RObject_type(s4)
a25;class(a25)

User<-setRefClass("User",fields=list(name="character"))
rc<-User$new(name="u1")
a26<-RObject_type(rc)
a26;class(a26)

a27<-RObject_type(function(x) x+2)
a27;class(a27)

a28<-Environment_type(new.env())
a28;class(a28)

SEXP_type('fdafdaa')
SEXP_type(rc)
SEXP_type(data.frame(a=rnorm(3),b=1:3))
SEXP_type(function(x) x+2)
