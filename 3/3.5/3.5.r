library("jiebaR")

wk = worker()
wk["我是《R的极客理想》图书作者"]
wk["我是R语言的深度用户"]

wk<='另一种符合的语法'
segment( "segment()函数语句的写法" , wk ) 

wk['./idea.txt']

wk = worker()
wk

library(pryr)
otype(wk)
class(wk)

show_dictpath()
dir(show_dictpath())

scan(file="D:/tool/R-3.2.3/library/jiebaRD/dict/jieba.dict.utf8",
          what=character(),nlines=50,sep='\n',
          encoding='utf-8',fileEncoding='utf-8')

scan(file="D:/tool/R-3.2.3/library/jiebaRD/dict/user.dict.utf8",
     what=character(),nlines=50,sep='\n',
     encoding='utf-8',fileEncoding='utf-8')

wk = worker(user='user.utf8')
wk['./idea.txt']

install.packages("devtools")
install.packages("stringi")
install.packages("pbapply")
install.packages("Rcpp")
install.packages("RcppProgress")
library(devtools)
install_github("qinwf/cidian")
library(cidian)

decode_scel(scel = "./17960.scel",cpp = TRUE)

scan(file="./17960.scel_2016-07-21_00_22_11.dict",
     what=character(),nlines=50,sep='\n',
     encoding='utf-8',fileEncoding='utf-8')


wk = worker(stop_word='stop_word.txt')
segment<-wk["我是《R的极客理想》图书作者"]
segment

filter<-c("作者")
filter_segment(segment,filter)

wk = worker()
segment<-wk["我是《R的极客理想》图书作者"]

filter<-c("我","我是","你","它","大家")
filter_segment(segment,filter)

scan(file="D:/tool/R-3.2.3/library/jiebaRD/dict/idf.utf8",
     what=character(),nlines=50,sep='\n',
     encoding='utf-8',fileEncoding='utf-8')

wk = worker()
segment<-wk["R的极客理想系列文章，涵盖了R的思想，使用，工具，创新等的一系列要点，以我个人的学习和体验去诠释R的强大。"]
freq(segment)

keys = worker("keywords",topn=5)
vector_keywords(segment,keys)
