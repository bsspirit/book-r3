library(magrittr)

set.seed(1)
n1<-rnorm(10000)
n2<-abs(n1)*50
n3<-matrix(n2,ncol = 100)
n4<-round(rowMeans(n3))
hist(n4%%7)

set.seed(1)
hist(round(rowMeans(matrix(abs(rnorm(10000))*50,ncol=100)))%%7)

set.seed(1)
rnorm(10000) %>%
  abs %>% `*` (50)  %>%
  matrix(ncol=100)  %>%
  rowMeans %>% round %>% 
  `%%`(7) %>% hist
  
set.seed(1)
rnorm(10000) %>%
  abs %>% `*` (50)  %>%
  matrix(ncol=100)  %>%
  rowMeans %>% round %>% 
  `%%`(7) %T>% hist %>% sum

set.seed(1)
data.frame(x=1:10,y=rnorm(10),z=letters[1:10]) %$% .[which(x>5),]

set.seed(1)
df<-data.frame(x=1:10,y=rnorm(10),z=letters[1:10])
df[which(df$x>5),]

set.seed(1)
x<-rnorm(100) %>% abs %<>% sort %>% head(10);x

set.seed(1)
x<-rnorm(100) %<>% abs %>% sort %>% head(10);x

set.seed(1)
x<-rnorm(100)
x %<>% abs %>% sort %>% head(10)
x %>% abs %<>% sort %>% head(10)
x %>% abs %>% sort %<>% head(10)

set.seed(1)
rnorm(10) %>% `*`(5) %>% `+`(5) 

set.seed(1)
rnorm(10) %>% multiply_by(5) %>% add(5) 

set.seed(1)
rnorm(10)    %>%
  multiply_by(5) %>%
  add(5)         %>%
  { 
    cat("mean:", mean(.), 
        "Var:", var(.), "\n")
    sort(.) %>% head
  }

iris %>%
  (function(x) {
    if (nrow(x) > 2) 
      rbind(head(x, 1), tail(x, 1))
    else x
  })

