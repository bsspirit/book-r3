library(stringr)

str_c('a','b')
str_c('a','b',sep='-')
str_c(c('a','a1'),c('b','b1'),sep='-')

str_c(head(letters), collapse = "")
str_c(head(letters), collapse = ", ")
str_c('a','b',collapse = "-")
str_c(c('a','a1'),c('b','b1'),collapse='-')

paste(head(letters), collapse = "")
paste(head(letters), collapse = ", ")
paste('a','b',collapse = "-")
paste(c('a','a1'),c('b','b1'),collapse='-')

str_c(c("a", NA, "b"), "-d")
paste(c("a", NA, "b"), "-d")

str_c('a','b')
paste('a','b',sep='')

str_c(head(letters), collapse = "")
paste(head(letters), collapse = "")

str_c(c("a", NA, "b"), "-d")
paste(c("a", NA, "b"), "-d")

str_trim("  left space\t\n",side='left')
str_trim("  left space\t\n",side='right')
str_trim("  left space\t\n",side='both')
str_trim("\nno space\n\t")

str_pad("conan", 20, "left")
str_pad("conan", 20, "right")
str_pad("conan", 20, "both")
str_pad("conan", 20, "both",'x')

val <- c("abca4", 123, "cba2")
str_dup(val, 2)
str_dup(val, 1:3)

txt<-'R语言作为统计学一门语言，一直在小众领域闪耀着光芒。直到大数据的爆发，R语言变成了一门炙手可热的数据分析的利器。随着越来越多的工程背景的人的加入，R语言的社区在迅速扩大成长。现在已不仅仅是统计领域，教育，银行，电商，互联网….都在使用R语言。'
cat(str_wrap(txt), "\n")
cat(str_wrap(txt, width = 40), "\n")
cat(str_wrap(txt, width = 60, indent = 2), "\n")
cat(str_wrap(txt, width = 60, exdent = 2), "\n")
cat(str_wrap(txt, width = 10, exdent = 4), "\n")

txt <- "I am Conan."
str_sub(txt, 1, 4)
str_sub(txt, end=6)
str_sub(txt, 6)
str_sub(txt, c(1, 4), c(6, 8))
str_sub(txt, -3)
str_sub(txt, end = -3)

x <- "AAABBBCCC"
str_sub(x, 1, 1) <- 1; x
str_sub(x, 2, -2) <- "2345"; x

str_count('aaa444sssddd', "a")
fruit <- c("apple", "banana", "pear", "pineapple")
str_count(fruit, "a")
str_count(fruit, "p")
str_count(fruit, "e")
str_count(fruit, c("a", "b", "p", "p"))
str_count(c("a.", ".", ".a.",NA), ".")
str_count(c("a.", ".", ".a.",NA), fixed("."))
str_count(c("a.", ".", ".a.",NA), "\\.")

str_length(c("I", "am", "张丹", NA))
str_length(letters)
str_length(factor("abc"))

str_sort(c('1',1,2,'11'), locale = "en")
str_sort(letters,decreasing=TRUE)
str_sort(c('你','好','粉','丝','日','志'),locale = "zh")

str_sort(c(NA,'1',NA),na_last=TRUE)
str_sort(c(NA,'1',NA),na_last=FALSE)
str_sort(c(NA,'1',NA),na_last=NA)

val <- "abc,123,234,iuuu"
s1<-str_split(val, ",");s1
s2<-str_split(val, ",",2);s2
class(s1)
s3<-str_split_fixed(val, ",",2)
class(s3)

val <- c("abc", 123, "cba")
str_subset(val, "a")
str_subset(val, "^a")
str_subset(val, "a$")

val <- c("I am Conan.", "http://fens.me, ok")
word(val, 1)
word(val, -1)
word(val, 2, -1)

val<-'111,222,333,444'
word(val, 1, sep = fixed(','))
word(val, 3, sep = fixed(','))

val <- c("abca4", 123, "cba2")
str_detect(val, "a")
str_detect(val, "^a")
str_detect(val, "a$")

val <- c("abc", 123, "cba")
str_match(val, "a")
str_match(val, "^a")
str_match(val, "a$")
str_match(val, "[0-9]")
str_match(val, "[0-9]*")
str_match_all(val, "a")
str_match_all(val, "^a")
str_match_all(val, "a$")
str_match_all(val, "[0-9]")
str_match_all(val, "[0-9]*")

val <- c("abc", 123, "cba", NA)
str_replace(val, "[ab]", "-")
str_replace_all(val, "[ab]", "-")
str_replace_all(val, "[a]", "\1\1")
str_replace_na(c(NA,'NA',"abc"),'x')

val <- c("abca", 123, "cba")
str_locate(val, "$")
str_locate(val, "a")
str_locate(val, "e")
str_locate(val, c("a", 12, "b"))
str_locate_all(val, "a")
str_locate_all(val, "[ab]")

val <- c("abca4", 123, "cba2")
str_extract(val, "\\d")
str_extract(val, "[a-z]+")
val <- c("abca4", 123, "cba2")
str_extract_all(val, "\\d")
str_extract_all(val, "[a-z]+")

x <- charToRaw('你好');x
str_conv(x, "GBK")
str_conv(x, "GB2312")
str_conv(x, "UTF-8")
x1 <- "\u5317\u4eac"
str_conv(x1, "UTF-8")
x <- "你好"
y <- charToRaw(x)
rawToChar(y)

val <- "I am conan. Welcome to my blog! http://fens.me"
str_to_upper(val)
str_to_lower(val)
str_to_title(val)

