% 
% created by /documair/ at 14_09_12@08:31 
% 
\name{text2vma} 
% 
\alias{text2vma} 
\alias{vma2text} 
% 
\title{transforms a character into a vector (or matrix, or array), and conversely} 
% 
\usage{ 
  text2vma(cha,what=rbsa0$vma$v["v"],
           xsep=rbsa0$sep1$v,nat="C",
           monitor=rbsa0$monitor$v) 
  vma2text(x,xsep=rbsa0$sep1$v,
           monitor=rbsa0$monitor$v) 
} 
% 
\examples{ 
  ## 
  ## text2vma 
  #### 
  # vectors 
  text2vma(letters,"c"); 
  text2vma(letters,"C",xsep="e"); 
  text2vma(letters); 
  text2vma(letters,"V"); 
  text2vma(letters,"u"); 
  text2vma(c(LETTERS,letters),rbsa0$vma$v["V"]); 
  text2vma(c("A","a","B","b","C","c"),rbsa0$vma$v["U"]); 
  #### 
  # matrices 
  text2vma(c(1:3,"//",4:6),rbsa0$vma$v["m"]); 
  text2vma(c(1:3,"//",4:6),rbsa0$vma$v["M"]); 
  text2vma(c(LETTERS[1:3],"//",1:3,"//",4:6),rbsa0$vma$v["n"]); 
  text2vma(c(LETTERS[1:3],"//",1:3,"//",4:6),"N"); 
  text2vma(c("a",1:3,"//","b",4:6),"o"); 
  text2vma(c(c(LETTERS[1:3],"//","a",1:3,"//","b",4:6)),rbsa0$vma$v["p"]); 
  #### 
  # arrays 
  text2vma(c(2:4,"//",1:24),"a"); 
  text2vma(c(2:4,"//","one","two","//",LETTERS[1:3],"//", 
  letters[1:4],"//",1:24),"A"); 
  text2vma(c(2:4,"//","one","two","//",LETTERS[1:3],"//", 
  letters[1:4],"//",1:24),"A",nat="N"); 
  ## 
  ## vma2text 
  #### 
  # vectors 
  vma2text(letters); 
  x <- letters; names(x) <- LETTERS; 
  xx <- vma2text(x); 
  text2vma(xx[[1]],xx[[2]]); 
  vma2text(character(0)); 
  #### 
  # matrices 
  x <- matrix(1:20,4); 
  vma2text(x); 
  dimnames(x) <- list(letters[1:4],LETTERS[1:5]); 
  vma2text(x); 
  x1 <- matrix(NA,3,0); 
  xx1 <- vma2text(x1); 
  text2vma(xx1[[1]],xx1[[2]]); 
  dimnames(x1) <- list(c("i","ii","iii"),NULL); 
  xx1 <- vma2text(x1); 
  text2vma(xx1[[1]],xx1[[2]]); 
  #### 
  # arrays 
  x <- array(1:24,2:4); 
  vma2text(x); 
  dimnames(x) <- list(1:2,c("i","ii","iii"),c("I","II","III","IV")); 
  vma2text(x,xsep="|||"); 
  x0 <- array(NA,c(3,0,2)); 
  dimnames(x0) <- list(1:3,NULL,c("i","ii")); 
  xx0 <- vma2text(x0); 
  text2vma(xx0[[1]],xx0[[2]]); 
} 
% 
\details{ 
   
   (text2vma) 
  The processing is done in character mode but the result can be 
  transformed into numerical or logical values with the help of argument \samp{nat}. 
  \cr 
  In fact \samp{rbsa0$vma$v} coding is used for the argument \samp{what}. 
  This allows to easily modify the coding. 
  \cr(vma2text) 
  When some dimnames exist, the possible missing 
  ones will be added. 
   
} 
% 
\description{ 
   
   (text2vma) 
  from a \samp{character} vector, returns a vector, or a matrix, or 
  an array of _characters_ with possibly names, or dimames. The information 
  can be supplied in different ways for each of the three possibilities. 
  It is advised to try the proposed examples. 
  \cr (vma2text)  
  from a vector, or a matrix, or 
  an array, builds a \samp{character} vector. More or less the 
  inverse function of \samp{text2vma}. This vector is the first 
  component of a returned list, the second component of the list 
  gives the type (\samp{vector}, \samp{matrix} or \samp{array}) of \samp{x}, 
  the converted object. 
   
} 
% 
\value{ 
   
   (text2vma) 
  a vector or a matrix or an array according to the arguments. 
  \cr (vma2text)  
  a list with two components: \samp{[[1]]} the coded character vector and 
  \samp{[[2]]} the type according to \samp{text2vma}. 
   
} 
% 
 
\arguments{ 
    \item{cha}{ The character to transform.} 
    \item{x}{ The object to transform.} 
    \item{what}{ Indicates which structure to return: either a vector, a matrix or an array.  \cr ~~ \cr For vectors, the possibilities are c/C/u/v/U/V (in fact the content of \samp{rbsa0$vma$v["c"]}, \samp{rbsa0$vma$v["C"]},... but for the sake of the simplicity, the names will be used because they are by default identical to the value; the same will be done for the other types): \cr\samp{c} for a no named character(1); collapsing is done with \samp{rbsa0$sep0$v}. \cr\samp{C} for a no named character() of any length (components are separated with \samp{xsep} which are removed from the result); collapsing \cr\samp{v} or \samp{u} for a no named vector; \cr\samp{V} for a named vector with all names before the values; then an even number of components must be provided. \cr\samp{U} for a named vector with names interlaced with the value (name_i, value_i); then an even number For matrices, the possibilities are m/n/o/p/M/N/O/P: \cr\samp{m} for a no named matrix given by rows, two adjacent rows being separated with \samp{xsep} sequence, introduced as one of the component of \samp{cha}, then for a 2x3 matrix, the length of \samp{cha} will be 6+2 = 8. \cr\samp{n} for a matrix with only the columns names. The expected sequence is the names of columns, then the values as for \samp{m}; then for a 2x3 matrix, the length of \samp{cha} will be 3+1+8=12. \cr\samp{o} for a matrix with only rows named. The expected sequence is name of row, values of rows... Then 2x3 will imply a length of 8+2=10. \cr\samp{p} when names for columns and rows, in a mixed way... Then 2x3 will imply a length of 14. \cr When \samp{M}, \samp{N}, \samp{O} or \samp{P}, the same but the matrix will be transposed after being read; said in another way, the values are given by columns. For arrays, the possibilities are a/A/b/B: \cr\samp{a} for a no named array, the dimensions, \samp{xsep}, the values in the classical order (varying the first index the fastest). 2x3 will give a length of 2+1+6=9. \cr\samp{A} for a dimnamed array, the dimensions, \samp{xsep}, the dimnames of each dimension in the right order also separated and finished with \samp{xsep}. 2x3 will gives a length of 2+1+2+1+3+1+6=16. \cr\samp{b} for a named dimensions array, the dimensions, \samp{xsep}, the names for the dimension in the right order not separated and finished with \samp{xsep}. 2x3 will gives a length of 2+1+2+1+6=12. \cr\samp{B} for a named and dimnamed array, the dimensions, \samp{xsep}, the names for the dimension in the right order not separated and finished with \samp{xsep}, then the dimnames separated before the values. 2x3 will gives a length of\cr (2+1)+(2+1)+(2+1+3+1)+(6)=19. } 
    \item{xsep}{ Character sequence used to separate the character vector into blocks giving information about the structure (see the examples).} 
    \item{nat}{ Nature of the returned structure. Can be \samp{C} for character, \samp{N} for numeric or \samp{L} for logical.} 
    \item{monitor}{List of constants indicating the monitoring choices, see the \samp{rbsa0$monitor$v} provided object as an example.} 
} 
 
% 
\keyword{input/output} 
\keyword{text} 
\keyword{miscellaneous} 
\keyword{miscellaneous} 
\keyword{input/output} 
