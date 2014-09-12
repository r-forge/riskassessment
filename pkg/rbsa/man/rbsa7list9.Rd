% 
% created by /documair/ at 14_09_12@08:31 
% 
\name{rbsa7list9} 
% 
\alias{rbsa7list9} 
% 
\title{checks whether a list is rbsa-compatible list} 
% 
\usage{ 
  rbsa7list9(lili,monitor=rbsa0$monitor$v) 
} 
% 
\examples{ 
  rbsa7list9(rbsa0$lis1$v); 
  rbsa7list9(list(rbsa0$lis1$v)); 
  rbsa7list9(list(rbsa0$lis1$v,nu=NULL)); 
} 
% 
\description{ 
   
  To be a rbsa-list, a list must satisfy the following two properties: 
  (i) all components and sub-components are named. 
  (ii) all components and sub-components are either a list 
  or a character vector/matrix/array (i.e. vma components); they are 
  the leafs of the rbsa-list.\cr 
  To be handled by \samp{list2file} or \samp{file2list} functions, a list must 
  rbsa-compatible. 
   
} 
% 
\value{ 
   
  \samp{TRUE} or \samp{FALSE} according to the results of the checks. 
   
} 
% 
 
\arguments{ 
    \item{lili}{ The list structure to be checked.} 
    \item{monitor}{ List of monitoring constants, see \samp{rbsa0$monitor$v} to know its structure.} 
} 
 
% 
\keyword{input/output} 
\keyword{list} 
\keyword{rbsa} 
