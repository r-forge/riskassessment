% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{print8chain} 
% 
\alias{print8chain} 
% 
\title{prints a /chain/ object} 
% 
\usage{ 
  print8chain(chain,digits=3,check=rbmn0$check$v) 
} 
% 
\examples{ 
  print8chain(rbmn0$chain1$v,check=TRUE); 
  print8chain(rbmn0$chain2$v,check=TRUE); 
  print8chain(rbmn0$chain3$v,check=TRUE); 
} 
% 
\details{ 
   
  See \samp{nbn2chain} code for some details about the 
  definition of a /chain/. 
   
} 
% 
\description{ 
   
  prints a /chain/ object. 
   
} 
% 
\value{ 
   
  nothing but something is printed 
   
} 
% 
 
\arguments{ 
    \item{chain}{ The \samp{chain} object to print.} 
    \item{digits}{ when not null, the number of digits for rounding the numerical values.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{chain} 
\keyword{print} 
