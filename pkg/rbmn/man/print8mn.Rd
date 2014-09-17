% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{print8mn} 
% 
\alias{print8mn} 
% 
\title{standard print function for a /mn/ object.} 
% 
\usage{ 
  print8mn(mn,what="mrsC",ordering=NULL,
           digits=3,printed=TRUE,
           check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  print8mn(rbmn0$mn1$v,check=FALSE); 
  loi <- list(mu=c(D=2,E=4), 
  rho=matrix(1:6,2,dimnames=list(LETTERS[4:5], 
  LETTERS[1:3])), 
  gamma=matrix(c(1,1,1,2),2,dimnames=list(LETTERS[4:5],LETTERS[4:5]))); 
  print8mn(loi,check=FALSE); 
} 
% 
\description{ 
   
  prints a /mn/ object completely 
  or a part of it. 
   
} 
% 
\value{ 
   
  The \samp{mn} is printed or a matrix having \samp{nn x ?} is returned 
  binding which elements precised in the argument \samp{what}. 
   
} 
% 
 
\arguments{ 
    \item{mn}{\samp{mn} object to be printed.} 
    \item{what}{a \samp{character(1)}; when comprising "m" the expectations are printed, "r" the regression coefficient if any, "s" the standard deviations are printed, "C" the correlation matrix is printed, "S" the variance matrix is printed, "P" the precision matrix is printed, "p" the normalized precision matrix is printed.} 
    \item{ordering}{ Nodes are given following the indices of "ordering" if \samp{numeric} or the names if it is \samp{character}. \samp{NULL} means the identity permutation. Repetitions or missing nodes are accepted.} 
    \item{digits}{ when not null, the number of digits for rounding the parameter values.} 
    \item{printed}{ \samp{TRUE} to issue a printing, if not the prepared matrix is returned.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{multinormal} 
\keyword{print} 
