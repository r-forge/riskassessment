% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{chain2pre} 
% 
\alias{chain2pre} 
% 
\title{computes the precision of a chain} 
% 
\usage{ 
  chain2pre(chain,corre=FALSE,check=rbmn0$check$v) 
} 
% 
\examples{ 
  chain2pre(rbmn0$chain2$v,check=TRUE); 
} 
% 
\description{ 
   
  returns the precision matrix 
  of a chain, that is the inverse of its 
  variance (correlation) matrix. Much better 
  to use this function that 
  \samp{solve(chain2mn(chain)$gamma)} since 
  exact formulae are applied. 
   
} 
% 
\value{ 
   
  A dimnamed matrix 
   
} 
% 
 
\arguments{ 
    \item{chain}{ The chain object to consider.} 
    \item{corre}{To get the inverse of the correlation matrix instead of.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{chain} 
\keyword{precision--matrix} 
