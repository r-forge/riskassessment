% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{rev8chain} 
% 
\alias{rev8chain} 
% 
\title{reverses the nodes of a chain} 
% 
\usage{ 
  rev8chain(chain,check=rbmn0$check$v) 
} 
% 
\examples{ 
  print8chain(rbmn0$chain2$v,check=TRUE); 
  print8chain(rev8chain(rbmn0$chain2$v,check=TRUE),check=TRUE); 
} 
% 
\description{ 
   
  returns the chain obtained 
  after reversing its node order 
   
} 
% 
\value{ 
   
  The resulting chain 
   
} 
% 
 
\arguments{ 
    \item{chain}{ The chain object to consider.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{chain} 
