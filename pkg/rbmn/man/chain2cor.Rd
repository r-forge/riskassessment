% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{chain2cor} 
% 
\alias{chain2cor} 
% 
\title{computes the correlation matrix of a chain} 
% 
\usage{ 
  chain2cor(chain,check=rbmn0$check$v) 
} 
% 
\examples{ 
  chain2cor(rbmn0$chain3$v,check=TRUE); 
} 
% 
\description{ 
   
  returns the correlation 
  matrix of a /chain/ object from closed expressions. 
   
} 
% 
\value{ 
   
  The correlation matrix. It is not 
  sorted to respect a topological order 
  contrary to \samp{chain2mn} function. 
   
} 
% 
 
\arguments{ 
    \item{chain}{ The chain object to consider.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{chain} 
\keyword{correlation} 
