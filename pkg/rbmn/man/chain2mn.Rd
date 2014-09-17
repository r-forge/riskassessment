% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{chain2mn} 
% 
\alias{chain2mn} 
% 
\title{computes the distribution of a chain} 
% 
\usage{ 
  chain2mn(chain,order=TRUE,check=rbmn0$check$v) 
} 
% 
\examples{ 
  print8mn(chain2mn(rbmn0$chain1$v,check=TRUE),check=TRUE); 
} 
% 
\description{ 
   
  returns the /mn/ object 
  associated to a /chain/ object. Much better 
  to use this function that the general function 
  \samp{nbn2mn} since closed formulae are applied. 
   
} 
% 
\value{ 
   
  The resulting /mn/ object. Following the 
  convention of \samp{mn} objects, a topological 
  order is given to it. This is necessary to retrieve 
  the associate /nbn/. 
   
} 
% 
 
\arguments{ 
    \item{chain}{ The chain object to consider.} 
    \item{order}{ Must a topological order be imposed?} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{chain} 
\keyword{multinormal} 
\section{Future}{ 
Think about imposing any order for the result... 
} 
