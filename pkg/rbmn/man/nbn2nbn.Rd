% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{nbn2nbn} 
% 
\alias{nbn2nbn} 
% 
\title{computes the /nbn/ changing its topological order} 
% 
\usage{ 
  nbn2nbn(nbn,norder) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  print8mn(nbn2mn(rbmn0$nbn1$v,algo=1)); 
  print8mn(nbn2mn(rbmn0$nbn1$v,algo=2)); 
  print8mn(nbn2mn(rbmn0$nbn1$v,algo=3)); 
  print8mn(nbn2mn(nbn2nbn(rbmn0$nbn2$v,c(1,2,4,5,3)))); 
  print8mn(nbn2mn(nbn2nbn(rbmn0$nbn2$v,c(4,1,2,3,5)))); 
} 
% 
\details{ 
   
  Be aware that for the moment, no check is made about the topological 
  order. A non topological order can produce a wrong result! 
   
} 
% 
\description{ 
   
  returns the proposed /nbn/ with a 
  new topological order without modifying 
  the joint distribution of all variables.\cr 
  This allows to directly find regression 
  formulae within the Gaussian Bayesian networks. 
   
} 
% 
\value{ 
   
  The resulting /nbn/. 
   
} 
% 
 
\arguments{ 
    \item{nbn}{ The /nbn/ to transform.} 
    \item{norder}{ The topological order to follow. It can be indicated by names or numbers. When not all nodes are included, the resulting /nbn/ is restricted to these nodes after marginalization.} 
} 
 
% 
\keyword{normal--Bayesian--network} 
\section{Future}{ 
check the topological order 
} 
