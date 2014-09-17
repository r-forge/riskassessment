% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{gema2nbn} 
% 
\alias{gema2nbn} 
% 
\title{computes a /nbn/ from  a /gema/} 
% 
\usage{ 
  gema2nbn(gema) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  print8nbn(gema2nbn(rbmn0$gema2$v)); 
} 
% 
\details{ 
   
  using general formulae rather a sequential algorithm 
  as done in the original \samp{gema2nbn} implementation. 
   
} 
% 
\description{ 
   
  from a /gema/ object defining a normal 
  Bayesian network, computes the corresponding 
  /nbn/ where each node is defined from its parents. 
   
} 
% 
\value{ 
   
  the corresponding /nbn/. 
   
} 
% 
 
\arguments{ 
    \item{gema}{Initial \samp{gema} object.} 
} 
 
% 
\keyword{generating--matrix} 
\keyword{normal--Bayesian--network} 
