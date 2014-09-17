% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{gema2mn} 
% 
\alias{gema2mn} 
% 
\title{computes a /mn/ from a /gema/} 
% 
\usage{ 
  gema2mn(gema) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  print8mn(gema2mn(rbmn0$gema4$v)); 
} 
% 
\description{ 
   
  from a /gema/ object defining a normal 
  Bayesian network, computes the expectation and 
  variance matrix, that is the associated /mn/ object. 
   
} 
% 
\value{ 
   
  a list with the following components: 
  \samp{mu} and \samp{gamma}. 
   
} 
% 
 
\arguments{ 
    \item{gema}{Initial \samp{gema} object.} 
} 
 
% 
\keyword{generating--matrix} 
\keyword{multinormal} 
