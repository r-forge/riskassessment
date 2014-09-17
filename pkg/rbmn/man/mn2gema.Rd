% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{mn2gema} 
% 
\alias{mn2gema} 
% 
\title{computes a /gema/ from a /mn/} 
% 
\usage{ 
  mn2gema(mn) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  print8gema(mn2gema(rbmn0$mn4$v)); 
} 
% 
\description{ 
   
  proposes generating matrices of a Bayesian network 
  from a /mn/ object defining a multinormal 
  distribution by expectation and variance, 
  assuming that the nodes are in topological order. 
   
} 
% 
\value{ 
   
  a list with the /gema/ components 
  \samp{$mu} and \samp{$li}. 
   
} 
% 
 
\arguments{ 
    \item{mn}{Initial \samp{mn} object.} 
} 
 
% 
\keyword{generating--matrix} 
\keyword{multinormal} 
