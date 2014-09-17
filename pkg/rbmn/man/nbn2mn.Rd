% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{nbn2mn} 
% 
\alias{nbn2mn} 
% 
\title{computes the joint distribution of a /nbn/} 
% 
\usage{ 
  nbn2mn(nbn,algo=3) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  print8mn(nbn2mn(rbmn0$nbn5$v)); 
} 
% 
\description{ 
   
  Computes the joint distribution of a /nbn/ with three 
  possible algorithms according to \samp{algo}. 
   
} 
% 
\value{ 
   
  the resulting /mn/ object 
   
} 
% 
 
\arguments{ 
    \item{nbn}{The \samp{nbn} object to be converted.} 
    \item{algo}{either \samp{1}: transforming the \samp{nbn} into a \samp{gema} first before getting the \samp{mn} form; or \samp{2}: one variable after another is added to the joint distribution following a topological order; or \samp{3}: expectations and variances are computed  through the differents paths of the DAG.} 
} 
 
% 
\keyword{multinormal} 
\keyword{normal--Bayesian--network} 
