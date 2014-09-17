% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{topo9} 
% 
\alias{topo9} 
% 
\title{checks a topological order} 
% 
\usage{ 
  topo9(nbn) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  topo9(rbmn0$nbn1$v); 
  topo9(rbmn0$adja1$v); 
} 
% 
\details{ 
   
  I am not completely certain about the validity of the 
  proposed algorithm... 
   
} 
% 
\description{ 
   
  checks that the supposed \samp{nbn} has got 
  a topological order. If not return some indications about 
  the inconsistencies. 
   
} 
% 
\value{ 
   
  \samp{character(0)} if true or a \samp{character} containing 
  the nodes involved in some cycle or, descendants of nodes involved 
  in some cycle. 
   
} 
% 
 
\arguments{ 
    \item{nbn}{ The \samp{nbn} object to check.} 
} 
 
% 
\keyword{topological--order} 
