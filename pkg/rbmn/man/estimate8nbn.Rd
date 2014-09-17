% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{estimate8nbn} 
% 
\alias{estimate8nbn} 
% 
\title{estimating the /nbn/ parameters} 
% 
\usage{ 
  estimate8nbn(nbn,data,check=rbmn0$check$v) 
} 
% 
\examples{ 
  data(boco); 
  rbmn0 <- complete8rbmn0(rbmn0); 
  print8nbn(rbmn0$nbn5$v); 
  print8nbn(estimate8nbn(rbmn0$nbn5$v,boco)); 
} 
% 
\details{ 
   
  No constraints are put on the parameters. 
   
} 
% 
\description{ 
   
  From a /nbn/ to describe the DAG, and a data.frame 
  containing the necessary observations, returns the /nbn/ with 
  all its parameters newly ML estimated. 
   
} 
% 
\value{ 
   
  The resulting /nbn/ with the estimated parameters. 
   
} 
% 
 
\arguments{ 
    \item{nbn}{ The initial /nbn/.} 
    \item{data}{The data frame comprising all /nbn/ nodes.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{estimation} 
\keyword{normal--Bayesian--network} 
