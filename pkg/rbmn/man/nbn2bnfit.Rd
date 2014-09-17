% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{nbn2bnfit} 
% 
\alias{nbn2bnfit} 
% 
\title{transforms a /nbn/ to a /bn.fit/ of /bnlearn/ package} 
% 
\usage{ 
  nbn2bnfit(nbn,onlydag=FALSE,check=rbmn0$check$v) 
} 
% 
\description{ 
   
  returns a \samp{bn.fit} object 
  from a Gaussian \samp{nbn} object of /rbmn/ 
  package. 
   
} 
% 
\value{ 
   
  The resulting \samp{bn.fit} (or \samp{bn}) object. 
   
} 
% 
 
\arguments{ 
    \item{nbn}{ The object to be transformed.} 
    \item{check}{ Must a check of the arguments be performed?} 
    \item{onlydag}{ Indicates if only the DAG must be computed. In that case a /bn/ object of /bnlearn/ is returned.} 
} 
 
% 
\keyword{bnlearn--package} 
\keyword{normal--Bayesian--network} 
