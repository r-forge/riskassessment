% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{simulate8nbn} 
% 
\alias{simulate8nbn} 
% 
\title{simulates from a /nbn/ object} 
% 
\usage{ 
  simulate8nbn(nbn,nbs,cova=NULL,check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  simulate8nbn(rbmn0$nbn1$v,10,check=TRUE); 
} 
% 
\details{ 
   
  Just the sequential simulations of the nodes. 
  The nodes must be in topological order. 
   
} 
% 
\description{ 
   
  returns a matrix of simulated 
  values with the variable in columns and the 
  simulations in rows. 
   
} 
% 
\value{ 
   
  A matrix of size : \samp{nbs x length(nbn)} 
  So if root nodes are imposed they are in the output. 
   
} 
% 
 
\arguments{ 
    \item{nbn}{ The \samp{nbn} object.} 
    \item{nbs}{ number of simulations to return.} 
    \item{cova}{ Possibly imposing some values to a root set of nodes (\samp{NULL} if not). Let \samp{nc} be the number of such nodes. \samp{cova} must be either a \samp{nbsxnc} matrix (with column names associated to the node names) or a named vector (the names being the node names) of the values to give to the nodes. When a vector, all covariate nodes will have identical values, when a matrix the values can change from one simulation to another.} 
    \item{check}{ Must a check of the argument be performed?} 
} 
 
% 
\keyword{normal--Bayesian--network} 
\keyword{simulation} 
\section{Future}{ 
Check and impose the topological order 
} 
