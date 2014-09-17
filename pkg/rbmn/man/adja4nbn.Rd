% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{adja4nbn} 
% 
\alias{adja4nbn} 
% 
\title{adjacency matrix of a /nbn/} 
% 
\usage{ 
  adja4nbn(nbn,topo=FALSE,check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  adja4nbn(rbmn0$nbn4$v,check=TRUE); 
} 
% 
\description{ 
   
  returns a dimnamed matrix 
  indicating with 1 an arc from row to column nodes 
  (0 everywhere else); i.e. the adjacency matrix. 
   
} 
% 
\value{ 
   
  A dimnamed matrix 
   
} 
% 
 
\arguments{ 
    \item{nbn}{ The initial \samp{nbn} object.} 
    \item{topo}{Must the topological ordering of the nodes be checked?} 
    \item{check}{ Must a check of the argument be performed?} 
} 
 
% 
\keyword{adjacency--matrix} 
\keyword{normal--Bayesian--network} 
