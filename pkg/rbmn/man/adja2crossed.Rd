% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{adja2crossed} 
% 
\alias{adja2crossed} 
% 
\title{creates a crossed-adjacency matrix from two ones} 
% 
\usage{ 
  adja2crossed(adj1,adj2,
               nona=as.vector(outer(dimnames(adj1)[[1]],
               dimnames(adj2)[[1]],
               paste,sep="_")),
               check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  print(adja2crossed(rbmn0$adja1$v,rbmn0$adja1$v,check=TRUE)); 
} 
% 
\details{ 
   
  Just two Kronecker products of matrices. 
   
} 
% 
\description{ 
   
  Like crossed4nbn1nbn but at the level of adjacency matrices. Must 
  be much efficient when regression coefficients are not needed. 
   
} 
% 
\value{ 
   
  The resulting crossed adjacency matrix. 
   
} 
% 
 
\arguments{ 
    \item{adj1}{ The first adjacency matrix.} 
    \item{adj2}{ The second adjacency matrix.} 
    \item{nona}{ The node names to give to the crossed /nbn/, the nodes of the \samp{nbn1} varying first.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{adjacency--matrix} 
\keyword{crossed} 
