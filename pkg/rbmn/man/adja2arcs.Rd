% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{adja2arcs} 
% 
\alias{adja2arcs} 
% 
\title{Arc matrix from an adjacency matrix} 
% 
\usage{ 
  adja2arcs(adj,check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  adja2arcs(rbmn0$adja2$v,check=TRUE); 
} 
% 
\description{ 
   
  returns the arc matrix from an adjacency matrix. 
   
} 
% 
\value{ 
   
  a matrix with two columns ("from","to") 
   
} 
% 
 
\arguments{ 
    \item{adj}{ The adjacency matrix.} 
    \item{check}{Must argument be checked?} 
} 
 
% 
\keyword{adjacency--matrix} 
\keyword{arc} 
