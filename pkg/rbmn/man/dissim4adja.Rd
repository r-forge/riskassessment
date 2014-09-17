% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{dissim4adja} 
% 
\alias{dissim4adja} 
% 
\title{Distance of matrix from an adjacency matrix} 
% 
\usage{ 
  dissim4adja(adja,nature="s",check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  dissim4adja(rbmn0$adja2$v,"s"); 
  dissim4adja(rbmn0$adja2$v,"n"); 
} 
% 
\details{ 
   
  The direct dissimilarity (\samp{d(A,B)}, between node \samp{A} 
  and node \samp{B}, is the length of the shortest directed 
  going from \samp{A} to \samp{B}. Dissimilarity between a node 
  and itself is zero; dissimilarity between non linked nodes is 
  \samp{Inf}.\cr 
  When \samp{nature=="s"}, the dissimilarity is symetricized, that is 
  \samp{d(A,B) = min(d(A,B),d(B,A))} is applied for every pair. 
   
} 
% 
\description{ 
   
  returns a dissimilarity matrix between all nodes; 
  the dissimilarity is based on the paths joining 
  every pair of nodes. 
   
} 
% 
\value{ 
   
  the resulting dissimilarity matrix 
   
} 
% 
 
\arguments{ 
    \item{adja}{ The adjacency matrix.} 
    \item{nature}{ must be \samp{"s"} to symetricize the dissimilarity.} 
    \item{check}{Must argument be checked?} 
} 
 
% 
\keyword{adjacency--matrix} 
\keyword{dissimilarity} 
\section{Future}{ 
Think to more possibilities (working on skeleton...) 
} 
