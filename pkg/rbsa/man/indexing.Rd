% 
% created by /documair/ at 14_09_12@08:31 
% 
\name{indexing} 
% 
\alias{indexing} 
\alias{ijk2n} 
\alias{n2ijk} 
% 
\title{transforms a series of indices into a unique index, and conversely} 
% 
\usage{ 
  ijk2n(ii,di,monitor=rbsa0$monitor$v) 
  n2ijk(n,di,monitor=rbsa0$monitor$v) 
} 
% 
\examples{ 
  ## 
  ## ijk2n 
  for (i1 in 1:4) { for (i2 in 1:2) { for (i3 in 1:3) { 
  print(c(i1,i2,i3,ijk2n(c(i1,i2,i3),c(4,2,3)))); 
  }}} 
  glo <- cbind(rep(1:4,each=6),rep(rep(1:3,each=2),4),rep(1:2,12)); 
  print(cbind(glo,ijk2n(glo,c(4,3,2)))); 
  ## 
  ## n2ijk 
  for (n in 1:24) { 
  print(c(n,n2ijk(n,c(4,2,3)))); 
  } 
  print(cbind(1:24,n2ijk(1:24,c(4,3,2)))); 
} 
% 
\details{ 
   
  When \samp{ii} is a matrix, this is equivalent to call the function 
  repeatedly for each row.\cr 
  \samp{length(ii)} (or \samp{ncol(ii)}) is supposed to be equal to \samp{length(di)}. 
   
} 
% 
\description{ 
   
   (ijk2n) 
  Probably, it is worth running the examples before 
  trying to understand the following explanation!\cr 
  \samp{ii[k]} (or \samp{ii[i,k]} is supposed to be a value in \samp{1:di[k]}, 
  this function returns the global index obtained by 
  running the nested loops \samp{ii[k]} (or \samp{ii[i,k]}).\cr 
  The reverse computation is done by \samp{n2ijk} 
  \cr (n2ijk)  
  From a global index obtained by 
  running the nested loops according to \samp{di} returns the matrix 
  \samp{ii} each row of which is the corresponding value in \samp{1:di[k]}. 
  \cr The reverse computation is done by \samp{ijk2n} 
   
} 
% 
\value{ 
   
   (ijk2n) 
  a positive integer (or a vector of length \samp{nrow(ii)}. 
  \cr (n2ijk)  
  a matrix of dimension \samp{c(length(n),length(di))}. 
   
} 
% 
 
\arguments{ 
    \item{ii}{vector of integers or a matrix of them.} 
    \item{di}{vector of integers giving the dimensions of each sub-index.} 
    \item{n}{an integer vector with values in between \samp{1} and \samp{prod(di)}.} 
    \item{monitor}{List providing the monitoring constants, see \samp{rbsa0$monitor$v} to know the contents.} 
} 
 
% 
\keyword{indexing} 
\keyword{indexing} 
\keyword{indexing} 
