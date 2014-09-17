% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{nbn4rmatrix} 
% 
\alias{nbn4rmatrix} 
% 
\title{a /nbn/ from a regression matrix} 
% 
\usage{ 
  nbn4rmatrix(rmatrix) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  print8nbn(nbn4rmatrix(rmatrix4nbn(rbmn0$nbn2$v))); 
} 
% 
\details{ 
   
  \samp{mu}s are put to nought 
   
} 
% 
\description{ 
   
  reverse of \samp{rmatrix4nbn} but the standard 
  deviations must be included. 
   
} 
% 
\value{ 
   
  A /nbn/ object 
   
} 
% 
 
\arguments{ 
    \item{rmatrix}{ The regression coefficient matrix with the standard deviations in the diagonal.} 
} 
 
% 
\keyword{normal--Bayesian--network} 
