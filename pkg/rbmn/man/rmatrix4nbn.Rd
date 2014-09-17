% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{rmatrix4nbn} 
% 
\alias{rmatrix4nbn} 
% 
\title{regression matrix of a /nbn/} 
% 
\usage{ 
  rmatrix4nbn(nbn,stdev=TRUE) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  rmatrix4nbn(rbmn0$nbn2$v); 
  (rmatrix4nbn(rbmn0$nbn2$v,FALSE)>0)*1; 
} 
% 
\description{ 
   
  returns a dimnamed matrix 
  indicating with \samp{rho} an arc from row to column nodes 
  (0 everywhere else) where \samp{rho} is the regression 
  coefficient. Also conditional standard deviations can be 
  introduced as diagonal elements but \samp{mu} coefficient are 
  lost... It is advisable to normalize the /nbn/ first. 
   
} 
% 
\value{ 
   
  A dimnamed matrix 
   
} 
% 
 
\arguments{ 
    \item{nbn}{ The initial \samp{nbn} object.} 
    \item{stdev}{Indicates if the standard deviations must placed in the diagonal positions.} 
} 
 
% 
\keyword{normal--Bayesian--network} 
