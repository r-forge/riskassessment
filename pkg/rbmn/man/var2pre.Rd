% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{var2pre} 
% 
\alias{var2pre} 
% 
\title{returns the precision matrix from the variance} 
% 
\usage{ 
  var2pre(ma,check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  var2pre(rbmn0$mn4$v$gamma,check=FALSE); 
} 
% 
\details{ 
   
  Non full rank matrices are accepted, a generalized inverse 
  is returned and a warning is issued. 
   
} 
% 
\description{ 
   
  returns the precision matrix from the variance 
  preserving possible variable names 
   
} 
% 
\value{ 
   
  The precision matrix 
   
} 
% 
 
\arguments{ 
    \item{ma}{ The variance matrix.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{precision--matrix} 
\keyword{variance--matrix} 
