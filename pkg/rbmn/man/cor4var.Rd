% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{cor4var} 
% 
\alias{cor4var} 
% 
\title{returns the correlation matrix from the variance} 
% 
\usage{ 
  cor4var(ma,check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  cor4var(rbmn0$mn4$v$gamma,check=TRUE); 
} 
% 
\details{ 
   
  Zero variances are detected and accepted (all associated correlation 
  coefficients are forced to be zero.>> 
   
} 
% 
\description{ 
   
  returns the correlation matrix from the variance 
  preserving possible variable names 
   
} 
% 
\value{ 
   
  The correlation matrix 
   
} 
% 
 
\arguments{ 
    \item{ma}{ The variance matrix.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{correlation} 
\keyword{variance--matrix} 
