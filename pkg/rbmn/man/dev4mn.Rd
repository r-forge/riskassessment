% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{dev4mn} 
% 
\alias{dev4mn} 
% 
\title{Computes the deviance for a sample of multinormal vector} 
% 
\usage{ 
  dev4mn(Y,EY,VY) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  dev4mn(matrix(runif(3),1),t(rbmn0$mn1$v$mu),rbmn0$mn1$v$gamma); 
} 
% 
\details{ 
   
  When \samp{EY} is a vector with length \samp{ncol(Y)} this 
  supposes that all observations have the same expectation. 
   
} 
% 
\description{ 
   
  Returns the deviance from the \samp{n} observed values of a vector of size \samp{p} (Y), 
  their expectations (EY) and the 
  variance matrix (VY) supposed identical for all vectors; 
  i.e. \samp{-2*log(p(Y))}. 
   
} 
% 
\value{ 
   
  A scalar 
   
} 
% 
 
\arguments{ 
    \item{Y}{Matrix \samp{nxp} of the \samp{n} observed values of length \samp{p}.} 
    \item{EY}{Expectation of \samp{Y} (matrix \samp{nxp} or vector \samp{p}).} 
    \item{VY}{Matrix of the variance of each row of \samp{Y} (matrix \samp{pxp}).} 
} 
 
% 
\keyword{deviance} 
\keyword{multinormal} 
