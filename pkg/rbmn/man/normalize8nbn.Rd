% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{normalize8nbn} 
% 
\alias{normalize8nbn} 
% 
\title{normalizes a /nbn/} 
% 
\usage{ 
  normalize8nbn(nbn,mu=0,sigma=1,check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  print8nbn(normalize8nbn(rbmn0$nbn1$v)); 
} 
% 
\description{ 
   
  returns a \samp{nbn} with a 
  given expectation and variance through 
  a transformation leaving the correlation 
  unchanged. 
   
} 
% 
\value{ 
   
  The transformed \samp{nbn}. 
   
} 
% 
 
\arguments{ 
    \item{nbn}{ The \samp{nbn} object to transform.} 
    \item{mu}{ Imposed expectations. When \samp{NULL} nothing is changed. When of length one, this value is given to all the node expectations. If not the complete vector of expectations.} 
    \item{sigma}{ The same as \samp{mu} but for the standard deviations.} 
    \item{check}{ Must a check of the argument be performed?} 
} 
 
% 
\keyword{normal--Bayesian--network} 
