% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{condi4joint} 
% 
\alias{condi4joint} 
% 
\title{computes some conditional distribution of a multinormal vector} 
% 
\usage{ 
  condi4joint(mn,par,pour,x2=rep(0,length(pour)),
              check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  print8mn(condi4joint(rbmn0$mn4$v,c("1.1","2.2","1.2","2.1"),NULL,check=FALSE),check=FALSE); 
  print8mn(condi4joint(rbmn0$mn4$v,c("1.1","2.2","1.2","2.1"),"C",0,check=FALSE),check=FALSE); 
  print(condi4joint(rbmn0$mn4$v,c("1.1","2.2","1.2","2.1"),"C",NULL,check=FALSE),check=FALSE); 
} 
% 
\description{ 
   
  returns the expectation and 
  variance of a sub-vector conditionned 
  with another (non overlapping) sub-vector 
  from an initial random vector described 
  by \samp{mn}. 
   
} 
% 
\value{ 
   
  A list:\cr 
  when \samp{x2} provides the values taken by the 
  conditioning part, it is a /mn/ object with its 
  two components: \samp{$mu} for the expectation vector 
  and \samp{$gamma} for the variance matrix.\cr 
  when \samp{x2} is \samp{NULL} the list has got three 
  components: \samp{$mu} for the fixed part of the 
  expectation vector, \samp{$rho} for the regression 
  coefficients to be associated to the non precised 
  \samp{x2} values, varying part of the expectation 
  and \samp{$gamma} for the variance matrix.\cr 
   
} 
% 
 
\arguments{ 
    \item{mn}{ list defining the distribution of the initial vector with \samp{$mu}, its expectation, and \samp{$gamma}, its variance matrix.} 
    \item{par}{ names (or indices) of the sub-vector to give the distribution.} 
    \item{pour}{ names (or indices) of the conditionning sub-vector (can be \samp{NULL} when for non conditionning.} 
    \item{x2}{values to consider for the conditioning sub-vector. When \samp{NULL} the general form is supplied, not a /mn/ object.  } 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{distribution} 
\keyword{distribution} 
