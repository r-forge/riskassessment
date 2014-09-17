% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{simulate8gmn} 
% 
\alias{simulate8gmn} 
% 
\title{simulates a multinormal vector with varying expectation} 
% 
\usage{ 
  simulate8gmn(loi,cova,nbs,tol=1e-7,check=rbmn0$check$v) 
} 
% 
\examples{ 
  loi <- list(mu=c(D=2,E=4), 
  rho=matrix(1:6,2,dimnames=list(LETTERS[4:5], 
  LETTERS[1:3])), 
  gamma=matrix(c(1,1,1,2),2,dimnames=list(LETTERS[4:5],LETTERS[4:5]))); 
  cova <- matrix(runif(36),12,dimnames=list(NULL,LETTERS[1:3])); 
  print(simulate8gmn(loi,cova,12,check=FALSE)); 
} 
% 
\details{ 
   
  Just a call to the function \samp{simulate8mn}, 
  adding the terms to the expectation due to the regression... 
   
} 
% 
\description{ 
   
  returns a matrix of simulated 
  values with the variable in columns and the 
  simulations in rows. 
   
} 
% 
\value{ 
   
  A matrix of size : \samp{nbs x length(loi$mu)} 
   
} 
% 
 
\arguments{ 
    \item{loi}{ list defining the distribution of the initial vector with \samp{$mu}, its expectation, \samp{$gamma}, its variance matrix and \samp{$rho} a matrix of regression coefficients for the covariables modifying the expectation.} 
    \item{cova}{ Values to give to the covariables. Must be a matrix with \samp{nbs} rows and \samp{ncol(loi$rho)} columns or a vector with \samp{ncol(loi$rho)} values to be used for all simulations (i.e to replace a matrix with identical rows..} 
    \item{nbs}{ number of simulations to return.} 
    \item{tol}{ tolerance value to be transmitted to \samp{mvrnorm}.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{simulation} 
\seealso{ 
\code{\link{simulate8mn}} 
} 
