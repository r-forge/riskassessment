% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{simulate8mn} 
% 
\alias{simulate8mn} 
% 
\title{simulates a multinormal vector} 
% 
\usage{ 
  simulate8mn(mn,nbs,tol=1e-7,check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  print(simulate8mn(rbmn0$mn1$v,12,check=FALSE)); 
} 
% 
\details{ 
   
  Just a call to the basic function \samp{mvrnorm}. 
  Names of the variables are taken from those of 
  \samp{mn$mu}, when these does not exist, standard 
  ones are provided. 
   
} 
% 
\description{ 
   
  returns a matrix of simulated 
  values with the variable in columns and the 
  simulations in rows. 
   
} 
% 
\value{ 
   
  A matrix/data frame of size : \samp{nbs x length(mn$mu)} 
   
} 
% 
 
\arguments{ 
    \item{mn}{ list defining the distribution of the initial vector with \samp{$mu}, its expectation, and \samp{$gamma}, its variance matrix.} 
    \item{nbs}{ number of simulations to return.} 
    \item{tol}{ tolerance value to be transmitted to \samp{mvrnorm}.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{multinormal} 
\keyword{simulation} 
\seealso{ 
\code{\link{simulate8gmn}} 
} 
