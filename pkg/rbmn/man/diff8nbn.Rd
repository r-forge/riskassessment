% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{diff8nbn} 
% 
\alias{diff8nbn} 
% 
\title{returns a score of the difference between two /nbn/s} 
% 
\usage{ 
  diff8nbn(nbn1,nbn2,type=1,scalar=TRUE,check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  diff8nbn(rbmn0$nbn1$v,rbmn0$nbn1$v,check=TRUE); 
  diff8nbn(rbmn0$nbn1$v,rbmn0$nbn1$v,scalar=FALSE,check=TRUE); 
} 
% 
\details{ 
   
  For \samp{type==1} it is the canonical euclidian difference between 
  all parameters, including the \samp{sigma}. 
  The score to use to measure the differences between two successive 
  estimations is not well established (see the code). 
   
} 
% 
\description{ 
   
  Returns a positive scalar value measuring, in some way, the difference 
  existing within two /nbn/s sharing the same structure.\cr 
   
} 
% 
\value{ 
   
  Either a scalar or a named vector (according to \samp{scalar}). 
   
} 
% 
 
\arguments{ 
    \item{nbn1}{First \samp{nbn} object.} 
    \item{nbn2}{Second \samp{nbn} object.} 
    \item{type}{When 1, the score includes the difference between the sigmas. When -1, sigmas are not taken into account.} 
    \item{scalar}{When \samp{TRUE} the squared norm is returned, if not the vector of difference.} 
    \item{check}{ Must a check of the argument be performed?} 
} 
 
% 
\keyword{discrepancy} 
\keyword{normal--Bayesian--network} 
