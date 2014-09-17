% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{nbn2chain} 
% 
\alias{nbn2chain} 
% 
\title{transforms a /nbn/ into a /chain/} 
% 
\usage{ 
  nbn2chain(nbn,check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  print8chain(nbn2chain(rbmn0$nbn2$v,check=TRUE),check=TRUE); 
} 
% 
\details{ 
   
  It is advised to use \samp{nbn7chain9} before 
  calling this function. 
   
} 
% 
\description{ 
   
  returns the chain obtained 
  from \samp{nbn} which is supposed to a chain. 
  If it is not a chain, an error is issued. 
   
} 
% 
\value{ 
   
  The resulting chain 
   
} 
% 
 
\arguments{ 
    \item{nbn}{ The /nbn/ object to consider.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{chain} 
\keyword{normal--Bayesian--network} 
