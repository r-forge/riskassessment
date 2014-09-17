% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{chain2nbn} 
% 
\alias{chain2nbn} 
% 
\title{transforms a /chain/ to a /nbn/} 
% 
\usage{ 
  chain2nbn(chain,check=rbmn0$check$v) 
} 
% 
\examples{ 
  print8nbn(chain2nbn(rbmn0$chain2$v,check=TRUE),ordering=names(rbmn0$nbn2$v),check=TRUE); 
} 
% 
\description{ 
   
  From a \samp{chain} object 
  returns the \samp{nbn} translation. 
   
} 
% 
\value{ 
   
  The corresponding \samp{nbn} object. 
   
} 
% 
 
\arguments{ 
    \item{chain}{ the \samp{chain} object to be transformed.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{chain} 
\keyword{normal--Bayesian--network} 
