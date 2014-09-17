% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{marginal4chain} 
% 
\alias{marginal4chain} 
% 
\title{returns marginal expectations and standard deviations of a chain} 
% 
\usage{ 
  marginal4chain(chain,check=rbmn0$check$v) 
} 
% 
\examples{ 
  marginal4chain(rbmn0$chain2$v,check=TRUE); 
} 
% 
\description{ 
   
  From a \samp{chain} object 
  returns a list with two components: \samp{$mu} and \samp{$sigma} 
  vectors of marginal expectations and standard deviations (not a /mn/ 
  object since the correlations are lost; for that use \samp{chain2mn}). 
   
} 
% 
\value{ 
   
  a list with the two components \samp{$mu} and \samp{$sigma}. 
   
} 
% 
 
\arguments{ 
    \item{chain}{ the \samp{chain} object to be considered.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{chain} 
\keyword{distribution} 
