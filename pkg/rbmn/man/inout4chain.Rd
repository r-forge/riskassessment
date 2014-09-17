% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{inout4chain} 
% 
\alias{inout4chain} 
% 
\title{reduces a chain to its inputs and outputs} 
% 
\usage{ 
  inout4chain(chain,check=rbmn0$check$v) 
} 
% 
\examples{ 
  print8chain(inout4chain(rbmn0$chain2$v,check=TRUE),check=TRUE); 
} 
% 
\description{ 
   
  From a \samp{chain} returns the 
  reduced \samp{chain} comprising only inputs 
  (that is root nodes) and outputs (that is 
  colliders and ends which are not roots) 
   
} 
% 
\value{ 
   
  The resulting chain 
   
} 
% 
 
\arguments{ 
    \item{chain}{ The chain object to consider.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{chain} 
