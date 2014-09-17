% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{state4chain} 
% 
\alias{state4chain} 
% 
\title{returns the states of each node of a chain} 
% 
\usage{ 
  state4chain(chain,check=rbmn0$check$v) 
} 
% 
\examples{ 
  state4chain(rbmn0$chain1$v,check=TRUE); 
  state4chain(rbmn0$chain3$v,check=TRUE); 
} 
% 
\description{ 
   
  From a \samp{chain} object 
  returns a named character precising the role of each node: 
  "r" for root, "c" for collider, "t" for transmitter and 
  "l" for leaf. 
   
} 
% 
\value{ 
   
  a character of the states named with node names. 
   
} 
% 
 
\arguments{ 
    \item{chain}{ the \samp{chain} object to be considered.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{chain} 
