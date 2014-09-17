% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{topo4chain} 
% 
\alias{topo4chain} 
% 
\title{returns a topological order of a /chain/ or checks a proposed order.} 
% 
\usage{ 
  topo4chain(chain,check=rbmn0$check$v) 
} 
% 
\examples{ 
  topo4chain(rbmn0$chain2$v,check=TRUE); 
} 
% 
\description{ 
   
  From a \samp{chain} object 
  returns one of the possible topological orders, 
  through a permutation when \samp{is.null(ord)}. 
  If not \samp{ord} must be a proposed order to be 
  checked given as a permutation if \samp{is.numeric(ord)} 
  or a vector of ordered names if \samp{is.character(ord)}. 
   
} 
% 
\value{ 
   
  a permutation vector of the nodes of the /nbn/ 
  or a named character with the nodes not having 
  their parents before them; when it is of 
  length zero this means that the check 
  was successful. 
   
} 
% 
 
\arguments{ 
    \item{chain}{ the \samp{chain} object to be considered.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{chain} 
\keyword{topological--order} 
\section{Future}{ 
Add the 'ord' option! 
} 
