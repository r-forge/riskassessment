% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{arc7nb4nbn} 
% 
\alias{arc7nb4nbn} 
% 
\title{returns the number(s) of arcs of a /nbn/} 
% 
\usage{ 
  arc7nb4nbn(nbn,each=FALSE,check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  arc7nb4nbn(rbmn0$nbn5$v,check=TRUE); 
} 
% 
\details{ 
   
  Parents associated with a zero regression coefficient 
  are not excluded in the counting. 
   
} 
% 
\description{ 
   
  returns the arc numbers of 
  of a /nbn/ object; either globally or the number 
  of parents of each node. 
   
} 
% 
\value{ 
   
  Either a number or a named vector of numbers (names being the node names). 
   
} 
% 
 
\arguments{ 
    \item{nbn}{ The \samp{nbn} object to consider.} 
    \item{each}{ When \samp{TRUE}, returns a named vector of the number of parents of each node. If not the total number of arcs.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{arc} 
\keyword{normal--Bayesian--network} 
\section{Future}{ 
To complete with a broad description of properties 
returning a list and associating a printing function. 
} 
