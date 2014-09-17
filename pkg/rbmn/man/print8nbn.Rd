% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{print8nbn} 
% 
\alias{print8nbn} 
% 
\title{print function for a /nbn/ object.} 
% 
\usage{ 
  print8nbn(nbn,what="pr",digits=3,ordering=NULL,check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  print8nbn(rbmn0$nbn1$v,check=TRUE); 
  print8nbn(rbmn0$nbn3$v,"pm",order=1:2,check=TRUE) 
} 
% 
\description{ 
   
  prints a /nbn/ object. 
   
} 
% 
\value{ 
   
  Nothing but but \samp{nbn} is printed. 
   
} 
% 
 
\arguments{ 
    \item{nbn}{\samp{nbn} object to be printed.} 
    \item{what}{a \samp{character(1)}; when comprising "p" the name of each node with its parents are given, when comprising "r the formula regression of each node is given with the node, when comprising "m" the model is given.} 
    \item{digits}{ when not null, the number of digits for rounding.} 
    \item{ordering}{ Nodes are given following the indices of "ordering" if \samp{numeric} or the names if it is \samp{character}. \samp{NULL} means the identity permutation. Repetitions or missing nodes are accepted.} 
    \item{check}{ Must a check of the argument be performed?} 
} 
 
% 
\keyword{normal--Bayesian--network} 
\keyword{print} 
\section{Future}{ 
add the option 'model' 
} 
