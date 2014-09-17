% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{read8chain} 
% 
\alias{read8chain} 
% 
\title{reads from a text file a /chain/ object.} 
% 
\usage{ 
  read8chain(file,check=rbmn0$check$v) 
} 
% 
\examples{ 
  print8chain(rbmn0$chain1$v); 
  write8chain(rbmn0$chain1$v,file="toto.txt"); 
  print8chain(read8chain("toto.txt")); 
  unlink("toto.txt"); 
} 
% 
\details{ 
   
  The function \samp{sssread8list} is called to read the file so standard 
  tags \samp{<<...>>} and \samp{[[...]]} can be replaced according the 
  user preferences. 
  Comments and more things can be introduced in the file, see 
  the \samp{sssread8list} documentation for full details. 
   
} 
% 
\description{ 
   
  reads from the text file \samp{file} 
  following a precised format an object describing 
  in the natural way a /chain/ that is a normal Bayesian 
  network where all nodes are in simple string.\cr 
  The /chain/ is described with several fields: 
  (i) the sequence of node names in their order, 
  (ii) the names of the roots (at least one). 
  Default first. 
  (iii) the names of the colliders (always in 
  between two roots). Default none. 
  (iv) the conditional expectations of 
  each node (if only one value is given, it is 
  interpreted as common for all nodes). 
  Default 0. 
  (v) the conditional standard deviation 
  of each node (if only one value is given, it is 
  interpreted as common for all nodes). 
  Default 1. 
  (vi) the correlations between neighbour nodes 
  (if only one value is given, it is 
  interpreted as common for all relatonships). 
  Default 0.5.\cr 
  But when a field \samp{marginal} is added to the list, then 
  the expectation and standard deviations of each node are 
  interpreted as being marginal and not conditional. Nevertherless 
  the resulting output is given in conditional status. 
   
} 
% 
\value{ 
   
  a /chain/ object, for the moment a mere list. 
   
} 
% 
 
\arguments{ 
    \item{file}{file to be read.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{chain} 
\keyword{file} 
