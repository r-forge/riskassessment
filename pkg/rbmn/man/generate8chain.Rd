% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{generate8chain} 
% 
\alias{generate8chain} 
% 
\title{generation of a /chain/ /nbn/} 
% 
\usage{ 
  generate8chain(rnn=c(3,7),proo=0.5,rcor=c(-1,1),
                 rmu=c(0,0),rsig=c(0,1),
                 nona=sssform3names(max(rnn)),check=rbmn0$check$v) 
} 
% 
\examples{ 
  set.seed(1234); 
  print8chain(generate8chain(check=TRUE)); 
  print8chain(generate8chain(check=TRUE)); 
  print8chain(generate8chain(rnn=10,rcor=0.5,check=TRUE)); 
  print8chain(generate8chain(rnn=10,rcor=0.5,check=TRUE)); 
} 
% 
\details{ 
   
  Proposed ranges can be a unique value, implying no randomness 
  in the value.\cr 
  Roots are placed according to \samp{proo} probabilities, then collider 
  are placed in between with uniform probability on the possibles nodes. 
   
} 
% 
\description{ 
   
  [randomly] generates a /chain/ /nbn/ for the structure 
  and for the regressions coefficients. 
   
} 
% 
\value{ 
   
  A /chain/ coding list is returned. 
   
} 
% 
 
\arguments{ 
    \item{rnn}{Range of the number of nodes.} 
    \item{proo}{Probabilit[y|ies] that the successive and acceptable nodes be colliders. Can be a vector.} 
    \item{rcor}{Range of the correlations between neighbour nodes.} 
    \item{rmu}{Range of the expectations.} 
    \item{rsig}{Range of the standard deviations.} 
    \item{nona}{Proposed names for the maximum number of nodes, only the necessary first ones will be used.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{chain} 
\keyword{generation} 
\section{Future}{ 
split the function into the generation of the structure and 
the generation of the regression coefficients. 
} 
