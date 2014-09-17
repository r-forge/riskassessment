% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{bn7nb} 
% 
\alias{bn7nb} 
% 
\title{number of distinct Bayesian networks} 
% 
\usage{ 
  bn7nb(n,label=FALSE,check=rbmn0$check$v) 
} 
% 
\examples{ 
  bn7nb(5,FALSE,TRUE) 
  bn7nb(5,TRUE,TRUE); 
} 
% 
\details{ 
   
  When not labelled nodes, the results were proposed by Sloane in 'the on line 
  encyclopedy of integer sequences' (http://oeis.org/A003087). 
  For labelled nodes, just the application of the recursive formula of Robinson 
  (rounding errors are possible for great values of \samp{n}). 
   
} 
% 
\description{ 
   
  returns the number of distinct Bayesian networks 
  having \samp{n} (labelled or not) nodes. Non labelled nodes means 
  that nodes are exchangeable: \samp{A -> B} is identical to 
  \samp{A <- B}. 
   
} 
% 
\value{ 
   
  Number of Bayesian networks 
   
} 
% 
 
\arguments{ 
    \item{n}{ number of nodes. Must be less or equal to 18.} 
    \item{label}{Indicates if the nodes must be considered as labelled or not.} 
    \item{check}{Must argument be checked?} 
} 
 
% 
\keyword{bnlearn--package} 
