% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{nbn2rr} 
% 
\alias{nbn2rr} 
% 
\title{computes standard matrices from  a /nbn/} 
% 
\usage{ 
  nbn2rr(nbn) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  nbn2rr(rbmn0$nbn1$v); 
} 
% 
\description{ 
   
  from a /nbn/ object defining a normal 
  Bayesian network, returns a list comprising (i) \samp{mm} 
  the vector of the mean of the different nodes when 
  the parents are nought, (ii) \samp{ss} the vector of the conditional 
  standard deviations and (iii) \samp{rr} the matrix of the 
  regression coefficients of the direct parents (\samp{rr[i,j]} contains 
  the regression coefficient of the node \samp{j} for its parents \samp{i} 
  or zero when \samp{i} is not a parent of \samp{j}. 
   
} 
% 
\value{ 
   
  the resulting list with the three components: 
  \samp{mm}, \samp{ss} and \samp{rr}. 
   
} 
% 
 
\arguments{ 
    \item{nbn}{\samp{nbn} object.} 
} 
 
% 
\keyword{normal--Bayesian--network} 
