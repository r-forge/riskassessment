% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{crossed4nbn1nbn} 
% 
\alias{crossed4nbn1nbn} 
% 
\title{creates a crossed-nbn from two /nbn/s} 
% 
\usage{ 
  crossed4nbn1nbn(nbn1,nbn2,
                  we1=rep(1,length(nbn1)),
                  we2=rep(1,length(nbn2)),
                  nona=as.vector(outer(names(nbn1),
                  names(nbn2),paste,sep="_")),
                  check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  print8nbn(crossed4nbn1nbn(rbmn0$nbn1$v,rbmn0$nbn4$v,check=TRUE),check=TRUE); 
} 
% 
\details{ 
   
  The \samp{mu} coefficient is the sum of the two corresponding \samp{mu}s 
  of the generating \samp{nbn}.\cr 
  The \samp{sigma} coefficient is the product of the two corresponding \samp{sigma}s 
  of the generating \samp{nbn}.\cr 
  The regression coefficient are directed inherited from the \samp{nbn} 
  which is duplicated with this arc. 
   
} 
% 
\description{ 
   
  A crossed /nbn/ is a /nbn/ obtained when replacing 
  each node of the first /nbn/ with the second /nbn/ and 
  vice-versa.\cr 
  Let \samp{nn1/nn2} and \samp{na1/na2} be the node and arc 
  numbers of the two \samp{nbn}s, the node number of the 
  crossed \samp{nbn} is \samp{nn1*nn2} and its arc number 
  is \samp{nn1*na2+nn2*na1}.\cr 
  The regression coefficients attributed to the crossed \samp{nbn} 
  are the products of the weights (\samp{we1/we2}) and the regression 
  coefficients of the initial \samp{nbn}s. 
   
} 
% 
\value{ 
   
  The resulting crossed \samp{nbn} object. 
   
} 
% 
 
\arguments{ 
    \item{nbn1}{ The first generating /nbn/.} 
    \item{nbn2}{ The second generating /nbn/.} 
    \item{we1}{ The weight to apply to the nodes of the first generating /nbn/.} 
    \item{we2}{ The weight to apply to the nodes of the second generating /nbn/.} 
    \item{nona}{ The node names to give to the crossed /nbn/, the nodes of the \samp{nbn1} varying first.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{crossed} 
\keyword{normal--Bayesian--network} 
