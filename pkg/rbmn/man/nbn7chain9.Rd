% 
% created by /documair/ at 14_09_16@11:37 
% 
\name{nbn7chain9} 
% 
\alias{nbn7chain9} 
% 
\title{Checks if a given /nbn/ is a /chain/} 
% 
\usage{ 
  nbn7chain9(nbn,order=FALSE,check=rbmn0$check$v) 
} 
% 
\examples{ 
  rbmn0 <- complete8rbmn0(rbmn0); 
  nbn7chain9(rbmn0$nbn1$v,check=TRUE); 
  nbn7chain9(rbmn0$nbn4$v,check=TRUE); 
} 
% 
\description{ 
   
  returns \samp{TRUE} [the order] or 
  \samp{FALSE} [NULL] according that \samp{nbn} 
  is a chain of not [according to \samp{order}]. 
   
} 
% 
\value{ 
   
  A \samp{logical(1)} when \samp{order} si \samp{TRUE} if not 
  the resulting chain order versus NULL. 
   
} 
% 
 
\arguments{ 
    \item{nbn}{ The nbn object to consider.} 
    \item{order}{ When \samp{FALSE} the answer to the question is returned with \samp{TRUE} or \samp{FALSE}.\cr When \samp{TRUE} the chain order of the nodes is returned if it is a /chain/ else \samp{NULL}.} 
    \item{check}{ Must a check of the arguments be performed?} 
} 
 
% 
\keyword{chain} 
\keyword{normal--Bayesian--network} 
